module Analysis.TypeSize where 

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import qualified Data.Map as Map


analyzeTypeSizeIssues :: CTranslUnit -> [Issue]
analyzeTypeSizeIssues ast = 
    checkPointerToInt ast
    ++ checkPointerToUInt ast
    ++ checkIntToPointer ast
    ++ checkLongToPointer ast
    ++ checkSizeOfInt ast
    ++ checkSizeOfLong ast
    ++ checkIntAsSizet ast
    ++ checkIntAsPtrdifft ast
    ++ checkUIntAsMemSize ast

-- ---------------------------------------------------------------------------
-- Pointer <-> Int cast checkers
-- ---------------------------------------------------------------------------

-- | Check for (int)ptr casts
checkPointerToInt :: CTranslUnit -> [Issue]
checkPointerToInt ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkCast tenv) Map.empty) decls
  where
    checkCast tenv env (CCast castDecl inner info) =
        let castTo   = resolveTypedef tenv (typeOfDecl castDecl)
            castFrom = resolveTypedef tenv (typeOfExpr env inner)
        in if isIntType' castTo && isPointer castFrom
           then [createIssue info Critical CastPointerToInt]
           else []
    checkCast _ _ _ = []

-- | Check for (unsigned int)ptr casts
checkPointerToUInt :: CTranslUnit -> [Issue]
checkPointerToUInt ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkCast tenv) Map.empty) decls
  where
    checkCast tenv env (CCast castDecl inner info) =
        let castTo   = resolveTypedef tenv (typeOfDecl castDecl)
            castFrom = resolveTypedef tenv (typeOfExpr env inner)
        in if isUIntType castTo && isPointer castFrom
           then [createIssue info Critical CastPointerToUInt]
           else []
    checkCast _ _ _ = []

-- | Check for (int*)x casts where x is an int
checkIntToPointer :: CTranslUnit -> [Issue]
checkIntToPointer ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkCast tenv) Map.empty) decls
  where
    checkCast tenv env (CCast castDecl inner info) =
        let castTo   = resolveTypedef tenv (typeOfDecl castDecl)
            castFrom = resolveTypedef tenv (typeOfExpr env inner)
        in if isPointer castTo && isIntType' castFrom
           then [createIssue info Critical CastIntToPointer]
           else []
    checkCast _ _ _ = []

-- | Check for (int*)x casts where x is a long
checkLongToPointer :: CTranslUnit -> [Issue]
checkLongToPointer ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkCast tenv) Map.empty) decls
  where
    checkCast tenv env (CCast castDecl inner info) =
        let castTo   = resolveTypedef tenv (typeOfDecl castDecl)
            castFrom = resolveTypedef tenv (typeOfExpr env inner)
        in if isPointer castTo && isLongType' castFrom
           then [createIssue info Critical CastLongToPointer]
           else []
    checkCast _ _ _ = []

-- ---------------------------------------------------------------------------
-- sizeof comparisons
-- ---------------------------------------------------------------------------

-- | Check for sizeof(int) == sizeof(void*) comparisons  
checkSizeOfInt :: CTranslUnit -> [Issue]
checkSizeOfInt (CTranslUnit decls _) =
    concatMap (analyzeDecl checkExpr Map.empty) decls
  where
    checkExpr :: TypeEnv -> CExpression NodeInfo -> [Issue]
    checkExpr _ expr = case expr of
        CBinary op left right info
            | op `elem` [CEqOp, CNeqOp] ->
                case (sizeofType left, sizeofType right) of
                    (Just TInt, Just (TPointer _)) ->
                        [createIssue info Critical SizeOfIntIsVoid]
                    (Just (TPointer _), Just TInt) ->
                        [createIssue info Critical SizeOfIntIsVoid]
                    _ -> []
        _ -> []

-- | Check for sizeof(long) == sizeof(void*) comparisons
checkSizeOfLong :: CTranslUnit -> [Issue]
checkSizeOfLong (CTranslUnit decls _) =
    concatMap (analyzeDecl checkExpr Map.empty) decls
  where
    checkExpr :: TypeEnv -> CExpression NodeInfo -> [Issue]
    checkExpr _ expr = case expr of
        CBinary op left right info
            | op `elem` [CEqOp, CNeqOp] ->
                case (sizeofType left, sizeofType right) of
                    (Just TLong, Just (TPointer _)) ->
                        [createIssue info Critical SizeOfLongIsVoid]
                    (Just (TPointer _), Just TLong) ->
                        [createIssue info Critical SizeOfLongIsVoid]
                    _ -> []
        _ -> []

-- | Extract the CType from a sizeof() expression
sizeofType :: CExpression a -> Maybe CType
sizeofType (CSizeofType decl _) = Just (typeOfDecl decl)
sizeofType _                    = Nothing

-- ---------------------------------------------------------------------------
-- Int used where size_t / ptrdiff_t / size expected
-- ---------------------------------------------------------------------------

-- | Flag when an int variable is assigned to a variable declared as size_t
--   (detected by the variable's actual declared type in the TypeEnv)
checkIntAsSizet :: CTranslUnit -> [Issue]
checkIntAsSizet ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAssign tenv) Map.empty) decls
  where
    checkAssign tenv env expr = case expr of
        -- int x; size_t s = x;  (init)
        -- size_t s; s = x;      (assign)
        CAssign CAssignOp lhs rhs info ->
            let lhsType = resolveTypedef tenv (typeOfExpr env lhs)
                rhsType = resolveTypedef tenv (typeOfExpr env rhs)
                mDeclPos = case lhs of
                    CVar (Ident name _ _) _ -> lookupDeclPos env name
                    _                       -> Nothing
            in if isSizetType lhsType && isIntType' rhsType
               then [createIssueWithDecl info mDeclPos Warning UsingIntAsSizet]
               else []
        _ -> []

    -- size_t is typically an unsigned long on x64
    isSizetType TULong = True
    isSizetType TUInt  = True   -- may appear on 32-bit; flag anyway
    isSizetType _      = False

-- | Flag when an int variable is used where ptrdiff_t is expected
checkIntAsPtrdifft :: CTranslUnit -> [Issue]
checkIntAsPtrdifft ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAssign tenv) Map.empty) decls
  where
    checkAssign tenv env expr = case expr of
        CAssign CAssignOp lhs rhs info ->
            let lhsType = resolveTypedef tenv (typeOfExpr env lhs)
                rhsType = resolveTypedef tenv (typeOfExpr env rhs)
                mDeclPos = case lhs of
                    CVar (Ident name _ _) _ -> lookupDeclPos env name
                    _                       -> Nothing
            in if isPtrdifftType lhsType && isIntType' rhsType
               then [createIssueWithDecl info mDeclPos Warning UsingIntAsPtrdifft]
               else []
        _ -> []

    -- ptrdiff_t is typically a signed long on x64
    isPtrdifftType TLong = True
    isPtrdifftType _     = False

-- | Flag when an unsigned int is passed to malloc/calloc/realloc
checkUIntAsMemSize :: CTranslUnit -> [Issue]
checkUIntAsMemSize ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAlloc tenv) Map.empty) decls
  where
    checkAlloc tenv env expr = case expr of
        CCall (CVar (Ident fname _ _) _) args info
            | fname `elem` ["malloc", "calloc", "realloc"] ->
                [ createIssueWithDecl info mDeclPos Warning UsingUIntAsMemSize
                | arg <- args
                , let t = resolveTypedef tenv (typeOfExpr env arg)
                , isIntType' t || isUIntType t
                , let mDeclPos = case arg of
                        CVar (Ident name _ _) _ -> lookupDeclPos env name
                        _                       -> Nothing
                ]
        _ -> []


