module Analysis.ConstantsLiterals where

import Language.C.Syntax.AST
import Language.C.Syntax.Constants (getCInteger)
import Language.C.Data.Node
import Language.C.Data.Ident
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import qualified Data.Map as Map

analyzeConstantsLiteralsIssues :: CTranslUnit -> [Issue]
analyzeConstantsLiteralsIssues ast =
    checkMagicValuesUsed ast
    ++ checkBitMaskingAssuming32bitPts ast
    ++ checkHardCodedAddressValues ast
    ++ checkConstantsUsedForSizeCalcs ast

-- | Flag malloc/calloc/realloc calls with a literal 4 or 8 as size
--   (likely hardcoding a 32-bit or 64-bit pointer width assumption).
checkMagicValuesUsed :: CTranslUnit -> [Issue]
checkMagicValuesUsed ast@(CTranslUnit decls _) =
    concatMap (analyzeDecl checkExpr Map.empty) decls
  where
    checkExpr _env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` ["malloc", "calloc", "realloc"] =
            [ createIssue info Warning MagicValuesUsed
            | any isMagicSize args ]
    checkExpr _ _ = []

    isMagicSize (CConst (CIntConst n _)) = getCInteger n `elem` [4, 8]
    isMagicSize _                           = False

-- | Flag bitwise AND of a pointer-typed value with the literal 0xFFFFFFFF
--   (assumes a 32-bit pointer).
checkBitMaskingAssuming32bitPts :: CTranslUnit -> [Issue]
checkBitMaskingAssuming32bitPts ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv env (CBinary CAndOp l r info) =
        checkSide tenv env l r info ++ checkSide tenv env r l info
    checkExpr _ _ _ = []

    checkSide tenv env ptr mask info =
        let ptrType = resolveTypedef tenv (typeOfExpr env ptr)
        in case mask of
            CConst (CIntConst n _)
                | getCInteger n == 0xFFFFFFFF && isPointer ptrType ->
                    [createIssue info Critical BitMaskingAssuming32bitPts]
            _ -> []

-- | Flag explicit casts of non-zero integer literals to pointer types
--   (hardcoded memory addresses).
checkHardCodedAddressValues :: CTranslUnit -> [Issue]
checkHardCodedAddressValues ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv _env (CCast castDecl inner info) =
        let castTo = resolveTypedef tenv (typeOfDecl castDecl)
        in case (isPointer castTo, inner) of
            (True, CConst (CIntConst n _))
                | getCInteger n /= 0 ->
                    [createIssue info Critical HardCodedAddressValues]
            _ -> []
    checkExpr _ _ _ = []

-- | Flag assignments to size_t (TULong/TUInt) variables from a bare integer
--   literal — a named constant or sizeof expression should be used instead.
checkConstantsUsedForSizeCalcs :: CTranslUnit -> [Issue]
checkConstantsUsedForSizeCalcs ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv env (CAssign CAssignOp lhs (CConst (CIntConst _ _)) info) =
        let lhsType = resolveTypedef tenv (typeOfExpr env lhs)
        in [ createIssue info Warning ConstantsUsedForSizeCalcs
           | isUIntType lhsType ]
    checkExpr _ _ _ = []