{-# LANGUAGE LambdaCase #-}
module Analysis.TypeChecker
  ( CType(..)
  , TypeEnv
  , TypedefEnv
  , StructEnv
  , resolveType
  , resolveBaseType
  , collectDecl
  , collectFunDef
  , buildTypeEnv
  , buildGlobalEnv
  , lookupType
  , lookupDeclPos
  , typeOfExpr
  , typeOfDecl
  , isPointer
  , isIntType'
  , isLongType'
  , isUIntType
  , buildTypedefEnv
  , resolveTypedef
  , buildStructEnv
  , structHasPointer
  , ptrToStructWithPtrs
  , promoteArith
  ) where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import qualified Data.Map.Strict as Map

-- | The resolved type of a C variable
data CType
    = TInt               -- int
    | TUInt              -- unsigned int
    | TShort             -- short
    | TUShort            -- unsigned short
    | TLong              -- long
    | TULong             -- unsigned long
    | TLongLong          -- long long
    | TULongLong         -- unsigned long long
    | TChar              -- char
    | TFloat             -- float
    | TDouble            -- double
    | TVoid              -- void
    | TPointer CType     -- pointer to some type e.g. int*
    | TArray CType       -- array of some type e.g. int[]
    | TStruct String     -- struct by name
    | TUnion String      -- union by name
    | TTypedef String    -- typedef name (unresolved)
    | TUnknown           -- fallback
    deriving (Show, Eq)

-- | Maps variable names to their resolved CType and declaration position
type TypeEnv = Map.Map String (CType, Maybe NodeInfo)

-- | Maps typedef names to their resolved CType
type TypedefEnv = Map.Map String CType

-- | Extract the CType from declaration specifiers + derived declarators
resolveType :: [CDeclarationSpecifier a] -> [CDerivedDeclarator a] -> CType
resolveType specs derived =
    let base = resolveBaseType specs
    in applyDerived base (reverse derived)
  where
    -- Apply derived declarators (pointers, arrays) from outermost in
    applyDerived t [] = t
    applyDerived t (CPtrDeclr _ _ : rest) = applyDerived (TPointer t) rest
    applyDerived t (CArrDeclr {} : rest) = applyDerived (TArray t) rest
    applyDerived t (_ : rest) = applyDerived t rest

-- | Resolve the base type from declaration specifiers
resolveBaseType :: [CDeclarationSpecifier a] -> CType
resolveBaseType specs =
    let typeSpecs = [s | CTypeSpec s <- specs]
    in classifyTypeSpecs typeSpecs

classifyTypeSpecs :: [CTypeSpecifier a] -> CType
classifyTypeSpecs specs
    | hasVoid specs                                          = TVoid
    | hasUnsigned specs && hasLong specs && countLong specs >= 2 = TULongLong
    | hasUnsigned specs && hasLong specs                     = TULong
    | hasUnsigned specs && hasShort specs                    = TUShort
    | hasUnsigned specs                                      = TUInt
    | hasLong specs && countLong specs >= 2                  = TLongLong
    | hasLong specs                                          = TLong
    | hasShort specs                                         = TShort
    | hasInt specs                                           = TInt
    | hasSigned specs                                        = TInt
    | hasChar specs                                          = TChar
    | hasFloat specs                                         = TFloat
    | hasDouble specs                                        = TDouble
    | hasTypedefName specs                                   = TTypedef (getTypedefName specs)
    | hasNamedStruct specs                                   = TStruct (getSUName specs)
    | hasNamedUnion specs                                    = TUnion  (getSUName specs)
    | otherwise                                              = TUnknown
  where
    hasVoid        = any (\case { CVoidType _   -> True; _ -> False })
    hasUnsigned    = any (\case { CUnsigType _  -> True; _ -> False })
    hasSigned      = any (\case { CSignedType _ -> True; _ -> False })
    hasLong        = any (\case { CLongType _   -> True; _ -> False })
    countLong  ss  = length [() | CLongType _ <- ss]
    hasShort       = any (\case { CShortType _  -> True; _ -> False })
    hasInt         = any (\case { CIntType _    -> True; _ -> False })
    hasChar        = any (\case { CCharType _   -> True; _ -> False })
    hasFloat       = any (\case { CFloatType _  -> True; _ -> False })
    hasDouble      = any (\case { CDoubleType _ -> True; _ -> False })
    hasTypedefName = any (\case { CTypeDef _ _  -> True; _ -> False })
    getTypedefName ss = head [n | CTypeDef (Ident n _ _) _ <- ss]
    hasNamedStruct = any (\case
        { CSUType (CStruct CStructTag (Just _) _ _ _) _ -> True; _ -> False })
    hasNamedUnion  = any (\case
        { CSUType (CStruct CUnionTag  (Just _) _ _ _) _ -> True; _ -> False })
    getSUName ss = head
        [ n | CSUType (CStruct _ (Just (Ident n _ _)) _ _ _) _ <- ss ]

-- | Add all variables declared in a CDeclaration into the TypeEnv,
--   recording each declarator's source position.
collectDecl :: CDeclaration NodeInfo -> TypeEnv -> TypeEnv
collectDecl (CDecl specs declrs _) env =
    foldr (addDeclr specs) env declrs
  where
    addDeclr s (Just (CDeclr (Just (Ident name _ _)) derived _ _ ni), _, _) acc =
        Map.insert name (resolveType s derived, Just ni) acc
    addDeclr _ _ acc = acc
collectDecl (CStaticAssert {}) env = env

-- | Build a TypeEnv by walking all compound block items (handles ordering)
buildTypeEnv :: [CCompoundBlockItem NodeInfo] -> TypeEnv -> TypeEnv
buildTypeEnv items env = foldl step env items
  where
    step acc (CBlockDecl decl) = collectDecl decl acc
    step acc _                 = acc

-- | Look up the type of a variable by name
lookupType :: TypeEnv -> String -> CType
lookupType env name = maybe TUnknown fst (Map.lookup name env)

-- | Look up the declaration position of a variable by name
lookupDeclPos :: TypeEnv -> String -> Maybe NodeInfo
lookupDeclPos env name = Map.lookup name env >>= snd

-- | Resolve the type of an expression given the current TypeEnv
typeOfExpr :: TypeEnv -> CExpression a -> CType
typeOfExpr env expr = case expr of
    CVar (Ident name _ _) _     -> lookupType env name
    CCast decl _ _              -> typeOfDecl decl
    CUnary CAdrOp inner _       -> TPointer (typeOfExpr env inner)
    CUnary CIndOp inner _       ->
        case typeOfExpr env inner of
            TPointer t -> t
            _          -> TUnknown
    -- Pointer arithmetic: ptr +/- int yields same pointer type
    CBinary op l r _
        | op `elem` [CAddOp, CSubOp] ->
            case (typeOfExpr env l, typeOfExpr env r) of
                (TPointer _, TPointer _) -> TLong        -- ptr - ptr = ptrdiff_t
                (TPointer t, _)          -> TPointer t   -- ptr + int
                (_, TPointer t)          -> TPointer t   -- int + ptr
                (lt, rt)                 -> promoteArith lt rt
    -- Multiplicative ops never involve pointers; just promote
    CBinary op l r _
        | op `elem` [CMulOp, CDivOp, CRmdOp] ->
            promoteArith (typeOfExpr env l) (typeOfExpr env r)
    -- Bitwise ops: result type follows usual arithmetic promotions
    CBinary op l r _
        | op `elem` [CAndOp, COrOp, CXorOp, CShlOp, CShrOp] ->
            promoteArith (typeOfExpr env l) (typeOfExpr env r)
    -- Comparison and logical ops always produce int
    CBinary op _ _ _
        | op `elem` [CLeOp, CGrOp, CLeqOp, CGeqOp, CEqOp, CNeqOp,
                     CLndOp, CLorOp] -> TInt
    -- Unary arithmetic: +x, -x, ~x preserve type; !x gives int
    CUnary CMinOp inner _       -> typeOfExpr env inner
    CUnary CPlusOp inner _      -> typeOfExpr env inner
    CUnary CCompOp inner _      -> typeOfExpr env inner
    CUnary CNegOp _ _           -> TInt
    -- Assignment expressions have the type of the lhs
    CAssign _ l _ _             -> typeOfExpr env l
    -- Array subscript: dereference the pointer/array type
    CIndex arr _ _              ->
        case typeOfExpr env arr of
            TPointer t -> t
            TArray t   -> t
            _          -> TUnknown
    -- Function call: look up the function name in env to get its return type.
    -- Function declarations and definitions are added to the env by
    -- 'buildGlobalEnv', so e.g. malloc() declared as @void *malloc(size_t)@
    -- will correctly resolve to TPointer TVoid here.
    CCall (CVar (Ident n _ _) _) _ _ -> lookupType env n
    -- Comma expression: type of last operand
    CComma exprs _              -> case exprs of
                                       [] -> TUnknown
                                       _  -> typeOfExpr env (last exprs)
    _                           -> TUnknown

-- | C usual arithmetic conversions (simplified): the higher-ranked type wins.
-- Ranking (ascending): TInt < TUInt < TLong < TULong.  Any unknown operand
-- yields TUnknown so callers can detect that the result is uncertain.
promoteArith :: CType -> CType -> CType
promoteArith TUnknown _       = TUnknown
promoteArith _       TUnknown = TUnknown
promoteArith l r
    | rank l >= rank r = l
    | otherwise        = r
  where
    rank :: CType -> Int
    rank TShort     = 0
    rank TUShort    = 0
    rank TChar      = 0
    rank TInt       = 1
    rank TUInt      = 2
    rank TLong      = 3
    rank TULong     = 4
    rank TLongLong  = 5
    rank TULongLong = 6
    rank TFloat     = 7
    rank TDouble    = 8
    rank _          = 1

-- | Get the CType from a cast declaration
typeOfDecl :: CDeclaration a -> CType
typeOfDecl (CDecl specs declrs _) =
    let derived = case declrs of
                    (Just (CDeclr _ d _ _ _), _, _) : _ -> d
                    _                                    -> []
    in resolveType specs derived
typeOfDecl (CStaticAssert {}) = TUnknown

-- | Record a function definition's name in the TypeEnv, mapped to its
--   return type.  This lets 'typeOfExpr' resolve the result of a call to
--   that function via the new 'CCall' case.
--
--   For @void *myfunc(int n) { … }@ the return type is @TPointer TVoid@.
--   'resolveType' already handles this correctly: the @CFunDeclr@ derived
--   declarator is skipped by the @_ : rest@ fallthrough in 'applyDerived',
--   so only the pointer (and any other) declarators contribute to the type.
collectFunDef :: CFunctionDef NodeInfo -> TypeEnv -> TypeEnv
collectFunDef (CFunDef specs (CDeclr (Just (Ident name _ _)) derived _ _ ni) _ _ _) env =
    Map.insert name (resolveType specs derived, Just ni) env
collectFunDef _ env = env

-- | Build a 'TypeEnv' seeded with every file-scope name in the translation
--   unit: global variable declarations, function prototype declarations, and
--   function definition names (mapped to their return types).  This seed is
--   passed to 'buildFunEnv' so that 'typeOfExpr' can resolve function-call
--   result types (e.g. knowing @malloc@ returns @void *@).
buildGlobalEnv :: [CExternalDeclaration NodeInfo] -> TypeEnv
buildGlobalEnv = foldr addExtDecl Map.empty
  where
    addExtDecl (CDeclExt decl)   env = collectDecl decl env
    addExtDecl (CFDefExt funDef) env = collectFunDef funDef env
    addExtDecl _                 env = env

-- | Predicate helpers used by analysis functions
isPointer :: CType -> Bool
isPointer (TPointer _) = True
isPointer _            = False

-- | True only for signed @int@.  Unsigned int is handled by 'isUIntType'.
isIntType' :: CType -> Bool
isIntType' TInt = True
isIntType' _    = False

-- | True for @long@ or @unsigned long@ (types whose size changes between
--   x86 and LP64).  Does NOT include @long long@ / @unsigned long long@
--   which are always 64-bit on both platforms.
isLongType' :: CType -> Bool
isLongType' TLong  = True
isLongType' TULong = True
isLongType' _      = False

-- | True only for @unsigned int@.  Does NOT include @unsigned long@ — on
--   LP64 systems @unsigned long@ is 64-bit and safe for pointer storage.
isUIntType :: CType -> Bool
isUIntType TUInt = True
isUIntType _     = False

-- ---------------------------------------------------------------------------
-- Typedef environment
-- ---------------------------------------------------------------------------

-- | Build a map from typedef names to their resolved CType by scanning
--   all top-level declarations in the translation unit.
buildTypedefEnv :: CTranslUnit -> TypedefEnv
buildTypedefEnv (CTranslUnit decls _) = foldr collectTypedef Map.empty decls
  where
    collectTypedef (CDeclExt decl) acc = collectTypedefDecl decl acc
    collectTypedef _               acc = acc

    collectTypedefDecl (CDecl specs declrs _) acc
        | isTypedefDecl specs =
            let baseSpecs = filter (not . isStorageSpec) specs
            in foldr (insertTypedef baseSpecs) acc declrs
        | otherwise = acc
    collectTypedefDecl (CStaticAssert {}) acc = acc

    isTypedefDecl = any isTypedefSpec
    isTypedefSpec (CStorageSpec (CTypedef _)) = True
    isTypedefSpec _                           = False

    isStorageSpec (CStorageSpec _) = True
    isStorageSpec _                = False

    insertTypedef baseSpecs (Just (CDeclr (Just (Ident name _ _)) derived _ _ _), _, _) acc =
        Map.insert name (resolveType baseSpecs derived) acc
    insertTypedef _ _ acc = acc

-- ---------------------------------------------------------------------------
-- Struct/union member environment
-- ---------------------------------------------------------------------------

-- | Maps struct or union tag names to their member (field name, field type) pairs.
type StructEnv = Map.Map String [(String, CType)]

-- | Build a StructEnv by scanning all top-level struct/union definitions.
buildStructEnv :: CTranslUnit -> StructEnv
buildStructEnv (CTranslUnit decls _) = foldr collectDecl' Map.empty decls
  where
    collectDecl' (CDeclExt (CDecl specs _ _)) acc =
        foldr collectFromSpec acc [s | CTypeSpec s <- specs]
    collectDecl' _ acc = acc

    collectFromSpec (CSUType (CStruct _ (Just (Ident name _ _)) (Just members) _ _) _) acc =
        Map.insert name (concatMap extractMembers members) acc
    collectFromSpec _ acc = acc

    extractMembers (CDecl specs declrs _) =
        [ (n, resolveType specs derived)
        | (Just (CDeclr (Just (Ident n _ _)) derived _ _ _), _, _) <- declrs ]
    extractMembers (CStaticAssert {}) = []

-- | True if the named struct/union contains at least one pointer-typed member.
structHasPointer :: StructEnv -> String -> Bool
structHasPointer senv name =
    case Map.lookup name senv of
        Just members -> any (isPointer . snd) members
        Nothing      -> False

-- | True when @t@ is @TPointer (TStruct name)@ and that struct has at least
--   one pointer-typed member.
ptrToStructWithPtrs :: StructEnv -> CType -> Bool
ptrToStructWithPtrs senv (TPointer (TStruct name)) = structHasPointer senv name
ptrToStructWithPtrs _ _                             = False

-- ---------------------------------------------------------------------------

-- | Resolve TTypedef references through the typedef environment,
--   following chains (e.g. typedef myuint bigint).
resolveTypedef :: TypedefEnv -> CType -> CType
resolveTypedef tenv (TTypedef name) =
    case Map.lookup name tenv of
        Just t  -> resolveTypedef tenv t  -- follow chains
        Nothing -> TTypedef name          -- external typedef (e.g. from headers)
resolveTypedef tenv (TPointer t) = TPointer (resolveTypedef tenv t)
resolveTypedef tenv (TArray t)   = TArray   (resolveTypedef tenv t)
resolveTypedef _    t            = t