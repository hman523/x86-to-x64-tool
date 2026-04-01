module Analysis.TypeChecker where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import qualified Data.Map.Strict as Map

-- | The resolved type of a C variable
data CType
    = TInt               -- int
    | TUInt              -- unsigned int
    | TLong              -- long
    | TULong             -- unsigned long
    | TShort             -- short
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
    applyDerived t (CArrDeclr _ _ _ : rest) = applyDerived (TArray t) rest
    applyDerived t (_ : rest) = applyDerived t rest

-- | Resolve the base type from declaration specifiers
resolveBaseType :: [CDeclarationSpecifier a] -> CType
resolveBaseType specs =
    let typeSpecs = [s | CTypeSpec s <- specs]
    in classifyTypeSpecs typeSpecs

classifyTypeSpecs :: [CTypeSpecifier a] -> CType
classifyTypeSpecs specs
    | hasVoid specs                          = TVoid
    | hasUnsigned specs && hasLong specs     = TULong
    | hasUnsigned specs && hasShort specs    = TUInt  -- unsigned short treated as uint
    | hasUnsigned specs                      = TUInt
    | hasLong specs                          = TLong
    | hasShort specs                         = TShort
    | hasInt specs                           = TInt
    | hasChar specs                          = TChar
    | hasFloat specs                         = TFloat
    | hasDouble specs                        = TDouble
    | hasTypedefName specs                   = TTypedef (getTypedefName specs)
    | hasNamedStruct specs                   = TStruct (getSUName specs)
    | hasNamedUnion specs                    = TUnion  (getSUName specs)
    | otherwise                              = TUnknown
  where
    hasVoid    ss = any (\s -> case s of CVoidType _ -> True; _ -> False) ss
    hasUnsigned ss = any (\s -> case s of CUnsigType _ -> True; _ -> False) ss
    hasLong    ss = any (\s -> case s of CLongType _ -> True; _ -> False) ss
    hasShort   ss = any (\s -> case s of CShortType _ -> True; _ -> False) ss
    hasInt     ss = any (\s -> case s of CIntType _ -> True; _ -> False) ss
    hasChar    ss = any (\s -> case s of CCharType _ -> True; _ -> False) ss
    hasFloat   ss = any (\s -> case s of CFloatType _ -> True; _ -> False) ss
    hasDouble  ss = any (\s -> case s of CDoubleType _ -> True; _ -> False) ss
    hasTypedefName ss = any (\s -> case s of CTypeDef _ _ -> True; _ -> False) ss
    getTypedefName ss = head [n | CTypeDef (Ident n _ _) _ <- ss]
    hasNamedStruct ss = any (\s -> case s of
        CSUType (CStruct CStructTag (Just _) _ _ _) _ -> True; _ -> False) ss
    hasNamedUnion ss = any (\s -> case s of
        CSUType (CStruct CUnionTag (Just _) _ _ _) _ -> True; _ -> False) ss
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
    -- For binary ops like pointer subtraction, result is ptrdiff_t ~ TLong
    CBinary CSubOp l r _        ->
        case (typeOfExpr env l, typeOfExpr env r) of
            (TPointer _, TPointer _) -> TLong
            _                        -> TInt
    _                           -> TUnknown

-- | Get the CType from a cast declaration
typeOfDecl :: CDeclaration a -> CType
typeOfDecl (CDecl specs declrs _) =
    let derived = case declrs of
                    (Just (CDeclr _ d _ _ _), _, _) : _ -> d
                    _                                    -> []
    in resolveType specs derived

-- | Predicate helpers used by analysis functions
isPointer :: CType -> Bool
isPointer (TPointer _) = True
isPointer _            = False

isIntType' :: CType -> Bool
isIntType' TInt  = True
isIntType' TUInt = True
isIntType' _     = False

isLongType' :: CType -> Bool
isLongType' TLong  = True
isLongType' TULong = True
isLongType' _      = False

isUIntType :: CType -> Bool
isUIntType TUInt  = True
isUIntType TULong = True
isUIntType _      = False

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

    isTypedefDecl ss = any isTypedefSpec ss
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

-- | True if the named struct/union contains at least one pointer-typed member.
structHasPointer :: StructEnv -> String -> Bool
structHasPointer senv name =
    case Map.lookup name senv of
        Just members -> any (isPointer . snd) members
        Nothing      -> False

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