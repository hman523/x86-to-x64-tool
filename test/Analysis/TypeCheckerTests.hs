module Analysis.TypeCheckerTests where

import Test.Hspec
import Language.C
import Language.C.Data.Node
import Analysis.TypeChecker
import qualified Data.Map.Strict as Map
import Parser.Parser (parseSourceString)
import qualified Data.ByteString.Char8 as BS
import Analysis.AnalysisTestUtils


-- | Helper to get the TypeEnv from the top-level declarations
-- of a simple C snippet (no function body needed)
envFromDecls :: String -> TypeEnv
envFromDecls src =
    let (CTranslUnit decls _) = parseSrc src
    in foldr collectExtDecl Map.empty decls
  where
    collectExtDecl (CDeclExt decl) env = collectDecl decl env
    collectExtDecl _               env = env

-- | Helper to get the TypeEnv from inside the first function body
envFromFunctionBody :: String -> TypeEnv
envFromFunctionBody src =
    let (CTranslUnit decls _) = parseSrc src
    in case [body | CFDefExt (CFunDef _ _ _ body _) <- decls] of
        (CCompound _ items _ : _) -> buildTypeEnv items Map.empty
        _                         -> Map.empty

-- | Helper to resolve an expression's type given a snippet and
--   the name of the variable to look up
resolveVarType :: String -> String -> CType
resolveVarType src varName =
    let env = envFromFunctionBody src
    in lookupType env varName

-- | Extract the last expression statement from the first function body
parseExprFrom :: String -> Maybe (CExpression NodeInfo)
parseExprFrom src =
    let (CTranslUnit decls _) = parseSrc src
    in case [body | CFDefExt (CFunDef _ _ _ body _) <- decls] of
        (CCompound _ items _ : _) ->
            let exprs = [ e | CBlockStmt (CExpr (Just e) _) <- items ]
            in case exprs of
                [] -> Nothing
                _  -> Just (last exprs)
        _ -> Nothing

-- | Extract the cast declaration from the last cast expression in the
--   first function body
parseCastDeclFrom :: String -> Maybe (CDeclaration NodeInfo)
parseCastDeclFrom src =
    case parseExprFrom src of
        Just (CCast decl _ _) -> Just decl
        _                     -> Nothing

typecheckerSpec :: Spec
typecheckerSpec = do
    describe "TypeChecker" $ do
        testResolveBaseTypes
        testResolvePointerTypes
        testResolveArrayTypes
        testCollectDecl
        testBuildTypeEnv
        testTypeOfExpr
        testTypeOfDecl
        testPredicates

-- ---------------------------------------------------------------------------
-- Base type resolution
-- ---------------------------------------------------------------------------

testResolveBaseTypes :: Spec
testResolveBaseTypes =
    describe "resolveBaseType" $ do

        it "resolves int" $
            resolveVarType "void f() { int x; }" "x"
                `shouldBe` TInt

        it "resolves unsigned int" $
            resolveVarType "void f() { unsigned int x; }" "x"
                `shouldBe` TUInt

        it "resolves long" $
            resolveVarType "void f() { long x; }" "x"
                `shouldBe` TLong

        it "resolves unsigned long" $
            resolveVarType "void f() { unsigned long x; }" "x"
                `shouldBe` TULong

        it "resolves short" $
            resolveVarType "void f() { short x; }" "x"
                `shouldBe` TShort

        it "resolves char" $
            resolveVarType "void f() { char x; }" "x"
                `shouldBe` TChar

        it "resolves float" $
            resolveVarType "void f() { float x; }" "x"
                `shouldBe` TFloat

        it "resolves double" $
            resolveVarType "void f() { double x; }" "x"
                `shouldBe` TDouble

        it "resolves void pointer" $
            resolveVarType "void f() { void *x; }" "x"
                `shouldBe` TPointer TVoid

        it "returns TUnknown for unrecognized type specifier" $
            resolveVarType "void f() { __int128 x; }" "x"
                `shouldBe` TUnknown

-- ---------------------------------------------------------------------------
-- Pointer type resolution
-- ---------------------------------------------------------------------------

testResolvePointerTypes :: Spec
testResolvePointerTypes =
    describe "resolveType (pointers)" $ do

        it "resolves int*" $
            resolveVarType "void f() { int *x; }" "x"
                `shouldBe` TPointer TInt

        it "resolves int**" $
            resolveVarType "void f() { int **x; }" "x"
                `shouldBe` TPointer (TPointer TInt)

        it "resolves void*" $
            resolveVarType "void f() { void *x; }" "x"
                `shouldBe` TPointer TVoid

        it "resolves char*" $
            resolveVarType "void f() { char *x; }" "x"
                `shouldBe` TPointer TChar

        it "resolves unsigned int*" $
            resolveVarType "void f() { unsigned int *x; }" "x"
                `shouldBe` TPointer TUInt

        it "resolves long*" $
            resolveVarType "void f() { long *x; }" "x"
                `shouldBe` TPointer TLong

-- ---------------------------------------------------------------------------
-- Array type resolution
-- ---------------------------------------------------------------------------

testResolveArrayTypes :: Spec
testResolveArrayTypes =
    describe "resolveType (arrays)" $ do

        it "resolves int[]" $
            resolveVarType "void f() { int x[10]; }" "x"
                `shouldBe` TArray TInt

        it "resolves char[]" $
            resolveVarType "void f() { char x[256]; }" "x"
                `shouldBe` TArray TChar

        it "resolves int*[] (array of int pointers)" $
            resolveVarType "void f() { int *x[10]; }" "x"
                `shouldBe` TArray (TPointer TInt)

-- ---------------------------------------------------------------------------
-- collectDecl
-- ---------------------------------------------------------------------------

testCollectDecl :: Spec
testCollectDecl =
    describe "collectDecl" $ do

        it "adds a single variable to an empty env" $
            let env = envFromDecls "int x;"
            in fmap fst (Map.lookup "x" env) `shouldBe` Just TInt

        it "adds multiple variables from one declaration" $
            let env = envFromDecls "int x, y, z;"
            in do
                fmap fst (Map.lookup "x" env) `shouldBe` Just TInt
                fmap fst (Map.lookup "y" env) `shouldBe` Just TInt
                fmap fst (Map.lookup "z" env) `shouldBe` Just TInt

        it "adds a pointer variable" $
            let env = envFromDecls "int *ptr;"
            in fmap fst (Map.lookup "ptr" env) `shouldBe` Just (TPointer TInt)

        it "does not add anonymous declarators" $
            let env = envFromDecls "struct { int a; };"
            in Map.size env `shouldBe` 0

        it "handles multiple declarations" $
            let env = envFromDecls "int x; long y; char *s;"
            in do
                fmap fst (Map.lookup "x" env) `shouldBe` Just TInt
                fmap fst (Map.lookup "y" env) `shouldBe` Just TLong
                fmap fst (Map.lookup "s" env) `shouldBe` Just (TPointer TChar)

-- ---------------------------------------------------------------------------
-- buildTypeEnv
-- ---------------------------------------------------------------------------

testBuildTypeEnv :: Spec
testBuildTypeEnv =
    describe "buildTypeEnv" $ do

        it "builds env from sequential declarations" $
            let env = envFromFunctionBody
                        "void f() { int x; long y; int *ptr; }"
            in do
                fmap fst (Map.lookup "x"   env) `shouldBe` Just TInt
                fmap fst (Map.lookup "y"   env) `shouldBe` Just TLong
                fmap fst (Map.lookup "ptr" env) `shouldBe` Just (TPointer TInt)

        it "later declarations shadow earlier ones with same name" $
            let env = envFromFunctionBody
                        "void f() { int x; long x; }"
            in fmap fst (Map.lookup "x" env) `shouldBe` Just TLong

        it "ignores non-declaration block items" $
            let env = envFromFunctionBody
                        "void f() { int x; x = 5; }"
            in fmap fst (Map.lookup "x" env) `shouldBe` Just TInt

        it "returns empty env for empty function body" $
            let env = envFromFunctionBody "void f() {}"
            in Map.size env `shouldBe` 0

-- ---------------------------------------------------------------------------
-- typeOfExpr
-- ---------------------------------------------------------------------------

testTypeOfExpr :: Spec
testTypeOfExpr =
    describe "typeOfExpr" $ do

        it "resolves CVar to its declared type" $
            let env  = Map.fromList [("x", (TInt, Nothing))]
                expr = parseExprFrom "void f() { int x; x; }"
            in case expr of
                Just e  -> typeOfExpr env e `shouldBe` TInt
                Nothing -> pendingWith "could not extract expr"

        it "resolves address-of (&x) to TPointer of x's type" $
            let env  = Map.fromList [("x", (TInt, Nothing))]
                expr = parseExprFrom "void f() { int x; &x; }"
            in case expr of
                Just e  -> typeOfExpr env e `shouldBe` TPointer TInt
                Nothing -> pendingWith "could not extract expr"

        it "resolves dereference (*ptr) to pointed-to type" $
            let env  = Map.fromList [("ptr", (TPointer TInt, Nothing))]
                expr = parseExprFrom "void f() { int *ptr; *ptr; }"
            in case expr of
                Just e  -> typeOfExpr env e `shouldBe` TInt
                Nothing -> pendingWith "could not extract expr"

        it "resolves dereference of non-pointer to TUnknown" $
            let env  = Map.fromList [("x", (TInt, Nothing))]
                expr = parseExprFrom "void f() { int x; *x; }"
            in case expr of
                Just e  -> typeOfExpr env e `shouldBe` TUnknown
                Nothing -> pendingWith "could not extract expr"

        it "resolves pointer subtraction to TLong" $
            let env  = Map.fromList [("p", (TPointer TInt, Nothing)), ("q", (TPointer TInt, Nothing))]
                expr = parseExprFrom "void f() { int *p, *q; p - q; }"
            in case expr of
                Just e  -> typeOfExpr env e `shouldBe` TLong
                Nothing -> pendingWith "could not extract expr"

        it "returns TUnknown for unknown variable" $
            lookupType (Map.empty :: TypeEnv) "nothere" `shouldBe` TUnknown

-- ---------------------------------------------------------------------------
-- typeOfDecl
-- ---------------------------------------------------------------------------

testTypeOfDecl :: Spec
testTypeOfDecl =
    describe "typeOfDecl" $ do

        it "resolves (int) cast declaration to TInt" $
            let decl = parseCastDeclFrom "void f() { int x; (int)x; }"
            in case decl of
                Just d  -> typeOfDecl d `shouldBe` TInt
                Nothing -> pendingWith "could not extract cast decl"

        it "resolves (int*) cast declaration to TPointer TInt" $
            let decl = parseCastDeclFrom "void f() { int x; (int*)x; }"
            in case decl of
                Just d  -> typeOfDecl d `shouldBe` TPointer TInt
                Nothing -> pendingWith "could not extract cast decl"

        it "resolves (void*) cast declaration to TPointer TVoid" $
            let decl = parseCastDeclFrom "void f() { int x; (void*)x; }"
            in case decl of
                Just d  -> typeOfDecl d `shouldBe` TPointer TVoid
                Nothing -> pendingWith "could not extract cast decl"

        it "resolves (unsigned int) cast declaration to TUInt" $
            let decl = parseCastDeclFrom "void f() { int x; (unsigned int)x; }"
            in case decl of
                Just d  -> typeOfDecl d `shouldBe` TUInt
                Nothing -> pendingWith "could not extract cast decl"

-- ---------------------------------------------------------------------------
-- Predicate helpers
-- ---------------------------------------------------------------------------

testPredicates :: Spec
testPredicates =
    describe "CType predicates" $ do

        describe "isPointer" $ do
            it "returns True for TPointer TInt"      $ isPointer (TPointer TInt)           `shouldBe` True
            it "returns True for TPointer TVoid"     $ isPointer (TPointer TVoid)           `shouldBe` True
            it "returns True for TPointer (TPointer TInt)" $ isPointer (TPointer (TPointer TInt)) `shouldBe` True
            it "returns False for TInt"              $ isPointer TInt                       `shouldBe` False
            it "returns False for TLong"             $ isPointer TLong                      `shouldBe` False
            it "returns False for TUnknown"          $ isPointer TUnknown                   `shouldBe` False

        describe "isIntType'" $ do
            it "returns True for TInt"               $ isIntType' TInt                      `shouldBe` True
            it "returns True for TUInt"              $ isIntType' TUInt                     `shouldBe` True
            it "returns False for TLong"             $ isIntType' TLong                     `shouldBe` False
            it "returns False for TPointer TInt"     $ isIntType' (TPointer TInt)           `shouldBe` False
            it "returns False for TUnknown"          $ isIntType' TUnknown                  `shouldBe` False

        describe "isLongType'" $ do
            it "returns True for TLong"              $ isLongType' TLong                    `shouldBe` True
            it "returns True for TULong"             $ isLongType' TULong                   `shouldBe` True
            it "returns False for TInt"              $ isLongType' TInt                     `shouldBe` False
            it "returns False for TPointer TLong"    $ isLongType' (TPointer TLong)         `shouldBe` False
            it "returns False for TUnknown"          $ isLongType' TUnknown                 `shouldBe` False