module Transformer.UsageClassifierTests where

import Test.Hspec
import Data.Generics        (listify)
import Data.Maybe           (listToMaybe)
import qualified Data.Map.Strict as Map
import Language.C.Syntax.AST
import Language.C.Data.Node  (NodeInfo)
import Language.C.Data.Ident (Ident(..))

import Parser.Parser           (parseSourceString)
import Transformer.UsageClassifier
import Analysis.TypeChecker    (TypeEnv, buildTypeEnv, collectDecl, typeOfExpr)

-- | Parse C source, extract the first function definition, build a TypeEnv
--   from its body, and classify the given variable name.
--   Finds the NodeInfo of the first declaration of @varName@ in the function
--   (covering both parameters and body-local declarations) so that the
--   scope-aware classifier knows which binding to track.
classifyIn :: String -> String -> AbstractType
classifyIn src varName =
    case parseSourceString src of
        Left err  -> error (show err)
        Right ast ->
            let funDefs = listify (const True) ast :: [CFunctionDef NodeInfo]
                fd@(CFunDef _ _ _ body _) = head funDefs
                env = case body of
                        CCompound _ items _ -> buildTypeEnv items Map.empty
                        _                   -> Map.empty
                allDecls = listify (const True) fd :: [CDeclaration NodeInfo]
                mDeclNi  = listToMaybe
                    [ ni
                    | CDecl _ declrs _ <- allDecls
                    , (Just (CDeclr (Just (Ident n _ _)) _ _ _ ni), _, _) <- declrs
                    , n == varName ]
            in case mDeclNi of
                Just ni -> classifyVar env ni varName fd
                Nothing -> NumberType

-- | Parse C source, extract the first function definition, and classify the
--   Nth declaration (0-indexed) of @varName@ (in AST traversal order).
--   This lets tests target specific declarations when the same name is used
--   in multiple nested scopes.
classifyNthIn :: String -> String -> Int -> AbstractType
classifyNthIn src varName n =
    case parseSourceString src of
        Left err  -> error (show err)
        Right ast ->
            let funDefs = listify (const True) ast :: [CFunctionDef NodeInfo]
                fd@(CFunDef _ (CDeclr _ derived _ _ _) _ body _) = head funDefs
                paramEnv = foldr collectDecl Map.empty (concatMap getParams derived)
                env = case body of
                        CCompound _ items _ -> buildTypeEnv items paramEnv
                        _                   -> paramEnv
                allDecls = listify (const True) fd :: [CDeclaration NodeInfo]
                matchingNis =
                    [ ni
                    | CDecl _ declrs _ <- allDecls
                    , (Just (CDeclr (Just (Ident nm _ _)) _ _ _ ni), _, _) <- declrs
                    , nm == varName ]
                mDeclNi = if n < length matchingNis
                          then Just (matchingNis !! n)
                          else Nothing
            in case mDeclNi of
                Just ni -> classifyVar env ni varName fd
                Nothing -> NumberType
  where
    getParams (CFunDeclr (Right (ps, _)) _ _) = ps
    getParams _                               = []

-- | Parse C source containing a single expression in an assignment to a
--   dummy variable, extract that expression, and return its rhsEvidence.
rhsEvidenceIn :: String -> [AbstractType]
rhsEvidenceIn src =
    case parseSourceString src of
        Left err  -> error (show err)
        Right ast ->
            let funDefs = listify (const True) ast :: [CFunctionDef NodeInfo]
                fd      = head funDefs
                exprs   = listify isAssignRhs fd :: [CExpression NodeInfo]
            in case exprs of
                 (CAssign _ _ rhs _):_ -> rhsEvidence Map.empty rhs
                 _                     -> []
  where
    isAssignRhs (CAssign{}) = True
    isAssignRhs _           = False

usageClassifierSpec :: Spec
usageClassifierSpec = describe "UsageClassifier" $ do

    describe "stronger" $ do
        it "PointerType is stronger than all others" $ do
            stronger PointerType NumberType  `shouldBe` PointerType
            stronger PointerType SizeType    `shouldBe` PointerType
            stronger PointerType OffsetType  `shouldBe` PointerType
            stronger PointerType BitSeqType  `shouldBe` PointerType

        it "SizeType beats OffsetType, BitSeqType, NumberType" $ do
            stronger SizeType OffsetType  `shouldBe` SizeType
            stronger SizeType BitSeqType  `shouldBe` SizeType
            stronger SizeType NumberType  `shouldBe` SizeType

        it "is commutative in the sense that the higher always wins" $ do
            stronger NumberType PointerType `shouldBe` PointerType
            stronger BitSeqType SizeType    `shouldBe` SizeType

        it "same type returns itself" $ do
            stronger NumberType NumberType `shouldBe` NumberType

    describe "classifyVar" $ do
        it "classifies variable assigned sizeof as SizeType" $
            classifyIn "void foo() { long x; x = sizeof(int); }" "x"
                `shouldBe` SizeType

        it "classifies variable assigned sizeof(self) as NumberType (suppressed)" $
            classifyIn "void foo() { long x; x = sizeof(x); }" "x"
                `shouldBe` NumberType

        it "classifies variable used in bitwise AND as BitSeqType" $
            classifyIn "void foo() { long x; x = 0xFF; long y = x & 0x0F; }" "x"
                `shouldBe` BitSeqType

        it "classifies variable assigned pointer cast as PointerType" $
            classifyIn "void foo() { int *p; long x; x = (long)p; }" "x"
                `shouldBe` PointerType

        it "classifies variable assigned ptr difference as OffsetType" $
            classifyIn "void foo() { int *a; int *b; long x; x = a - b; }" "x"
                `shouldBe` OffsetType

        it "defaults to NumberType when no evidence" $
            classifyIn "void foo() { long x; x = 42; }" "x"
                `shouldBe` NumberType

        it "pointer evidence overrides size evidence" $
            classifyIn "void foo() { int *p; long x; x = sizeof(int); x = (long)p; }" "x"
                `shouldBe` PointerType

        it "classifies variable initialized with sizeof as SizeType" $
            classifyIn "void foo() { long x = sizeof(int); }" "x"
                `shouldBe` SizeType

        it "classifies bitwise complement target as BitSeqType" $
            classifyIn "void foo() { long x = 0; long y = ~x; }" "x"
                `shouldBe` BitSeqType

        it "classifies bitwise-assign (|=) as BitSeqType" $
            classifyIn "void foo() { long x = 0; x |= 0xFF; }" "x"
                `shouldBe` BitSeqType

    describe "rhsEvidence" $ do
        it "sizeof(type) yields SizeType" $
            rhsEvidenceIn "void foo() { long x; x = sizeof(int); }"
                `shouldBe` [SizeType]

        it "bitwise OR on RHS yields BitSeqType" $
            rhsEvidenceIn "void foo() { long a; long b; long x; x = a | b; }"
                `shouldBe` [BitSeqType]

        it "no evidence for plain integer literal" $
            rhsEvidenceIn "void foo() { long x; x = 42; }"
                `shouldBe` []

    describe "classifyVarAcrossFuns" $ do
        it "takes strongest evidence across multiple functions" $ do
            let src = "void f() { long g; g = sizeof(int); } void h() { int *p; long g; g = (long)p; }"
            case parseSourceString src of
                Left err  -> fail (show err)
                Right ast ->
                    let funDefs = listify (const True) ast :: [CFunctionDef NodeInfo]
                    in classifyVarAcrossFuns Map.empty "g" funDefs
                        `shouldBe` PointerType

        it "returns NumberType when variable not used in any function" $ do
            let src = "void f() { int y = 1; } void g() { int z = 2; }"
            case parseSourceString src of
                Left err  -> fail (show err)
                Right ast ->
                    let funDefs = listify (const True) ast :: [CFunctionDef NodeInfo]
                    in classifyVarAcrossFuns Map.empty "x" funDefs
                        `shouldBe` NumberType

    describe "scope-aware classifyVar: variable name shadowing" $ do

        it "if-branch long x = (long)p is PointerType, independent of else-branch long x = 42" $
            classifyNthIn
                "void f(void *p, int c) { if (c) { long x = (long)p; } else { long x = 42; } }"
                "x" 0
                `shouldBe` PointerType

        it "else-branch long x = 42 is NumberType, independent of if-branch long x = (long)p" $
            classifyNthIn
                "void f(void *p, int c) { if (c) { long x = (long)p; } else { long x = 42; } }"
                "x" 1
                `shouldBe` NumberType

        it "outer long x = 42 is NumberType even when inner block has long x = (long)p" $
            classifyIn
                "void f(void *p) { long x = 42; { long x = (long)p; } }"
                "x"
                `shouldBe` NumberType

        it "inner long x = (long)p is PointerType even though outer long x = 42" $
            classifyNthIn
                "void f(void *p) { long x = 42; { long x = (long)p; } }"
                "x" 1
                `shouldBe` PointerType

        it "outer long x = sizeof(int) is SizeType even when inner block has long x = 42" $
            classifyIn
                "void f(void) { long x = sizeof(int); { long x = 42; } }"
                "x"
                `shouldBe` SizeType

        it "inner long x = 42 is NumberType even though outer long x = sizeof(int)" $
            classifyNthIn
                "void f(void) { long x = sizeof(int); { long x = 42; } }"
                "x" 1
                `shouldBe` NumberType

        it "three-level nesting: outermost long x = 0 is NumberType" $
            classifyIn
                "void f(void *p) { long x = 0; { long x = sizeof(int); { long x = (long)p; } } }"
                "x"
                `shouldBe` NumberType

        it "three-level nesting: middle long x = sizeof(int) is SizeType" $
            classifyNthIn
                "void f(void *p) { long x = 0; { long x = sizeof(int); { long x = (long)p; } } }"
                "x" 1
                `shouldBe` SizeType

        it "three-level nesting: innermost long x = (long)p is PointerType" $
            classifyNthIn
                "void f(void *p) { long x = 0; { long x = sizeof(int); { long x = (long)p; } } }"
                "x" 2
                `shouldBe` PointerType
