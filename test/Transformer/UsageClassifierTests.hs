module Transformer.UsageClassifierTests where

import Test.Hspec
import Data.Generics        (listify)
import qualified Data.Map.Strict as Map
import Language.C.Syntax.AST
import Language.C.Data.Node  (NodeInfo)

import Parser.Parser           (parseSourceString)
import Transformer.UsageClassifier
import Analysis.TypeChecker    (TypeEnv, buildTypeEnv, collectDecl, typeOfExpr)

-- | Parse C source, extract the first function definition, build a TypeEnv
--   from its body, and classify the given variable name.
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
            in classifyVar env varName fd

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
