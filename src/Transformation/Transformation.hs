module Transformation.Transformation where

import Language.C.Syntax.AST
import Analysis.UtilTypes
import qualified Transformation.Alignment          as Align
import qualified Transformation.BitManipulation    as Bit
import qualified Transformation.Comparison         as Cmp
import qualified Transformation.ConstantsLiterals  as Const
import qualified Transformation.FormatStrings      as Fmt
import qualified Transformation.FunctionSignatures as Func
import qualified Transformation.MemoryAllocation   as Mem
import qualified Transformation.PlatformSpecifics  as Plat
import qualified Transformation.PointerMath        as Ptr
import qualified Transformation.Serialization      as Ser
import qualified Transformation.TypeSize           as Typ

-- | Run all transformations over the AST.
--   Returns the transformed AST and any issues that could not be
--   automatically resolved (because the correct fix depends on intent).
transformation :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformation ast issues =
    foldr applyModule (ast, []) modules
  where
    -- Each entry: (category predicate, sub-module transformer)
    modules =
        [ (AlignmentIssue,          Align.transformAlignmentIssues)
        , (BitManipulationIssue,    Bit.transformBitManipulationIssues)
        , (ComparisonIssue,         Cmp.transformComparisonIssues)
        , (ConstantLiteralsIssue,   Const.transformConstantsLiteralsIssues)
        , (FormatStringsIssue,      Fmt.transformFormatStringsIssues)
        , (FunctionSignaturesIssue, Func.transformFunctionSignaturesIssues)
        , (MemoryAllocationIssue,   Mem.transformMemoryAllocationIssues)
        , (PlatformSpecificsIssue,  Plat.transformPlatformSpecificsIssues)
        , (PointerMathIssue,        Ptr.transformPointerMathIssues)
        , (SerializationIssue,      Ser.transformSerializationIssues)
        , (TypeSizeIssue,           Typ.transformTypeSizeIssues)
        ]

    applyModule (cat, transform) (a, unresolved) =
        let categoryIssues = filter ((== cat) . catagory) issues
            (a', newUnresolved) = transform a categoryIssues
        in (a', unresolved ++ newUnresolved)