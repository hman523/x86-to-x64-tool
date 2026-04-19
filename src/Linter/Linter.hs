module Linter.Linter
  ( lint
  ) where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import qualified Linter.Alignment          as Align
import qualified Linter.BitManipulation    as Bit
import qualified Linter.Comparison         as Cmp
import qualified Linter.ConstantsLiterals  as Const
import qualified Linter.FormatStrings      as Fmt
import qualified Linter.FunctionSignatures as Func
import qualified Linter.MemoryAllocation   as Mem
import qualified Linter.PlatformSpecifics  as Plat
import qualified Linter.PointerMath        as Ptr
import qualified Linter.Serialization      as Ser
import qualified Linter.TypeSize           as Typ

-- | Run all linting passes over the AST.
--   Returns the linted AST and any issues that could not be
--   automatically resolved (because the correct fix depends on intent).
lint :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lint ast issues =
    foldr applyModule (ast, []) modules
  where
    -- Each entry: (category predicate, sub-module linter)
    modules =
        [ (AlignmentIssue,          Align.lintAlignmentIssues)
        , (BitManipulationIssue,    Bit.lintBitManipulationIssues)
        , (ComparisonIssue,         Cmp.lintComparisonIssues)
        , (ConstantLiteralsIssue,   Const.lintConstantsLiteralsIssues)
        , (FormatStringsIssue,      Fmt.lintFormatStringsIssues)
        , (FunctionSignaturesIssue, Func.lintFunctionSignaturesIssues)
        , (MemoryAllocationIssue,   Mem.lintMemoryAllocationIssues)
        , (PlatformSpecificsIssue,  Plat.lintPlatformSpecificsIssues)
        , (PointerMathIssue,        Ptr.lintPointerMathIssues)
        , (SerializationIssue,      Ser.lintSerializationIssues)
        , (TypeSizeIssue,           Typ.lintTypeSizeIssues)
        ]

    applyModule (cat, linter) (a, unresolved) =
        let categoryIssues = filter ((== cat) . category) issues
            (a', newUnresolved) = linter a categoryIssues
        in (a', unresolved ++ newUnresolved)