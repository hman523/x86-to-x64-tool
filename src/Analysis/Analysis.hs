module Analysis.Analysis where 

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Analysis.Alignment
import Analysis.BitManipulation
import Analysis.Comparison
import Analysis.ConstantsLiterals
import Analysis.FormatStrings
import Analysis.FunctionSignatures
import Analysis.MemoryAllocation
import Analysis.PlatformSpecifics
import Analysis.PointerMath
import Analysis.Serialization
import Analysis.TypeSize

analysis :: CTranslUnit -> [Issue]
analysis ast = 
    analyzeAlignmentIssues ast
    ++ analyzeBitManipulationIssues ast
    ++ analyzeComparisonIssues ast
    ++ analyzeConstantsLiteralsIssues ast
    ++ analyzeFormatStringIssues ast
    ++ analyzeFunctionSignatureIssues ast
    ++ analyzeMemoryAllocationIssues ast
    ++ analyzePlatformSpecificIssues ast
    ++ analyzePointerMathIssues ast
    ++ analyzeSerializationIssues ast
    ++ analyzeTypeSizeIssues ast