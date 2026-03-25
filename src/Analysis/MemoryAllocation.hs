module Analysis.MemoryAllocation where

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzeMemoryAllocationIssues :: CTranslUnit -> [Issue]
analyzeMemoryAllocationIssues ast =
    checkAllocationSizeCalculationsMayOverflow ast
    ++ checkMallocWithoutOverflowChecking ast
    ++ checkUsingIntToStoreAllocationSizes ast

-- allocationSizeCalculationsMayOverflow
checkAllocationSizeCalculationsMayOverflow :: CTranslUnit -> [Issue]
checkAllocationSizeCalculationsMayOverflow ast = []

-- mallocWithoutOverflowChecking
checkMallocWithoutOverflowChecking :: CTranslUnit -> [Issue]
checkMallocWithoutOverflowChecking ast = []

-- usingIntToStoreAllocationSizes
checkUsingIntToStoreAllocationSizes :: CTranslUnit -> [Issue]
checkUsingIntToStoreAllocationSizes ast = []