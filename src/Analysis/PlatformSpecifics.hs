module Analysis.PlatformSpecifics where

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzePlatformSpecificIssues :: CTranslUnit -> [Issue]
analyzePlatformSpecificIssues ast =
    checkInlineAsmWithx86Instructions ast
    ++ checkAsmBlocks ast
    ++ checkHandleTypesCastToInt ast
    ++ checkx86SpecificCompilerIntrinsics ast
    ++ checkAssumptionsAboutRegSizes ast

-- inlineAsmWithx86Instructions
checkInlineAsmWithx86Instructions :: CTranslUnit -> [Issue]
checkInlineAsmWithx86Instructions ast = []

-- asmBlocks
checkAsmBlocks :: CTranslUnit -> [Issue]
checkAsmBlocks ast = []

-- handleTypesCastToInt
checkHandleTypesCastToInt :: CTranslUnit -> [Issue]
checkHandleTypesCastToInt ast = []

-- x86SpecificCompilerIntrinsics
checkx86SpecificCompilerIntrinsics :: CTranslUnit -> [Issue]
checkx86SpecificCompilerIntrinsics ast = []

-- assumptionsAboutRegSizes
checkAssumptionsAboutRegSizes :: CTranslUnit -> [Issue]
checkAssumptionsAboutRegSizes ast = []
