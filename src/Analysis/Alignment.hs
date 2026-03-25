module Analysis.Alignment where 

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzeAlignmentIssues :: CTranslUnit -> [Issue]
analyzeAlignmentIssues ast =
    checkStructsWithMixedPtrNonPtrMembers ast
    ++ checkUnionsContainingPtrAndInts ast
    ++ checkPackedStructsWithPtrs ast
    ++ checkStructContainingPtrWrittenToBinFile ast
    ++ checkStructContainingPtrReadFromBinFile ast
    ++ checkSizeofStoredIn32bits ast
    ++ checkHardCodedStructSizes ast


-- StructContainingPtrWrittenToBinFile 
checkStructContainingPtrWrittenToBinFile :: CTranslUnit -> [Issue]
checkStructContainingPtrWrittenToBinFile ast = []

-- StrucContainingPtrReadFromBinFile
checkStructContainingPtrReadFromBinFile :: CTranslUnit -> [Issue]
checkStructContainingPtrReadFromBinFile ast = []

-- StructsWithMixedPtrNonPtrMembers
checkStructsWithMixedPtrNonPtrMembers :: CTranslUnit -> [Issue]
checkStructsWithMixedPtrNonPtrMembers ast = []

-- UnionsContainingPtrAndInts
checkUnionsContainingPtrAndInts :: CTranslUnit -> [Issue]
checkUnionsContainingPtrAndInts ast = []

-- PackedStructsWithPtrs
checkPackedStructsWithPtrs :: CTranslUnit -> [Issue]
checkPackedStructsWithPtrs ast = []

-- sizeofStoredin32bits
checkSizeofStoredIn32bits :: CTranslUnit -> [Issue]
checkSizeofStoredIn32bits ast = []

-- HardCodedStructSizes
checkHardCodedStructSizes :: CTranslUnit -> [Issue]
checkHardCodedStructSizes ast = []