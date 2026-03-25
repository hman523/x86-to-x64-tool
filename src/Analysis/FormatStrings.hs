module Analysis.FormatStrings where

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzeFormatStringIssues :: CTranslUnit -> [Issue]
analyzeFormatStringIssues ast =
    checkdUsedWithSizet ast
    ++ checkuUsedWithSizet ast
    ++ checkxUsedWithSizet ast
    ++ checkdUsedWithPtrdifft ast
    ++ checkuUsedWithPtrdifft ast
    ++ checkdUsedWithPtr ast
    ++ checkuUsedWithPtr ast
    ++ checkxUsedWithPtr ast
    ++ checkluUsedForPtrSizedVals ast
    ++ checkldUsedWithLongAssuming64bits ast


-- dUsedWithSizet
checkdUsedWithSizet :: CTranslUnit -> [Issue]
checkdUsedWithSizet ast = []


-- uUsedWithSizet
checkuUsedWithSizet :: CTranslUnit -> [Issue]
checkuUsedWithSizet ast = []

-- xUsedWithSizet
checkxUsedWithSizet :: CTranslUnit -> [Issue]
checkxUsedWithSizet ast = []

-- dUsedWithPtrdifft
checkdUsedWithPtrdifft :: CTranslUnit -> [Issue]
checkdUsedWithPtrdifft ast = []

-- uUsedWithPtrdifft
checkuUsedWithPtrdifft :: CTranslUnit -> [Issue]
checkuUsedWithPtrdifft ast = []

-- dUsedWithPtr
checkdUsedWithPtr :: CTranslUnit -> [Issue]
checkdUsedWithPtr ast = []

-- uUsedWithPtr
checkuUsedWithPtr :: CTranslUnit -> [Issue]
checkuUsedWithPtr ast = []

-- xUsedWithPtr
checkxUsedWithPtr :: CTranslUnit -> [Issue]
checkxUsedWithPtr ast = []

-- luUsedForPtrSizedVals
checkluUsedForPtrSizedVals :: CTranslUnit -> [Issue]
checkluUsedForPtrSizedVals ast = []

-- ldUsedWithLongAssuming64bits
checkldUsedWithLongAssuming64bits :: CTranslUnit -> [Issue]
checkldUsedWithLongAssuming64bits ast = []