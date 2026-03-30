module Transformation.FormatStrings where

import Language.C.Syntax.AST
import Analysis.UtilTypes

transformFormatStringsIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformFormatStringsIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      DUsedWithSizet               -> transformDUsedWithSizet               a issue
      UUsedWithSizet               -> transformUUsedWithSizet               a issue
      XUsedWithSizet               -> transformXUsedWithSizet               a issue
      DUsedWithPtrdifft            -> transformDUsedWithPtrdifft            a issue
      UUsedWithPtrdifft            -> transformUUsedWithPtrdifft            a issue
      DUsedWithPtr                 -> transformDUsedWithPtr                 a issue
      UUsedWithPtr                 -> transformUUsedWithPtr                 a issue
      XUsedWithPtr                 -> transformXUsedWithPtr                 a issue
      LuUsedForPtrSizedVals        -> transformLuUsedForPtrSizedVals        a issue
      LdUsedWithLongAssuming64bits -> transformLdUsedWithLongAssuming64bits a issue
      _                            -> (a, Just issue)

transformDUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformDUsedWithSizet _ issue = undefined

transformUUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUUsedWithSizet _ issue = undefined

transformXUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformXUsedWithSizet _ issue = undefined

transformDUsedWithPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformDUsedWithPtrdifft _ issue = undefined

transformUUsedWithPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUUsedWithPtrdifft _ issue = undefined

transformDUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformDUsedWithPtr _ issue = undefined

transformUUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUUsedWithPtr _ issue = undefined

transformXUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformXUsedWithPtr _ issue = undefined

transformLuUsedForPtrSizedVals :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformLuUsedForPtrSizedVals _ issue = undefined

transformLdUsedWithLongAssuming64bits :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformLdUsedWithLongAssuming64bits _ issue = undefined
