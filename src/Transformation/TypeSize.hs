module Transformation.TypeSize where

import Language.C.Syntax.AST
import Analysis.UtilTypes

transformTypeSizeIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformTypeSizeIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      CastPointerToInt   -> transformCastPointerToInt   a issue
      CastPointerToUInt  -> transformCastPointerToUInt  a issue
      CastIntToPointer   -> transformCastIntToPointer   a issue
      CastLongToPointer  -> transformCastLongToPointer  a issue
      SizeOfIntIsVoid    -> transformSizeOfIntIsVoid    a issue
      SizeOfLongIsVoid   -> transformSizeOfLongIsVoid   a issue
      UsingIntAsSizet    -> transformUsingIntAsSizet    a issue
      UsingIntAsPtrdifft -> transformUsingIntAsPtrdifft a issue
      UsingUIntAsMemSize -> transformUsingUIntAsMemSize a issue
      _                  -> (a, Just issue)

transformCastPointerToInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformCastPointerToInt _ issue = undefined

transformCastPointerToUInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformCastPointerToUInt _ issue = undefined

transformCastIntToPointer :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformCastIntToPointer _ issue = undefined

transformCastLongToPointer :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformCastLongToPointer _ issue = undefined

transformSizeOfIntIsVoid :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeOfIntIsVoid _ issue = undefined

transformSizeOfLongIsVoid :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeOfLongIsVoid _ issue = undefined

transformUsingIntAsSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntAsSizet _ issue = undefined

transformUsingIntAsPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntAsPtrdifft _ issue = undefined

transformUsingUIntAsMemSize :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingUIntAsMemSize _ issue = undefined
