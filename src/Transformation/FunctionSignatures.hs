module Transformation.FunctionSignatures where

import Language.C.Syntax.AST
import Analysis.UtilTypes

transformFunctionSignaturesIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformFunctionSignaturesIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      FnsReturnPtrAsInt              -> transformFnsReturnPtrAsInt              a issue
      FnsReturnPtrAsLong             -> transformFnsReturnPtrAsLong             a issue
      FnsParamDeclaredAsIntTakesPtr  -> transformFnsParamDeclaredAsIntTakesPtr  a issue
      VaargUsingWrongTypesForPtrArgs -> transformVaargUsingWrongTypesForPtrArgs a issue
      _                              -> (a, Just issue)

transformFnsReturnPtrAsInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformFnsReturnPtrAsInt _ issue = undefined

transformFnsReturnPtrAsLong :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformFnsReturnPtrAsLong _ issue = undefined

transformFnsParamDeclaredAsIntTakesPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformFnsParamDeclaredAsIntTakesPtr _ issue = undefined

transformVaargUsingWrongTypesForPtrArgs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformVaargUsingWrongTypesForPtrArgs _ issue = undefined
