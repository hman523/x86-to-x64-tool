module Transformation.FunctionSignatures where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Transformation.Helpers

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
transformFnsReturnPtrAsInt ast issue =
    (wrapReturnExpr (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

transformFnsReturnPtrAsLong :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformFnsReturnPtrAsLong ast issue =
    (wrapReturnExpr (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

transformFnsParamDeclaredAsIntTakesPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformFnsParamDeclaredAsIntTakesPtr ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "intptr_t") ast, Nothing)
    Nothing -> (ast, Just issue)

transformVaargUsingWrongTypesForPtrArgs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformVaargUsingWrongTypesForPtrArgs ast issue =
    (replaceVaArgType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)
