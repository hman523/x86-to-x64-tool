module Linter.FunctionSignatures where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintFunctionSignaturesIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintFunctionSignaturesIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      FnsReturnPtrAsInt              -> lintFnsReturnPtrAsInt              a issue
      FnsReturnPtrAsLong             -> lintFnsReturnPtrAsLong             a issue
      FnsParamDeclaredAsIntTakesPtr  -> lintFnsParamDeclaredAsIntTakesPtr  a issue
      VaargUsingWrongTypesForPtrArgs -> lintVaargUsingWrongTypesForPtrArgs a issue
      _                              -> (a, Just issue)

lintFnsReturnPtrAsInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintFnsReturnPtrAsInt ast issue =
    (wrapReturnExpr (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

lintFnsReturnPtrAsLong :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintFnsReturnPtrAsLong ast issue =
    (wrapReturnExpr (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

lintFnsParamDeclaredAsIntTakesPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintFnsParamDeclaredAsIntTakesPtr ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "intptr_t") ast, Nothing)
    Nothing -> (ast, Just issue)

lintVaargUsingWrongTypesForPtrArgs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintVaargUsingWrongTypesForPtrArgs ast issue =
    (replaceVaArgType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)
