module Transformation.PlatformSpecifics where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Transformation.Helpers

transformPlatformSpecificsIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformPlatformSpecificsIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      InlineAsmWithx86Instructions  -> transformInlineAsmWithx86Instructions  a issue
      AsmBlocks                     -> transformAsmBlocks                     a issue
      HandleTypesCastToInt          -> transformHandleTypesCastToInt          a issue
      X86SpecificCompilerIntrinsics -> transformX86SpecificCompilerIntrinsics a issue
      AssumptionsAboutRegSizes      -> transformAssumptionsAboutRegSizes      a issue
      _                             -> (a, Just issue)

transformInlineAsmWithx86Instructions :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformInlineAsmWithx86Instructions ast issue = (ast, Just issue)

transformAsmBlocks :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformAsmBlocks ast issue = (ast, Just issue)

transformHandleTypesCastToInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformHandleTypesCastToInt ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

transformX86SpecificCompilerIntrinsics :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformX86SpecificCompilerIntrinsics ast issue = (ast, Just issue)

transformAssumptionsAboutRegSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformAssumptionsAboutRegSizes ast issue = (ast, Just issue)
