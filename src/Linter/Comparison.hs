module Linter.Comparison where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintComparisonIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintComparisonIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      LoopCounterAsIntWhenIteratingOverPtrArrays -> lintLoopCounterAsIntWhenIteratingOverPtrArrays a issue
      PtrComparisonWithIntConsts                 -> lintPtrComparisonWithIntConsts                 a issue
      UsingIntForFileOffsets                     -> lintUsingIntForFileOffsets                     a issue
      _                                          -> (a, Just issue)

lintLoopCounterAsIntWhenIteratingOverPtrArrays :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintLoopCounterAsIntWhenIteratingOverPtrArrays ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)
    Nothing -> (ast, Just issue)

-- Cannot be done automatically: ordering a pointer against an integer constant
-- (e.g., ptr < 0x1000) is undefined behavior. The correct fix — casting to
-- uintptr_t and comparing against a symbolic constant — requires knowing what
-- range or address the programmer is guarding against, which is not recoverable
-- from the AST alone.
lintPtrComparisonWithIntConsts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintPtrComparisonWithIntConsts = unlintable

lintUsingIntForFileOffsets :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUsingIntForFileOffsets ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "off_t") ast, Nothing)
    Nothing -> (ast, Just issue)
