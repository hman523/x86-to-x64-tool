{-# LANGUAGE LambdaCase #-}
module Linter.PlatformSpecifics
  ( lintPlatformSpecificsIssues
  ) where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintPlatformSpecificsIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintPlatformSpecificsIssues = dispatchLinter $ \case
    InlineAsmWithx86Instructions  -> Just lintInlineAsmWithx86Instructions
    AsmBlocks                     -> Just lintAsmBlocks
    HandleTypesCastToInt          -> Just lintHandleTypesCastToInt
    HandleTypesCastToUInt         -> Just lintHandleTypesCastToUInt
    X86SpecificCompilerIntrinsics -> Just lintX86SpecificCompilerIntrinsics
    AssumptionsAboutRegSizes      -> Just lintAssumptionsAboutRegSizes
    _                             -> Nothing

-- Cannot be done automatically: the asm block uses x86 32-bit register names
-- (%eax, %ebx, etc.). The 64-bit counterparts (%rax, %rbx, etc.) differ in
-- width and calling convention, so the entire block must be semantically
-- understood and rewritten. Syntactic register-name substitution is not
-- sufficient to produce correct 64-bit code.
lintInlineAsmWithx86Instructions :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintInlineAsmWithx86Instructions = unlintable

-- Cannot be done automatically: any inline asm block is architecture-specific
-- by definition. Translating it to x86-64 requires understanding the assembly's
-- purpose and rewriting it; there is no general-purpose asm translator that can
-- be applied safely from the AST.
lintAsmBlocks :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintAsmBlocks = unlintable

lintHandleTypesCastToInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintHandleTypesCastToInt ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

lintHandleTypesCastToUInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintHandleTypesCastToUInt ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "uintptr_t") ast, Nothing)

-- Cannot be done automatically: x86 SIMD intrinsics (_mm_*, __builtin_ia32_*)
-- are 32-bit-specific. While 64-bit x86 also supports SSE/AVX, the programmer
-- must decide whether to migrate to a wider intrinsic family, use a portable
-- abstraction, or replace them with scalar code. The right choice depends on
-- performance requirements and target ABI.
lintX86SpecificCompilerIntrinsics :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintX86SpecificCompilerIntrinsics = unlintable

-- Cannot be done automatically: sizeof(T) == 4 was true on 32-bit but may be
-- false on 64-bit. The literal 4 could encode pointer size, int size, or
-- something else, and the guarded code may need to become the unconditional
-- path, be removed, or be restructured. The tool cannot determine which
-- branch reflects the intended 64-bit behavior.
lintAssumptionsAboutRegSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintAssumptionsAboutRegSizes = unlintable
