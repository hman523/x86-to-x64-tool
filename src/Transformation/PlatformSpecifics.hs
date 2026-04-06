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

-- Cannot be done automatically: the asm block uses x86 32-bit register names
-- (%eax, %ebx, etc.). The 64-bit counterparts (%rax, %rbx, etc.) differ in
-- width and calling convention, so the entire block must be semantically
-- understood and rewritten. Syntactic register-name substitution is not
-- sufficient to produce correct 64-bit code.
transformInlineAsmWithx86Instructions :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformInlineAsmWithx86Instructions = untransformable

-- Cannot be done automatically: any inline asm block is architecture-specific
-- by definition. Translating it to x86-64 requires understanding the assembly's
-- purpose and rewriting it; there is no general-purpose asm translator that can
-- be applied safely from the AST.
transformAsmBlocks :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformAsmBlocks = untransformable

transformHandleTypesCastToInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformHandleTypesCastToInt ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

-- Cannot be done automatically: x86 SIMD intrinsics (_mm_*, __builtin_ia32_*)
-- are 32-bit-specific. While 64-bit x86 also supports SSE/AVX, the programmer
-- must decide whether to migrate to a wider intrinsic family, use a portable
-- abstraction, or replace them with scalar code. The right choice depends on
-- performance requirements and target ABI.
transformX86SpecificCompilerIntrinsics :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformX86SpecificCompilerIntrinsics = untransformable

-- Cannot be done automatically: sizeof(T) == 4 was true on 32-bit but may be
-- false on 64-bit. The literal 4 could encode pointer size, int size, or
-- something else, and the guarded code may need to become the unconditional
-- path, be removed, or be restructured. The tool cannot determine which
-- branch reflects the intended 64-bit behavior.
transformAssumptionsAboutRegSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformAssumptionsAboutRegSizes = untransformable
