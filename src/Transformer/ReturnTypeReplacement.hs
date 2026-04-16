-- | Rewrites @long@ and @unsigned long@ function return types to
--   semantically equivalent fixed-width types.
--
--   The replacement type is determined using the same five-category
--   classification that 'Transformer.LongReplacement' applies to variables:
--   the function body's return statements are inspected to determine what
--   kind of value is actually returned, and the most specific applicable
--   category wins.
--
--   This pass is semantically equivalent because:
--     * Only the function's own translation unit is modified.
--     * The new return type faithfully represents the range/semantics of the
--       returned value (e.g. a function that returns a pointer-sized integer
--       gets @intptr_t@, which has the same representation on LP64).
--     * Callers in the same file use the return value through the declared
--       type, which is updated consistently by this pass.
--
--   Functions whose return statements cannot be classified default to
--   @int32_t@ / @uint32_t@ (the 'NumberType' default).
module Transformer.ReturnTypeReplacement
    ( transformReturnTypes
    ) where

import Data.Generics           (listify)
import Language.C.Syntax.AST
import Language.C.Data.Node    (NodeInfo)
import Transformer.Helpers     (retypeFunReturnType, hasExactlyOneLong,
                                hasUnsignedSpec)
import Transformer.LongReplacement
    ( buildFunEnv, toSpec )
import Transformer.UsageClassifier
    ( AbstractType(..), rhsEvidence, stronger )

-- | Rewrite the declared return type of every function in the translation
--   unit whose return type is @long@ or @unsigned long@.
transformReturnTypes :: CTranslUnit -> CTranslUnit
transformReturnTypes ast@(CTranslUnit decls _) = foldl applyToDecl ast decls

applyToDecl :: CTranslUnit -> CExternalDeclaration NodeInfo -> CTranslUnit
applyToDecl ast (CFDefExt funDef) = applyToFunDef funDef ast
applyToDecl ast _                 = ast

-- | If the function's declared return type is @long@ or @unsigned long@,
--   classify the return value and retype the function header.
applyToFunDef :: CFunctionDef NodeInfo -> CTranslUnit -> CTranslUnit
applyToFunDef funDef@(CFunDef specs (CDeclr _ _ _ _ dNi) _ _ _) ast
    | hasExactlyOneLong specs =
        let cls     = classifyReturnType funDef
            newSpec = toSpec (hasUnsignedSpec specs) cls
        in retypeFunReturnType dNi newSpec ast
    | otherwise = ast

-- ---------------------------------------------------------------------------
-- Return-value classification
-- ---------------------------------------------------------------------------

-- | Classify the return type of a function by scanning all its @return@
--   statements and taking the highest-priority category.  Falls back to
--   'NumberType' when no evidence is found.
classifyReturnType :: CFunctionDef NodeInfo -> AbstractType
classifyReturnType funDef@(CFunDef _ _ _ body _) =
    let env    = buildFunEnv funDef
        retExprs = [ e | CReturn (Just e) _ <- listify isReturn body ]
        evidence = concatMap (rhsEvidence env) retExprs
    in if null evidence then NumberType
       else foldr1 stronger evidence
  where
    isReturn :: CStatement NodeInfo -> Bool
    isReturn (CReturn _ _) = True
    isReturn _             = False
