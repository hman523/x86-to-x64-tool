module Analysis.MemoryAllocation
  ( analyzeMemoryAllocationIssues
  , checkAllocationSizeCalcsMayOverflow
  , checkMallocWithoutOverflowChecking
  , checkUsingIntToStoreAllocationSizes
  ) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import Analysis.KnownFunctions (allocFns)
import qualified Data.Map as Map

analyzeMemoryAllocationIssues :: CTranslUnit -> [Issue]
analyzeMemoryAllocationIssues ast =
    checkAllocationSizeCalcsMayOverflow ast
    ++ checkMallocWithoutOverflowChecking ast
    ++ checkUsingIntToStoreAllocationSizes ast

-- | Flag malloc/calloc/realloc where the size argument is a multiplication of
--   two int-typed values (product may overflow before widening to size_t).
checkAllocationSizeCalcsMayOverflow :: CTranslUnit -> [Issue]
checkAllocationSizeCalcsMayOverflow ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAlloc tenv) Map.empty) decls
  where
    checkAlloc tenv env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` allocFns =
            concatMap (checkSizeExpr tenv env info) args
    checkAlloc _ _ _ = []

    checkSizeExpr tenv env callInfo (CBinary CMulOp l r _) =
        let lt = resolveTypedef tenv (typeOfExpr env l)
            rt = resolveTypedef tenv (typeOfExpr env r)
        in [ createIssue callInfo Critical AllocationSizeCalcsMayOverflow
           | (isIntType' lt || isUIntType lt) && (isIntType' rt || isUIntType rt) ]
    checkSizeExpr _ _ _ _ = []

-- | Flag malloc/calloc/realloc where the size arg is an integer addition
--   without overflow protection (sum can wrap before reaching size_t width).
checkMallocWithoutOverflowChecking :: CTranslUnit -> [Issue]
checkMallocWithoutOverflowChecking ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAlloc tenv) Map.empty) decls
  where
    checkAlloc tenv env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` allocFns =
            concatMap (checkSizeExpr tenv env info) args
    checkAlloc _ _ _ = []

    checkSizeExpr tenv env callInfo (CBinary CAddOp l r _) =
        let lt = resolveTypedef tenv (typeOfExpr env l)
            rt = resolveTypedef tenv (typeOfExpr env r)
        in [ createIssue callInfo Warning MallocWithoutOverflowChecking
           | (isIntType' lt || isUIntType lt) && (isIntType' rt || isUIntType rt) ]
    checkSizeExpr _ _ _ _ = []

-- | Flag assignments where a sizeof-based expression is stored in an
--   int/uint variable (sizeof returns size_t; truncating to int loses the
--   high 32 bits on 64-bit).
--
--   Note: this intentionally overlaps with
--   'Analysis.Alignment.checkSizeofStoredIn32bits' — the two checks live in
--   different categories (MemoryAllocation vs Alignment) so both are reported.
checkUsingIntToStoreAllocationSizes :: CTranslUnit -> [Issue]
checkUsingIntToStoreAllocationSizes ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAssign tenv) Map.empty) decls
  where
    checkAssign tenv env (CAssign CAssignOp lhs rhs info) =
        let lhsType  = resolveTypedef tenv (typeOfExpr env lhs)
            mDeclPos = case lhs of
                CVar (Ident name _ _) _ -> lookupDeclPos env name
                _                       -> Nothing
        in [ createIssueWithDecl info mDeclPos Warning UsingIntToStoreAllocationSizes
           | (isIntType' lhsType || isUIntType lhsType) && anySizeof rhs ]
    checkAssign _ _ _ = []

    anySizeof (CSizeofType _ _) = True
    anySizeof (CSizeofExpr _ _) = True
    anySizeof (CBinary _ l r _) = anySizeof l || anySizeof r
    anySizeof _                 = False