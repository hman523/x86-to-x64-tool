module Analysis.MemoryAllocation where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Analysis.UtilTypes
import Analysis.TypeChecker
import qualified Data.Map as Map

analyzeMemoryAllocationIssues :: CTranslUnit -> [Issue]
analyzeMemoryAllocationIssues ast =
    checkAllocationSizeCalculationsMayOverflow ast
    ++ checkMallocWithoutOverflowChecking ast
    ++ checkUsingIntToStoreAllocationSizes ast

allocFns :: [String]
allocFns = ["malloc", "calloc", "realloc"]

-- | Flag malloc/calloc/realloc where the size argument is a multiplication of
--   two int-typed values (product may overflow before widening to size_t).
checkAllocationSizeCalculationsMayOverflow :: CTranslUnit -> [Issue]
checkAllocationSizeCalculationsMayOverflow ast@(CTranslUnit decls _) =
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
        in [ createIssue callInfo Critical AllocationSizeCalculationsMayOverflow
           | isIntType' lt && isIntType' rt ]
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
           | isIntType' lt && isIntType' rt ]
    checkSizeExpr _ _ _ _ = []

-- | Flag assignments where a sizeof result is stored in an int/uint variable
--   (sizeof returns size_t; truncating to int loses the high 32 bits on 64-bit).
checkUsingIntToStoreAllocationSizes :: CTranslUnit -> [Issue]
checkUsingIntToStoreAllocationSizes ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAssign tenv) Map.empty) decls
  where
    checkAssign tenv env (CAssign CAssignOp lhs rhs info) =
        let lhsType = resolveTypedef tenv (typeOfExpr env lhs)
        in [ createIssue info Warning UsingIntToStoreAllocationSizes
           | isIntType' lhsType && hasSizeof rhs ]
    checkAssign _ _ _ = []

    hasSizeof (CSizeofType _ _) = True
    hasSizeof (CSizeofExpr _ _) = True
    hasSizeof (CBinary _ l r _) = hasSizeof l || hasSizeof r
    hasSizeof _                 = False