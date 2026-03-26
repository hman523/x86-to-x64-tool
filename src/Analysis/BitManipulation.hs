module Analysis.BitManipulation where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Analysis.UtilTypes
import Analysis.TypeChecker
import qualified Data.Map as Map

analyzeBitManipulationIssues :: CTranslUnit -> [Issue]
analyzeBitManipulationIssues ast =
    checkPackingPtrsWithFlagsInInt ast
    ++ checkBitShiftsOnPtr ast
    ++ checkExtractingPtrBitsIn32BitVar ast

-- | Flag bit-OR of a pointer with flags, then cast to int (packs pointer+flags in int).
checkPackingPtrsWithFlagsInInt :: CTranslUnit -> [Issue]
checkPackingPtrsWithFlagsInInt ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkCast tenv) Map.empty) decls
  where
    checkCast tenv env (CCast castDecl inner info) =
        let castTo = resolveTypedef tenv (typeOfDecl castDecl)
        in case (isIntType' castTo, inner) of
            (True, CBinary COrOp l _ _) ->
                let lt = resolveTypedef tenv (typeOfExpr env l)
                in [ createIssue info Critical PackingPtrsWithFlagsInInt
                   | isPointer lt ]
            _ -> []
    checkCast _ _ _ = []

-- | Flag bitwise shift operations directly on a pointer-typed value.
checkBitShiftsOnPtr :: CTranslUnit -> [Issue]
checkBitShiftsOnPtr ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv env (CBinary op l _ info)
        | op `elem` [CShlOp, CShrOp] =
            let lt = resolveTypedef tenv (typeOfExpr env l)
            in [ createIssue info Critical BitShiftsOnPtr | isPointer lt ]
    checkExpr _ _ _ = []

-- | Flag cast to int of a right-shifted pointer (extracts pointer bits in 32-bit var).
checkExtractingPtrBitsIn32BitVar :: CTranslUnit -> [Issue]
checkExtractingPtrBitsIn32BitVar ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkCast tenv) Map.empty) decls
  where
    checkCast tenv env (CCast castDecl inner info) =
        let castTo = resolveTypedef tenv (typeOfDecl castDecl)
        in case (isIntType' castTo, inner) of
            (True, CBinary CShrOp l _ _) ->
                let lt = resolveTypedef tenv (typeOfExpr env l)
                in [ createIssue info Critical ExtractingPtrBitsIn32BitVar
                   | isPointer lt ]
            _ -> []
    checkCast _ _ _ = []