module Analysis.PlatformSpecifics
  ( analyzePlatformSpecificIssues
  , checkInlineAsmWithx86Instructions
  , checkAsmBlocks
  , checkHandleTypesCastToInt
  , checkx86SpecificCompilerIntrinsics
  , checkAssumptionsAboutRegSizes
  ) where

import Language.C.Syntax.AST
import Language.C.Syntax.Constants (getCInteger, CString(..))
import Language.C.Data.Node
import Language.C.Data.Ident
import Data.List (isPrefixOf, isInfixOf)
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import Analysis.KnownFunctions (handleTypes, intrinsicPrefixes)
import qualified Data.Map as Map

analyzePlatformSpecificIssues :: CTranslUnit -> [Issue]
analyzePlatformSpecificIssues ast =
    checkInlineAsmWithx86Instructions ast
    ++ checkAsmBlocks ast
    ++ checkHandleTypesCastToInt ast
    ++ checkx86SpecificCompilerIntrinsics ast
    ++ checkAssumptionsAboutRegSizes ast

-- ---------------------------------------------------------------------------
-- Statement-level walker (needed for CAsm nodes)
-- ---------------------------------------------------------------------------

-- | Apply @f@ to every statement in the translation unit (depth-first).
walkAllStmts :: (CStatement NodeInfo -> [Issue]) -> CTranslUnit -> [Issue]
walkAllStmts f (CTranslUnit decls _) = concatMap checkTop decls
  where
    checkTop (CFDefExt (CFunDef _ _ _ body _)) = walkStmt body
    checkTop _                                  = []

    walkStmt stmt = f stmt ++ case stmt of
        CCompound _ items _ -> concatMap walkItem items
        CIf _ t e _         -> walkStmt t ++ maybe [] walkStmt e
        CWhile _ body _ _   -> walkStmt body
        CFor _ _ _ body _   -> walkStmt body
        CSwitch _ body _    -> walkStmt body
        CLabel _ s _ _      -> walkStmt s
        _                   -> []

    walkItem (CBlockStmt s) = walkStmt s
    walkItem _              = []

-- ---------------------------------------------------------------------------
-- Checks
-- ---------------------------------------------------------------------------

-- | Flag any inline assembly block.
checkAsmBlocks :: CTranslUnit -> [Issue]
checkAsmBlocks = walkAllStmts checkStmt
  where
    checkStmt (CAsm _ info) = [createIssue info Critical AsmBlocks]
    checkStmt _             = []

-- | Flag inline asm containing x86 register names (eax, ebx, ecx, edx, etc.).
checkInlineAsmWithx86Instructions :: CTranslUnit -> [Issue]
checkInlineAsmWithx86Instructions = walkAllStmts checkStmt
  where
    checkStmt (CAsm (CAsmStmt _ asmStr _ _ _ _) info) =
        let str = getAsmStr asmStr
        in [ createIssue info Critical InlineAsmWithx86Instructions
           | any (`isInfixOf` str) x86Regs ]
    checkStmt _ = []

    getAsmStr (CStrLit (CString s _) _) = s

    x86Regs :: [String]
    x86Regs = [ "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%esp", "%ebp"
              , "eax", "ebx", "ecx", "edx", "esi", "edi", "esp", "ebp" ]

-- | Flag casts of Windows HANDLE-family typedefs to int.
checkHandleTypesCastToInt :: CTranslUnit -> [Issue]
checkHandleTypesCastToInt ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkCast tenv) Map.empty) decls
  where
    checkCast tenv env (CCast castDecl inner info) =
        let castTo    = resolveTypedef tenv (typeOfDecl castDecl)
            innerType = peelCastType env inner
        in case innerType of
            TTypedef name | name `elem` handleTypes ->
                if isIntType' castTo      then [createIssue info Warning HandleTypesCastToInt]
                else [createIssue info Warning HandleTypesCastToUInt | isUIntType castTo]
            _ -> []
    checkCast _ _ _ = []

-- | Flag calls to x86-specific SIMD / compiler intrinsics.
checkx86SpecificCompilerIntrinsics :: CTranslUnit -> [Issue]
checkx86SpecificCompilerIntrinsics (CTranslUnit decls _) =
    concatMap (analyzeDecl checkExpr Map.empty) decls
  where
    checkExpr _env (CVar (Ident name _ _) info) =
        [ createIssue info Warning X86SpecificCompilerIntrinsics
        | any (`isPrefixOf` name) intrinsicPrefixes ]
    checkExpr _ _ = []

-- | Flag comparisons of sizeof(T) with the literal 4 (assumes 32-bit register size).
checkAssumptionsAboutRegSizes :: CTranslUnit -> [Issue]
checkAssumptionsAboutRegSizes (CTranslUnit decls _) =
    concatMap (analyzeDecl checkExpr Map.empty) decls
  where
    checkExpr _env expr = case expr of
        CBinary op (CSizeofType _ _) (CConst (CIntConst n _)) info
            | op `elem` [CEqOp, CNeqOp] && getCInteger n == 4 ->
                [createIssue info Warning AssumptionsAboutRegSizes]
        CBinary op (CConst (CIntConst n _)) (CSizeofType _ _) info
            | op `elem` [CEqOp, CNeqOp] && getCInteger n == 4 ->
                [createIssue info Warning AssumptionsAboutRegSizes]
        _ -> []
