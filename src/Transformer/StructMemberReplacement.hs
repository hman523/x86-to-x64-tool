-- | Rewrites @long@ and @unsigned long@ members of struct and union types to
--   semantically equivalent fixed-width types.
--
--   On LP64 (Linux/macOS x86-64) the @long@ type grows from 4 to 8 bytes.
--   Any struct or union that contains a @long@ member will silently change
--   size and field alignment, breaking ABI compatibility.  Replacing those
--   members with @int32_t@ / @uint32_t@ preserves the original layout.
--
--   No usage-based classification is performed: all struct/union @long@
--   members default to @int32_t@ (or @uint32_t@ if unsigned) because the
--   goal is layout preservation, not semantic classification.
--
--   The pass targets @CStructureUnion@ nodes via a generic @everywhere@
--   traversal so it correctly rewrites:
--     * top-level struct\/union definitions
--     * struct\/union types embedded in function parameters or local decls
--     * nested struct\/union definitions
module Transformer.StructMemberReplacement
    ( transformStructMembers
    ) where

import Data.Generics          (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo)

import Transformer.Helpers    (typedefSpec, hasExactlyOneLong, hasUnsignedSpec,
                               isTypeSpec)

-- | Rewrite @long@ / @unsigned long@ members in every struct and union in
--   the translation unit.
transformStructMembers :: CTranslUnit -> CTranslUnit
transformStructMembers = everywhere (mkT fixSU)

-- | Rewrite the member declarations of a struct or union definition.
--   Leaves structs and unions that have no body (@Nothing@) untouched.
fixSU :: CStructureUnion NodeInfo -> CStructureUnion NodeInfo
fixSU (CStruct tag name (Just members) attrs ni) =
    CStruct tag name (Just (map fixMember members)) attrs ni
fixSU su = su

-- | Rewrite a single member declaration if its base type is @long@ or
--   @unsigned long@ (but not @long long@).
fixMember :: CDeclaration NodeInfo -> CDeclaration NodeInfo
fixMember (CDecl specs declrs ni)
    | hasExactlyOneLong specs =
        let newSpec = typedefSpec (if hasUnsignedSpec specs then "uint32_t" else "int32_t")
        in CDecl (newSpec : filter (not . isTypeSpec) specs) declrs ni
fixMember d = d
