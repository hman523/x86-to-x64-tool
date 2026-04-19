# x86-to-x64-tool: Design Documentation

## Table of Contents

1. [Project Overview](#1-project-overview)
2. [High-Level Architecture](#2-high-level-architecture)
3. [Module Structure](#3-module-structure)
4. [The Parser Layer](#4-the-parser-layer)
5. [The Analysis Layer](#5-the-analysis-layer)
   - 5.1 [Type Checker](#51-type-checker)
   - 5.2 [AST Traversal Framework](#52-ast-traversal-framework)
   - 5.3 [Issue Types](#53-issue-types)
   - 5.4 [Analysis Orchestrator](#54-analysis-orchestrator)
   - 5.5 [Analysis Sub-Modules](#55-analysis-sub-modules)
   - 5.6 [Known Functions Registry](#56-known-functions-registry)
6. [The Linter Layer](#6-the-linter-layer)
   - 6.1 [Linter Orchestrator](#61-linter-orchestrator)
   - 6.2 [Linter Helpers](#62-linter-helpers)
   - 6.3 [Linter Sub-Modules](#63-linter-sub-modules)
7. [The Transformer Layer](#7-the-transformer-layer)
   - 7.1 [Transformer Orchestrator and Pipeline](#71-transformer-orchestrator-and-pipeline)
   - 7.2 [Usage Classifier](#72-usage-classifier)
   - 7.3 [Long Replacement](#73-long-replacement)
   - 7.4 [Supporting Transformer Passes](#74-supporting-transformer-passes)
   - 7.5 [Semantically Equivalent vs. Non-SE Fixes](#75-semantically-equivalent-vs-non-se-fixes)
8. [Public API (`X86_to_X64`)](#8-public-api-x86_to_x64)
9. [Command-Line Interface](#9-command-line-interface)
10. [Test Suite](#10-test-suite)
    - 10.1 [Unit and Integration Tests](#101-unit-and-integration-tests)
    - 10.2 [Cross-Architecture Semantic Tests](#102-cross-architecture-semantic-tests)
11. [Key Design Decisions](#11-key-design-decisions)
12. [Data-Flow Diagrams](#12-data-flow-diagrams)

---

## 1. Project Overview

`x86-to-x64-tool` is a static-analysis and source-transformation tool for C programs that need to be ported from 32-bit (x86 / ILP32 [Ints, Longs, and Pointers are 32 bit]) to 64-bit (x86-64 / LP64 [Longs and Pointers are 64 bit]) platforms.  It is implemented in Haskell and uses the [`language-c`](https://hackage.haskell.org/package/language-c) library to parse C source into a typed abstract syntax tree (AST).

The tool operates in three distinct modes:

| Mode | Flag | What it does |
|------|------|--------------|
| **Analysis** | _(default)_ | Parse a C file and print all detected portability issues with file/line information. |
| **Lint** | `-l` | Apply automated AST-level fixes for every issue that has a semantically unambiguous fix; write the result to `<input>.x64.c`; report any issues that could not be resolved. |
| **Transform** | `-t` | Rewrite `long`/`unsigned long` declarations to semantically equivalent fixed-width types (`int32_t`, `intptr_t`, `size_t`, etc.) based on inferred usage; write the result to `<input>.x64.c`. |

`-l` and `-t` are mutually exclusive because they apply different (and
potentially conflicting) policies to `long`-typed variables.

---

## 2. High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                          app/Main.hs                                │
│          (CLI argument parsing, mode dispatch, colored output)      │
└────────────────────────────┬────────────────────────────────────────┘
                             │  calls
                             ▼
┌─────────────────────────────────────────────────────────────────────┐
│                       src/X86_to_X64.hs                             │
│              (Public API: analyzeFile, lintFile, transformFile)     │
└──────┬──────────────────────┬───────────────────────────────────────┘
       │                      │                       │
       ▼                      ▼                       ▼
┌─────────────┐   ┌───────────────────┐   ┌──────────────────────────┐
│   Parser    │   │     Analysis      │   │       Linter /           │
│  (language-c│   │  (11 sub-modules) │   │     Transformer          │
│   library)  │   │                   │   │  (11 + 8 sub-modules)    │
└──────┬──────┘   └─────────┬─────────┘   └─────────────┬────────────┘
       │                    │                           │
       │  CTranslUnit       │  [Issue]                  │  CTranslUnit
       └───────────────────►│◄──────────────────────────┘
                            │
                   AST + Issues drive all three layers
```

All three layers share a single representation: the `language-c`
`CTranslUnit` (C translation unit).  The Parser produces it; the Analysis
layer consumes it read-only; and the Linter and Transformer layers produce
a modified copy.

---

## 3. Module Structure

```
src/
├── X86_to_X64.hs               ← Public API (library root)
├── Parser/
│   ├── Parser.hs               ← Thin wrapper around language-c
│   └── FormatSpecParser.hs     ← Shared printf length-modifier parser
├── Analysis/
│   ├── Analysis.hs             ← Orchestrator (calls all 11 sub-modules)
│   ├── IssueTypes.hs           ← Issue, IssueTag, Severity, Category ADTs
│   ├── ASTTraversal.hs         ← Generic traversal helpers (analyzeDecl etc.)
│   ├── TypeChecker.hs          ← CType ADT, TypeEnv, type resolution
│   ├── KnownFunctions.hs       ← Central registry of known C library functions 
|   |                                 (ie malloc, calloc, etc)
│   └── {Alignment,BitManipulation,Comparison,ConstantsLiterals,
│         FormatStrings,FunctionSignatures,MemoryAllocation,
│         PlatformSpecifics,PointerMath,Serialization,TypeSize}.hs
├── Linter/
│   ├── Linter.hs               ← Orchestrator
│   ├── Helpers.hs              ← Shared AST-rewrite utilities
│   └── {Alignment,…,TypeSize}.hs  (mirrors Analysis sub-modules)
└── Transformer/
    ├── Transformer.hs          ← Pipeline orchestrator + addRequiredIncludes
    ├── UsageClassifier.hs      ← 5-category usage classification for `long`
    ├── LongReplacement.hs      ← Core `long` → fixed-width rewrite pass
    ├── Helpers.hs              ← Shared AST-rewrite utilities
    ├── StructMemberReplacement.hs
    ├── TypedefReplacement.hs
    ├── ReturnTypeReplacement.hs
    ├── CastSync.hs
    ├── StandaloneCastFix.hs
    ├── FunPtrReplacement.hs
    └── FormatFix.hs

app/
└── Main.hs                     ← Executable entry point

test/
├── Main.hs                     ← Test runner (hspec)
├── Parser/
├── Analysis/
├── Linter/
├── Transformer/
├── CrossArch/                  ← Semantic equivalence tests via Podman
└── c_progs/                    ← Test C files (plain + cross_arch/)
```

---

## 4. The Parser Layer

**Files:** `Parser/Parser.hs`, `Parser/FormatSpecParser.hs`

### `Parser.Parser`

A thin wrapper around `language-c`.  Exposes four entry points:

| Function | Description |
|---|---|
| `parseSource` | Parse a `ByteString` directly. |
| `parseSourceString` | Parse from a `String`. |
| `parseSourceFile` | Read a file from disk and parse it, recording the filename in position info. |
| `parseSourceFileWithCPP` | Run `gcc -E` first (preprocesses `#include`s and macros), then parse. |

All functions return `Either ParseError CTranslUnit`.  The `CTranslUnit` type
from `language-c` is the universal currency passed between all subsequent
layers.  Importantly, the preprocessor strips directives from the AST; this is
why `addRequiredIncludes` in the Transformer layer must re-inject `#include`
directives into the output string after pretty-printing.

### `Parser.FormatSpecParser`

Parses C printf-style **length modifiers** (e.g. `""`, `"h"`, `"hh"`, `"l"`,
`"ll"`, `"z"`, `"t"`).  Previously duplicated across `Analysis.FormatStrings`,
`Linter.FormatStrings`, and `Transformer.FormatFix`; centralised here so a bug
fix only needs to be applied once.

---

## 5. The Analysis Layer

The analysis layer is a **read-only pass** over the AST.  It produces a list
of `Issue` values, each describing one portability problem found in the source.

### 5.1 Type Checker

**File:** `Analysis/TypeChecker.hs`

The type checker provides a lightweight representation of C types and
environments that is sufficient for the kinds of questions the analysis and
transformation passes need to ask (e.g. "is this expression a pointer?", "does
this struct contain pointer-typed members?").  It does **not** attempt full
C11 type checking.

#### `CType` ADT

```
TInt | TUInt | TShort | TUShort | TLong | TULong | TLongLong | TULongLong
| TChar | TFloat | TDouble | TVoid
| TPointer CType    -- pointer-to, e.g. int *
| TArray CType      -- array-of, e.g. int []
| TStruct String    -- struct by tag name
| TUnion String     -- union by tag name
| TTypedef String   -- typedef name (unresolved alias)
| TUnknown          -- fallback when type cannot be determined
```

#### Key environments

| Type | Description |
|---|---|
| `TypeEnv` | `Map String (CType, Maybe NodeInfo)` — maps variable names to their resolved type and declaration position. Threaded through statement/expression traversal. |
| `TypedefEnv` | `Map String CType` — maps typedef names to the underlying type. |
| `StructEnv` | (used implicitly) — determines whether a struct contains pointer members via `structHasPointer`. |

#### Important functions

- `resolveType` / `resolveBaseType` — convert `language-c` declaration specifiers + derived declarators into a `CType`.
- `typeOfExpr` — infer the `CType` of a `CExpression` from the current `TypeEnv`.
- `collectDecl` — add a declaration to a `TypeEnv`.
- `buildTypeEnv` — build an env from a list of block items.
- `buildGlobalEnv` — build an env from all file-scope declarations.
- `resolveTypedef` — chase typedef chains until a concrete type is found.
- `promoteArith` — model C arithmetic promotion rules.

### 5.2 AST Traversal Framework

**File:** `Analysis/ASTTraversal.hs`

Every analysis sub-module follows the same traversal pattern:

```haskell
concatMap (analyzeDecl myChecker Map.empty) (translUnitDecls ast)
```

where `myChecker :: TypeEnv -> CExpression NodeInfo -> [Issue]` inspects a
single expression and returns any issues it detects.

The traversal helpers handle the structural descent:

```
analyzeDecl
  └── analyzeFunDef (extracts parameter env)
        └── analyzeStmt (extends env at each new scope)
              └── walkExpr (recurses into sub-expressions)
```

`analyzeStmt` handles all statement forms:

- **`CExpr`** — walk the single expression.
- **`CCompound`** — process items left-to-right via `stepItem`, extending the `TypeEnv` each time a declaration is encountered.
- **`CIf`** / **`CWhile`** / **`CFor`** / **`CReturn`** / **`CSwitch`** / **`CCase`** / **`CDefault`** / **`CLabel`** / **`CGoto`** / etc.

The `for`-loop initialiser is treated specially: if it declares a new variable,
that variable is added to the env before the condition, step, and body are
checked.

### 5.3 Issue Types

**File:** `Analysis/IssueTypes.hs`

```haskell
data Severity = Critical | Warning

data Issue = Issue
  { issueType    :: IssueTag
  , issueSeverity :: Severity
  , issuePos     :: NodeInfo      -- position of the offending expression
  , issueDeclPos :: Maybe NodeInfo -- position of the relevant declaration
  }

data Category
  = AlignmentIssue | BitManipulationIssue | ComparisonIssue
  | ConstantLiteralsIssue | FormatStringsIssue | FunctionSignaturesIssue
  | MemoryAllocationIssue | PlatformSpecificsIssue | PointerMathIssue
  | SerializationIssue | TypeSizeIssue
```

`IssueTag` is an exhaustive enum with one constructor per distinct check
(~70 constructors total).  `getCategory` maps each tag to its `Category`.

The `prettyPrintIssues` function renders a list of issues to a terminal-width-
aware, optionally colour-coded, optionally verbose string suitable for display.

### 5.4 Analysis Orchestrator

**File:** `Analysis/Analysis.hs`

```haskell
analysis :: CTranslUnit -> [Issue]
analysis ast =
    analyzeAlignmentIssues ast
    ++ analyzeBitManipulationIssues ast
    ++ ...   -- 10 more sub-modules
```

Each sub-module independently traverses the full AST and returns its share of
issues.  The results are concatenated in a well-defined order (matching the
`Category` enum) but otherwise not deduplicated, because it is intentional for
two different checks to flag the same expression for distinct reasons.

### 5.5 Analysis Sub-Modules

Each sub-module has the signature:

```haskell
analyze<Category>Issues :: CTranslUnit -> [Issue]
```

and internally calls one or more focused checker functions, combining their
results.  Most checkers call `analyzeDecl` with a custom expression predicate.

| Module | Key checks |
|---|---|
| `Alignment` | Structs with pointers written/read via `fwrite`/`fread`, mixed pointer/non-pointer members, packed structs with pointers, `sizeof` stored in a 32-bit variable, hard-coded struct sizes. |
| `BitManipulation` | Packing pointers with integer flags, bit-shifts on pointer expressions, extracting pointer bits into a 32-bit variable. |
| `Comparison` | Loop counters declared `int` when iterating over pointer-sized arrays, pointer comparison against integer constants, `int` used for file offsets. |
| `ConstantsLiterals` | Magic literal values, bit-masks that assume 32-bit pointers, hard-coded addresses. |
| `FormatStrings` | `%d`/`%u`/`%x` used with `size_t`, `ptrdiff_t`, or pointer types; `%lu` used for pointer-sized values; `%ld` assuming 64-bit `long`. |
| `FunctionSignatures` | Functions declared to return `int`/`long` but actually returning a pointer; parameters declared `int` but passed a pointer; `va_arg` with incorrect types. |
| `MemoryAllocation` | Size calculations that may overflow, `malloc` without overflow checking, allocation sizes stored in `int`. |
| `PlatformSpecifics` | Inline `asm` blocks with x86 registers, x86-specific compiler intrinsics (`_mm_`, `__builtin_ia32_`...), Windows `HANDLE` types cast to `int`. |
| `PointerMath` | Pointer difference stored in a 32-bit type, pointer addition overflow, pointer subtraction underflow, array indexing with `int` on arrays larger than 2^31. |
| `Serialization` | Writing pointer values directly to files/network/shared memory. |
| `TypeSize` | `(int)ptr` / `(unsigned int)ptr` casts, `(int*)x` casts, `sizeof(int) == sizeof(void*)` comparisons, `int` used where `size_t` or `ptrdiff_t` is required. |

### 5.6 Known Functions Registry

**File:** `Analysis/KnownFunctions.hs`

A central registry of C standard-library function names referenced by multiple
modules:

| Export | Contents |
|---|---|
| `allocFns` | `malloc`, `calloc`, `realloc` |
| `sizeArgFunctions` | `allocFns` + `memcpy`, `memmove`, `memset`, `memcmp`, `alloca` |
| `ioWriteFns` / `ioReadFns` | `fwrite`/`write` and `fread`/`read` |
| `networkSendFns` | `send`, `sendto` |
| `handleTypes` | `HANDLE`, `HWND`, `HMODULE`, … |
| `intrinsicPrefixes` | `_mm_`, `_mm256_`, `__builtin_ia32_`, … |

Centralising these eliminates the risk of modules diverging in their coverage.

---

## 6. The Linter Layer

The linter applies AST-level source transformations to automatically fix issues
that have an unambiguous, semantically equivalent repair.  Issues that require
human judgement are returned as "unresolved" and reported to the user.

### 6.1 Linter Orchestrator

**File:** `Linter/Linter.hs`

```haskell
lint :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
```

Takes the AST and the full issue list produced by `analysis`.  Iterates over
the 11 issue categories, calling each sub-module's linter with only the issues
belonging to that category.  Returns the accumulated (and potentially modified)
AST together with any issues that could not be resolved.

The fold is right-to-left over the module list so that each module sees a fresh
partial result; an issue resolved by one module is not re-presented to another.

### 6.2 Linter Helpers

**File:** `Linter/Helpers.hs`

Provides the building blocks used by every linter sub-module:

| Helper | Purpose |
|---|---|
| `unlintable` | Mark an issue unresolvable — returns the AST unchanged and the issue in the unresolved set. |
| `dispatchLinter` | Fold over a list of issues, looking up each `IssueTag` in a user-supplied table; unhandled tags become unresolved. |
| `typedefSpec` | Construct a `CTypeSpec` node for a typedef name (e.g. `"intptr_t"`). |
| `replaceCastType` | Walk the AST and replace the type specifier in a specific `CCast` node (identified by `NodeInfo` position). |
| `replaceCastTypeCollapsing` | Like `replaceCastType` but also collapses an intermediate cast (`(int)(long)ptr` → `(intptr_t)ptr`). |
| `retypeDecl` | Replace all type specifiers in a specific `CDecl` node. |
| `wrapReturnExpr` | Wrap the return expression of a specific function in a cast. |
| `replaceVaArgType` | Update the type argument of a `va_arg` call. |

All modifications are done via `Data.Generics.everywhere` / `mkT`, which
performs a bottom-up traversal of the full AST and rewrites every node that
matches a position predicate.

### 6.3 Linter Sub-Modules

Each sub-module has the signature:

```haskell
lint<Category>Issues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
```

and is implemented as a `dispatchLinter` call with a `case`-on-`IssueTag`
dispatch table.

**Example automated fixes:**

| IssueTag | Fix applied |
|---|---|
| `CastPointerToInt` | `(int)ptr` → `(intptr_t)ptr` (collapses intermediate cast) |
| `CastPointerToUInt` | `(unsigned int)ptr` → `(uintptr_t)ptr` |
| `DUsedWithSizet` | `printf("%d", n)` → `printf("%zu", n)` (format specifier rewrite) |
| `UsingIntAsSizet` | `int x = sizeof(...)` → `size_t x = sizeof(...)` (declaration retype) |
| `FnsReturnPtrAsInt` | Wraps return expression in `(intptr_t)` cast |
| `MallocWithoutOverflowChecking` | Adds `if (n > SIZE_MAX / sizeof(T))` guard |

**Examples that are intentionally unlintable** (require human judgement):

| IssueTag | Reason |
|---|---|
| `CastIntToPointer` | Whether `(T*)x` should become `(T*)(intptr_t)x` or the variable type should change depends on intent. |
| `SizeOfIntIsVoid` | The branch that was 32-bit-correct may need to become unconditional, or be deleted entirely. |
| `InlineAsmWithx86Instructions` | Rewriting inline assembly is outside the scope of this tool. |

---

## 7. The Transformer Layer

The transformer is a heavier-weight rewrite pass dedicated specifically to the
`long` / `unsigned long` problem: on ILP32, `long` is 32 bits; on LP64, `long`
is 64 bits.  Code written for 32-bit that uses `long` for plain arithmetic will
silently change behaviour on 64-bit.

The transformer replaces every `long` and `unsigned long` declaration with a
semantically equivalent fixed-width type, chosen by inspecting how the variable
is actually used.

### 7.1 Transformer Orchestrator and Pipeline

**File:** `Transformer/Transformer.hs`

```haskell
transform :: CTranslUnit -> CTranslUnit
```

The pipeline is applied left-to-right (outer to inner in Haskell's function
composition):

```
1.  lint (SE-only issues)          -- fixes unambiguous non-long issues first
2.  transformStructMembers         -- long struct/union members → int32_t
3.  transformTypedefs              -- typedef long T → typedef int32_t T
4.  transformSizeofLong            -- sizeof(long) → sizeof(int32_t)
5.  transformReturnTypes           -- long f(...) return type → classified type
6.  splitMultiLongDecls            -- long a, b; → long a; long b; (normalise)
7.  transformLongs                 -- remaining long variables → classified type
                                      (produces RetypeMap)
8.  transformFunPtrParams          -- function-pointer parameters retyped
9.  syncCasts (RetypeMap)          -- (long) casts paired with retyped variables
10. fixStandaloneCasts             -- remaining (long) casts → (int32_t)/(intptr_t)
11. fixFormatStrings (RetypeMap)   -- %ld/%lu for retyped variables → %d/%u
12. stripLongSuffixes (RetypeMap)  -- 100L for int32_t variables → 100
```

After `transform`, `addRequiredIncludes` prepends the necessary `#include`
directives (`<stdint.h>`, `<stddef.h>`) to the pretty-printed output string,
since the parser strips preprocessor directives from the AST.

### 7.2 Usage Classifier

**File:** `Transformer/UsageClassifier.hs`

Classifies each `long` variable into one of five abstract type categories:

| Category | Replacement type | Evidence of this usage |
|---|---|---|
| `PointerType` | `intptr_t` / `uintptr_t` | Assigned from/to a pointer expression |
| `SizeType` | `size_t` | Assigned from `sizeof`, passed to a size-argument function |
| `OffsetType` | `ptrdiff_t` | Assigned from pointer subtraction |
| `BitSeqType` | `uint32_t` | Bitwise operations (`&`, `|`, `^`, `<<`, `>>`) |
| `NumberType` | `int32_t` / `uint32_t` | Default (pure arithmetic or no evidence found) |

Priority is `PointerType > SizeType > OffsetType > BitSeqType > NumberType`.
When multiple kinds of evidence are found for the same variable, the highest-
priority category wins (via `stronger`).

The classifier scans all expression nodes and declaration initialisers in a
function definition using `Data.Generics.listify`, inspecting assignments,
binary operations, function call arguments, and `return` statements.

`classifyVarAcrossFuns` extends this to handle cases where a global variable
is used in multiple functions.

### 7.3 Long Replacement

**File:** `Transformer/LongReplacement.hs`

The core rewrite pass.  Entry point:

```haskell
transformLongs :: CTranslUnit -> (CTranslUnit, RetypeMap)
```

where `RetypeMap = Map String (CDeclarationSpecifier NodeInfo)` records, for
each variable that was retyped, the new type specifier it received.  This map
is threaded through the remaining passes so they can make consistent decisions.

The pass classifies and rewrites:
- Function parameters
- Local variable declarations (including for-loop initialisers)
- File-scope (`extern`/`static`/plain) declarations
- `sizeof(long)` / `sizeof(unsigned long)` expressions

**Deliberately excluded** (handled by dedicated passes):
- `struct`/`union` member declarations → `StructMemberReplacement`
- Function return types → `ReturnTypeReplacement`
- Cast expressions → `CastSync` / `StandaloneCastFix`

`splitMultiLongDecls` runs before `transformLongs` to normalise declarations
like `long a, b;` into two separate `long a; long b;` declarations, so that
each variable can be classified independently.

### 7.4 Supporting Transformer Passes

| Pass | File | Purpose |
|---|---|---|
| `StructMemberReplacement` | `Transformer/StructMemberReplacement.hs` | Rewrites `long` struct and union members to `int32_t`. |
| `TypedefReplacement` | `Transformer/TypedefReplacement.hs` | Rewrites `typedef long T` to `typedef int32_t T`. |
| `ReturnTypeReplacement` | `Transformer/ReturnTypeReplacement.hs` | Classifies function return values and rewrites `long` return types to the appropriate fixed-width type; also updates matching forward declarations/prototypes in the same translation unit. |
| `CastSync` | `Transformer/CastSync.hs` | After `transformLongs` retypes `long x = (long)p` to `intptr_t x = (long)p`, the cast `(long)` is now inconsistent. `syncCasts` updates only those casts that are directly paired (by assignment or declaration) with a variable in the `RetypeMap`. |
| `StandaloneCastFix` | `Transformer/StandaloneCastFix.hs` | Rewrites remaining `(long)` / `(unsigned long)` casts that `syncCasts` did not handle. When the inner expression is pointer-typed, uses `(intptr_t)` / `(uintptr_t)`; otherwise `(int32_t)` / `(uint32_t)`. |
| `FunPtrReplacement` | `Transformer/FunPtrReplacement.hs` | Retypes function-pointer parameters that accept `long` arguments. |
| `FormatFix` | `Transformer/FormatFix.hs` | Updates `%ld` / `%lu` format specifiers in `printf`-family calls for variables that were retyped to `int32_t`/`uint32_t` (becomes `%d`/`%u`). |

### 7.5 Semantically Equivalent vs. Non-SE Fixes

The transformer is required to produce a **semantically equivalent** 64-bit
program — one that has identical observable behaviour (specifically: identical
output) on both 32-bit and 64-bit targets.  This is what the cross-architecture
test suite validates (see [§10.2](#102-cross-architecture-semantic-tests)).

Certain linter fixes are **not** semantically equivalent and are therefore
excluded from the transformer pipeline:

| Excluded tag | Why it is not SE |
|---|---|
| `CastPointerToInt` / `CastPointerToUInt` | Replacing `(int)ptr` with `(intptr_t)ptr` changes the type width. |
| `CastIntToPointer` / `CastLongToPointer` | Replacing `(T*)x` changes from pointer to integer result or vice versa. |
| `HandleTypesCastToInt` / `HandleTypesCastToUInt` | Windows HANDLE semantics. |
| `FnsReturnPtrAsInt` / `FnsReturnPtrAsUInt` / `FnsReturnPtrAsLong` | Widening the return type changes the function signature. |
| `LdUsedWithLongAssuming64bits` / `LuUsedForPtrSizedVals` | Format-string fixes for pointer-sized values require human judgement about intent. |

---

## 8. Public API (`X86_to_X64`)

**File:** `src/X86_to_X64.hs`

The library exposes a clean, IO-based API:

```haskell
-- Analysis
analyzeFile         :: FilePath -> IO (Either String [Issue])
analyzeFileWithCPP  :: FilePath -> IO (Either String [Issue])
analyzeSource       :: String   -> IO (Either String [Issue])

-- Lint
lintFile            :: FilePath -> IO (Either String (String, [Issue]))
lintFileWithCPP     :: FilePath -> IO (Either String (String, [Issue]))
lintSource          :: String   -> IO (Either String (String, [Issue]))

-- Transform
transformFile       :: FilePath -> IO (Either String String)
transformFileWithCPP:: FilePath -> IO (Either String String)
transformSource     :: String   -> IO (Either String String)
```

All functions return `Left errMsg` on a parse failure; otherwise:
- **Analyze** returns `Right [Issue]`.
- **Lint** returns `Right (lintedSource, unresolvedIssues)`.
- **Transform** returns `Right transformedSource`.

The `WithCPP` variants invoke `gcc -E` before parsing; they are needed when
the input file uses `#include` directives or preprocessor macros.

Re-exported for callers: `Issue(..)`, `IssueTag(..)`, `Severity(..)`,
`Category(..)`, `prettyPrintIssues`.

---

## 9. Command-Line Interface

**File:** `app/Main.hs`

The CLI is configured via a `Config` record:

```haskell
data Config = Config
  { cfgInputFile  :: FilePath
  , cfgOutputFile :: Maybe FilePath
  , cfgVerbose    :: Bool
  , cfgLint       :: Bool
  , cfgTransform  :: Bool
  , cfgNoColor    :: Bool
  , cfgCPP        :: Bool
  , cfgStrict     :: Bool
  }
```

Argument parsing is a manual left-fold over `getArgs` — no external options
library is used.  The flag `-h`/`--help` returns `Left ""` (signals a clean
exit), while unknown or conflicting flags return `Left errMsg` (signals an
error exit).

**Output formatting:**
- Colour is enabled if stdout is a TTY and `--no-color` was not passed.
- Terminal width is read from the `COLUMNS` environment variable (set by
  zsh/bash), falling back to 80 columns.
- `--strict` causes the process to exit with a non-zero status even for
  `Warning`-severity issues.

**Mode dispatch:**

```
run cfg
  ├── cfgTransform → transformFile/transformFileWithCPP → write .x64.c
  ├── cfgLint      → lintFile/lintFileWithCPP → write .x64.c + report unresolved
  └── (default)    → analyzeFile/analyzeFileWithCPP → print issues
```

---

## 10. Test Suite

**File:** `test/Main.hs`

The test suite uses [`hspec`](https://hackage.haskell.org/package/hspec).  All
test specs are combined in a single `hspec` run:

```haskell
main = hspec $ do
  parserSpec
  formatSpecParserSpec
  analysisSpec       -- per-category analysis unit tests
  linterSpec         -- per-category linter unit tests
  transformerSpec    -- per-pass transformer unit tests
  transformerIntegrationSpec
  usageClassifierSpec
  crossArchSpec
```

### 10.1 Unit and Integration Tests

- **`Analysis/`** — Each sub-module has a test file (e.g. `AlignmentTests.hs`)
  that parses small inline C snippets via `analyzeSource` and checks that
  the expected `IssueTag`s are (or are not) reported.
- **`Linter/`** — Mirrors `Analysis/`; checks that each automated fix is
  applied correctly.
- **`Transformer/`** — Tests each transformer pass in isolation, plus
  end-to-end integration tests that run the full `transform` pipeline on
  small C programs.
- **`Transformer/UsageClassifierTests.hs`** — Directly tests the
  `classifyVar` function against a range of usage patterns.
- **`Parser/`** — Tests `parseSourceString` and the format-specifier parser.

Test utilities (`AnalysisTestUtils.hs`, `LinterTestsUtils.hs`) provide
`shouldDetect` / `shouldFix` combinators that keep individual tests concise.

### 10.2 Cross-Architecture Semantic Tests

**File:** `test/CrossArch/CrossArchTests.hs`

These tests verify that the transformer produces a program with **identical
observable output** on 32-bit and 64-bit targets.  For each C program in
`test/c_progs/cross_arch/`:

1. Compile and run as **32-bit** (`gcc -m32`) inside a container. Record stdout.
2. Compile and run as **64-bit** (`gcc -m64`) inside the same container. The output is expected to **differ** from the 32-bit run (demonstrating that the original code is broken on 64-bit).
3. Apply `transformFile` to produce a `.x64.c` file.
4. Compile and run the transformed source as **64-bit**. The output must **match** the 32-bit baseline.

**Infrastructure:** Compilation runs inside a [Podman](https://podman.io/)
container built from `Dockerfile.test-env` (`debian:bookworm-slim` with
`gcc-multilib` for `gcc -m32` cross-compilation support on amd64).  All
compilations for a given test run are batched into two `podman exec` calls to
minimise container startup overhead.

If Podman is not available (e.g. on a developer's machine without it
installed), the entire `crossArchSpec` group is skipped with
`pendingWith "podman not available"` rather than failing.

**Cross-arch test programs** (`test/c_progs/cross_arch/`):

| File | What it tests |
|---|---|
| `long_arithmetic.c` | Basic `long` overflow behaviour |
| `long_array.c` | `long` used as array index |
| `long_in_struct.c` | `long` struct members |
| `long_size_of.c` | `sizeof(long)` comparisons |
| `pointer_as_long.c` | Storing a pointer in a `long` |
| `typedef_long.c` | `typedef long T` usage |
| `format_string.c` | `%ld` format specifiers |
| `malloc_sizeof_patterns.c` | `malloc(n * sizeof(long))` |
| `sizeof_in_malloc.c` | `malloc(sizeof(long) * n)` |
| `struct_with_sizeof.c` | Struct size calculations |
| `multi_long_vars.c` | Multiple `long` vars with different classifications |
| `nested_scopes.c` | `long` in nested blocks |
| `scoped_longs.c` | Shadowing and scope issues |
| `global_and_local.c` | Global + local `long` with cross-function evidence |
| `loop_with_stride.c` | `long` loop counter |
| `self_sizeof_patterns.c` | `sizeof(x)` where `x` is `long` |
| `self_alignof_patterns.c` | `_Alignof(long)` patterns |

---

## 11. Key Design Decisions

### Reuse the `language-c` AST throughout

Rather than defining a custom C AST, the tool uses `language-c`'s
`CTranslUnit` as its universal representation.  This provides:
- A complete, standards-compliant C parser for free.
- A `pretty` instance that renders the AST back to syntactically valid C.
- `Data.Generics`-compatible traversal (`everywhere`, `listify`, `mkT`) for
  writing concise whole-tree rewrites.

The tradeoff is that pretty-printing can alter formatting and drops preprocessor
directives, so the tool is unsuitable as a formatter-preserving patch generator.

### Read-only analysis, mutable linting

The `analysis` function never modifies the AST.  The `lint` and `transform`
functions produce a new `CTranslUnit` by threading the AST through a fold over
the issue list.  This separation makes it straightforward to run analysis in
isolation (for reporting) or pair it with linting.

### Position-keyed AST rewrites

Linter and transformer rewrites are keyed by `NodeInfo` position rather than
by content pattern-matching, which avoids false matches when the same syntactic
construct appears multiple times.  The `analysis` pass records the exact
`NodeInfo` of each problematic expression (and optionally the declaration
position), and the linter passes locate the target node during the subsequent
`everywhere` walk.

### Usage-based classification for `long`

Rather than blindly replacing all `long` with `int32_t`, the transformer
infers the programmer's intent by examining how each variable is used.  A
variable assigned from pointer subtraction becomes `ptrdiff_t`; one passed to
`malloc` becomes `size_t`; one used in bitwise operations becomes `uint32_t`.
This produces idiomatic, readable output rather than a mechanical substitution.

### Separate linter and transformer pipelines

`-l` and `-t` are intentionally separate because they apply conflicting
policies.  The linter fixes pointer-cast issues (e.g. `(int)ptr` →
`(intptr_t)ptr`) which change type widths and are therefore not semantically
equivalent on a 32-bit comparison.  The transformer excludes those same fixes
to preserve 32-bit observable semantics.

### Cross-architecture test suite using containers

The cross-arch tests provide end-to-end confidence that the transformer
produces correct output by actually running programs on two architectures.
Using Podman (rather than requiring a physical 32-bit machine) makes this
reproducible on modern macOS/ARM developer machines.

---

## 12. Data-Flow Diagrams

### Analysis mode

```
                  ┌─────────────────────────────┐
  foo.c  ────────►│  parseSourceFile            │
                  │  (or parseSourceFileWithCPP)│
                  └──────────────┬──────────────┘
                                 │ CTranslUnit
                                 ▼
                  ┌──────────────────────────────┐
                  │     analysis ast             │
                  │  ┌────────────────────────┐  │
                  │  │ analyzeAlignmentIssues │  │
                  │  │ analyzeBitManip...     │  │
                  │  │ ...  (11 sub-modules)  │  │
                  │  └────────────────────────┘  │
                  └──────────────┬───────────────┘
                                 │ [Issue]
                                 ▼
                  prettyPrintIssues  ──► stdout
```

### Lint mode

```
  foo.c  ──► parse ──► CTranslUnit ──┬──► analysis ──► [Issue]
                                     │                      │
                                     └──────────────────────┤
                                                            │ (ast, issues)
                                                            ▼
                                               lint ast issues
                                            ┌─────────────────────┐
                                            │  fold over 11       │
                                            │  category linters   │
                                            └──────────┬──────────┘
                                                       │
                                    ┌──────────────────┴──────────────────┐
                                    │                                     │
                             CTranslUnit'                         [Issue] (unresolved)
                                    │
                             pretty-print
                                    │
                              foo.c.x64.c
```

### Transform mode

```
  foo.c  ──► parse ──► CTranslUnit
                            │
                    transform pipeline
                    ┌────────────────────────────────────┐
                    │ 1. lint (SE issues only)           │
                    │ 2. transformStructMembers          │
                    │ 3. transformTypedefs               │
                    │ 4. transformSizeofLong             │
                    │ 5. transformReturnTypes            │
                    │ 6. splitMultiLongDecls             │
                    │ 7. transformLongs ──► RetypeMap    │
                    │ 8. transformFunPtrParams           │
                    │ 9. syncCasts (RetypeMap)           │
                    │10. fixStandaloneCasts              │
                    │11. fixFormatStrings (RetypeMap)    │
                    │12. stripLongSuffixes (RetypeMap)   │
                    └────────────────┬───────────────────┘
                                     │ CTranslUnit'
                                     ▼
                            pretty-print + addRequiredIncludes
                                     │
                              foo.c.x64.c
```
