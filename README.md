# x86-to-x64-tool

A static analysis and automated linting tool for C source code that identifies and fixes portability issues encountered when migrating from 32-bit (x86) to 64-bit (x64) architectures.

## Overview

When porting C programs from 32-bit to 64-bit platforms, several categories of bugs become possible: pointer sizes change from 4 bytes to 8 bytes, type assumptions break, and platform-specific code may no longer compile or behave correctly. This tool parses C source files, detects those issues, and where possible applies automated linting to fix them.

The tool is implemented in Haskell and uses the `language-c` library for parsing.

## Features

- **Analysis mode**: parse a C file and report all detected portability issues with file and line information
- **Lint mode**: apply automated fixes and write the result to a new file, reporting any issues that could not be resolved automatically
- **Haskell library API** for use from other Haskell programs

### Issue Categories Detected

| Category | Description |
|---|---|
| Alignment | Structs with pointers written to binary files, hard-coded struct sizes, packed structs containing pointers |
| Bit Manipulation | Packing pointers with flags in integers, bit shifts on pointers, extracting pointer bits into 32-bit variables |
| Comparison | Loop counters declared as `int` when iterating over pointer arrays, pointer comparison with integer constants |
| Constants and Literals | Magic values, bit masks assuming 32-bit pointers, hard-coded address values |
| Format Strings | Using `%d`/`%u`/`%x` with `size_t`, `ptrdiff_t`, or pointer types |
| Function Signatures | Functions returning a pointer as `int` or `long`, parameters declared as `int` but receiving a pointer |
| Memory Allocation | Allocation size calculations that may overflow, missing overflow checks on `malloc`, storing allocation sizes in `int` |
| Platform Specifics | Inline x86 assembly, x86 compiler intrinsics, assumptions about register sizes, `HANDLE` types cast to `int` |
| Pointer Math | Pointer difference stored in a 32-bit type, pointer addition overflow, pointer subtraction underflow |
| Serialization | Writing pointers directly to files, sending pointers over the network, pointers in shared memory or memory-mapped files |
| Type Size | Casting pointers to/from `int` or `long`, using `int` where `size_t` or `ptrdiff_t` is required |

## Requirements

- GHC 9.6 or later
- Cabal 3.0 or later

## Building

```
cabal build
```

## Running

```
cabal run x86-to-x64-tool -- <file.c> [options]
```

Or, after installing:

```
x86-to-x64-tool <file.c> [options]
```

### Options

```
Usage: x86-to-x64-tool <file.c> [options]

Options:
  -v           Verbose: print a one-sentence explanation for each issue
  -t           Lint: apply automated x86-to-x64 fixes
  -o <file>    Write linted source to this file (default: <input>.x64.c)
  --no-color   Disable colored output
  -h, --help   Show this help message
```

### Examples

Analyze a file and print all issues:

```
x86-to-x64-tool myprogram.c
```

Analyze with explanations for each issue:

```
x86-to-x64-tool myprogram.c -v
```

Apply automated linting and write the result to `myprogram.x64.c`:

```
x86-to-x64-tool myprogram.c -t
```

Lint and write the output to a specific file:

```
x86-to-x64-tool myprogram.c -t -o fixed.c
```

## Library API

The tool is also available as a Haskell library. The main entry points are in `X86_to_X64`:

```haskell
-- Analyze a file; returns Left on parse error, Right issues on success
analyzeFile :: FilePath -> IO (Either String [Issue])

-- Lint a file; returns Left on parse error,
-- Right (lintedSource, unresolvedIssues) on success
lintFile :: FilePath -> IO (Either String (String, [Issue]))

-- Same operations on an in-memory String
analyzeSource :: String -> Either String [Issue]
lintSource :: String -> Either String (String, [Issue])
```

## Running Tests

```
cabal test
```

## Project Structure

```
src/
  X86_to_X64.hs          -- Public API
  Parser/
    Parser.hs            -- C source parser (wraps language-c)
  Analysis/
    Analysis.hs          -- Top-level analysis pass
    IssueTypes.hs        -- Issue / severity / category types
    ASTTraversal.hs      -- Generic AST traversal utilities
    TypeChecker.hs       -- Type inference helpers
    Alignment.hs         -- Alignment checks
    BitManipulation.hs   -- Bit manipulation checks
    Comparison.hs        -- Comparison checks
    ConstantsLiterals.hs -- Constant and literal checks
    FormatStrings.hs     -- Format string checks
    FunctionSignatures.hs-- Function signature checks
    MemoryAllocation.hs  -- Memory allocation checks
    PlatformSpecifics.hs -- Platform-specific checks
    PointerMath.hs       -- Pointer arithmetic checks
    Serialization.hs     -- Serialization checks
    TypeSize.hs          -- Type size checks
  Linter/
    Linter.hs            -- Top-level linting pass
    Helpers.hs           -- Shared linting utilities
    Alignment.hs         -- Alignment fixes
    BitManipulation.hs   -- Bit manipulation fixes
    Comparison.hs        -- Comparison fixes
    ConstantsLiterals.hs -- Constant and literal fixes
    FormatStrings.hs     -- Format string fixes
    FunctionSignatures.hs-- Function signature fixes
    MemoryAllocation.hs  -- Memory allocation fixes
    PlatformSpecifics.hs -- Platform-specific fixes
    PointerMath.hs       -- Pointer arithmetic fixes
    Serialization.hs     -- Serialization fixes
    TypeSize.hs          -- Type size fixes
app/
  Main.hs                -- CLI entry point
test/
  ...                    -- Unit and integration tests
```

## License

BSD 3-Clause. See [LICENSE](LICENSE).
