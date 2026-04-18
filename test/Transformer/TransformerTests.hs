module Transformer.TransformerTests where

import Test.Hspec

import X86_to_X64 (transformSource)
import Transformer.TransformerTestUtils

transformerSpec :: Spec
transformerSpec = describe "Transformer" $ do

    -- -------------------------------------------------------------------
    -- NumberType: plain arithmetic -> int32_t
    -- -------------------------------------------------------------------
    describe "long (number) -> int32_t" $ do
        shouldTransformToContain
            "long local with arithmetic becomes int32_t"
            "void f(void) { long n = 42; n = n + 1; }"
            "int32_t"

        shouldTransformToContain
            "long parameter with arithmetic becomes int32_t"
            "void f(long n) { n = n + 1; }"
            "int32_t"

        shouldTransformToContain
            "global long becomes int32_t"
            "long g = 0;"
            "int32_t"

    -- -------------------------------------------------------------------
    -- PointerType: assigned from a pointer expression -> intptr_t
    -- -------------------------------------------------------------------
    describe "long (pointer) -> intptr_t" $ do
        shouldTransformToContain
            "long local assigned from pointer cast becomes intptr_t"
            "void f(void *p) { long x = (long)p; }"
            "intptr_t"

        shouldTransformToContain
            "long local assigned from pointer expression becomes intptr_t"
            "void f(int *p) { long x; x = (long)p; }"
            "intptr_t"

    -- -------------------------------------------------------------------
    -- SizeType: assigned from sizeof -> size_t
    -- -------------------------------------------------------------------
    describe "long (size) -> size_t" $ do
        shouldTransformToContain
            "long local assigned from sizeof(type) becomes size_t"
            "void f(void) { long sz = sizeof(int); }"
            "size_t"

        shouldTransformToContain
            "long local assigned from sizeof(expr) becomes size_t"
            "void f(int x) { long sz = sizeof(x); }"
            "size_t"

    -- -------------------------------------------------------------------
    -- OffsetType: assigned from pointer subtraction -> ptrdiff_t
    -- -------------------------------------------------------------------
    describe "long (offset) -> ptrdiff_t" $ do
        shouldTransformToContain
            "long local assigned from ptr - ptr becomes ptrdiff_t"
            "void f(int *a, int *b) { long d = a - b; }"
            "ptrdiff_t"

    -- -------------------------------------------------------------------
    -- BitSeqType: used in bitwise operation -> uint32_t
    -- -------------------------------------------------------------------
    describe "long (bit-sequence) -> uint32_t" $ do
        shouldTransformToContain
            "long local used in bitwise-and becomes uint32_t"
            "void f(long val) { long m = val & 255; }"
            "uint32_t"

        shouldTransformToContain
            "long local used in bitwise-or becomes uint32_t"
            "void f(void) { long n = 0; n = n | 1; }"
            "uint32_t"

        shouldTransformToContain
            "long parameter used in bitwise-and becomes uint32_t"
            "void f(long val) { long m = val & 255; }"
            "uint32_t"

    -- -------------------------------------------------------------------
    -- Unsigned long
    -- -------------------------------------------------------------------
    describe "unsigned long -> uint32_t (number)" $ do
        shouldTransformToContain
            "unsigned long local becomes uint32_t"
            "void f(void) { unsigned long n = 0; }"
            "uint32_t"

        shouldTransformToContain
            "unsigned long parameter becomes uint32_t"
            "void f(unsigned long n) { n = n + 1; }"
            "uint32_t"

    -- -------------------------------------------------------------------
    -- long long must NOT be touched
    -- -------------------------------------------------------------------
    describe "long long is preserved" $ do
        shouldTransformNotToContain
            "long long local is not converted to int32_t"
            "void f(void) { long long n = 0; }"
            "int32_t"

        shouldTransformNotToContain
            "long long local is not converted to uint32_t"
            "void f(void) { unsigned long long n = 0; }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Non-long types are untouched
    -- -------------------------------------------------------------------
    describe "non-long types are untouched" $ do
        shouldTransformNotToContain
            "int local is not rewritten"
            "void f(void) { int n = 0; }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Cast rewrites are NOT applied (not SE without variable retype)
    -- -------------------------------------------------------------------
    describe "non-SE cast rewrites are not applied" $ do
        shouldTransformNotToContain
            "(int)ptr cast is left unchanged (variable still int, would truncate)"
            "void f(void) { int *p = 0; int x = (int)p; }"
            "intptr_t"

        shouldTransformNotToContain
            "(unsigned int)ptr cast is left unchanged"
            "void f(void) { int *p = 0; unsigned int x = (unsigned int)p; }"
            "uintptr_t"

        shouldTransformNotToContain
            "(int*)int cast is left unchanged (would flip expr type from ptr to int)"
            "void f(void) { int x = 0; int *p = (int*)x; }"
            "intptr_t"

    -- -------------------------------------------------------------------
    -- Return wrapping is NOT applied (not SE: return type still int)
    -- -------------------------------------------------------------------
    describe "non-SE return wrapping is not applied" $ do
        shouldTransformNotToContain
            "int function returning pointer is not wrapped (return type unchanged)"
            "int f(void) { int *p = 0; return p; }"
            "intptr_t"

    -- -------------------------------------------------------------------
    -- Pointer-math variable retyping (subsumed from linter)
    -- -------------------------------------------------------------------
    describe "pointer math variables" $ do
        shouldTransformToContain
            "int diff = ptr - ptr becomes ptrdiff_t"
            "void f(void) { int *p = 0; int *q = 0; int d; d = p - q; }"
            "ptrdiff_t"

        shouldTransformToContain
            "int array index in large array becomes ptrdiff_t"
            "void f(int *arr, int n) { int i; int el = arr[i]; }"
            "ptrdiff_t"

    -- -------------------------------------------------------------------
    -- Memory allocation variable retyping (subsumed from linter)
    -- -------------------------------------------------------------------
    describe "allocation size variables" $ do
        shouldTransformToContain
            "int sz = sizeof(...) becomes size_t"
            "void f(void) { int n; n = sizeof(int); }"
            "size_t"

    -- -------------------------------------------------------------------
    -- Function parameter retypings ARE applied (SE: changes declaration,
    -- consistent with 64-bit calling convention within same file)
    -- -------------------------------------------------------------------
    describe "function signatures" $ do
        shouldTransformToContain
            "int parameter receiving pointer becomes intptr_t"
            "void f(int h) { int *r = 0; h = r; }"
            "intptr_t"

    -- -------------------------------------------------------------------
    -- File-offset variable retyping (subsumed from linter)
    -- -------------------------------------------------------------------
    describe "file offset variables" $ do
        shouldTransformToContain
            "int used with fseek becomes off_t"
            "void f(void) { int off; fseek(0, off, 0); }"
            "off_t"

    -- -------------------------------------------------------------------
    -- Struct / union member declaration retyping (Phase 1)
    -- -------------------------------------------------------------------
    describe "struct member long -> int32_t" $ do
        shouldTransformToContain
            "long struct member becomes int32_t"
            "struct S { long x; int y; }; void f(void) {}"
            "int32_t"

        shouldTransformToContain
            "unsigned long struct member becomes uint32_t"
            "struct S { unsigned long n; }; void f(void) {}"
            "uint32_t"

        shouldTransformToContain
            "union member long becomes int32_t"
            "union U { long a; int b; }; void f(void) {}"
            "int32_t"

        shouldTransformNotToContain
            "long long struct member is not changed"
            "struct S { long long x; }; void f(void) {}"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Typedef retyping (Phase 2)
    -- -------------------------------------------------------------------
    describe "typedef long T -> typedef int32_t T" $ do
        shouldTransformToContain
            "typedef long word_t becomes int32_t"
            "typedef long word_t;"
            "int32_t"

        shouldTransformToContain
            "typedef unsigned long u_word_t becomes uint32_t"
            "typedef unsigned long u_word_t;"
            "uint32_t"

        shouldTransformNotToContain
            "typedef long long bignum is not changed"
            "typedef long long bignum;"
            "int32_t"

    -- -------------------------------------------------------------------
    -- sizeof(long) rewriting (Phase 3)
    -- -------------------------------------------------------------------
    describe "sizeof(long) -> sizeof(int32_t)" $ do
        shouldTransformToContain
            "sizeof(long) becomes sizeof(int32_t)"
            "void f(void) { int n = sizeof(long); }"
            "sizeof(int32_t)"

        shouldTransformToContain
            "sizeof(unsigned long) becomes sizeof(uint32_t)"
            "void f(void) { int n = sizeof(unsigned long); }"
            "sizeof(uint32_t)"

        shouldTransformNotToContain
            "sizeof(long) is NOT rewritten to sizeof(long) still present"
            "void f(void) { int n = sizeof(long); }"
            "sizeof(long)"

    -- -------------------------------------------------------------------
    -- Function return type retyping (Phase 4)
    -- -------------------------------------------------------------------
    describe "long function return type retyping" $ do
        shouldTransformToContain
            "long f() returning plain int becomes int32_t"
            "long f(void) { return 42; }"
            "int32_t"

        shouldTransformToContain
            "long f() returning pointer expression becomes intptr_t"
            "long f(void *p) { return (long)p; }"
            "intptr_t"

        shouldTransformToContain
            "long f() returning sizeof becomes size_t"
            "long f(void) { return sizeof(int); }"
            "size_t"

        shouldTransformToContain
            "long f() returning ptr-diff becomes ptrdiff_t"
            "long f(int *a, int *b) { return (long)(a - b); }"
            "ptrdiff_t"

    -- -------------------------------------------------------------------
    -- Cast sync: (long) cast updated to match retyped variable (Phase 5)
    -- -------------------------------------------------------------------
    describe "cast sync matches retyped variable" $ do
        shouldTransformToContain
            "(long)ptr in initialiser updated to (intptr_t)"
            "void f(void *p) { long x = (long)p; }"
            "(intptr_t)"

        shouldTransformNotToContain
            "after cast sync, stale (long) cast is gone from intptr_t variable"
            "void f(void *p) { long x = (long)p; }"
            "(long)"

        shouldTransformToContain
            "(long)ptr in assignment updated to (intptr_t)"
            "void f(void *p) { long x; x = (long)p; }"
            "(intptr_t)"

    -- Format string fix: %ld / %lu updated after long retype (Phase 6)
    -- ------------------------------------------------------------------
    describe "format string fix after long retype" $ do
        shouldTransformToContain
            "%ld of int32_t variable becomes %d"
            "void f(void) { long n = 42; printf(\"%ld\\n\", n); }"
            "%d"

        shouldTransformNotToContain
            "%ld of int32_t variable: stale %ld removed"
            "void f(void) { long n = 42; printf(\"%ld\\n\", n); }"
            "%ld"

        shouldTransformToContain
            "%lu of uint32_t variable becomes %u"
            "void f(void) { unsigned long n = 0; printf(\"%lu\\n\", n); }"
            "%u"

        shouldTransformNotToContain
            "%lu of uint32_t variable: stale %lu removed"
            "void f(void) { unsigned long n = 0; printf(\"%lu\\n\", n); }"
            "%lu"

        shouldTransformToContain
            "%ld of intptr_t variable stays %ld (correct on LP64)"
            "void f(void *p) { long x = (long)p; printf(\"%ld\\n\", x); }"
            "%ld"

        shouldTransformToContain
            "flags/width preserved: %5ld of int32_t becomes %5d"
            "void f(void) { long n = 0; printf(\"%5ld\\n\", n); }"
            "%5d"

        -- linter changes %d -> %td for long (DUsedWithPtrdifft),
        -- then transformer retypes long -> int32_t; %td must become %d
        shouldTransformToContain
            "%td overshoot fixed: linter %d->%td then transformer int32_t -> %d"
            "void f(void) { long n = 42; printf(\"%d\\n\", n); }"
            "%d"

        shouldTransformNotToContain
            "%td overshoot: no %td in output for int32_t variable"
            "void f(void) { long n = 42; printf(\"%d\\n\", n); }"
            "%td"

        -- linter changes %d -> %zd for unsigned long (DUsedWithSizet),
        -- then transformer retypes to uint32_t; %zd must become %d (%u is also
        -- valid but we preserve the original conversion character)
        shouldTransformNotToContain
            "%zd overshoot: no %zd in output for uint32_t variable"
            "void f(void) { unsigned long n = 0; printf(\"%d\\n\", n); }"
            "%zd"

        -- long classified as ptrdiff_t (pointer subtraction usage)
        shouldTransformToContain
            "%ld of ptrdiff_t variable becomes %td"
            "void f(int *a, int *b) { long d = a - b; printf(\"%ld\\n\", d); }"
            "%td"

        shouldTransformToContain
            "%d of ptrdiff_t variable becomes %td"
            "void f(int *a, int *b) { long d = a - b; printf(\"%d\\n\", d); }"
            "%td"

    -- -------------------------------------------------------------------
    -- Standalone (long) casts (Bug 1): not paired with a retyped variable
    -- -------------------------------------------------------------------
    describe "standalone (long) casts" $ do
        shouldTransformToContain
            "standalone (long) cast in function arg becomes (int32_t)"
            "void g(int); void f(int x) { g((long)x); }"
            "(int32_t)"

        shouldTransformNotToContain
            "standalone (long) cast: stale (long) removed"
            "void g(int); void f(int x) { g((long)x); }"
            "(long)"

        shouldTransformToContain
            "standalone (unsigned long) cast becomes (uint32_t)"
            "void g(int); void f(int x) { g((unsigned long)x); }"
            "(uint32_t)"

        shouldTransformToContain
            "standalone (long *) pointer cast becomes (int32_t *)"
            "void f(char *buf) { int x = *(long *)buf; }"
            "(int32_t *)"

        shouldTransformNotToContain
            "standalone (long *) cast: stale (long *) removed"
            "void f(char *buf) { int x = *(long *)buf; }"
            "(long *)"

    -- -------------------------------------------------------------------
    -- Chained casts (Bug 6): inner (long) rewritten by standalone pass
    -- -------------------------------------------------------------------
    describe "chained casts" $ do
        shouldTransformNotToContain
            "inner (long) in chained cast (int)(long)x is rewritten"
            "void f(int x) { int y = (int)(long)x; }"
            "(long)"

    -- -------------------------------------------------------------------
    -- Global variable classification (Bug 3): pointer usage detected
    -- -------------------------------------------------------------------
    describe "global long classification" $ do
        shouldTransformToContain
            "global long used as pointer becomes intptr_t"
            "long g; void f(void *p) { g = (long)p; }"
            "intptr_t"

        shouldTransformToContain
            "global long used in sizeof becomes size_t"
            "long g; void f(void) { g = sizeof(int); }"
            "size_t"

        shouldTransformToContain
            "global long with no usage defaults to int32_t"
            "long g;"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Ternary expression classification (Bug 4)
    -- -------------------------------------------------------------------
    describe "ternary expression classification" $ do
        shouldTransformToContain
            "long initialized from ternary with pointer branch becomes intptr_t"
            "void f(int cond, void *p) { long x = cond ? (long)p : 0; }"
            "intptr_t"

        shouldTransformToContain
            "long initialized from ternary with sizeof branches becomes size_t"
            "void f(int cond) { long x = cond ? sizeof(int) : sizeof(char); }"
            "size_t"

    -- -------------------------------------------------------------------
    -- Compound assignment BitSeqType detection (Bug 5)
    -- -------------------------------------------------------------------
    describe "compound assignment bitwise detection" $ do
        shouldTransformToContain
            "long with |= assignment becomes uint32_t"
            "void f(void) { long x = 0; x |= 1; }"
            "uint32_t"

        shouldTransformToContain
            "long with &= assignment becomes uint32_t"
            "void f(void) { long x = 0xFF; x &= 0x0F; }"
            "uint32_t"

        shouldTransformToContain
            "long with <<= assignment becomes uint32_t"
            "void f(void) { long x = 1; x <<= 4; }"
            "uint32_t"

    -- -------------------------------------------------------------------
    -- Function pointer parameter types (Missing 1)
    -- -------------------------------------------------------------------
    describe "function pointer long params" $ do
        shouldTransformToContain
            "long parameter in function pointer becomes int32_t"
            "void f(void) { void (*fp)(long); }"
            "int32_t"

        shouldTransformToContain
            "unsigned long parameter in function pointer becomes uint32_t"
            "void f(void) { void (*fp)(unsigned long); }"
            "uint32_t"

    -- -------------------------------------------------------------------
    -- #include header insertion (Missing 2)
    -- -------------------------------------------------------------------
    describe "#include header insertion" $ do
        it "adds #include <stdint.h> when int32_t is introduced" $
            case transformSource "void f(void) { long n = 42; }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "#include <stdint.h>"

        it "adds #include <stddef.h> when size_t is introduced" $
            case transformSource "void f(void) { long sz = sizeof(int); }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "#include <stddef.h>"

        it "adds both headers when both type families used" $
            case transformSource "void f(void) { long n = 42; long sz = sizeof(int); }" of
                Left err  -> fail err
                Right src -> do
                    src `shouldContain` "#include <stdint.h>"
                    src `shouldContain` "#include <stddef.h>"

    -- -------------------------------------------------------------------
    -- long double preservation (must NOT be rewritten)
    -- -------------------------------------------------------------------
    describe "long double preservation" $ do
        shouldTransformToContain
            "long double local is preserved"
            "void f(void) { long double x = 3.14; }"
            "long double"

        shouldTransformNotToContain
            "long double is not rewritten to int32_t"
            "void f(void) { long double x = 3.14; }"
            "int32_t"

        shouldTransformToContain
            "long double parameter is preserved"
            "void f(long double x) { }"
            "long double"

        shouldTransformToContain
            "long double return type is preserved"
            "long double f(void) { long double x = 1.0; return x; }"
            "long double"

    -- -------------------------------------------------------------------
    -- sizeof(long) with array declarator
    -- -------------------------------------------------------------------
    describe "sizeof(long[N])" $ do
        shouldTransformToContain
            "sizeof(long[10]) becomes sizeof(int32_t[10])"
            "void f(void) { int sz = sizeof(long[10]); }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Compound literals: (long){expr}
    -- -------------------------------------------------------------------
    describe "compound literal (long){expr}" $ do
        shouldTransformToContain
            "(long){42} becomes (int32_t){42}"
            "void f(void) { long n = 42; int x = (long){n}; }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- _Alignas(long)
    -- -------------------------------------------------------------------
    describe "_Alignas(long)" $ do
        shouldTransformToContain
            "_Alignas(long) becomes _Alignas(int32_t)"
            "void f(void) { _Alignas(long) int x = 0; }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Multi-declarator splitting
    -- -------------------------------------------------------------------
    describe "multi-declarator long splitting" $ do
        shouldTransformNotToContain
            "long a, b splits so each can be independently typed"
            "void f(void *p) { long a = 42, b = (long)p; }"
            "long"

        shouldTransformToContain
            "split multi-declarator: first var gets int32_t"
            "void f(void *p) { long a = 42, b = (long)p; }"
            "int32_t"

        shouldTransformToContain
            "split multi-declarator: second var gets intptr_t"
            "void f(void *p) { long a = 42, b = (long)p; }"
            "intptr_t"

    -- -------------------------------------------------------------------
    -- Standalone (long) cast of pointer expression -> (intptr_t) (Phase 1)
    -- -------------------------------------------------------------------
    describe "standalone (long) cast of pointer -> intptr_t" $ do
        shouldTransformToContain
            "standalone (long)ptr in function arg becomes (intptr_t)"
            "void g(long); void f(void *p) { g((long)p); }"
            "(intptr_t)"

        shouldTransformNotToContain
            "standalone (long)ptr: stale (long) removed"
            "void g(long); void f(void *p) { g((long)p); }"
            "(long)"

        shouldTransformToContain
            "standalone (unsigned long)ptr becomes (uintptr_t)"
            "void g(unsigned long); void f(void *p) { g((unsigned long)p); }"
            "(uintptr_t)"

        shouldTransformToContain
            "standalone (long) of non-pointer stays (int32_t)"
            "void g(int); void f(int x) { g((long)x); }"
            "(int32_t)"

        shouldTransformToContain
            "standalone (long) of address-of becomes (intptr_t)"
            "void g(long); void f(void) { int x; g((long)&x); }"
            "(intptr_t)"

    -- -------------------------------------------------------------------
    -- Forward declaration / prototype sync (Phase 3)
    -- -------------------------------------------------------------------
    describe "forward declaration sync" $ do
        shouldTransformToContain
            "prototype updated when definition is retyped to int32_t"
            "long f(void); long f(void) { return 42; }"
            "int32_t f"

        shouldTransformNotToContain
            "no stale long in prototype after retype"
            "long f(void); long f(void) { return 42; }"
            "long f"

        shouldTransformToContain
            "prototype updated to intptr_t when definition returns pointer"
            "long f(void *p); long f(void *p) { return (long)p; }"
            "intptr_t f"

        shouldTransformToContain
            "prototype updated to size_t when definition returns sizeof"
            "long f(void); long f(void) { return sizeof(int); }"
            "size_t f"

    -- -------------------------------------------------------------------
    -- Chained casts involving pointer expressions (edge cases)
    -- -------------------------------------------------------------------
    describe "chained casts with pointer inner expression" $ do
        shouldTransformToContain
            "inner (long)ptr in chained cast (int)(long)p becomes (intptr_t)p"
            "void f(void *p) { int x = (int)(long)p; }"
            "(intptr_t)"

        shouldTransformNotToContain
            "stale (long) removed from chained (int)(long)p cast"
            "void f(void *p) { int x = (int)(long)p; }"
            "(long)"

        shouldTransformToContain
            "inner (unsigned long)ptr in chained cast becomes (uintptr_t)p"
            "void f(void *p) { unsigned int x = (unsigned int)(unsigned long)p; }"
            "(uintptr_t)"

    -- -------------------------------------------------------------------
    -- Format string flags/width preservation (edge cases)
    -- -------------------------------------------------------------------
    describe "format string flag and precision preservation" $ do
        shouldTransformToContain
            "sign flag preserved: %+5ld of int32_t becomes %+5d"
            "void f(void) { long n = 0; printf(\"%+5ld\\n\", n); }"
            "%+5d"

        shouldTransformToContain
            "left-align flag preserved: %-10ld of int32_t becomes %-10d"
            "void f(void) { long n = 0; printf(\"%-10ld\\n\", n); }"
            "%-10d"

        shouldTransformToContain
            "zero-pad flag preserved: %05lu of uint32_t becomes %05u"
            "void f(void) { unsigned long n = 0; printf(\"%05lu\\n\", n); }"
            "%05u"

        shouldTransformToContain
            "precision preserved: %10.5ld of int32_t becomes %10.5d"
            "void f(void) { long n = 0; printf(\"%10.5ld\\n\", n); }"
            "%10.5d"

        shouldTransformToContain
            "flags/width/precision all preserved: %+010.3ld of int32_t becomes %+010.3d"
            "void f(void) { long n = 0; printf(\"%+010.3ld\\n\", n); }"
            "%+010.3d"

    -- -------------------------------------------------------------------
    -- Typedef chain: transformer classifies through double typedef
    -- -------------------------------------------------------------------
    describe "typedef chain transformation" $ do
        shouldTransformToContain
            "typedef base = long, typedef mylong = base: mylong var -> int32_t"
            "typedef long base; typedef base mylong; void f(void) { mylong x = 42; }"
            "int32_t"

        shouldTransformNotToContain
            "underlying typedef base is rewritten: 'long base' no longer appears in output"
            "typedef long base; typedef base mylong; void f(void) { mylong x = 42; }"
            "long base"

    -- -------------------------------------------------------------------
    -- L-suffix stripping
    -- -------------------------------------------------------------------
    describe "L-suffix stripping" $ do
        shouldTransformNotToContain
            "100L in int32_t initialiser has L stripped"
            "void f(void) { long x = 100L; }"
            "100L"

        shouldTransformNotToContain
            "100UL in uint32_t initialiser has L stripped"
            "void f(void) { unsigned long x = 100UL; }"
            "100UL"

        shouldTransformToContain
            "L stripped from assignment RHS"
            "void f(void) { long x = 0; x = 100L; }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Multiple differently-classified longs in same function
    -- -------------------------------------------------------------------
    describe "multiple long vars with different classifications" $ do
        shouldTransformToContain
            "pointer-classified long gets intptr_t in same function as number-classified"
            "void f(void *p) { long addr = (long)p; long count = 5; }"
            "intptr_t"

        shouldTransformToContain
            "number-classified long gets int32_t in same function as pointer-classified"
            "void f(void *p) { long addr = (long)p; long count = 5; }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- unsigned long classified as PointerType -> uintptr_t
    -- -------------------------------------------------------------------
    describe "unsigned long -> uintptr_t (pointer)" $ do
        shouldTransformToContain
            "unsigned long assigned from pointer cast becomes uintptr_t"
            "void f(void *p) { unsigned long addr = (unsigned long)p; }"
            "uintptr_t"

    -- -------------------------------------------------------------------
    -- Nested struct long members
    -- -------------------------------------------------------------------
    describe "nested struct long members" $ do
        shouldTransformToContain
            "long in nested struct becomes int32_t"
            "struct outer { struct inner { long x; } i; }; void f(void) {}"
            "int32_t"

        shouldTransformNotToContain
            "no stale long in nested struct"
            "struct outer { struct inner { long x; } i; }; void f(void) {}"
            "long x"

    -- -------------------------------------------------------------------
    -- Function pointer with long return type
    -- -------------------------------------------------------------------
    describe "function pointer long return type" $ do
        shouldTransformNotToContain
            "long return type in function pointer is rewritten"
            "void f(void) { long (*fp)(int); }"
            "long (*"

    -- -------------------------------------------------------------------
    -- Scoping: classification independence across function boundaries
    -- -------------------------------------------------------------------
    describe "classification independence across functions" $ do
        -- The classifier is per-function; two functions sharing a variable
        -- name classify each independently from the other's usage evidence.
        shouldTransformToContain
            "long x = 42 in f -> int32_t, independent of pointer-typed x in g"
            "void f(void) { long x = 42; } void g(void *p) { long x = (long)p; }"
            "int32_t"

        shouldTransformToContain
            "long x = (long)p in g -> intptr_t, independent of number-typed x in f"
            "void f(void) { long x = 42; } void g(void *p) { long x = (long)p; }"
            "intptr_t"

        shouldTransformToContain
            "long n used with sizeof in f -> size_t, long n = 0 in g -> int32_t (both present)"
            "void f(void) { long n = sizeof(int); } void g(void) { long n = 0; }"
            "size_t"

        shouldTransformToContain
            "long n = 0 in g -> int32_t alongside size_t n in f"
            "void f(void) { long n = sizeof(int); } void g(void) { long n = 0; }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Scoping: for-loop initialiser declaration
    -- -------------------------------------------------------------------
    describe "long declared in for-loop initialiser" $ do
        shouldTransformToContain
            "for (long i = 0; i < n; i++) -> i becomes int32_t"
            "void f(int n) { for (long i = 0; i < n; i++) { } }"
            "int32_t"

        shouldTransformToContain
            "for (long n = sizeof(int); ...) -> n becomes size_t"
            "void f(void) { for (long n = sizeof(int); n > 0; n--) { } }"
            "size_t"

        shouldTransformToContain
            "for (long i = 0; ...) with pointer in body -> i still int32_t (index, not assigned)"
            "void f(int *arr, int c) { for (long i = 0; i < c; i++) { arr[i]; } }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Scoping: long declared inside an if block
    -- -------------------------------------------------------------------
    describe "long declared inside if block" $ do
        shouldTransformToContain
            "long in if block with literal initialiser -> int32_t"
            "void f(int c) { if (c) { long x = 42; } }"
            "int32_t"

        shouldTransformToContain
            "long in if block initialised from pointer cast -> intptr_t"
            "void f(int c, void *p) { if (c) { long x = (long)p; } }"
            "intptr_t"

        shouldTransformToContain
            "long in else block initialised from sizeof -> size_t"
            "void f(int c) { if (c) { } else { long n = sizeof(int); } }"
            "size_t"

    -- -------------------------------------------------------------------
    -- Scoping: long declared inside a while body
    -- -------------------------------------------------------------------
    describe "long declared inside while body" $ do
        shouldTransformToContain
            "long in while body with literal -> int32_t"
            "void f(int c) { while (c) { long n = 0; } }"
            "int32_t"

        shouldTransformToContain
            "long in while body initialised from sizeof -> size_t"
            "void f(int c) { while (c) { long n = sizeof(int); } }"
            "size_t"

        shouldTransformToContain
            "long in while body initialised from pointer -> intptr_t"
            "void f(int c, void *p) { while (c) { long x = (long)p; } }"
            "intptr_t"

    -- -------------------------------------------------------------------
    -- Scoping: long declared inside a switch case
    -- -------------------------------------------------------------------
    describe "long declared inside switch case" $ do
        shouldTransformToContain
            "long in switch case with literal -> int32_t"
            "void f(int n) { switch (n) { case 1: { long x = 99; break; } } }"
            "int32_t"

        shouldTransformToContain
            "long in switch case initialised from sizeof -> size_t"
            "void f(int n) { switch (n) { case 1: { long x = sizeof(int); break; } } }"
            "size_t"

    -- -------------------------------------------------------------------
    -- Scoping: deeply nested blocks
    -- -------------------------------------------------------------------
    describe "long in deeply nested blocks" $ do
        shouldTransformToContain
            "long in if-inside-while initialised from pointer -> intptr_t"
            "void f(int c, void *p) { while (c) { if (c > 1) { long x = (long)p; } } }"
            "intptr_t"

        shouldTransformToContain
            "long in if-inside-while with literal -> int32_t"
            "void f(int c) { while (c) { if (c > 1) { long x = 42; } } }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Scoping: global long - evidence aggregated across all functions
    --   (classifyVarAcrossFuns: highest-priority evidence from any function wins)
    -- -------------------------------------------------------------------
    describe "global long: evidence aggregated across functions" $ do
        shouldTransformToContain
            "pointer assignment in one function overrides number assignment in another -> intptr_t"
            "long g; void f(void *p) { g = (long)p; } void h(void) { g = 42; }"
            "intptr_t"

        shouldTransformToContain
            "only number assignments across all functions -> int32_t"
            "long g; void f(void) { g = 1; } void h(void) { g = g + 2; }"
            "int32_t"

        shouldTransformToContain
            "sizeof assignment in one function, arithmetic in another -> size_t"
            "long g; void f(void) { g = sizeof(int); } void h(void) { g = g + 1; }"
            "size_t"

        shouldTransformToContain
            "global long unreferenced in any function -> int32_t default"
            "long g; void f(void) { }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- Scoping: variable name shadowing (flat name-based classification)
    --   classifyVar uses listify across the whole function, so pointer
    --   evidence in the inner 'x' bleeds into the outer 'x' of the same name.
    -- -------------------------------------------------------------------
    describe "variable name shadowing (flat name-based classification)" $ do
        shouldTransformToContain
            "outer long x = 42 reclassified to intptr_t by inner-scope pointer use of same name"
            "void f(void *p) { long x = 42; { long x = (long)p; } }"
            "intptr_t"

        -- With different names there is no bleed between the two variables
        shouldTransformToContain
            "outer long a = 42 stays int32_t when inner var uses a different name"
            "void f(void *p) { long a = 42; { long b = (long)p; } }"
            "int32_t"

        shouldTransformToContain
            "inner long b = (long)p -> intptr_t independently of outer long a"
            "void f(void *p) { long a = 42; { long b = (long)p; } }"
            "intptr_t"

    -- -------------------------------------------------------------------
    -- Scoping: long declared at outer scope, assigned in branches
    -- -------------------------------------------------------------------
    describe "long assigned inside control-flow branches" $ do
        shouldTransformToContain
            "long with pointer assignment in if-branch, literal in else -> intptr_t"
            "void f(void *p) { long x; if (1) { x = (long)p; } else { x = 42; } }"
            "intptr_t"

        shouldTransformToContain
            "long with sizeof assignment in if-branch, literal in else -> size_t"
            "void f(void) { long x; if (1) { x = sizeof(int); } else { x = 0; } }"
            "size_t"

        shouldTransformToContain
            "long with only literal assignments across both branches -> int32_t"
            "void f(int c) { long x; if (c) { x = 1; } else { x = 2; } }"
            "int32_t"

    -- -------------------------------------------------------------------
    -- CCall return-type evidence: function declared to return a pointer
    --   A long variable assigned from (long)fn() where fn returns void* or
    --   any other pointer type must be classified PointerType -> intptr_t,
    --   not fall through to NumberType -> int32_t.
    -- -------------------------------------------------------------------
    describe "long assigned from function returning pointer" $ do
        shouldTransformToContain
            "long x = (long)fn() where fn: void* -> void* becomes intptr_t"
            "void *get(void); void f(void) { long x = (long)get(); }"
            "intptr_t"

        shouldTransformNotToContain
            "long x = (long)fn() where fn: void* -> void*: no int32_t"
            "void *get(void); void f(void) { long x = (long)get(); }"
            "int32_t"

        shouldTransformToContain
            "long x = (long)fn() where fn defined in same TU -> intptr_t"
            "void *get(void) { return 0; } void f(void) { long x = (long)get(); }"
            "intptr_t"

        shouldTransformToContain
            "global long assigned from (long)fn() returning int* -> intptr_t"
            "int *alloc(void); long g; void f(void) { g = (long)alloc(); }"
            "intptr_t"
