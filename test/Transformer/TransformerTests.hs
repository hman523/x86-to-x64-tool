module Transformer.TransformerTests where

import Test.Hspec

import X86_to_X64 (transformSource)
import Transformer.TransformerTestUtils

transformerSpec :: Spec
transformerSpec = describe "Transformer" $ do

    -- -------------------------------------------------------------------
    -- NumberType: plain arithmetic → int32_t
    -- -------------------------------------------------------------------
    describe "long (number) → int32_t" $ do
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
    -- PointerType: assigned from a pointer expression → intptr_t
    -- -------------------------------------------------------------------
    describe "long (pointer) → intptr_t" $ do
        shouldTransformToContain
            "long local assigned from pointer cast becomes intptr_t"
            "void f(void *p) { long x = (long)p; }"
            "intptr_t"

        shouldTransformToContain
            "long local assigned from pointer expression becomes intptr_t"
            "void f(int *p) { long x; x = (long)p; }"
            "intptr_t"

    -- -------------------------------------------------------------------
    -- SizeType: assigned from sizeof → size_t
    -- -------------------------------------------------------------------
    describe "long (size) → size_t" $ do
        shouldTransformToContain
            "long local assigned from sizeof(type) becomes size_t"
            "void f(void) { long sz = sizeof(int); }"
            "size_t"

        shouldTransformToContain
            "long local assigned from sizeof(expr) becomes size_t"
            "void f(int x) { long sz = sizeof(x); }"
            "size_t"

    -- -------------------------------------------------------------------
    -- OffsetType: assigned from pointer subtraction → ptrdiff_t
    -- -------------------------------------------------------------------
    describe "long (offset) → ptrdiff_t" $ do
        shouldTransformToContain
            "long local assigned from ptr - ptr becomes ptrdiff_t"
            "void f(int *a, int *b) { long d = a - b; }"
            "ptrdiff_t"

    -- -------------------------------------------------------------------
    -- BitSeqType: used in bitwise operation → uint32_t
    -- -------------------------------------------------------------------
    describe "long (bit-sequence) → uint32_t" $ do
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
    describe "unsigned long → uint32_t (number)" $ do
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
    describe "struct member long → int32_t" $ do
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
    describe "typedef long T → typedef int32_t T" $ do
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
    describe "sizeof(long) → sizeof(int32_t)" $ do
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
