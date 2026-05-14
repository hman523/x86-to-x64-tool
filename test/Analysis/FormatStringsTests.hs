module Analysis.FormatStringsTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.FormatStrings
import Analysis.IssueTypes

formatStringsSpec :: Spec
formatStringsSpec = describe "FormatStrings Analysis" $ do

    describe "checkdUsedWithPtr" $ do
        shouldFlagError
            "flags %d used with a pointer argument"
            "void foo() { int *p; printf(\"%d\", p); }"
            checkdUsedWithPtr

        shouldNotFlagError
            "does not flag %d with int argument"
            "void foo() { int x; printf(\"%d\", x); }"
            checkdUsedWithPtr

    describe "checkuUsedWithPtr" $ do
        shouldFlagError
            "flags %u used with a pointer argument"
            "void foo() { int *p; printf(\"%u\", p); }"
            checkuUsedWithPtr

    describe "checkxUsedWithPtr" $ do
        shouldFlagError
            "flags %x used with a pointer argument"
            "void foo() { int *p; printf(\"%x\", p); }"
            checkxUsedWithPtr

    describe "checkluUsedForPtrSizedVals" $ do
        shouldFlagError
            "flags %lu used with a pointer argument"
            "void foo() { int *p; printf(\"%lu\", p); }"
            checkluUsedForPtrSizedVals

        shouldNotFlagError
            "does not flag %lu with unsigned long argument"
            "void foo() { unsigned long n; printf(\"%lu\", n); }"
            checkluUsedForPtrSizedVals

    describe "checkdUsedWithSizet" $ do
        shouldFlagError
            "flags %d used with unsigned long (size_t) argument"
            "void foo() { unsigned long sz; printf(\"%d\", sz); }"
            checkdUsedWithSizet

    describe "checkuUsedWithSizet" $ do
        shouldFlagError
            "flags %u used with unsigned long (size_t) argument"
            "void foo() { unsigned long sz; printf(\"%u\", sz); }"
            checkuUsedWithSizet

    describe "checkxUsedWithSizet" $ do
        shouldFlagError
            "flags %x used with unsigned long (size_t) argument"
            "void foo() { unsigned long sz; printf(\"%x\", sz); }"
            checkxUsedWithSizet

    describe "checkdUsedWithPtrdifft" $ do
        shouldFlagError
            "flags %d used with long (ptrdiff_t) argument"
            "void foo() { long d; printf(\"%d\", d); }"
            checkdUsedWithPtrdifft

    describe "checkuUsedWithPtrdifft" $ do
        shouldFlagError
            "flags %u used with long (ptrdiff_t) argument"
            "void foo() { long d; printf(\"%u\", d); }"
            checkuUsedWithPtrdifft

    describe "checkldUsedWithLongAssuming64bits" $ do
        shouldFlagError
            "flags %ld used with long (assumes 64-bit long)"
            "void foo() { long n; printf(\"%ld\", n); }"
            checkldUsedWithLongAssuming64bits

        shouldNotFlagError
            "does not flag %d with int"
            "void foo() { int n; printf(\"%d\", n); }"
            checkldUsedWithLongAssuming64bits

    describe "format string parsing" $ do
        shouldFlagNIssues
            "correctly identifies multiple format issues in one call"
            "void foo() { int *p; int *q; printf(\"%d %x\", p, q); }"
            analyzeFormatStringIssues
            2

    describe "multiple issues" $ do
        shouldFlagAllTags
            "four format specifier issues fire from a single printf call"
            "void foo() { int *p; long n; printf(\"%d %x %u %ld\", p, p, p, n); }"
            analyzeFormatStringIssues
            [DUsedWithPtr, XUsedWithPtr, UUsedWithPtr, LdUsedWithLongAssuming64bits]

        shouldFlagNIssues
            "two printf calls each with one bad specifier produce two issues"
            "void foo() { int *p; int *q; printf(\"%d\", p); printf(\"%x\", q); }"
            analyzeFormatStringIssues
            2

        shouldFlagAllTags
            "pointer arg and size_t arg both printed with percent-d fire two distinct tags"
            "void foo() { int *p; unsigned long sz; printf(\"%d\", p); printf(\"%d\", sz); }"
            analyzeFormatStringIssues
            [DUsedWithPtr, DUsedWithSizet]

    describe "flags and width in format specifiers" $ do

        shouldFlagError
            "flags %+5ld with long argument: still flagged as LdUsedWithLongAssuming64bits"
            "void foo() { long n; printf(\"%+5ld\\n\", n); }"
            checkldUsedWithLongAssuming64bits

        shouldFlagError
            "flags %-10ld with long argument: still flagged"
            "void foo() { long n; printf(\"%-10ld\\n\", n); }"
            checkldUsedWithLongAssuming64bits

        shouldFlagError
            "flags %05lu with pointer argument: still flagged as LuUsedForPtrSizedVals"
            "void foo() { int *p; printf(\"%05lu\\n\", p); }"
            checkluUsedForPtrSizedVals

        shouldFlagError
            "flags %+ld with long: flagged even when sign flag present"
            "void foo() { long n; printf(\"%+ld\\n\", n); }"
            checkldUsedWithLongAssuming64bits

        shouldFlagError
            "precision .5 present: %10.5ld still flagged"
            "void foo() { long n; printf(\"%10.5ld\\n\", n); }"
            checkldUsedWithLongAssuming64bits

        shouldNotFlagError
            "flags and width with correct %ld for long: not flagged when correct specifier used"
            "void foo() { int n; printf(\"%+5d\\n\", n); }"
            checkldUsedWithLongAssuming64bits

    describe "printf-family variant edge cases" $ do

        -- fprintf is in fmtArgIndex at index 1; first arg is the FILE*
        shouldFlagError
            "flags %d with pointer in fprintf"
            "void foo() { int *p; fprintf(0, \"%d\", p); }"
            checkdUsedWithPtr

        -- sprintf has format string at index 1; first arg is the buffer
        shouldFlagError
            "flags %d with pointer in sprintf"
            "void foo() { int *p; char buf[64]; sprintf(buf, \"%d\", p); }"
            checkdUsedWithPtr

        -- snprintf has format string at index 2
        shouldFlagError
            "flags %d with pointer in snprintf"
            "void foo() { int *p; char buf[64]; snprintf(buf, 64, \"%d\", p); }"
            checkdUsedWithPtr

        -- %p is the correct specifier for pointers — must NOT flag
        shouldNotFlagError
            "does not flag printf with %p and pointer argument (correct specifier)"
            "void foo() { int *p; printf(\"%p\", p); }"
            checkdUsedWithPtr

        -- %% escape: the doubled percent is NOT a format specifier and must not
        -- consume an argument slot, so the real %d is the only issue
        shouldFlagNIssues
            "printf with '%%' escape before real %d: only one issue (the %d), not two"
            "void foo() { int *p; printf(\"%%d %d\", p); }"
            checkdUsedWithPtr
            1

        -- Width specified but conversion is still wrong
        shouldFlagError
            "flags %10d with pointer (width modifier does not suppress the issue)"
            "void foo() { int *p; printf(\"%10d\", p); }"
            checkdUsedWithPtr

        -- Already-correct %zd should not be flagged for size_t
        shouldNotFlagError
            "does not flag printf with %zd and size_t argument (already correct)"
            "void foo() { unsigned long sz; printf(\"%zd\", sz); }"
            checkdUsedWithSizet
