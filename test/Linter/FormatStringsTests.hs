module Linter.FormatStringsTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.FormatStrings
import Analysis.FormatStrings

formatStringsLintSpec :: Spec
formatStringsLintSpec = describe "FormatStrings Linting" $ do
  testLintDUsedWithSizet
  testLintUUsedWithSizet
  testLintXUsedWithSizet
  testLintDUsedWithPtrdifft
  testLintUUsedWithPtrdifft
  testLintDUsedWithPtr
  testLintUUsedWithPtr
  testLintXUsedWithPtr
  testLintLuUsedForPtrSizedVals
  testLintLdUsedWithLongAssuming64bits
  testFormatStringsIntegration

testLintDUsedWithSizet :: Spec
testLintDUsedWithSizet =
  describe "lintDUsedWithSizet" $ do
    shouldResolveIssue
      "resolves %d with size_t arg by changing to %zd"
      "void foo() { unsigned long sz; printf(\"%d\", sz); }"
      checkdUsedWithSizet
      lintDUsedWithSizet
    shouldLintTo
      "output contains %zd after linting"
      "void foo() { unsigned long sz; printf(\"%d\", sz); }"
      checkdUsedWithSizet
      lintFormatStringsIssues
      "%zd"
    shouldResolveIssue
      "resolves %d with size_t when flags and width present (e.g. %-10d)"
      "void foo() { unsigned long sz; printf(\"%-10d\", sz); }"
      checkdUsedWithSizet
      lintDUsedWithSizet

testLintUUsedWithSizet :: Spec
testLintUUsedWithSizet =
  describe "lintUUsedWithSizet" $ do
    shouldResolveIssue
      "resolves %u with size_t arg by changing to %zu"
      "void foo() { unsigned long sz; printf(\"%u\", sz); }"
      checkuUsedWithSizet
      lintUUsedWithSizet
    shouldLintTo
      "output contains %zu after linting"
      "void foo() { unsigned long sz; printf(\"%u\", sz); }"
      checkuUsedWithSizet
      lintFormatStringsIssues
      "%zu"

testLintXUsedWithSizet :: Spec
testLintXUsedWithSizet =
  describe "lintXUsedWithSizet" $ do
    shouldResolveIssue
      "resolves %x with size_t arg by changing to %zx"
      "void foo() { unsigned long sz; printf(\"%x\", sz); }"
      checkxUsedWithSizet
      lintXUsedWithSizet
    shouldLintTo
      "output contains %zx after linting"
      "void foo() { unsigned long sz; printf(\"%x\", sz); }"
      checkxUsedWithSizet
      lintFormatStringsIssues
      "%zx"

testLintDUsedWithPtrdifft :: Spec
testLintDUsedWithPtrdifft =
  describe "lintDUsedWithPtrdifft" $ do
    shouldResolveIssue
      "resolves %d with ptrdiff_t arg by changing to %td"
      "void foo() { long n; printf(\"%d\", n); }"
      checkdUsedWithPtrdifft
      lintDUsedWithPtrdifft
    shouldLintTo
      "output contains %td after linting"
      "void foo() { long n; printf(\"%d\", n); }"
      checkdUsedWithPtrdifft
      lintFormatStringsIssues
      "%td"

testLintUUsedWithPtrdifft :: Spec
testLintUUsedWithPtrdifft =
  describe "lintUUsedWithPtrdifft" $ do
    shouldResolveIssue
      "resolves %u with ptrdiff_t arg by changing to %tu"
      "void foo() { long n; printf(\"%u\", n); }"
      checkuUsedWithPtrdifft
      lintUUsedWithPtrdifft
    shouldLintTo
      "output contains %tu after linting"
      "void foo() { long n; printf(\"%u\", n); }"
      checkuUsedWithPtrdifft
      lintFormatStringsIssues
      "%tu"

testLintDUsedWithPtr :: Spec
testLintDUsedWithPtr =
  describe "lintDUsedWithPtr" $ do
    shouldResolveIssue
      "resolves %d with pointer arg by changing to %p"
      "void foo() { int *p; printf(\"%d\", p); }"
      checkdUsedWithPtr
      lintDUsedWithPtr
    shouldLintTo
      "output contains %p after linting"
      "void foo() { int *p; printf(\"%d\", p); }"
      checkdUsedWithPtr
      lintFormatStringsIssues
      "%p"

testLintUUsedWithPtr :: Spec
testLintUUsedWithPtr =
  describe "lintUUsedWithPtr" $ do
    shouldResolveIssue
      "resolves %u with pointer arg by changing to %p"
      "void foo() { int *p; printf(\"%u\", p); }"
      checkuUsedWithPtr
      lintUUsedWithPtr
    shouldLintTo
      "output contains %p after linting"
      "void foo() { int *p; printf(\"%u\", p); }"
      checkuUsedWithPtr
      lintFormatStringsIssues
      "%p"

testLintXUsedWithPtr :: Spec
testLintXUsedWithPtr =
  describe "lintXUsedWithPtr" $ do
    shouldResolveIssue
      "resolves %x with pointer arg by changing to %p"
      "void foo() { int *p; printf(\"%x\", p); }"
      checkxUsedWithPtr
      lintXUsedWithPtr
    shouldLintTo
      "output contains %p after linting"
      "void foo() { int *p; printf(\"%x\", p); }"
      checkxUsedWithPtr
      lintFormatStringsIssues
      "%p"

testLintLuUsedForPtrSizedVals :: Spec
testLintLuUsedForPtrSizedVals =
  describe "lintLuUsedForPtrSizedVals" $ do
    shouldResolveIssue
      "resolves %lu with pointer arg by changing to %zu"
      "void foo() { int *p; printf(\"%lu\", p); }"
      checkluUsedForPtrSizedVals
      lintLuUsedForPtrSizedVals
    shouldLintTo
      "output contains %zu after linting"
      "void foo() { int *p; printf(\"%lu\", p); }"
      checkluUsedForPtrSizedVals
      lintFormatStringsIssues
      "%zu"

testLintLdUsedWithLongAssuming64bits :: Spec
testLintLdUsedWithLongAssuming64bits =
  describe "lintLdUsedWithLongAssuming64bits" $ do
    shouldResolveIssue
      "resolves %ld with long arg by changing to %td"
      "void foo() { long n; printf(\"%ld\", n); }"
      checkldUsedWithLongAssuming64bits
      lintLdUsedWithLongAssuming64bits
    shouldLintTo
      "output contains %td after linting"
      "void foo() { long n; printf(\"%ld\", n); }"
      checkldUsedWithLongAssuming64bits
      lintFormatStringsIssues
      "%td"

testFormatStringsIntegration :: Spec
testFormatStringsIntegration =
  describe "integration" $ do
    shouldFullyLint
      "resolves all four distinct issues in a single printf call"
      "void foo() { int *p; long n; printf(\"%d %x %u %ld\", p, p, p, n); }"
      analyzeFormatStringIssues
      lintFormatStringsIssues
    shouldFullyLint
      "resolves one issue per call across two separate printf calls"
      "void foo() { int *p; int *q; printf(\"%d\", p); printf(\"%x\", q); }"
      analyzeFormatStringIssues
      lintFormatStringsIssues
    shouldFullyLint
      "resolves pointer and size_t issues in separate calls"
      "void foo() { int *p; unsigned long sz; printf(\"%d\", p); printf(\"%d\", sz); }"
      analyzeFormatStringIssues
      lintFormatStringsIssues
    shouldFullyLint
      "resolves all ten distinct issues in a single printf call"
      "void foo() { unsigned long sz; long n; int *p; printf(\"%d %u %x %d %u %d %u %x %lu %ld\", sz, sz, sz, n, n, p, p, p, p, n); }"
      analyzeFormatStringIssues
      lintFormatStringsIssues
    shouldFullyLint
      "resolves %d in fprintf (format arg at index 1)"
      "void foo() { int *p; fprintf(stdout, \"%d\", p); }"
      analyzeFormatStringIssues
      lintFormatStringsIssues
    shouldFullyLint
      "resolves %d in sprintf (format arg at index 1)"
      "void foo() { char buf[64]; int *p; sprintf(buf, \"%d\", p); }"
      analyzeFormatStringIssues
      lintFormatStringsIssues
    shouldFullyLint
      "resolves %d in snprintf (format arg at index 2)"
      "void foo() { char buf[64]; int *p; snprintf(buf, 64, \"%d\", p); }"
      analyzeFormatStringIssues
      lintFormatStringsIssues

