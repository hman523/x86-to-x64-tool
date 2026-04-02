module Transformation.FormatStringsTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.FormatStrings
import Analysis.FormatStrings
import Analysis.IssueTypes

formatStringsTransformSpec :: Spec
formatStringsTransformSpec = describe "FormatStrings Transformations" $ do
  testTransformDUsedWithSizet
  testTransformUUsedWithSizet
  testTransformXUsedWithSizet
  testTransformDUsedWithPtrdifft
  testTransformUUsedWithPtrdifft
  testTransformDUsedWithPtr
  testTransformUUsedWithPtr
  testTransformXUsedWithPtr
  testTransformLuUsedForPtrSizedVals
  testTransformLdUsedWithLongAssuming64bits
  testFormatStringsIntegration

testTransformDUsedWithSizet :: Spec
testTransformDUsedWithSizet =
  describe "transformDUsedWithSizet" $ do
    shouldResolveIssue
      "resolves %d with size_t arg by changing to %zd"
      "void foo() { unsigned long sz; printf(\"%d\", sz); }"
      checkdUsedWithSizet
      transformDUsedWithSizet
    shouldTransformTo
      "output contains %zd after transformation"
      "void foo() { unsigned long sz; printf(\"%d\", sz); }"
      checkdUsedWithSizet
      transformFormatStringsIssues
      "%zd"
    shouldResolveIssue
      "resolves %d with size_t when flags and width present (e.g. %-10d)"
      "void foo() { unsigned long sz; printf(\"%-10d\", sz); }"
      checkdUsedWithSizet
      transformDUsedWithSizet

testTransformUUsedWithSizet :: Spec
testTransformUUsedWithSizet =
  describe "transformUUsedWithSizet" $ do
    shouldResolveIssue
      "resolves %u with size_t arg by changing to %zu"
      "void foo() { unsigned long sz; printf(\"%u\", sz); }"
      checkuUsedWithSizet
      transformUUsedWithSizet
    shouldTransformTo
      "output contains %zu after transformation"
      "void foo() { unsigned long sz; printf(\"%u\", sz); }"
      checkuUsedWithSizet
      transformFormatStringsIssues
      "%zu"

testTransformXUsedWithSizet :: Spec
testTransformXUsedWithSizet =
  describe "transformXUsedWithSizet" $ do
    shouldResolveIssue
      "resolves %x with size_t arg by changing to %zx"
      "void foo() { unsigned long sz; printf(\"%x\", sz); }"
      checkxUsedWithSizet
      transformXUsedWithSizet
    shouldTransformTo
      "output contains %zx after transformation"
      "void foo() { unsigned long sz; printf(\"%x\", sz); }"
      checkxUsedWithSizet
      transformFormatStringsIssues
      "%zx"

testTransformDUsedWithPtrdifft :: Spec
testTransformDUsedWithPtrdifft =
  describe "transformDUsedWithPtrdifft" $ do
    shouldResolveIssue
      "resolves %d with ptrdiff_t arg by changing to %td"
      "void foo() { long n; printf(\"%d\", n); }"
      checkdUsedWithPtrdifft
      transformDUsedWithPtrdifft
    shouldTransformTo
      "output contains %td after transformation"
      "void foo() { long n; printf(\"%d\", n); }"
      checkdUsedWithPtrdifft
      transformFormatStringsIssues
      "%td"

testTransformUUsedWithPtrdifft :: Spec
testTransformUUsedWithPtrdifft =
  describe "transformUUsedWithPtrdifft" $ do
    shouldResolveIssue
      "resolves %u with ptrdiff_t arg by changing to %tu"
      "void foo() { long n; printf(\"%u\", n); }"
      checkuUsedWithPtrdifft
      transformUUsedWithPtrdifft
    shouldTransformTo
      "output contains %tu after transformation"
      "void foo() { long n; printf(\"%u\", n); }"
      checkuUsedWithPtrdifft
      transformFormatStringsIssues
      "%tu"

testTransformDUsedWithPtr :: Spec
testTransformDUsedWithPtr =
  describe "transformDUsedWithPtr" $ do
    shouldResolveIssue
      "resolves %d with pointer arg by changing to %p"
      "void foo() { int *p; printf(\"%d\", p); }"
      checkdUsedWithPtr
      transformDUsedWithPtr
    shouldTransformTo
      "output contains %p after transformation"
      "void foo() { int *p; printf(\"%d\", p); }"
      checkdUsedWithPtr
      transformFormatStringsIssues
      "%p"

testTransformUUsedWithPtr :: Spec
testTransformUUsedWithPtr =
  describe "transformUUsedWithPtr" $ do
    shouldResolveIssue
      "resolves %u with pointer arg by changing to %p"
      "void foo() { int *p; printf(\"%u\", p); }"
      checkuUsedWithPtr
      transformUUsedWithPtr
    shouldTransformTo
      "output contains %p after transformation"
      "void foo() { int *p; printf(\"%u\", p); }"
      checkuUsedWithPtr
      transformFormatStringsIssues
      "%p"

testTransformXUsedWithPtr :: Spec
testTransformXUsedWithPtr =
  describe "transformXUsedWithPtr" $ do
    shouldResolveIssue
      "resolves %x with pointer arg by changing to %p"
      "void foo() { int *p; printf(\"%x\", p); }"
      checkxUsedWithPtr
      transformXUsedWithPtr
    shouldTransformTo
      "output contains %p after transformation"
      "void foo() { int *p; printf(\"%x\", p); }"
      checkxUsedWithPtr
      transformFormatStringsIssues
      "%p"

testTransformLuUsedForPtrSizedVals :: Spec
testTransformLuUsedForPtrSizedVals =
  describe "transformLuUsedForPtrSizedVals" $ do
    shouldResolveIssue
      "resolves %lu with pointer arg by changing to %zu"
      "void foo() { int *p; printf(\"%lu\", p); }"
      checkluUsedForPtrSizedVals
      transformLuUsedForPtrSizedVals
    shouldTransformTo
      "output contains %zu after transformation"
      "void foo() { int *p; printf(\"%lu\", p); }"
      checkluUsedForPtrSizedVals
      transformFormatStringsIssues
      "%zu"

testTransformLdUsedWithLongAssuming64bits :: Spec
testTransformLdUsedWithLongAssuming64bits =
  describe "transformLdUsedWithLongAssuming64bits" $ do
    shouldResolveIssue
      "resolves %ld with long arg by changing to %td"
      "void foo() { long n; printf(\"%ld\", n); }"
      checkldUsedWithLongAssuming64bits
      transformLdUsedWithLongAssuming64bits
    shouldTransformTo
      "output contains %td after transformation"
      "void foo() { long n; printf(\"%ld\", n); }"
      checkldUsedWithLongAssuming64bits
      transformFormatStringsIssues
      "%td"

testFormatStringsIntegration :: Spec
testFormatStringsIntegration =
  describe "integration" $ do
    shouldFullyTransform
      "resolves all four distinct issues in a single printf call"
      "void foo() { int *p; long n; printf(\"%d %x %u %ld\", p, p, p, n); }"
      analyzeFormatStringIssues
      transformFormatStringsIssues
    shouldFullyTransform
      "resolves one issue per call across two separate printf calls"
      "void foo() { int *p; int *q; printf(\"%d\", p); printf(\"%x\", q); }"
      analyzeFormatStringIssues
      transformFormatStringsIssues
    shouldFullyTransform
      "resolves pointer and size_t issues in separate calls"
      "void foo() { int *p; unsigned long sz; printf(\"%d\", p); printf(\"%d\", sz); }"
      analyzeFormatStringIssues
      transformFormatStringsIssues
    shouldFullyTransform
      "resolves all ten distinct issues in a single printf call"
      "void foo() { unsigned long sz; long n; int *p; printf(\"%d %u %x %d %u %d %u %x %lu %ld\", sz, sz, sz, n, n, p, p, p, p, n); }"
      analyzeFormatStringIssues
      transformFormatStringsIssues

