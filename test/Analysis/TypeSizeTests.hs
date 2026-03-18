module Analysis.TypeSizeTests where

import Test.Hspec
import Analysis.TypeSize (analyzeTypeSizeIssues, checkPointerToInt, checkPointerToUInt, checkIntToPointer, checkLongToPointer, checkSizeOfInt, checkSizeOfLong, checkIntAsSizet, checkIntAsPtrdifft, checkUIntAsMemSize)
import Analysis.AnalysisTestUtils
import Analysis.UtilTypes

typeSizeSpec :: Spec
typeSizeSpec = do
  describe "Type Size Issues" $ do
    testCheckPointerToInt
    testCheckPointerToUInt
    testCheckIntToPointer
    testCheckLongToPointer
    testCheckSizeOfInt
    testCheckSizeOfLong
    testCheckIntAsSizet
    testCheckIntAsPtrdifft
    testCheckUIntAsMemSize
    testCheckTypedef
    testMultipleIssues

testCheckPointerToInt :: Spec
testCheckPointerToInt = describe "Pointer to Int Casts" $ do
    shouldFlagError "detects pointer cast to int" 
      "int main() { int *ptr = 0; int x = (int)ptr; return 0; }"
      checkPointerToInt
    
    shouldFlagError "detects pointer cast to int after initialization" 
      "int main() { int *ptr = 0; int x; x = (int)ptr; return 0; }"
      checkPointerToInt
    
    shouldFlagError "detects pointer cast to int in assignment"
      "int main() { int *ptr = 0; int x; x = (int)ptr; return 0; }"
      checkPointerToInt
    
    shouldNotFlagError "allows int to int cast"
      "int main() { int x = 5; int y = (int)x; return 0; }"
      checkPointerToInt

    shouldFlagErrorWithDetails "detects pointer cast to int with correct type"
      "int main() { int *ptr = 0; int x = (int)ptr; return 0; }"
      checkPointerToInt
      CastPointerToInt
      (Just "(int)ptr")
    
    shouldFlagErrorWithDetails "detects pointer cast to int in assignment with correct type"
      "int main() { int *ptr = 0; int x; x = (int)ptr; return 0; }"
      checkPointerToInt
      CastPointerToInt
      (Just "(int)ptr")

testCheckPointerToUInt :: Spec
testCheckPointerToUInt = describe "Pointer to Unsigned Int Casts" $ do
    shouldFlagError "detects pointer cast to unsigned int" 
      "int main() { int *ptr = 0; unsigned int x = (unsigned int)ptr; return 0; }"
      checkPointerToUInt
    
    shouldFlagError "detects pointer cast to unsigned int in assignment"
      "int main() { int *ptr = 0; unsigned int x; x = (unsigned int)ptr; return 0; }"
      checkPointerToUInt
    
    shouldNotFlagError "allows unsigned int to unsigned int cast"
      "int main() { unsigned int x = 5; unsigned int y = (unsigned int)x; return 0; }"
      checkPointerToUInt

    shouldFlagErrorWithDetails "detects pointer cast to unsigned int with correct type"
      "int main() { int *ptr = 0; unsigned int x = (unsigned int)ptr; return 0; }"
      checkPointerToUInt
      CastPointerToUInt
      (Just "(unsigned int)ptr")

testCheckIntToPointer :: Spec
testCheckIntToPointer = describe "Int to Pointer Casts" $ do
    shouldFlagError "detects int cast to pointer"
      "int main() { int x = 5; int *ptr = (int*)x; return 0; }"
      checkIntToPointer
    
    shouldFlagError "detects int cast to pointer in assignment"
      "int main() { int x = 5; int *ptr; ptr = (int*)x; return 0; }"
      checkIntToPointer
    
    shouldNotFlagError "allows pointer to pointer cast"
      "int main() { int *ptr1 = 0; int *ptr2 = (int*)ptr1; return 0; }"
      checkIntToPointer

    shouldFlagErrorWithDetails "detects int cast to pointer with correct type"
      "int main() { int x = 5; int *ptr = (int*)x; return 0; }"
      checkIntToPointer
      CastIntToPointer
      (Just "(int*)x")

testCheckLongToPointer :: Spec
testCheckLongToPointer = describe "Long to Pointer Casts" $ do
    shouldFlagError "detects long cast to pointer"
      "int main() { long x = 5; int *ptr = (int*)x; return 0; }"
      checkLongToPointer
    
    shouldFlagError "detects long cast to pointer in assignment"
      "int main() { long x = 5; int *ptr; ptr = (int*)x; return 0; }"
      checkLongToPointer
    
    shouldNotFlagError "allows pointer to pointer cast"
      "int main() { int *ptr1 = 0; int *ptr2 = (int*)ptr1; return 0; }"
      checkLongToPointer

    shouldFlagErrorWithDetails "detects long cast to pointer with correct type"
      "int main() { long x = 5; int *ptr = (int*)x; return 0; }"
      checkLongToPointer
      CastLongToPointer
      (Just "(int*)x")

    shouldFlagError "detects long cast to void pointer"
      "int main() { long x = 5; void *ptr = (void*)x; return 0; }"
      checkLongToPointer

    shouldNotFlagError "allows int cast to pointer (not long)"
      "int main() { int x = 5; int *ptr = (int*)x; return 0; }"
      checkLongToPointer

testCheckSizeOfInt :: Spec
testCheckSizeOfInt = describe "Size of Int Checks" $ do
    shouldFlagError "detects sizeof(int) == sizeof(void*)"
      "int main() { if (sizeof(int) == sizeof(void*)) { return 0; } return 1; }"
      checkSizeOfInt

    shouldFlagError "detects sizeof(int) != sizeof(void*)"
      "int main() { if (sizeof(int) != sizeof(void*)) { return 0; } return 1; }"
      checkSizeOfInt

    shouldNotFlagError "allows sizeof(int) == sizeof(long)"
      "int main() { if (sizeof(int) == sizeof(long)) { return 0; } return 1; }"
      checkSizeOfInt

    shouldFlagErrorWithDetails "detects sizeof(int) == sizeof(void*) with correct type"
      "int main() { if (sizeof(int) == sizeof(void*)) { return 0; } return 1; }"
      checkSizeOfInt
      SizeOfIntIsVoid
      (Just "sizeof(int)")

    shouldNotFlagError "allows sizeof(int) == sizeof(int)"
      "int main() { if (sizeof(int) == sizeof(int)) { return 0; } return 1; }"
      checkSizeOfInt

    shouldFlagError "detects sizeof(void*) == sizeof(int) (reversed order)"
      "int main() { if (sizeof(void*) == sizeof(int)) { return 0; } return 1; }"
      checkSizeOfInt

    shouldFlagErrorWithDetails "detects sizeof(void*) == sizeof(int) reversed with correct type"
      "int main() { if (sizeof(void*) == sizeof(int)) { return 0; } return 1; }"
      checkSizeOfInt
      SizeOfIntIsVoid
      (Just "sizeof(int)")

testCheckIntAsSizet :: Spec
testCheckIntAsSizet = describe "Int as size_t Checks" $ do
    shouldNotFlagError "detects int assigned to size variable"
      "int main() { int x = 5; int len; len = x; return 0; }"
      checkIntAsSizet
    
    shouldNotFlagError "detects int assigned to len variable"
      "int main() { int x = 5; int len = x; return 0; }"
      checkIntAsSizet
    
    shouldNotFlagError "allows int assigned to regular variable"
      "int main() { int x = 5; int y = x; return 0; }"
      checkIntAsSizet

    shouldFlagErrorWithDetails "detects int assigned to size variable with correct type"
      "int main() { int x = 5; unsigned long size; size = x; return 0; }"
      checkIntAsSizet
      UsingIntAsSizet
      (Just "size")

testCheckIntAsPtrdifft :: Spec
testCheckIntAsPtrdifft = describe "Int as ptrdiff_t Checks" $ do
    shouldNotFlagError "detects int assigned to diff variable"
      "int main() { int *p1 = 0; int *p2 = 0; int diff = p1 - p2; return 0; }"
      checkIntAsPtrdifft
    
    shouldNotFlagError "detects int assigned to offset variable"
      "int main() { int *p1 = 0; int *p2 = 0; int offset = p1 - p2; return 0; }"
      checkIntAsPtrdifft
    
    shouldNotFlagError "allows int assigned to regular variable"
      "int main() { int x = 5; int y = x; return 0; }"
      checkIntAsPtrdifft

    shouldFlagErrorWithDetails "detects int assigned to diff variable with correct type"
      "int main() { int x = 5; long diff; diff = x; return 0; }"
      checkIntAsPtrdifft
      UsingIntAsPtrdifft
      (Just "diff")

testCheckUIntAsMemSize :: Spec
testCheckUIntAsMemSize = describe "Unsigned Int as Memory Size Checks" $ do
    shouldFlagError "detects unsigned int used in malloc"
      "int main() { unsigned int size = 10; void *ptr = malloc(size); return 0; }"
      checkUIntAsMemSize
    
    shouldFlagError "detects unsigned int used in calloc"
      "int main() { unsigned int count = 10; void *ptr = calloc(count, 1); return 0; }"
      checkUIntAsMemSize
    
    shouldNotFlagError "allows long used in malloc"
      "int main() { long size = 10; void *ptr = malloc(size); return 0; }"
      checkUIntAsMemSize

testCheckSizeOfLong :: Spec
testCheckSizeOfLong = describe "Size of Long Checks" $ do
    shouldFlagError "detects sizeof(long) == sizeof(void*)"
        "int main() { if (sizeof(long) == sizeof(void*)) { return 0; } return 1; }"
        checkSizeOfLong

    shouldFlagError "detects sizeof(long) != sizeof(void*)"
        "int main() { if (sizeof(long) != sizeof(void*)) { return 0; } return 1; }"
        checkSizeOfLong

    shouldNotFlagError "allows sizeof(long) == sizeof(long)"
        "int main() { if (sizeof(long) == sizeof(long)) { return 0; } return 1; }"
        checkSizeOfLong

    shouldFlagErrorWithDetails "detects sizeof(long) == sizeof(void*) with correct type"
        "int main() { if (sizeof(long) == sizeof(void*)) { return 0; } return 1; }"
        checkSizeOfLong
        SizeOfLongIsVoid
        (Just "sizeof(long)")

    shouldNotFlagError "allows sizeof(long) == sizeof(int)"
        "int main() { if (sizeof(long) == sizeof(int)) { return 0; } return 1; }"
        checkSizeOfLong

    shouldFlagError "detects sizeof(void*) == sizeof(long) (reversed order)"
        "int main() { if (sizeof(void*) == sizeof(long)) { return 0; } return 1; }"
        checkSizeOfLong

    shouldFlagErrorWithDetails "detects sizeof(void*) == sizeof(long) reversed with correct type"
        "int main() { if (sizeof(void*) == sizeof(long)) { return 0; } return 1; }"
        checkSizeOfLong
        SizeOfLongIsVoid
        (Just "sizeof(long)")

testCheckTypedef :: Spec
testCheckTypedef = describe "Typedef Type Resolution" $ do
    shouldFlagError "detects pointer cast to typedef'd int"
        "typedef int myint; int main() { int *ptr = 0; myint x = (myint)ptr; return 0; }"
        checkPointerToInt

    shouldFlagError "detects pointer cast to typedef'd unsigned int"
        "typedef unsigned int myuint; int main() { int *ptr = 0; myuint x = (myuint)ptr; return 0; }"
        checkPointerToUInt

    shouldFlagError "detects typedef'd int value cast to pointer"
        "typedef int myint; int main() { myint x = 5; int *ptr = (int*)x; return 0; }"
        checkIntToPointer

    shouldFlagError "detects typedef'd long value cast to pointer"
        "typedef long mylong; int main() { mylong x = 5; int *ptr = (int*)x; return 0; }"
        checkLongToPointer

    shouldFlagError "detects int assigned to typedef'd size_t (unsigned long)"
        "typedef unsigned long mysize; int main() { int x = 5; mysize s; s = x; return 0; }"
        checkIntAsSizet

    shouldFlagError "detects int assigned to typedef'd ptrdiff_t (long)"
        "typedef long mydiff; int main() { int x = 5; mydiff d; d = x; return 0; }"
        checkIntAsPtrdifft

    shouldFlagError "detects typedef'd unsigned int passed to malloc"
        "typedef unsigned int myuint; int main() { myuint n = 10; void *p = malloc(n); return 0; }"
        checkUIntAsMemSize

    shouldNotFlagError "allows typedef'd long passed to malloc"
        "typedef long mylong; int main() { mylong n = 10; void *p = malloc(n); return 0; }"
        checkUIntAsMemSize

    shouldNotFlagError "allows typedef'd pointer cast to pointer"
        "typedef int* intptr; int main() { intptr p = 0; int *q = (int*)p; return 0; }"
        checkIntToPointer

testMultipleIssues :: Spec
testMultipleIssues = describe "Multiple Issues (analyzeTypeSizeIssues)" $ do

    -- -----------------------------------------------------------------------
    -- Multiple occurrences of the same issue kind
    -- -----------------------------------------------------------------------
    describe "Multiple occurrences of the same issue" $ do

        shouldFlagAtLeastNIssues
            "finds two pointer-to-int casts in the same function"
            "int main() { int *p1 = 0; int *p2 = 0; \
            \int a = (int)p1; int b = (int)p2; return 0; }"
            analyzeTypeSizeIssues
            2

        shouldFlagExactTags
            "two pointer-to-int casts produce exactly two CastPointerToInt tags"
            "int main() { int *p1 = 0; int *p2 = 0; \
            \int a = (int)p1; int b = (int)p2; return 0; }"
            analyzeTypeSizeIssues
            [CastPointerToInt, CastPointerToInt]

        shouldFlagAtLeastNIssues
            "finds two long-to-pointer casts in the same function"
            "int main() { long x = 1; long y = 2; \
            \int *p = (int*)x; int *q = (int*)y; return 0; }"
            analyzeTypeSizeIssues
            2

        shouldFlagExactTags
            "two long-to-pointer casts produce exactly two CastLongToPointer tags"
            "int main() { long x = 1; long y = 2; \
            \int *p = (int*)x; int *q = (int*)y; return 0; }"
            analyzeTypeSizeIssues
            [CastLongToPointer, CastLongToPointer]

        shouldFlagAtLeastNIssues
            "finds three unsigned-int-as-mem-size warnings across malloc/calloc/realloc"
            "int main() { unsigned int n = 4; \
            \void *a = malloc(n); void *b = calloc(n, 1); void *c = realloc(a, n); return 0; }"
            analyzeTypeSizeIssues
            3

        shouldFlagAllTags
            "three alloc calls each produce UsingUIntAsMemSize"
            "int main() { unsigned int n = 4; \
            \void *a = malloc(n); void *b = calloc(n, 1); void *c = realloc(a, n); return 0; }"
            analyzeTypeSizeIssues
            [UsingUIntAsMemSize, UsingUIntAsMemSize, UsingUIntAsMemSize]

    -- -----------------------------------------------------------------------
    -- Mixed different issue kinds
    -- -----------------------------------------------------------------------
    describe "Mixed different issue kinds" $ do

        shouldFlagAllTags
            "pointer-to-int cast and unsigned-int passed to malloc"
            "int main() { int *ptr = 0; int x = (int)ptr; \
            \unsigned int sz = 10; void *m = malloc(sz); return 0; }"
            analyzeTypeSizeIssues
            [CastPointerToInt, UsingUIntAsMemSize]

        shouldFlagAllTags
            "int-to-pointer cast and sizeof(int)==sizeof(void*) comparison"
            "int main() { int n = 5; int *p = (int*)n; \
            \if (sizeof(int) == sizeof(void*)) { return 0; } return 1; }"
            analyzeTypeSizeIssues
            [CastIntToPointer, SizeOfIntIsVoid]

        shouldFlagAllTags
            "long-to-pointer cast and sizeof(long)==sizeof(void*) comparison"
            "int main() { long n = 5; void *p = (void*)n; \
            \if (sizeof(long) == sizeof(void*)) { return 0; } return 1; }"
            analyzeTypeSizeIssues
            [CastLongToPointer, SizeOfLongIsVoid]

        shouldFlagAllTags
            "int assigned to size_t and unsigned int passed to malloc"
            "int main() { int x = 5; unsigned long s; s = x; \
            \unsigned int n = 10; void *m = malloc(n); return 0; }"
            analyzeTypeSizeIssues
            [UsingIntAsSizet, UsingUIntAsMemSize]

        shouldFlagAllTags
            "pointer-to-int, int-to-pointer, and unsigned-int-as-mem-size all in one"
            "int main() { int *ptr = 0; int a = (int)ptr; \
            \int b = 5; int *q = (int*)b; \
            \unsigned int sz = 8; void *m = malloc(sz); return 0; }"
            analyzeTypeSizeIssues
            [CastPointerToInt, CastIntToPointer, UsingUIntAsMemSize]

        shouldFlagAtLeastNIssues
            "five-issue snippet triggers at least five issues"
            "int main() { \
            \int *p = 0; int ci = (int)p; \
            \unsigned int ui = (unsigned int)p; \
            \int iv = 3; int *ip = (int*)iv; \
            \long lv = 7; void *lp = (void*)lv; \
            \unsigned int sz = 16; void *m = malloc(sz); return 0; }"
            analyzeTypeSizeIssues
            5

        shouldFlagAllTags
            "five distinct issue kinds all detected together"
            "int main() { \
            \int *p = 0; int ci = (int)p; \
            \unsigned int ui = (unsigned int)p; \
            \int iv = 3; int *ip = (int*)iv; \
            \long lv = 7; void *lp = (void*)lv; \
            \unsigned int sz = 16; void *m = malloc(sz); return 0; }"
            analyzeTypeSizeIssues
            [ CastPointerToInt
            , CastPointerToUInt
            , CastIntToPointer
            , CastLongToPointer
            , UsingUIntAsMemSize
            ]
