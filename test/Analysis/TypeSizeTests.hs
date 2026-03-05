module Analysis.TypeSizeTests where

import Test.Hspec
import Analysis.TypeSize (checkPointerToInt)
import Analysis.AnalysisTestUtils
import Analysis.UtilTypes

typeSizeSpec :: Spec
typeSizeSpec = do
  describe "Type Size Issues" $ do
    testCheckPointerToInt
    
    
testCheckPointerToInt :: Spec
testCheckPointerToInt = do
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
