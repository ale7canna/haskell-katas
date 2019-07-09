import Test.Hspec
import Test.QuickCheck

examples = do
  it "should work for some examples" $ do
    tribonacci (1, 1, 1) 10 `shouldBe` [1,1,1,3,5,9,17,31,57,105]
    tribonacci (0, 0, 1) 10 `shouldBe` [0,0,1,1,2,4,7,13,24,44]
    tribonacci (0, 1, 1) 10 `shouldBe` [0,1,1,2,4,7,13,24,44,81]
    
main :: IO ()
main = hspec $ do
  describe "Examples" examples

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n =
  take n $ a : b : c : next a b c

next :: Num a => a -> a -> a -> [a]
next a b c = nextVal : next b c nextVal
  where nextVal = a + b + c
