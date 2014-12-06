module Sgd.Num.VectorT (main) where 

import Test.HUnit
import qualified Data.Vector.Unboxed as VU
import qualified Sgd.Num.Vector as V

dot = TestCase $ do
  assertEqual 
    "dotFS Should work"
    10
    (V.dotFS (fromList [0, 1,2,3,4])  [(1, 1.0), (3, 3.0)])

  assertEqual 
    "dot Should work"
    40
    (V.dot (fromList [0, 1,2,3,4])  (fromList [1,2,3,4,5]))


addFS = TestCase $ do
  assertEqual 
    "addFS Should work"
    (fromList [0, 2, 2, 6, 4])
    (V.addFS (fromList [0, 1,2,3,4])  [(1, 1.0), (3, 3.0)])

dim = TestCase $ do
  assertEqual
    "dim should work"
    100
    (V.dim [(1,1.0), (5, 5.0), (100, 100)])

--norm = TestCase $ do
--    "normalizeL2 should work"
    
fromList :: [Double] -> V.FullVector  
fromList x = VU.fromList x :: V.FullVector

main :: IO Counts 
main =  
    runTestTT $ TestList [dot, addFS, dim]


