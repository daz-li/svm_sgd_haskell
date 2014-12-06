module Sgd.Svm.TrainT where 

import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Sgd.Svm.Train as T
import qualified Sgd.Svm.LossFun as L
import qualified Sgd.Svm.Read as R

-- use ghci debugger
tryDebug = (T.f x) + y + z
  where x = 1
        y = 2
        z = 3

ss = R.normalizeEachSample [ ([(2,2.5), (5,5.1)], 1)
            , ([(3,3.1), (4,4.5)], -1)
            , ([(5,5.5), (7, 7.7)], -1)]

trainOneT = T.trainOne (L.dloss L.logLoss) (ss !! 0) 1e-5 wParam0 8
  where
    wParam0 = T.initParam 7

trainManyT = T.trainMany (L.dloss L.logLoss) ss lambda (T.initParam 7) eta
  where
    lambda = 1e-5
    eta0 = 8
    eta = map (\t -> eta0 / (1 + lambda * eta0 * t)) [0..]

testManyT = T.testMany (L.loss L.logLoss) ss 1e-5 model
  where 
    model = trainManyT


determineEta0T = T.determineEta0 (fTest . fTrain) (2, 1, 2)
  where
    lambda = 0.00001
    wParam0 = T.initParam 7
    fTrain =  T.trainMany (L.dloss L.logLoss) ss lambda wParam0
    fTest =  T.testMany (L.loss L.logLoss) ss lambda 


{--
determineEta0 = TestCase $ do
  assertEqual
    "determineEta0"
    5
    (x+y) 
      where x = 3
            y = 2

main :: IO Counts 
main =  
    runTestTT $ TestList [determineEta0]
--}




