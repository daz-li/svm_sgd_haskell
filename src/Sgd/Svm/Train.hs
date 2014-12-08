module Sgd.Svm.Train where

import qualified Sgd.Svm.Read as R
import Sgd.Num.Vector
import Sgd.Svm.LossFun 

type Sample = R.Sample Int
type Model = (FullVector, Double, Double)       -- model parameter (w, wDivsor, wBias)
type PredLoss = (Double, Double, Double)        -- prediction error on the Model (los, cost, num.error)

{-- TODO: remove
fTest :: [Sample] -> [Sample]
fTest x = x
--}

f x = t
  where 
    t = x + 10

train :: Loss                             -- loss and dloss function
      -> [Sample]                         -- list of samples
      -> Double                           -- regularizer 
      -> Int                              -- epochs
      -> Model
train l x lambda epochs = foldl (go) wParam0 [1..epochs]
  where 
    go wParam _  = trainMany (dloss l) x lambda wParam eta 

    -- daz-li: memory performance tuning, 
    dim = R.dimSample x
    -- dim = 2000
    wParam0 = initParam dim 

    -- daz-li: memory performance tuning, 
    n = min 1000 . length $ x
    -- n = 1000

    fTrain =  trainMany (dloss l) (take n x) lambda wParam0
    fTest =  testMany (loss l) (take n x) lambda 
    eta0 = determineEta0 (fTest . fTrain) (2, 1, 2)
    eta = map (\t -> eta0 / (1 + lambda * eta0 * t)) [0..]



initParam :: Int -> Model
initParam dimVar = (rep (dimVar + 1)  0, 1, 0)
 
determineEta0 :: ([Double] -> PredLoss)     -- fTest . fTrain params: given [eta] return PredLoss
              -> (Double, Double, Double)   -- (factor, lowEta, highEta)
              -> Double                     -- eta
determineEta0 f e@(factor, loEta, hiEta) 
  | loCost < hiCost = slideEta f e hiCost
  | loCost > hiCost = climbEta f e loCost
  where 
    (_, loCost, _) = f $ repeat loEta
    (_, hiCost, _) = f $ repeat hiEta

slideEta :: ([Double] -> PredLoss)        -- fTest . fTrain params: given [eta] return PredLoss
          -> (Double, Double, Double)     -- (factor, lowEta, highEta)
          -> Double                       -- highCost 
          -> Double                       -- eta
slideEta f (factor, loEta, hiEta) hiCost
  | loCost < hiCost = slideEta f (factor, loEta/factor, loEta) loCost
  | otherwise       = loEta
  where 
    (_, loCost, _) = f $ repeat loEta

climbEta :: ([Double] -> PredLoss)        -- fTest . fTrain params: given [eta] return PredLoss
          -> (Double, Double, Double)     -- (factor, lowEta, highEta)
          -> Double                       -- lowCost 
          -> Double                       -- eta
climbEta f (factor, loEta, hiEta) loCost
  | loCost > hiCost = climbEta f (factor, hiEta, hiEta*2) hiCost
  | otherwise       = loEta
  where 
    (_, hiCost, _) = f $ repeat hiEta
  

trainMany :: (Double -> Double -> Double)     -- dloss function
          -> [Sample]                         -- list of samples
          -> Double                           -- regularizer 
          -> Model                            -- current model parameter 
          -> [Double]                         -- sgd gain eta for each iteration (or sample)
          -> Model 
trainMany dloss x lambda wParam0 eta = foldl go wParam0 $ zip x eta 
  where
    go wParam (xt, etat) = trainOne dloss xt lambda wParam etat 
    
     
trainOne  :: (Double -> Double -> Double) -- dloss function
          -> (SparseVector, Double)       -- single input and response (x, y)
          -> Double                       -- regularizer 
          -> Model                        -- current model parameter 
          -> Double                       -- sgd gain eta
          -> Model
trainOne dloss (x, y) lambda (w, wDiv, wBias) eta = (w'', wDiv'', wBias')
  where 
    s = (dotFS w x) / wDiv + wBias
    d = dloss s y                                                         
    wDiv' = wDiv / (1 - eta * lambda)
    (w', wDiv'') = renorm w wDiv'
    -- TODO should use accumulate function of Vector!
    w'' = addFS w' . mul x $ eta * d * wDiv''
    wBias' = wBias + d * eta * 0.01; 

testMany :: (Double -> Double -> Double)      -- loss function
          -> [Sample]                         -- list of samples
          -> Double                           -- regularizer 
          -> Model                            -- model parameter 
          -> PredLoss
testMany loss x lambda wParam = (los, cost, nerr)
  where
    los = ploss / fromIntegral (length x)
    nerr = (fromIntegral pnerr) / fromIntegral (length x)
    cost = los + 0.5 * lambda * (wnorm wParam)
    (ploss, pnerr) = (\(t1, t2, _) -> (sum t1, sum t2)) . unzip3 . map go $ x
      where 
        go x = testOne loss x lambda wParam
    
 
testOne :: (Double -> Double -> Double)    -- loss function
          -> (SparseVector, Double)        -- single input and response (x, y)
          -> Double                        -- regularizer 
          -> Model                         -- model parameter 
          -> (Double, Int, Double)
testOne loss (x, y) lambda (w, wDiv, wBias) = (ploss, pnerr, s)
  where 
    s = (dotFS w x) / wDiv + wBias
    ploss = loss s y
    pnerr = if s * y <= 0 then 1 else 0


renorm :: FullVector -> Double -> (FullVector, Double) 
renorm w wDiv 
  | wDiv == 1.0 || wDiv <= 1e5  = (w, wDiv)
  | otherwise                   = (scale w $ 1 / wDiv, 1.0) 

wnorm (w, wDiv, wBias) = (dot w w ) / wDiv / wDiv 
