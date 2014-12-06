module Sgd.Svm.LossFun where

data Loss = Loss {
    loss :: Double -> Double -> Double
    --  -dloss(a,y)/da
  , dloss :: Double -> Double -> Double
  }
  
-- logloss(a,y) = log(1+exp(-a*y))
logLoss :: Loss 
logLoss = Loss { 
    loss = loss'
  , dloss = dloss'  
  }
  where 
    loss' a y 
      | z > 18      = exp (-z)
      | z < -18     = -z
      | otherwise   = log $ 1 + exp (-z)
      where z = a*y
 
    dloss' a y
      | z > 18      = y * exp (-z)
      | z < -18     = y
      | otherwise   = y / (1 + exp z)
      where z = a*y

-- hingeloss(a,y) = max(0, 1-a*y)
hingeLoss :: Loss 
hingeLoss = Loss {
    loss = \a y -> let z = a*y
                    in if z > 1 then 0 else 1 - z

  , dloss = \a y -> if a*y > 1 then 0 else y
  }

-- squaredhingeloss(a,y) = 1/2 * max(0, 1-a*y)^2
squaredHingeLoss :: Loss 
squaredHingeLoss = Loss {
    loss = \a y -> let z = a*y
                    in if z > 1 then 0 else 0.5 * (1 - z)^2 

  , dloss = \a y -> let z = a*y
                    in if z > 1 then 0 else y * (1 - z)
  }

--
smoothHingeLoss :: Loss 
smoothHingeLoss = Loss {
    loss = loss'
  , dloss = dloss' 
  }
  where 
    loss' a y 
      | z > 1       = 0
      | z < 0       = 0.5 - z
      | otherwise   = 0.5 * (1 - z)^2
      where z = a*y

    dloss' a y
      | z > 1       = 0
      | z < 0       = y
      | otherwise   = y * (1 - z)
      where z = a*y


