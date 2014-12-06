module Sgd.Num.Vector where

-- http://stackoverflow.com/questions/17892065/mutable-random-access-array-vector-with-high-performance-in-haskell
-- http://haskell.1045720.n5.nabble.com/fishing-for-ST-mutable-Vector-examples-td4333461.html

-- import qualified Data.IntMap as IntMap
import qualified Data.Vector.Unboxed as VU

-- type SparseVector = IntMap
type SparseVector = [(Int, Double)]
type FullVector = VU.Vector (Double)
-- type Sample = (SparseVector, Double)          -- (input, response)

dotFS :: FullVector -> SparseVector -> Double
dotFS u v = foldl go 0 v
  where 
    go :: Double -> (Int, Double) -> Double
    go acc (ind, val) = acc + u VU.! ind * val

dotFS' :: FullVector -> SparseVector -> Double
--dotFS' u v = sum . zipWith (*) vals . VU.toList . map (\x -> u VU.! x) $ inds
dotFS' u v = sum . zipWith (*) vals . slice u $ inds
  where 
    (inds, vals) = unzip v
    slice :: FullVector -> [Int] -> [Double]
    slice u v = map (\x -> u VU.! x) $ v


addFS :: FullVector -> SparseVector -> FullVector
addFS = VU.accum (+) 

dot :: FullVector -> FullVector -> Double
dot u v = VU.sum $ VU.zipWith (*) u v

scale :: FullVector -> Double -> FullVector
scale u d = VU.map (*d) u


dim :: SparseVector -> Int
dim = foldl1 max . fst . unzip 


rep :: Int -> Double -> FullVector
rep l x = VU.replicate l x

mul :: SparseVector -> Double -> SparseVector
mul x d = map (\(a1, a2) -> (a1, a2*d)) x

normalizeL2 :: SparseVector -> SparseVector
normalizeL2 v = mul v $ 1 / (l2norm v)

l2norm :: SparseVector -> Double
l2norm v = sqrt . sum . zipWith (\x y -> snd x * snd y) v $ v 
