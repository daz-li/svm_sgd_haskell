{-# LANGUAGE TypeSynonymInstances #-}

module Sgd.Svm.Read where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Lex.Lazy.Double (readDouble)
import Control.Applicative
import qualified Sgd.Num.Vector as V

--type HashTable k v = H.BasicHashTable k v
type SparseVector a = [(a, Double)]
type Response = Double
type Sample a = (SparseVector a, Response)




-- | On error handling
-- Error handling is done by Maybe. There are pros and cons
-- of any approach: Maybe, Either, Excpetion. e.g. facing 
-- error, Maybe returns Nothing, which is un-informative about 
-- which part of which line has the wrong format. More ref:
-- http://book.realworldhaskell.org/read/error-handling.html
-- http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/

class Reads a where
  read :: C.ByteString -> Maybe [Sample a]
  read = sequence . map readSample . C.lines 

  readSample :: C.ByteString -> Maybe (Sample a)
  readSample str = fmap ((,)) feas <*> response
    where 
      tokens = C.words str
      feas = sequence . map readPair . tail $ tokens
      response = fmap fst . readDouble . head $ tokens

  readPair :: C.ByteString -> Maybe (a, Double)
  readPair str = let xs = C.split ':' str
                    in if length xs /= 2 
                        then Nothing
                        else let (x:y:_) = xs
                              in fmap (,) (readId x) <*> readDouble' y
  
  readId :: C.ByteString -> Maybe a



-- | On TypeSynonymInstances
-- http://stackoverflow.com/a/2125769/1311956
instance Reads Int where
  readId = readInt'
  
instance Reads C.ByteString where
  readId = Just . id


--transform :: [Sample C.ByteString] -> [Sample Int]

-- hashtable, pure vs unpure
-- ~ 1k-10k keys, ~ 10 M lookups
--indexing :: [C.ByteString] -> HashTable C.ByteString Int
--indexing = foldl go (H.new, 1) 
--  where 
--  TODO
  

readInt' :: C.ByteString -> Maybe Int
readInt' str = case C.readInt str of 
                Nothing -> Nothing
                Just (x, y) -> if C.null y then Just x else Nothing

                -- On implementation:
                -- Note the following pattern matching is illegal:
                --   Just (x, C.Empty) -> Just x 
                -- or 
                --   Just (x, C.empty) -> Just x 
                -- The former, Bytestring does not export its constructor.
                -- The latter, pattern matching does not accept normal function.
                --
                -- Alternative (Using {-# LANGUAGE ViewPatterns #-}):
                -- case C.readInt str of 
                -- Just (x, C.null -> True) -> Just x
                -- _                 -> Nothing
                -- 
            

readDouble' :: C.ByteString -> Maybe Double
readDouble' str = case readDouble str of 
                    Nothing -> Nothing
                    Just (x, y) -> if C.null y then Just x else Nothing

dimSample :: [Sample Int] -> Int
dimSample = foldl (\x y -> max x . V.dim $ y ) 0 . fst . unzip 

normalizeEachSample = map (\(x, y) -> (V.normalizeL2 x, y))




