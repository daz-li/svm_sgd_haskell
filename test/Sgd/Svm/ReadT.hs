module Sgd.Svm.ReadT (main) where 

import qualified Data.ByteString.Lazy.Char8 as C
import Test.HUnit
import Sgd.Svm.Read (readPair, readSample, Sample, dimSample)

readPairT = TestCase $ do
  assertEqual
    "Missing left part"
    Nothing
    (readPair $ C.pack ":0.1" :: Maybe (Int, Double)) 

  assertEqual
    "Missing mid part"
    Nothing
    (readPair $ C.pack "500.1" :: Maybe (Int, Double)) 

  assertEqual
    "Missing right part"
    Nothing
    (readPair $ C.pack "1:" :: Maybe (Int, Double)) 

  assertEqual
    "Wrong left part"
    Nothing
    (readPair $ C.pack "a1:0.1" :: Maybe (Int, Double)) 

  assertEqual
    "Wrong left part"
    Nothing
    (readPair $ C.pack "2a:0.2" :: Maybe (Int, Double)) 

  assertEqual
    "Wrong spaces in between"
    Nothing
    (readPair $ C.pack "1a :0.1" :: Maybe (Int, Double)) 

  assertEqual
    "Wrong spaces in between"
    Nothing
    (readPair $ C.pack "2a: 0.2" :: Maybe (Int, Double)) 

  assertEqual
    "Wrong right part"
    Nothing
    (readPair $ C.pack "1:0.1a" :: Maybe (Int, Double)) 

  assertEqual
    "Wrong right part"
    Nothing
    (readPair $ C.pack "2:a0.2" :: Maybe (Int, Double)) 

  assertEqual
    "Should work for standard input"
    (Just (1, 0.1))
    (readPair $ C.pack "1:0.1" :: Maybe (Int, Double)) 

  -- assertEqual
  -- "Should work for standard input, leading and trailing spaces"
  --  (Just (1, 0.1))
  --  (readPair $ C.pack "  1:0.1  " :: Maybe (Int, Double)) 

readSampleT = TestCase $ do
  assertEqual
    "Wrong format of *response*"
    Nothing
    (readSample $ C.pack "a1 1:0.1  2:0.2" :: Maybe (Sample Int)) 

  assertEqual
    "Wrong format of *spaces"
    Nothing
    (readSample $ C.pack "1 1 :0.1  2:0.2" :: Maybe (Sample Int)) 

  assertEqual
    "fine spaces at two ends"
    (Just ([(1, 0.1), (2, 0.2)], (-1.0)))
    (readSample $ C.pack "   -1 1:0.1  2:0.2  " :: Maybe (Sample Int)) 

  assertEqual
    "In theory, wrong response format; practically, let it go"
    (Just ([(1, 0.1), (2, 0.2)], (1.0)))
    (readSample $ C.pack "1a 1:0.1  2:0.2" :: Maybe (Sample Int)) 

  assertEqual
    "Should work for standard input"
    (Just ([(1, 0.1), (2, 0.2)], (-1.0)))
    (readSample $ C.pack "-1 1:0.1  2:0.2" :: Maybe (Sample Int)) 

dimSampleT = TestCase $ do
  assertEqual
    "dimSample should work"
    199
    (dimSample [ ([(1,1.0), (5, 5.0), (100, 100)], 1)
                  ,([(5,5.0), (7, 7.0), (99, 99)], -1)
                  ,([(3,3.0), (6, 6.0), (199, 199)], 1)])
  
main :: IO Counts 
main =  
    runTestTT $ TestList [readPairT, readSampleT, dimSampleT]
