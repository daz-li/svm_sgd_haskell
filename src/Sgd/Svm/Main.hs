{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
 
-- module Sgd.Svm.Main where

import qualified Data.ByteString.Lazy.Char8 as C
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import qualified Sgd.Svm.Read as R
import qualified Sgd.Svm.Train as T
import qualified Sgd.Svm.LossFun as L

data SgdOpts = SgdOpts
    { trainFile :: String 
    , testFile :: String 
    , lambda :: Double
    , epochs :: Int
    , dontnormalize :: Bool
    --, directory :: FilePath
    } deriving (Data, Typeable, Show, Eq)

sgdOpts :: SgdOpts
sgdOpts = SgdOpts
    { trainFile = def &= argPos 0 &= typ "TRAINING-FILE"  
    , testFile = def  &=typ "TESTING-FILE" &= help "Testing data"
    , lambda = 1e-5   &= help "Regularization parameter, default 1e-5"
    , epochs = 5      &= help "Number of training epochs, default 5"
    , dontnormalize = False &= help "Not normalize the L2 norm of patterns, default normalize"
    }
 

getOpts :: IO SgdOpts
getOpts = cmdArgs $ sgdOpts
--    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

--TODO, also describe trainfile, testfile format!!!

_PROGRAM_NAME = "svmsgd"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "svm + sgd + haskell"
_COPYRIGHT = "(C) Dazhuo Li 2013"
 
main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    optionHandler opts
 
-- Before directly calling your main program, you should warn your user about incorrect arguments, if any.
optionHandler :: SgdOpts -> IO ()
optionHandler opts@SgdOpts{..}  = do
    -- Take the opportunity here to weed out ugly, malformed, or invalid arguments.
    -- TODO also check trianFile is file
    when (null trainFile) $ putStrLn "--trainFile is blank!" >> exitWith (ExitFailure 1)
    when (lambda <= 0 || lambda > 10000) $ putStrLn "--lambda must be in (0, 1e4]" >> exitWith (ExitFailure 1)
    when (epochs <= 0 || epochs > 1000000) $ putStrLn "--epochs must be in (0, 1e6]" >> exitWith (ExitFailure 1)

    -- When you're done, pass the (corrected, or not) options to your actual program.
    --putStrLn $ "hello train file , " ++ trainFile ++ "!"
    --putStrLn $ "hello lambda, " ++ (show lambda) ++ " years old."
    exec opts

 
exec opts@SgdOpts{..} = do 
    contents <- C.readFile trainFile
    --putStrLn "hello"
    let dat = case R.read contents :: Maybe [R.Sample Int] of
                Nothing -> error "Wrong input format"
                Just x  -> if dontnormalize 
                            then x
                            else R.normalizeEachSample x
    --print dat
    --putStrLn "hello2"
    let model = T.train L.logLoss dat lambda epochs
    --let predLoss = T.testMany (L.loss L.logLoss ) dat lambda model
    print model
    --print predLoss
