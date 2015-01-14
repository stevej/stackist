module Main where

import Stackist.Interpreter1 (redex)
import Stackist.Parser
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Error


getFilename [filename] = filename
getFilename _ = error "needs one argument. no filename given. ex: cabal run 1.joy"

main :: IO ()
main = do
  filename <- getArgs
  program <- readFile $ getFilename filename
  putStrLn $ case (parseJoy program) of
    Left x -> "invalid program: " ++ show x
    Right x -> show (redex [] [x])
