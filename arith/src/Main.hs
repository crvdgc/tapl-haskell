module Main where

import Arith.Evaluator
import Arith.Parser
import System.Environment
import Text.Parsec

main :: IO ()
main =
  do args <- getArgs
     case args of
       [sourceFile] -> 
         do parseTree <- fmap (parse parseTerm "arith") $ readFile sourceFile
            putStrLn $ show parseTree
            case parseTree of
              -- eval for small-step, eval' for big-step
              Right expr -> putStrLn $ "=> " ++ (show . eval') expr
              Left err   -> putStrLn $ show err
       _ -> putStrLn "Usage: arith <sourceFile>"
