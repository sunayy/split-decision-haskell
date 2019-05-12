module Main where

import Pin(isSplit)
import System.Environment(getArgs)
import Text.Regex.Posix

main :: IO ()
main = do
  args <- getArgs
  print $ isSplit args
