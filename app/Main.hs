module Main where

import Pin(getRemainsPins)

main :: IO ()
main = do
  let pins = getRemainsPins [1, 2, 3, 5] [False, False, False, False, False, False, False, False, False, False]
  print pins