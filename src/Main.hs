module Main where

import qualified Identicon
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let string = args !! 0
  Identicon.run string
