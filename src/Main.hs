module Main where

import qualified Identicon
import System.Environment (getArgs)

main :: IO ()
main = do
  (string:_) <- getArgs
  Identicon.run string
