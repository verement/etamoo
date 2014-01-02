
module Main where

import Network
import System.Posix

main :: IO ()
main = withSocketsDo $ do
  installHandler sigPIPE Ignore Nothing
  undefined
