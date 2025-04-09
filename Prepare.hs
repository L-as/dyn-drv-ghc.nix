{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Procex.Execve
import System.Directory
import System.IO
import System.Posix.Env.ByteString
import System.Posix.Types
import Text.Parsec
import Text.Parsec.Char

{- This script prepares the environment then launches GHC.
   Specifically, we prepare the interface files for all the imports,
   since GHC expects a specific directory structure for them.
   For import A.B.C, we will link A/B/C.hi to /nix/store/something.hi.
   When compiling, we just set `-i.`.
-}

placeholderCaOut = "/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9"

splitOn' :: (Eq a) => [a] -> a -> [a] -> [[a]]
splitOn' acc splitter [] = []
splitOn' acc splitter (x : xs)
  | splitter == x = reverse acc : splitOn' [] splitter xs
  | otherwise = splitOn' (x : acc) splitter xs

splitOn = splitOn' []

main :: IO ()
main =
  do
    putStrLn "hello"
    Just input <- getEnv "INPUT"
    Just cc <- getEnv "CC"
    Just spec <- getEnv "SPEC"
    Just o <- getEnv "GHC_OUTPUT_OPTION"
    prepareLinks (splitOn (fromIntegral $ fromEnum '\n') $ B.unpack $ B.fromStrict spec)
    exec
      (B.fromStrict cc)
      ["ghc", "-c", "-fsplit-sections", "-fPIC", "-O1", B.fromStrict o, B.fromStrict placeholderCaOut]
      Nothing
      [Just (Fd 0), Just (Fd 1), Just (Fd 2)]
