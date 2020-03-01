-- This module should contain the argument parsing and then call Run.run
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  ) where

import qualified Driver
import RIO
import qualified RIO.ByteString as BS

main :: IO ()
main = do
  asm <- tshow <$> BS.getContents
  BS.putStr $ Driver.assemble asm
