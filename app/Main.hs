-- This module should contain the argument parsing and then call Run.run
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Asm
import           RIO
import qualified RIO.ByteString as BS

main :: IO ()
main = do
  asm <- tshow <$> BS.getContents
  BS.putStr $ Asm.assemble asm