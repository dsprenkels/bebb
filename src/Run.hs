{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import           RIO
import qualified RIO.ByteString as BS

run :: IO ()
run = BS.putStr "We're inside the application!\n"
