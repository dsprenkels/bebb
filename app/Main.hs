-- This module should contain the argument parsing and then call Run.run
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  )
where

import qualified Syntax
import           RIO
import qualified RIO.Text                      as T
import qualified RIO.ByteString                as BS
import           Text.Megaparsec.Error          ( errorBundlePretty )

main :: IO ()
main = do
  src <- decodeUtf8Lenient <$> BS.getContents
  let result = Syntax.parse "<stdin>" src in
   BS.putStr $ T.encodeUtf8 $ T.pack $ case result of
    Right ast -> show ast
    Left  err -> errorBundlePretty err
