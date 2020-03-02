{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  )
where

import qualified Syntax
import           RIO
import qualified RIO.Text                      as T
import qualified RIO.ByteString                as BS
import           Error

main :: IO ()
main = do
  src <- decodeUtf8Lenient <$> BS.getContents
  let result = Syntax.parse "<stdin>" src in
   BS.putStr $ T.encodeUtf8 $ T.pack $ case result of
    Right ast -> show ast
    Left  err -> errorBundlePretty err
