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
  let filename = "<stdin>"
      result   = Syntax.parse filename src in
    case result of
    Right ast -> BS.putStr $ T.encodeUtf8 $ T.pack $ show ast
    Left  err -> fmtBundle filename src err
