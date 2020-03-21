{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  ) where

import Emit (emit)
import Error
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text as T
import qualified Syntax

main :: IO ()
main = do
  src <- decodeUtf8Lenient <$> BS.getContents
  let filename = "<stdin>"
  let result = Syntax.parse filename src
  case result of
    Right ast ->
      case emit ast of
        Right code -> BS.putStr code
        Left err -> fmtBundle filename src err
    Left err -> fmtBundle filename src err
