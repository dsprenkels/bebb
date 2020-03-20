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
import Emit (emit)

main :: IO ()
main = do
  src <- decodeUtf8Lenient <$> BS.getContents
  let filename = "<stdin>"
  let result   = Syntax.parse filename src
  case result of
    Right ast -> BS.putStr $ T.encodeUtf8 $ T.pack $ show $ emit ast
    Left  err -> fmtBundle filename src err
