{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where

import Emit (emit)
import Error
import Options.Applicative
import RIO
import qualified RIO.ByteString as BS
import qualified Syntax

runAssembler :: IO ByteString -> IO ()
runAssembler input = do
  src <- decodeUtf8Lenient <$> input
  let filename = "<stdin>"
  let result = Syntax.parse filename src
  let printError = fmtBundle filename src
  case result of
    Right ast ->
      case emit ast of
        Right code -> BS.putStr code
        Left err -> printError err
    Left err -> printError err

inputFile :: Parser (IO ByteString)
inputFile = readInputFile <$> argument str (metavar "FILE" <> showDefault <> value "-")
  where
    readInputFile "-" = BS.getContents
    readInputFile path = BS.readFile path

main :: IO ()
main = runAssembler =<< execParser opts
  where
    opts = info (inputFile <**> helper)
      ( fullDesc
     <> progDesc "Read assembly source from FILE"
     <> header "bebb-as - assembler for Daan & Marrit's BreadBoard CPU" )
