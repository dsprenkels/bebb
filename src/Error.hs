{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Error where

import           RIO
import           RIO.List                       ( intercalate )
import           Text.Megaparsec                ( PosState )
import qualified Text.Megaparsec.Error         as M


data Error = ParseError (M.ParseError Text Void)
           | ResolveError () deriving (Show)

data ErrorBundle = ErrorBundle { bundleErrors::NonEmpty Error
                               , bundlePosState :: PosState Text
                               } deriving (Show)

fromParseError :: M.ParseError Text Void -> Error
fromParseError = ParseError

fromParseErrorBundle :: M.ParseErrorBundle Text Void -> ErrorBundle
fromParseErrorBundle M.ParseErrorBundle { M.bundleErrors, M.bundlePosState } =
  ErrorBundle { bundleErrors = fromParseError <$> bundleErrors, bundlePosState }

errorText :: Error -> String
errorText (ParseError err) = M.parseErrorTextPretty err

errorSpan :: Error -> (Int, Maybe Int)
errorSpan (ParseError   err) = (M.errorOffset err, Nothing)
errorSpan (ResolveError err) = error "unimplemented"

errorBundlePretty :: ErrorBundle -> String
errorBundlePretty ErrorBundle { bundleErrors, bundlePosState } =
  intercalate "\n\n" (errorText <$> toList bundleErrors) <> "\n"
