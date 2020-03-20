{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}

module Error where

import           RIO
import           RIO.List                       ( sortOn
                                                , replicate
                                                )
import           Data.List                      ( dropWhileEnd )
import           Data.Char                      ( isSpace )
import qualified RIO.Text                      as T
import           Text.Printf                    ( printf )
import           RIO.List.Partial               ( (!!) )
import qualified RIO.HashMap                   as HM
import qualified Text.Megaparsec.Error         as M
import qualified System.Console.ANSI           as ANSI
import           System.IO                      ( putStrLn
                                                , putStr
                                                )
import           AST                            ( Span )


data Error = ParseError (M.ParseError Text Void)
           | CustomError String Span
           | InternalCompilerError String Span deriving (Show)

type ErrorBundle = NonEmpty Error

type RealPos = (Int, Int)
type RealSpan = (RealPos, RealPos)

fromParseError :: M.ParseError Text Void -> Error
fromParseError = ParseError

fromParseErrorBundle :: M.ParseErrorBundle Text Void -> ErrorBundle
fromParseErrorBundle M.ParseErrorBundle { M.bundleErrors } =
  fromParseError <$> bundleErrors

getErrorTypeDesc :: Error -> String
getErrorTypeDesc (ParseError _   ) = "parse error"
getErrorTypeDesc (CustomError _ _) = "error"

getErrorText :: Error -> String
getErrorText (ParseError err) = trim (M.parseErrorTextPretty err)
  where trim = dropWhileEnd isSpace . dropWhile isSpace
getErrorText (CustomError msg _) = msg

getSpan :: Error -> Span
getSpan (ParseError (M.TrivialError lo _ _)) = (lo, lo)
getSpan (ParseError (M.FancyError lo _    )) = (lo, lo)
getSpan (CustomError _ sp                  ) = sp

resolveAllOffsets :: Text -> [Int] -> [RealPos]
resolveAllOffsets srcText allOffsets =
  let sortedOffsets = sortOn snd (zip [0 :: Int ..] allOffsets)
  in  map snd $ sortOn
        fst
        (resolveAllOffsets' (T.unpack srcText) sortedOffsets (0, 0, 0))
 where
  resolveAllOffsets' [] _  _ = error "unreachable: offset past end of source"
  resolveAllOffsets' _  [] _ = []
  resolveAllOffsets' (c : src) offsets@((idx, offset) : rest) counts@(offsetCount, lineCount, colCount)
    | offset == offsetCount
    = (idx, (lineCount, colCount)) : resolveAllOffsets' src rest counts
    | c == '\n'
    = resolveAllOffsets' src offsets (offsetCount + 1, lineCount + 1, 0)
    | otherwise
    = resolveAllOffsets' src offsets (offsetCount + 1, lineCount, colCount + 1)

fmtSpan :: [String] -> RealSpan -> IO ()
fmtSpan srcLines (lo@(loLine, loCol), hi@(hiLine, hiCol)) = do
  ansiBlueBold
  putStrLn (replicate (length lineNo) ' ' ++ " |")
  ansiBlueBold
  putStr (lineNo ++ " | ")
  ansiPurpleBold
  putStrLn (srcLines !! loLine)
  ansiBlueBold
  putStr (replicate (length lineNo) ' ' ++ " | ")
  ansiPurpleBold
  if
    | lo == hi -> putStrLn (replicate loCol ' ' ++ "^")
    | loLine == hiLine -> do
      putStr (replicate loCol ' ')
      putStrLn (replicate (hiCol - loCol) '^')
    | otherwise -> error "unimplemented"
  ansiReset
  where lineNo = show $ loLine + 1

fmtError :: String -> [String] -> (Span -> RealSpan) -> Error -> IO ()
fmtError filename srcLines getRealSpan err = do
  ansiRedBold
  putStrLn (getErrorTypeDesc err ++ ":")
  ansiReset
  ansiBold
  putStrLn $ leftPad $ mangleErrorText (getErrorText err)
  ansiReset
  putStrLn (printf " --> %s:%d:%d" filename lineNo col)
  fmtSpan srcLines realSpan
 where
  lineNo                    = line + 1
  realSpan@((line, col), _) = getRealSpan $ getSpan err
  mangleErrorText []         = []
  mangleErrorText ('\n' : s) = "\n" ++ leftPad (mangleErrorText s)
  mangleErrorText (c    : s) = c : mangleErrorText s
  leftPad s = "    " ++ s

fmtBundle :: String -> Text -> ErrorBundle -> IO ()
fmtBundle filename src errors = sequence_
  (fmtError filename srcLines realSpan <$> toList errors)
 where
  srcLines = T.unpack <$> T.lines src
  realSpan (lo, hi) = (resolveOffset lo, resolveOffset hi)
  resolveOffset offset = HM.lookupDefault notFound offset offsetTable
  notFound    = error "unreachable: unresolved span in error"
  offsetTable = HM.fromList $ zip allOffsets (resolveAllOffsets src allOffsets)
  allOffsets  = concatMap getOffsetsFromError errors
  getOffsetsFromError err = let (lo, hi) = getSpan err in lo : [hi]

ansiReset :: IO ()
ansiReset = ANSI.setSGR [ANSI.Reset]

ansiBold :: IO ()
ansiBold = ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]

ansiColorBold :: ANSI.Color -> IO ()
ansiColorBold color = ANSI.setSGR
  [ ANSI.SetColor ANSI.Foreground ANSI.Vivid color
  , ANSI.SetConsoleIntensity ANSI.BoldIntensity
  ]

ansiRedBold :: IO ()
ansiRedBold = ansiColorBold ANSI.Red

ansiBlueBold :: IO ()
ansiBlueBold = ansiColorBold ANSI.Blue

ansiPurpleBold :: IO ()
ansiPurpleBold = ansiColorBold ANSI.Magenta

