module Day1.Solution (task1, task2) where

import Debug.Trace (traceShowId)
import Data.Char (isDigit)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP.Char
import Utils (Parser)
import Utils qualified

task1 :: IO String
task1 = do
  input <- Utils.loadFile "src/Day1/Data.txt"
  case sum <$> mapM parseLine (lines input) of
    Just res -> pure $ show res
    Nothing -> fail "Some lines contain no digits."

parseLine :: String -> Maybe Int
parseLine str = do
  firstNum <- Utils.head numChars
  lastNum <- Utils.last numChars
  pure $ read (firstNum : [lastNum])
 where
  numChars = filter isDigit str

task2 :: IO String
task2 = do
  input <- Utils.loadAndParseFile "src/Day1/Data.txt" task2Parser
  case sum <$> mapM parseLine (lines input) of
    Just res -> pure $ show res
    Nothing -> fail "Some lines contain no digits. (This means the parser is incorrect.)"

task2Parser :: Parser [Char]
task2Parser =
  MP.some
    $ MP.choice
      [ MP.Char.digitChar
      , MP.Char.newline
      , parseNum "zero" '0'
      , parseNum "one" '1'
      , parseNum "two" '2'
      , parseNum "three" '3'
      , parseNum "four" '4'
      , parseNum "five" '5'
      , parseNum "six" '6'
      , parseNum "seven" '7'
      , parseNum "eight" '8'
      , parseNum "nine" '9'
      , MP.Char.printChar
      ]
 where
  parseNum :: String -> Char -> Parser Char
  parseNum str res = do
    _ <- MP.lookAhead $ MP.Char.string str
    MP.Char.char (head str)
    pure res
