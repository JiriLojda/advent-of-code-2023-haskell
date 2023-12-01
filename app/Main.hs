module Main (main) where

import System.IO

import Day1.Solution qualified as Day1

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  print "What Day to try?"
  line <- getLine
  res <- callDay line
  putStrLn res

callDay :: String -> IO String
callDay str = case str of
  "1" -> printDay (fmap show Day1.task1) (fmap show Day1.task2)
  _ -> pure "Not a valid day"

printDay :: IO String -> IO String -> IO String
printDay task1 task2 = do
  t1 <- fmap ("task1: " ++) task1
  t2 <- fmap ("task2: " ++) task2
  pure (t1 ++ "\n" ++ t2)
