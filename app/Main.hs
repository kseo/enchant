module Main where

import Data.Foldable
import Language.Enchant
import System.Environment

spellCheck :: String -> IO ()
spellCheck word = withBroker (\broker ->
  withDict broker "en_US" (\dict -> do
    putStrLn "Did you mean?"
    suggestions <- dictSuggest dict word
    traverse_ (putStrLn . ("- " ++)) suggestions
                          )
                        )

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then putStrLn "Usage: enchant-examples-spell-check [word]"
     else spellCheck (head args)
