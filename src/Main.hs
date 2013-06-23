module Main where
import System.IO
import System.Environment

import qualified Sass

usage :: IO ()
usage = do
    putStrLn "Usage: hfe <action> [arguments], for action:"
    Sass.usage

handle :: [[Char]] -> IO ()
handle args = case args !! 0 of
                "sass" -> Sass.sassMain (drop 1 args)
                other  -> usage

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
    then
        usage
    else
        handle args
