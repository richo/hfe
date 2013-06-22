import System.IO
import System.Environment

usage :: IO ()
usage = do
    putStrLn "Usage: hfe <action> [arguments], for action:"
    putStrLn "           sass filename"

handle :: [[Char]] -> IO ()
handle args = case args !! 0 of
                "sass" -> putStrLn "Sass compiler"

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
    then
        usage
    else
        handle args
