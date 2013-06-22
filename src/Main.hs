import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args !! 0 of
        "sass" -> putStrLn "Sass compiler"
