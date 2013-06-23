module Sass where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative ((<$>))
import Control.Monad
import Data.IORef

spaces :: Parser ()
spaces = skipMany1 space

selectorMeta :: Parser Char
selectorMeta = oneOf ".#"

hyphen :: Parser Char
hyphen = oneOf "-"

selector :: Parser SassVal
selector = do first <- letter <|> selectorMeta
              rest  <- many (letter <|> digit <|> hyphen)
              let name = first : rest
              return $ Selector name

directive :: Parser SassVal
directive = do
                name <- many letter
                spaces
                char ':'
                actns <- sepBy (many letter) spaces
                char ';'
                return $ Directive name actns

-- -- | Parse stdin and return either failure or the program.
-- loadStdin :: IO (Either String Stm)
-- loadStdin = parseString "Failed to parse stdin; " <$> getContents

-- sassDirective :: Parser [Char]

-- type AcceptsBlock = Either SassBlock

-- parseSassFunction :: Parser SassVal
-- parseSassFunction = do char '@'
--                        exprName <- many char
--                        char '('
--                        args <- funcParams
--                        char ')'
--                        block <-

data SassVal = Directive { key :: String, rules :: [String] }
             | Rule { selectors :: [SassVal], directives :: [SassVal] }
             | Selector String
             -- | Css {selectors :: [string], rules :: [Rule]}
-- data Rule { key :: String,
-- data SassVal = Rules [SassVal]

type Env = IORef [(String, IORef SassVal)]

parseExpr :: Parser SassVal
parseExpr = parseCSSRule
        -- <|> parseFuncDeclaration
        --

parseCSSRule :: Parser SassVal
parseCSSRule = do  selectors <- sepBy selector spaces
                   char '{'
                   directives <- many directive
                   char '}'
                   return $ Rule selectors directives

readSassExpr :: String -> String
readSassExpr input = case parse parseExpr "sass" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found val"

readAndEval :: [String] -> IO ()
readAndEval val@[_] = putStrLn $ (readSassExpr $ val !! 0)
readAndEval other   = usage

sassMain :: [String] -> IO ()
sassMain args = case args !! 0 of
    "eval" -> readAndEval (drop 1 args)
    other  -> usage

usage :: IO ()
usage = putStrLn "           sass eval 'expression'"
