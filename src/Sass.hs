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

colon :: Parser Char
colon = oneOf ":"

semicolon :: Parser Char
semicolon = oneOf ";"

selector :: Parser SassVal
selector = do first <- letter <|> selectorMeta
              rest  <- many (letter <|> digit <|> hyphen)
              let name = first : rest
              return $ Selector name

directive :: Parser SassVal
-- directive = do
--                 name <- many (noneOf " :")
--                 -- spaces
--                 char ':'
--                 -- spaces
--                 actns <- endBy (noneOf ";") semicolon
--                 return $ Directive name actns

directive = do
                name <- many letter
                return $ FakeDirective name

data SassVal = Directive { key :: String, rules :: String }
             | Rule { selectors :: [SassVal], directives :: [SassVal] }
             | Selector String
             | FakeDirective { content :: String }
instance Show SassVal where show = showVal

showVal :: SassVal -> String
showVal (FakeDirective {content = c})          = "FakeDirective: " ++ c
showVal (Selector str)                         = "Selector: " ++ str
showVal (Directive {key = k, rules = r})       = "Directive: " ++ k ++ " => " ++ r
showVal (Rule {selectors = s, directives = d}) = "Rule: " ++ show s ++ " => " ++ show d

type Env = IORef [(String, IORef SassVal)]

parseExpr :: Parser SassVal
parseExpr = parseCSSRule
        -- <|> parseFuncDeclaration
        --

parseCSSRule :: Parser SassVal
parseCSSRule = do  selectors <- sepBy selector spaces
                   char '{'
                   directives <- sepBy directive semicolon
                   char '}'
                   return $ Rule selectors directives

readSassExpr :: String -> String
readSassExpr input = case parse parseExpr "sass" input of
    Left err -> "No match: " ++ show err
    Right val -> "Matched: " ++ show val

readAndEval :: [String] -> IO ()
readAndEval val@[_] = putStrLn $ (readSassExpr $ val !! 0)
readAndEval other   = usage

sassMain :: [String] -> IO ()
sassMain args = case args !! 0 of
    "eval" -> readAndEval (drop 1 args)
    other  -> usage

usage :: IO ()
usage = putStrLn "           sass eval 'expression'"
