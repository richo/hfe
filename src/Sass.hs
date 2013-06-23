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

comma :: Parser Char
comma = oneOf ","

selector :: Parser SassVal
selector = do first <- letter <|> selectorMeta
              rest  <- many (letter <|> digit <|> hyphen)
              let name = first : rest
              return $ Selector name

parseKeyword :: Parser SassVal
parseKeyword = do
                char '@'
                keyword <- many letter
                case keyword of
                    "import"    -> parseImport
                    "include"   -> parseInclude

directive :: Parser SassVal
-- directive = do
--                 name <- many (noneOf " :")
--                 -- spaces
--                 char ':'
--                 -- spaces
--                 actns <- endBy (noneOf ";") semicolon
--                 return $ Directive name actns

parseString :: Parser SassVal
parseString = do char '"' -- Read until we find this char
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseImport :: Parser SassVal
parseImport = do -- Oh lawdy FIXME
                spaces
                path <- parseString
                return $ StmImport path

funcArgs :: Parser SassVal
funcArgs = parseString

parseInclude :: Parser SassVal
parseInclude = do -- Oh lawdy FIXME
                spaces
                funcName <- many letter
                char '('
                args <- sepBy funcArgs comma
                char ')'
                return $ StmInclude funcName args

directiveAction :: Parser String
directiveAction = do
                action <- many letter
                return action

directive = do
                name <- many letter
                char ':'
                actions <- sepBy directiveAction spaces
                return $ Directive name actions

data SassVal = Directive { key :: String, rules :: [String] }
             | Rule { selectors :: [SassVal], directives :: [SassVal] }
             | Selector String
             | Keyword String
             | StmImport SassVal
             | StmInclude { funcName :: String, args :: [SassVal] }
             | String String
instance Show SassVal where show = showVal

showVal :: SassVal -> String
showVal (Selector str)                         =  str
showVal (Directive {key = k, rules = r})       = k ++ ":" ++ show r
showVal (Rule {selectors = s, directives = d}) = show s ++ "{\n " ++ show d ++ "\n}"
showVal (StmImport path)                       = "Import => " ++ show path
showVal (StmInclude {funcName = f, args = a})  = "Include => " ++ f ++ "(" ++ show a ++ ")"
showVal (String str)                           = "\"" ++ str ++ "\""

type Env = IORef [(String, IORef SassVal)]

parseExpr :: Parser SassVal
parseExpr = parseCSSRule
        <|> parseKeyword
        --

parseCSSRule :: Parser SassVal
parseCSSRule = do  selectors <- sepBy selector spaces
                   char '{'
                   directives <- endBy directive semicolon
                   char '}'
                   return $ Rule selectors directives

readSassExpr :: String -> String
readSassExpr input = case parse parseExpr "sass" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

readAndEval :: [String] -> IO ()
readAndEval val@[_] = putStrLn $ (readSassExpr $ val !! 0)
readAndEval other   = usage

sassMain :: [String] -> IO ()
sassMain args = case args !! 0 of
    "eval" -> readAndEval (drop 1 args)
    other  -> usage

usage :: IO ()
usage = putStrLn "           sass eval 'expression'"
