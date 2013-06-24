module Sass where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative ((<$>))
import Control.Monad
import Data.IORef
import System.Exit

spaces :: Parser ()
spaces = skipMany1 space

ignoreSpaces :: Parser ()
ignoreSpaces = skipMany space

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

identifier :: Parser String
identifier = do
             first <- letter
             rest  <- many (letter <|> digit <|> hyphen)
             return $ first:rest

selector :: Parser SassVal
selector = do first <- letter <|> selectorMeta
              rest  <- many (letter <|> digit <|> hyphen)
              let name = first : rest
              return $ Selector name

parseKeyword :: Parser SassVal
parseKeyword = do
                char '@'
                keyword <- identifier
                case keyword of
                    "import"    -> parseImport
                    "include"   -> parseInclude

parseVariable :: Parser SassVal
parseVariable = do
                char '$'
                name <- identifier
                char ':'
                ignoreSpaces
                value <- parseValue
                return $ Variable name value

parseValue :: Parser SassVal
-- TODO this should parse arrays, strings and literals.
parseValue = do
             value <- try parseString <|> parseArray <|> parseScalar
             char ';'
             return value

parseString :: Parser SassVal
parseString = do char '"' -- Read until we find this char
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseArray :: Parser SassVal
parseArray = do char '('
                x <- sepBy identifier comma
                char ')'
                return $ Array (map String x)

parseNumber :: Parser Integer
parseNumber = do digits <- many1 digit
                 return $ read digits

parseScalar :: Parser SassVal
parseScalar = do magnitude <- parseNumber
                 scale     <- many letter
                 -- return $ Scalar magnitude "identity"
                 return $ case scale of
                     ""   -> Scalar magnitude "identity"
                     "em" -> Scalar magnitude "em"
                     "px" -> Scalar magnitude "px"

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
                funcName <- identifier
                char '('
                args <- sepBy funcArgs comma
                char ')'
                return $ StmInclude funcName args

parseIdentifier :: Parser SassVal
parseIdentifier = do
                    id <- identifier
                    return $ String id

directiveAction :: Parser SassVal
directiveAction = do
                action <- try parseScalar <|> parseIdentifier
                return action

directive = do
                name <- identifier
                char ':'
                ignoreSpaces
                actions <- sepBy directiveAction spaces
                return $ Directive name actions


data SassVal = Directive { key :: String, rules :: [SassVal] }
             | Rule { selectors :: [SassVal], directives :: [SassVal] }
             | Selector String
             | Keyword String
             | StmImport SassVal
             | StmInclude { funcName :: String, args :: [SassVal] }
             | String String
             | Array [SassVal]
             | Scalar { magnitude :: Integer, unit :: String } -- Enum field?
             | Variable { name :: String, value :: SassVal }
instance Show SassVal where show = showVal

showVal :: SassVal -> String
showVal (Selector str)                         =  str
showVal (Directive {key = k, rules = r})       = k ++ ":" ++ show r
showVal (Rule {selectors = s, directives = d}) = show s ++ "{\n " ++ show d ++ "\n}"
showVal (StmImport path)                       = "Import => " ++ show path
showVal (StmInclude {funcName = f, args = a})  = "Include => " ++ f ++ "(" ++ show a ++ ")"
showVal (String str)                           = "\"" ++ str ++ "\""
showVal (Array arr)                            = "(" ++ show arr ++ ")"
showVal (Scalar {magnitude=m, unit=u})         = show m ++ u
showVal (Variable {name=n, value=v})           = "$" ++ n ++ " => " ++ show v


type Env = IORef [(String, IORef SassVal)]

parseExpr :: Parser SassVal
parseExpr = parseCSSRule
        <|> parseKeyword
        <|> parseVariable
        --

parseCSSRule :: Parser SassVal
parseCSSRule = do  selectors <- sepBy selector spaces
                   char '{'
                   directives <- endBy directive semicolon
                   char '}'
                   return $ Rule selectors directives

parseStatements :: Parser [SassVal]
parseStatements = endBy parseExpr semicolon

displayExpr :: SassVal -> IO ()
displayExpr expr = putStrLn $ show expr

readSassExpr :: String -> IO ()
readSassExpr input = case parse parseStatements "sass" input of
                                Left err  -> do
                                                putStrLn $ "No match: " ++ show err
                                                exitWith (ExitFailure 1)
                                Right val -> do
                                                sequence_ $ map displayExpr val



readAndEval :: [String] -> IO ()
readAndEval val@[_] = (readSassExpr $ val !! 0)
readAndEval other   = usage

sassMain :: [String] -> IO ()
sassMain args = case args !! 0 of
    "eval" -> readAndEval (drop 1 args)
    other  -> usage

usage :: IO ()
usage = putStrLn "           sass eval 'expression'"
