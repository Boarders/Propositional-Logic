module BasicParser where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser{parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap f (Parser p) = Parser (\st -> map (\(a,st) -> (f a, st)) $ p st
                             )

instance Applicative Parser where
  pure a = Parser (\st -> [(a, st)])
  (<*>) (Parser functionParser) (Parser argParser) = Parser
          (\st -> [ (f a, st'') |  (f, st') <- functionParser st, (a, st'') <- argParser st'])



instance Alternative Parser where
  empty = Parser (\st -> [])
  (<|>) (Parser leftParser) (Parser rightParser) = Parser
          (\st -> case leftParser st of
            (token:tokens) -> (token:tokens)
            [] -> rightParser st
          )
  some = some'
  many = many'

instance Monad Parser where
  return = pure
  (>>=) (Parser firstParser) cont = Parser
          (\st -> [ (b, st'') | (a, st') <- firstParser st, (b, st'') <- parse (cont a) st'])

instance MonadPlus Parser where
  mzero = empty
  mplus (Parser left) (Parser right) = Parser $ \st -> left st ++ right st

runParser :: Parser a -> String -> a
runParser p st = case parse p st of
                   [(out, "")]     -> out
                   [(partial, st)] -> error "unconsumed string"
                   _               -> error "Parser failed"

sat :: (Char -> Bool) -> Parser Char
sat p = Parser (\st -> case st of
                 [] -> []
                 (x:xs) -> if p x then [(x,xs)] else []
               )

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl inp op a = (chainl1 inp op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 inp op = do p1 <- inp
                    rest p1
  where rest a = (do op1 <- op
                     p  <- inp
                     rest (op1 a p))
                     <|> return a
                    
                       
                       

digit, lower, upper, letter, alphanum :: Parser Char
digit = sat (isDigit)
lower = sat (isLower)
upper = sat (isUpper)
letter = sat (isAlpha)
alphanum = sat (isAlphaNum)

char :: Char -> Parser Char
char x = sat (==x)

string :: String ->Parser String
string [] = return []
string st@(x:xs) = (:) <$> (char x ) <*> (string xs)

many' :: Parser a -> Parser [a]
many' p = some' p <|> (return [])

some' :: Parser a -> Parser [a]
some' p = (:) <$> p <*> (many p)

oneOf :: [Char] -> Parser Char
oneOf st = sat (\c -> c `elem` st) 

spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = p <* spaces

ignore :: Parser a -> Parser ()
ignore = ((const ()) <$> )

posInt :: Parser Int
posInt = read <$> (some digit)

integer :: Parser Int
integer = posInt <|> (sign <$> (char '-') <*> posInt)
  where sign = ((.).(.)) negate (flip const)

parens :: Parser a -> Parser a
parens p = token (char '(') *> p <* token (char ')')













