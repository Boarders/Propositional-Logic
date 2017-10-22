module PropParser where

import Control.Applicative
import Control.Monad
import Data.Char
import BasicParser as BP
import PropData



{-

grammar to parse is something like: 

boolean = Bool
Bool  = "False" | "True"
Variable = Var String
expr0 = expr1 {Iff expr1}
expr1   = expr2 { Impl expr2 }
expr2   = expr3 { Disj expr3 }
expr3   = expr4 { Conj expr3 }
expr4 = expr5 {Not expr5}
expr5 "(" expr0 ")" | Bool | Variable 
                                  
-}

bool' :: Parser Bool
bool' = read <$> (BP.string "False" <|> BP.string "True")

bool :: Parser Prop
bool = BP.spaces *> (PropData.Const <$> bool') <* BP.spaces

alphabet :: [String]
alphabet = fmap return ['A'..'Z']

var' :: Parser String
var' = foldr (\lt acc -> acc <|> BP.string lt) mzero alphabet

var :: Parser Prop
var = BP.spaces *> (Var <$> var') <* BP.spaces

parseOp :: String -> a -> Parser a
parseOp st f = (flip const) <$> token (string st) <*> return f


conj, disj, impl, iff :: Parser (Prop -> Prop -> Prop)
conj = parseOp "Conj" Conj <|>  parseOp "/\\" Conj
disj = parseOp "Disj" Disj <|>  parseOp "\\/" Disj
impl = parseOp "Implies" Impl <|>  parseOp "=>" Impl
iff =  parseOp "iff" Iff <|>  parseOp "<=>" Iff
notProp :: Parser (Prop -> Prop)
notProp = parseOp "not" Not <|> parseOp "!" Not

expr0 :: Parser Prop
expr0 = expr1 `chainl1` iff

expr1 :: Parser Prop
expr1 = expr2 `chainl1` impl

expr2 :: Parser Prop
expr2 = expr3 `chainl1` disj

expr3 :: Parser Prop
expr3 = expr4 `chainl1` conj

expr4 :: Parser Prop
expr4 = expr5 <|> (notProp <*> expr5)

expr5 :: Parser Prop
expr5 = bool <|> var <|> parens expr0  


propParse :: String -> Prop
propParse = BP.runParser expr0











