{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Text.Pretty.Simple


data Prog = Prog Cmt Identifier CST
             deriving (Show)

data CST = Declaration Cmt Decl
         | TLBindr Binder
         | CSTS [CST]
             deriving (Show)

data Binder = Binder Cmt Identifier [Identifier] Body
                deriving (Show)

data Decl = Decl
              deriving (Show)

newtype Identifier = I Text
                       deriving (Show)

newtype Cmt = Cmt Text
                deriving (Show)

newtype Body = Body Expr
                 deriving (Show)

data Literal = LInt Integer
             | LFloat Double
             | LBool Bool
             | LChar Char
             | LList [Literal]
             | LPair (Literal, Literal)
                 deriving (Show)

data Op = Mult
        | Div
        | Add
        | Sub
      deriving Show

data Expr = Lit Cmt Literal
          | Neg Expr
          | BinOp Op Expr Expr
          | Var Cmt Identifier
          | Ap Expr [Expr]
          | Lam Cmt [Identifier] Body
          | Let [Binder] Body
              deriving (Show)

type Parser = Parsec Void Text

sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

scn = L.space space1 empty empty

lexeme = L.lexeme sc



operators :: [[Operator Parser Expr]]
operators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (BinOp Mult <$ symbol "*")
    , InfixL (BinOp Div   <$ symbol "/") ]
  , [ InfixL (BinOp Add      <$ symbol "+")
    , InfixL (BinOp Sub <$ symbol "-") ]
  ]

exprP :: Parser Expr
exprP = makeExprParser termP operators 

termP :: Parser Expr
termP = choice [literalP, letP, funP, parens exprP]

literalP :: Parser Expr
literalP = Lit (Cmt "") <$> (charP <|> numberP <|> boolP)

funP :: Parser Expr
funP = try apP <|> try lamP <|> varP

varP :: Parser Expr
varP = Var (Cmt "") <$> nameP

lamP :: Parser Expr
lamP
  = do ((ps, b), c) <- cmtP $
                         do symbol "\\"
                            ps <- many nameP
                            symbol "->"
                            b <- bodyP
                            return (ps, b)
       return $ Lam c ps b

apP :: Parser Expr
apP
  = do e <- eitherP lamP varP
       space
       case e of
           Left e -> g e
           Right e -> g e
       

  where g e
          = do es <- some exprP
               return $ Ap e es

letP :: Parser Expr
letP = 
    do symbol "let"
       --bs <- L.indentBlock scn p
       bs <- many(lexeme binderP)
       (symbol "in")
       b <- bodyP
       return $ Let bs b
--  where p = do return (L.IndentSome Nothing return binderP)

binderP :: Parser Binder
binderP
  = do
       ilevel <- L.indentLevel
       ((x, params), c) <- cmtP $
                             do n <- nameP
                                ps <- many nameP
                                return (n, ps)
       symbol "="
       L.indentGuard sc GT ilevel
       b <- bodyP
       space
       return $ Binder c x params b

integerP :: Parser Integer
integerP = lexeme L.decimal

floatP :: Parser Double
floatP = lexeme L.float

signedIntP = LInt <$> L.signed sc integerP
signedFloatP = LFloat <$> L.signed sc floatP

numberP :: Parser Literal
numberP = try signedFloatP <|> signedIntP

charP :: Parser Literal
charP
  = lexeme $
      do char '\''
         c <- latin1Char
         char '\''
         return $ LChar c

boolP :: Parser Literal
boolP
  = lexeme $
      do x <- symbol "True" <|> symbol "False"
         return $ if x == "True" then LBool True else LBool False


rws = ["in"]
nameP :: Parser Identifier
nameP
  = I . T.pack <$> (p >>= check)
    where
      p = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "Identifier")
      check x = if x `elem` rws
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

symbol = L.symbol scn
brackets = between (symbol "[") (symbol "]")
parens = between (symbol "(") (symbol ")")

cmtStringP :: Parser Cmt
cmtStringP
  = Cmt . T.pack <$>
      lexeme (many (alphaNumChar <|> spaceChar) <?> "Comment String")

cmtP :: Parser a -> Parser (a, Cmt)
cmtP p
  = do brackets $
         do x <- lexeme p
            symbol "|"
            c <- lexeme $ cmtStringP
            return (x, c)

progP :: Parser Prog
progP
  = do (c,i) <- moduleP
       --ds <- many declP
       bs <- many (tlbndrsP <?> "Top level Binder")
       eof
       return $ Prog c i (CSTS bs)

-- :ds++bs

moduleP :: Parser (Cmt,Identifier)
moduleP
  = do symbol "module"
       (n, c) <- cmtP nameP
       symbol "where"
       return (c,n)

tlbndrsP :: Parser CST
tlbndrsP
  = L.nonIndented scn $ do          
      b <- binderP
      space
      return $ TLBindr b


bodyP :: Parser Body
bodyP
  = do e <- exprP <?> "body"
       return $ Body e

main :: IO ()
main
  = do parseTest (progP) "module [Foo|This is the Foo Module] where"
       parseTest (progP)
         "module [Foo|This is the Foo Module] where\n\n[foo|A bndr] = \n -2"
       parseTest (progP)
         "module [Foo|This is the Foo Module] where\n\n[foo|A bndr] = \n 2.33"
       parseTest (progP)
         "module [Foo|This is the Foo Module] where\n\n[foo|A bndr] = 'Ä'"
       parseTest (progP)
         "module [Foo|This is the Foo Module] where\n\n[foo a b e |A bndr] = False"
       parseTest (apP) "a b"
       parseTest (apP) "[\\a -> a|lamda] b"
       parseTest (apP) "[\\a b -> a b|lamda] c" 
       parseTest (letP) "let [x|x] = a in x"
       f <- T.readFile "meep.fcc"
       case runParser progP "meep.fcc" f of
           Left bundle -> putStr (errorBundlePretty bundle)
           Right r -> pPrint r 
