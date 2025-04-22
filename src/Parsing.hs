{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 |||

{-
The monad of parsers
--------------------
-}

newtype HuttonParser a              =  P (String -> [(a,String)])

instance Functor HuttonParser where
   fmap f p = do p' <- p
                 return (f p')

instance Applicative HuttonParser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

instance Monad HuttonParser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative HuttonParser where
   empty = mzero
   p <|> q = p ||| q

instance MonadPlus HuttonParser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: HuttonParser a
failure                       =  mzero

item                          :: HuttonParser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: HuttonParser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

(|||)                         :: HuttonParser a -> HuttonParser a -> HuttonParser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> HuttonParser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: HuttonParser Char
digit                         =  sat isDigit

lower                         :: HuttonParser Char
lower                         =  sat isLower

upper                         :: HuttonParser Char
upper                         =  sat isUpper

letter                        :: HuttonParser Char
letter                        =  sat isAlpha

alphanum                      :: HuttonParser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> HuttonParser Char
char x                        =  sat (== x)

string                        :: String -> HuttonParser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)
anyparse                      :: HuttonParser Char 
anyparse                      = do x <- item
                                   return x

many                          :: HuttonParser a -> HuttonParser [a]
many p                        =  many1 p ||| return []

many1                         :: HuttonParser a -> HuttonParser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: HuttonParser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: HuttonParser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: HuttonParser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

space                         :: HuttonParser ()
space                         =  do many (sat isSpace)
                                    return ()
{-
Ignoring spacing
----------------
-}

token                         :: HuttonParser a -> HuttonParser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: HuttonParser String
identifier                    =  token ident

natural                       :: HuttonParser Int
natural                       =  token nat

integer                       :: HuttonParser Int
integer                       =  token int

symbol                        :: String -> HuttonParser String
symbol xs                     =  token (string xs)

---------------------------------------CUSTOM PARSERS------------------------------------------
allparse = parse (some anyparse)


pTurn = string "Black" <|> string "White" -- will always have one of these
