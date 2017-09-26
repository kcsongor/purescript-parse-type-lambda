module Type.Program.Parser where

import Type.Data.Symbol (class ConsSymbol)
import Type.Program.Lambda
import Type.Program.Convert (class Convert)

class Parse (input :: Symbol) (out :: Lang) (rest :: Symbol) | input -> out rest

instance parseParseP ::
  ( ConsSymbol h t input
  , ParseP PTop h t out rest
  ) => Parse input out rest

class ParseP
  (state :: ParseState)
  (h :: Symbol)
  (t :: Symbol)
  (out :: Lang)
  (rest :: Symbol)
  | state h t -> out rest

foreign import kind ParseState
foreign import data PTop   :: ParseState
foreign import data PParen :: ParseState
foreign import data PLam   :: ParseState
foreign import data PApp   :: ParseState
foreign import data PVar   :: ParseState

instance parseP1 ::
  Parse input out rest
  => ParseP PTop " " input out rest
else instance parseP2 ::
  ( ConsSymbol h t input
  , ParseP PParen h t out rest
  ) => ParseP PTop "(" input out rest
else instance parseP3 ::
  ParseP PVar x xs var rest
  => ParseP PTop x xs var rest

instance parseParen1 ::
  ( ConsSymbol h t input
  , ParseP PLam h t out rest
  ) => ParseP PParen "\\" input out rest
else instance parseParen2 ::
  ParseP PApp h t out rest
  => ParseP PParen h t out rest

instance parseLambda1 ::
  ( ConsSymbol h t xs
  , ParseP PLam h t out rest
  ) => ParseP PLam " " xs out rest
instance parseLambda2 ::
  ( ParseP PVar h t (Var binder) rest'
  , ConsSymbol "." rest rest'
  , Parse rest expr rest''
  ) => ParseP PLam h t (Lam binder expr) rest''

instance parseApp ::
  ( ParseP PTop h t fun rest
  , ConsSymbol h' t' rest
  , ParseP PTop h' t' arg rest'
  ) => ParseP PApp h t (App fun arg) rest'

instance parseVar1 ::
  ConsSymbol "." xs xs'
  => ParseP PVar "." xs (Var "") xs'
else instance parseVar2 ::
  ParseP PVar ")" xs (Var "") xs
else instance parseVar3 ::
  ParseP PVar " " xs (Var "") xs
else instance parseVar4 ::
  ParseP PVar a "" (Var a) ""
else instance parseVar5 ::
  ( ConsSymbol h t xs
  , ParseP PVar h t (Var x') rest
  , ConsSymbol x x' var
  ) => ParseP PVar x xs (Var var) rest

parse
  :: forall input output rest expr
  .  Convert output expr
  => Parse input output rest
  => @input
  -> @expr
parse _ = @expr
