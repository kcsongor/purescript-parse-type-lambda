module Type.Program.Convert where

import Type.Program.Lambda

import Type.Program.Lang as Lang
import Type.Utils (kind Nat, Z, S)

class Convert (cs :: Lang) (l :: Lang.Lang) | cs -> l
instance convert :: ConvertS cs SNil l => Convert cs l

foreign import kind Scope
foreign import data SNil :: Scope
foreign import data SCons :: Symbol -> Scope -> Scope

class Find (scope :: Scope) (var :: Symbol) (pos :: Nat) | scope var -> pos

instance findHere :: Find (SCons sym xs) sym Z
else instance findThere :: Find xs sym pos  => Find (SCons sym' xs) sym (S pos)

instance findNowhere ::
  Fail (TypeConcat "Error: unbound variable: " sym)
  => Find SNil sym Z

class ConvertS (cs :: Lang) (scope :: Scope) (l :: Lang.Lang) | cs scope -> l

instance convertVar ::
  Find scope var pos
  => ConvertS (Var var) scope (Lang.Var pos)
else instance convertLam ::
  ConvertS expr (SCons sym scope) expr'
  => ConvertS (Lam sym expr) scope (Lang.Lam expr')
else instance convertApp ::
  ( ConvertS fun scope fun'
  , ConvertS arg scope arg'
  ) => ConvertS (App fun arg) scope (Lang.App fun' arg')
