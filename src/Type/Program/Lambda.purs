module Type.Program.Lambda where

import Type.Data.Symbol (class ConsSymbol)

foreign import kind Lang
foreign import data Var :: Symbol -> Lang
foreign import data Lam :: Symbol -> Lang -> Lang
foreign import data App :: Lang -> Lang -> Lang
