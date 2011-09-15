{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Generics.LIGD.Base where

data Unit    = Unit           deriving Show
data a :+: b = Inl a | Inr b  deriving Show
data a :*: b = a :*: b        deriving Show

infixr 5 :+:
infixr 6 :*:

data EP b c = EP { from :: (b -> c), to :: (c -> b) }

data Rep t where
  RChar    ::                    Rep Char
  RInt     ::                    Rep Int
  RString  ::                    Rep String
  RUnit    ::                    Rep Unit
  RSum     :: Rep a  -> Rep b -> Rep (a :+: b)
  RProd    :: Rep a  -> Rep b -> Rep (a :*: b)
  RCon     :: String -> Rep a -> Rep a
  RType    :: EP b a -> Rep a -> Rep b

data Rep1 g a where
  RChar1   ::                         Rep1 g Char
  RInt1    ::                         Rep1 g Int
  RUnit1   ::                         Rep1 g Unit
  RSum1    :: Rep1 g a -> Rep1 g b -> Rep1 g (a :+: b)
  RProd1   :: Rep1 g a -> Rep1 g b -> Rep1 g (a :*: b)
  RCon1    :: String -> Rep1 g a ->   Rep1 g a
  RType1   :: EP b a -> Rep1 g a ->   Rep1 g b
  RVar1    :: g a ->                  Rep1 g a

data Rep2 g a b where
  RChar2   ::                                   Rep2 g Char Char
  RInt2    ::                                   Rep2 g Int Int
  RUnit2   ::                                   Rep2 g Unit Unit
  RSum2    :: Rep2 g a b -> Rep2 g c d ->       Rep2 g (a :+: c) (b :+: d)
  RProd2   :: Rep2 g a b -> Rep2 g c d ->       Rep2 g (a :*: c) (b :*: d)
  RCon2    :: String -> Rep2 g a b ->           Rep2 g a b
  RType2   :: EP a c -> EP b d -> Rep2 g c d -> Rep2 g a b
  RVar2    :: g a b ->                          Rep2 g a b

