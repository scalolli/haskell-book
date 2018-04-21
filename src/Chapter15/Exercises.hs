module Exercises where

  import Data.Monoid

  data Optional a = Nada | Only a
      deriving (Eq, Show)


  instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only a) (Only b) = Only (mappend a b)
    mappend x Nada = x
    mappend Nada x = x


  type Verb = String
  type Adjective = String
  type Adverb = String
  type Noun = String
  type Exclamation = String

  madlibbinBetter' :: Exclamation
                   -> Adverb
                   -> Noun
                   -> Adjective
                   -> String
  madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv,
              " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]