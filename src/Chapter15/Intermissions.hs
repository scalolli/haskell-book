module Chapter15.Intermissions where

  import Data.Monoid
  import Test.QuickCheck

  data Optional a = Nada | Only a
      deriving (Eq, Show)


  instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only a) (Only b) = Only (mappend a b)
    mappend x Nada = x
    mappend Nada x = x

  newtype First' a = First' {getFirst' :: Optional a}
    deriving (Eq, Show)

  instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' Nada) (First' b) = First' b
    mappend (First' a) (First' Nada) = First' a
    mappend (First' a) (First' b) = First' a


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

  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c =
      (a <> (b <> c)) == ((a <> b) <> c)

  type S = String
  type B = Bool

  monoidLeftIdentity :: (Monoid m, Eq m) => m -> Bool
  monoidLeftIdentity a = (a <> mempty) == a

  monoidRightIdentity :: (Monoid m, Eq m) => m -> Bool
  monoidRightIdentity a = (mempty <> a) == a


  runMain :: IO ()
  runMain = do
    quickCheck (monoidAssoc :: S -> S -> S -> B)
    quickCheck (monoidLeftIdentity :: S -> B)
    quickCheck (monoidRightIdentity :: S -> B)