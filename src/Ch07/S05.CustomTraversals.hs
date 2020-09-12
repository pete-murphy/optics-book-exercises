module Ch07.S05.CustomTraversals where

import Control.Lens

-- | Note that it's normally bad-style™ to have field names on types with
-- multiple constructors, but it's okay so long as all constructors have the
-- EXACT same field names and types like they do here.
data Transaction
  = Withdrawal {_amount :: Int}
  | Deposit {_amount :: Int}
  deriving Show
makeLenses ''Transaction

newtype BankAccount = 
  BankAccount 
  { _transactions :: [Transaction]
  } deriving Show
makeLenses ''BankAccount

deposits :: Traversal' [Transaction] Int
--          Applicative f
--          => (Int -> f Int) -> [Transaction] -> f [Transaction]
deposits f = foldr go (pure [])
  where
    go x xs = case x of 
      Deposit a -> (:) <$> Deposit <$> f a <*> xs
      w         -> (:) <$> pure w          <*> xs

-- | Exercises
amountT :: Traversal' Transaction Int
--         Applicative f
--         => (Int -> f Int) -> Transaction -> f Transaction
amountT f (Withdrawal x) = Withdrawal <$> f x
amountT f (Deposit    x) = Deposit    <$> f x

bothT :: Traversal (a, a) (b, b) a b
--       Applicative f
--       => (a -> f b) -> (a, a) -> f (b, b)
bothT f (x, y) = (,) <$> f x <*> f y 

-- >>> Deposit 10 ^? transactionDelta
-- Just 10
-- >>> Withdrawal 10 ^? transactionDelta
-- Just (-10)
-- >>> Deposit 10 & transactionDelta .~ 15
-- Deposit {_amount = 15}
-- >>> Withdrawal 10 & transactionDelta .~ (-15)
-- Withdrawal {_amount = 15}
-- >>> Deposit 10 & transactionDelta +~ 5
-- Deposit {_amount = 15}
-- >>> Withdrawal 10 & transactionDelta +~ 5
-- Withdrawal {_amount = 5}
transactionDelta :: Traversal' Transaction Int
--                  Applicative f
--                  => (Int -> f Int)
--                  -> Transaction
--                  -> f Transaction
transactionDelta f (Withdrawal n) = Withdrawal . negate <$> f (negate n)
transactionDelta f (Deposit n)    = Deposit <$> f n

leftT :: Traversal (Either a b) (Either a' b) a a'
--       Applicative f
--       => (a -> f a')
--       -> Either a b
--       -> f (Either a' b)
leftT f (Left x)  = Left <$> f x
leftT _ (Right x) = pure (Right x)

beside :: Traversal s t a b
       -> Traversal s' t' a b
       -> Traversal (s,s') (t,t') a b
--     (Applicative f => (a -> f b) -> s -> f t) ->
--     (Applicative f => (a -> f b) -> s' -> f t') ->
--      Applicative f => (a -> f b) -> (s,s') -> f (t,t')
beside tr tr' = \f (s,s') -> 
  (,) <$> tr f s <*> tr' f s'
