{-# LANGUAGE RankNTypes, MultiWayIf #-}

module Ch07.S04.TraversalActions.Exercises where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Char

-- | 1.
-- >>> ex1_1 _1 (Nothing, "Rosebud") :: Maybe (Int, String)
-- Nothing
ex1_1 ::
  Traversal (Maybe a, String) (a, String) (Maybe a) a ->
  (Maybe a, String) ->
  Maybe (a, String)
ex1_1 = sequenceAOf

-- >>> sequenceAOf (traversed . _1) ex1_2
-- [[('a',1),('c',2)],[('a',1),('d',2)],[('b',1),('c',2)],[('b',1),('d',2)]]
ex1_2 :: [(String, Int)]
ex1_2 = [("ab", 1), ("cd", 2)]

-- >>> sequenceAOf ex1_3 [ZipList [1,2], ZipList [3,4]]
-- ZipList {getZipList = [[1,3],[2,4]]}
ex1_3 :: Traversal [ZipList Int] [Int] (ZipList Int) Int
ex1_3 = traversed

-- >>> import Control.Monad.State
-- >>> let result = traverseOf ex1_4 (\n -> modify (+n) >> get) ([1, 1, 1], (1, 1)) :: State Int ([Int], (Int, Int))
-- >>> runState result 0 :: (([Int], (Int,Int)), Int)
-- (([1,2,3],(4,5)),5)

{- I think this is the type of `traverseOf` in this situation
traverseOf ::
  Traversal ([Int], (Int, Int)) ([Int], (Int, Int)) Int Int ->
    (Int -> (State Int) Int) ->
    ([Int], (Int, Int)) ->
    State Int Int
-}
ex1_4 :: Traversal' ([Int], (Int, Int)) Int
ex1_4 = beside traversed each

-- | 2.

-- >>> import Data.Char (toUpper, toLower)
-- >>> ex2_1
-- [("ab",True),("aB",True),("Ab",True),("AB",True)]
ex2_1 :: [(String, Bool)]
ex2_1 =
  ("ab", True)
    & _1 . traversed %%~ sequenceA [toLower, toUpper]

-- >>> import Data.Char (toUpper, toLower)
-- >>> ex2_2
-- [[('a',True),('b',False)],[('a',True),('B',False)],[('A',True),('b',False)],[('A',True),('B',False)]]
ex2_2 :: [[(Char, Bool)]]
ex2_2 = [('a', True), ('b', False)] & traversed . _1 %%~ sequenceA [toLower, toUpper]

-- | 3.
data User
  = User
      { _name :: String,
        _age :: Int
      }
  deriving (Show)

makeLenses ''User

data Account
  = Account
      { _id :: String,
        _user :: User
      }
  deriving (Show)

makeLenses ''Account

validateAge :: Account -> Either String Account
validateAge = user . age %%~ 
     \a -> if a > 0
            a < 150 then pure a else Left "Invalid age"
