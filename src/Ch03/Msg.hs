module Ch03.Msg where

import Control.Lens

data Err
  = ReallyBadError
      {_msg :: String}
  | ExitCode
      {_code :: Int}
  deriving (Eq, Show)

-- |
-- >>> set msg "foo" err
-- ReallyBadError {_msg = "foo"}
msg :: Lens' Err String
msg = lens g s
  where
    g :: Err -> String
    g (ReallyBadError m) = m
    g _ = ""
    s :: Err -> String -> Err
    s (ReallyBadError _) m = ReallyBadError m
    s e _ = e

err :: Err
err = ReallyBadError ""
