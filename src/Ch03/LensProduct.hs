module Ch03.LensProduct where

import Control.Lens
import Control.Lens.Unsound

type UserName = String

type UserId = String

data Session
  = Session
      { _userId :: UserId,
        _userName :: UserName,
        _createdTime :: String,
        _expiryTime :: String
      }
  deriving (Show, Eq)

makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

-- | Unlawful lens
alongsideUserId :: Lens' Session (Session, UserId)
alongsideUserId = lensProduct id userId

session :: Session
session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"

newSession :: Session
newSession = session {_userId = "USER-5678"}
