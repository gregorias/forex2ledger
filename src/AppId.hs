module AppId (
  AppId (..),
) where

import Relude (
  Eq,
  IsString,
  Read,
  Show,
  Text,
 )

newtype AppId = AppId
  { unAppId :: Text
  }
  deriving newtype (Read, Show, Eq, IsString)
