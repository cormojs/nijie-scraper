module Web.Nijie.Types
       ( NjeUser(..)
       , NjeKind(..)
       , NjeLink(..) ) where

import Data.ByteString (ByteString)


data NjeUser = NjeUser { njeUserName :: ByteString
                       , njeUserId   :: ByteString }
             deriving (Eq, Show)

data NjeKind = NjeDoujin | NjeManga | NjeSingle
             deriving (Eq, Show)

data NjeLink = NjeLink { njeId        :: ByteString
                       , njeThumbUrl  :: String
                       , njeTitle     :: ByteString
                       , njeAuthor    :: NjeUser
                       , njeKind      :: NjeKind
                       } deriving (Eq, Show)

