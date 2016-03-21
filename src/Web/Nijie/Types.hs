{-# LANGUAGE OverloadedStrings #-}
module Web.Nijie.Types
       ( User(..)
       , Kind(..)
       , Link(..)
       , API (..)
       , RankType(..)
       , Sort(..)
       , Login(..)
       , convertAPIToQuery
       ) where


-- http-conduit
import qualified Network.HTTP.Types   as Types

--- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8


data User = User { userName :: ByteString
                 , userId   :: ByteString }
             deriving (Eq, Show)

data Kind =
  Doujin | Manga | Single | Anime
  deriving (Eq, Show)

data Link = Link { illustId    :: ByteString
                 , thumbUrl    :: String
                 , illustTitle :: ByteString
                 , author      :: User
                 , kind        :: Kind
                 } deriving (Eq, Show)

type Page = Int

data Login = Login { email    :: ByteString
                   , password :: ByteString }

data API =
    Like Page
  | Fav Page
  | Rank RankType
  | View ByteString
  | ViewPopup ByteString
  | FavAdd ByteString
  | NuiAdd ByteString
  | GoodAdd ByteString
  | UserIllust User
  | UserBookmark User Page
  | Search ByteString Sort Page
  deriving (Eq, Show)

data Sort =
    SortDateA
  | SortDateD
  | SortNui
  | SortGood
  deriving Eq
instance Show Sort where
  show SortDateA = "0"
  show SortDateD = "3"
  show SortNui   = "2"
  show SortGood  = "1"


data RankType = RankNow | RankDay | RankWeek | RankMonth
                 deriving Eq

instance Show RankType where
  show RankNow   = "now"
  show RankDay   = "day"
  show RankWeek  = "week"
  show RankMonth = "month"

convertAPIToQuery :: API -> (String, Types.SimpleQuery)
convertAPIToQuery api = case api of
  Like p    -> ("like_user_view", [ ("p", ps p) ])
  Fav p     -> ("okiniiri",       [ ("p", ps p) ])
  Rank typ  -> ("okazu",          [ ("type", ps typ) ])
  View id   -> ("view",           [ ("id", id) ])
  ViewPopup id -> ("view_popup",  [ ("id", id) ])
  FavAdd id -> ("bookmark_add",   [ ("tag", "")
                                  , ("id", id) ])
  NuiAdd id -> ("php/ajax/add_nuita", [("id", id) ])
  GoodAdd id -> ("php/ajax/add_good", [("id", id) ])
  UserIllust user -> ("members_illust", [("id", userId user) ])
  UserBookmark user p ->
    ("user_like_illust_view", [ ("id", userId user)
                              , ("p", ps p) ])
  Search word sort page -> ("search", [ ("word", word)
                                      , ("p", ps page)
                                      , ("sort", ps sort) ])
  where ps :: Show a => a -> ByteString
        ps = Char8.pack . show
