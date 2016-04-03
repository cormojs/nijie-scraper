{-# LANGUAGE OverloadedStrings #-}
module Web.Nijie.Types
       ( User(..)
       , Kind(..)
       , Link(..)
       , API (..)
       , PostAPI (..)
       , ViewAPI (..)
       , ListAPI (..)
       , RankType(..)
       , Sort(..)
       , FavSort(..)
       , Login(..)
       , ParseExcepiton(..)
       , NotLoggedInException(..)
       ) where


-- http-conduit
import qualified Network.HTTP.Types   as Types

--- bytestring
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as BSUTF8

-- exception
import Control.Exception (Exception)


-- exceptions
data ParseExcepiton = ParseExcepiton deriving Show
data NotLoggedInException = NotLoggedInException deriving Show

instance Exception ParseExcepiton
instance Exception NotLoggedInException

-- User data
data User = User { userName :: ByteString
                 , userId   :: ByteString }
             deriving Eq

instance Show User where
  show (User name id) | name /= "" = BSUTF8.toString name
                      | otherwise  = "id = " ++ BSUTF8.toString id

-- Kind of link
data Kind =
  Doujin | Manga | Single | Anime
  deriving (Eq, Show)

-- Link representation
data Link = Link { illustId    :: ByteString
                 , thumbUrl    :: String
                 , illustTitle :: ByteString
                 , author      :: User
                 , kind        :: Kind
                 } deriving (Eq, Show)

-- Page number is Int
type Page = Int

-- Login data
data Login = Login { email    :: ByteString
                   , password :: ByteString }

-- API
data PostAPI =
    FavAdd ByteString
  | NuiAdd ByteString
  | GoodAdd ByteString
  deriving (Eq, Show)

data ViewAPI =
    View ByteString
  | ViewPopup ByteString
  deriving (Eq, Show)

data ListAPI =
    Like Page
  | Fav FavSort Page
  | Rank RankType
  | UserIllust User
  | UserBookmark User Page
  | Search ByteString SearchSort Page
  | Illust Page
  deriving (Eq, Show)

class API api where
  convertAPIToQuery :: api -> (String, Types.SimpleQuery)

instance API PostAPI where
  convertAPIToQuery api = case api of
    FavAdd id -> ("bookmark_add",   [ ("tag", "")
                                    , ("id", id) ])
    NuiAdd id -> ("php/ajax/add_nuita", [("id", id) ])
    GoodAdd id -> ("php/ajax/add_good", [("id", id) ])

instance API ViewAPI where
  convertAPIToQuery api = case api of
    View id   -> ("view",           [ ("id", id) ])
    ViewPopup id -> ("view_popup",  [ ("id", id) ])

instance API ListAPI where
  convertAPIToQuery api = case api of
    Illust p  -> ("illust_view", [ ("p", ps p) ])
    Like p    -> ("like_user_view", [ ("p", ps p) ])
    Fav s p   -> ("okiniiri",       [ ("p", ps p)
                                      , ("sort", ps s) ])
    Rank typ  -> ("okazu",          [ ("type", ps typ) ])
    UserIllust user -> ("members_illust", [("id", userId user) ])
    UserBookmark user p ->
      ("user_like_illust_view", [ ("id", userId user)
                                , ("p", ps p) ])
    Search word sort page -> ("search", [ ("word", word)
                                        , ("p", ps page)
                                        , ("sort", ps sort) ])

ps :: Show a => a -> ByteString
ps = Char8.pack . show

-- Kind of sorts
data SearchSort =
    SortDateA
  | SortDateD
  | SortNui
  | SortGood
  deriving Eq

instance Show SearchSort where
  show SortDateA = "0"
  show SortDateD = "3"
  show SortNui   = "2"
  show SortGood  = "1"

data FavSort = FavAsc | FavDesc deriving Eq

instance Show FavSort where
  show FavAsc  = "0"
  show FavDesc = "1"

class Sort s where
  sortName :: s -> String
  toggleSort :: s -> s

instance Sort SearchSort where
  sortName SortDateA = "by date(a)"
  sortName SortDateD = "by date(d)"
  sortName SortNui   = "by nukareta"
  sortName SortGood  = "by good"
  toggleSort SortDateA = SortDateD
  toggleSort SortDateD = SortNui
  toggleSort SortNui   = SortGood
  toggleSort SortGood  = SortDateA

instance Sort FavSort where
  sortName FavAsc  = "asc"
  sortName FavDesc = "desc"
  toggleSort FavAsc  = FavDesc
  toggleSort FavDesc = FavAsc

-- Kind of rank
data RankType = RankNow | RankDay | RankWeek | RankMonth
                 deriving Eq

instance Show RankType where
  show RankNow   = "now"
  show RankDay   = "day"
  show RankWeek  = "week"
  show RankMonth = "month"

