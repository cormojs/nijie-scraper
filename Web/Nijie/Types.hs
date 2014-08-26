module Web.Nijie.Types
       ( NjeUser(..)
       , NjeKind(..)
       , NjeLink(..)
       , NjeAPI (..)
       , NjeRankType(..)
       , NjeSort(..)
       , NjeLogin(..)
       , njeApiToQuery
       ) where


-- http-conduit
import qualified Network.HTTP.Types   as Types

--- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8


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
                       , njeIsAnime   :: Bool
                       } deriving (Eq, Show)

type Page = Int

data NjeLogin = NjeLogin { njeEmail    :: ByteString
                         , njePassword :: ByteString }

data NjeAPI = NjeLike Page
            | NjeFav Page
            | NjeRank NjeRankType
            | NjeView ByteString
            | NjeFavAdd ByteString
            | NjeNuiAdd ByteString
            | NjeGoodAdd ByteString
            | NjeUserIllust NjeUser
            | NjeUserBookmark NjeUser
            | NjeUserNuita NjeUser
            | NjeSearch ByteString NjeSort Page
            deriving (Eq, Show)

data NjeSort = NjeSortDateA | NjeSortDateD
             | NjeSortNui | NjeSortGood
             deriving Eq
instance Show NjeSort where
  show NjeSortDateA = "0"
  show NjeSortDateD = "3"
  show NjeSortNui   = "2"
  show NjeSortGood  = "1"


data NjeRankType = NjeRankNow | NjeRankDay | NjeRankWeek | NjeRankMonth
                 deriving Eq

instance Show NjeRankType where
  show NjeRankNow   = "now"
  show NjeRankDay   = "day"
  show NjeRankWeek  = "week"
  show NjeRankMonth = "month"

njeApiToQuery :: NjeAPI -> (String, Types.SimpleQuery)
njeApiToQuery api = case api of
  (NjeLike p)    -> ("like_user_view", [("p", ps p)])
  (NjeFav p)     -> ("okiniiri",       [("p", ps p)])
  (NjeRank typ)  -> ("okazu",          [("type", ps typ)])
  (NjeView id)   -> ("view",           [("id", id)])
  (NjeFavAdd id) -> ("bookmark_add",   [("tag", ""),
                                        ("id", id)])
  (NjeNuiAdd id) -> ("php/ajax/add_nuita", [("id", id)])
  (NjeGoodAdd id) -> ("php/ajax/add_good", [("id", id)])
  (NjeUserIllust   user) ->
    ("members_illust",        [("id", njeUserId user)])
  (NjeUserBookmark user) ->
    ("user_like_illust_view", [("id", njeUserId user)])
  (NjeUserNuita    user) ->
    ("history_nuita",         [("id", njeUserId user)])
  (NjeSearch word sort page) ->
    ("search", [("word", word), ("p", ps page),
                ("sort", ps sort)])
  where ps :: Show a => a -> ByteString
        ps = Char8.pack . show
