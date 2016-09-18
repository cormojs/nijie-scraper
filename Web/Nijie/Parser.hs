module Web.Nijie.Parser where

-- internal
import Web.Nijie.Types

-- html-conduit
import Text.XML.Cursor (($//), (&//), (>=>))
import qualified Text.HTML.DOM   as HTML
import qualified Text.XML        as XML
import qualified Text.XML.Cursor as XMLC
import qualified Text.Blaze.Html as BlazeHtml (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- regex-posix
import Text.Regex.Posix ((=~))

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Lazy  as BSLazy
import qualified Data.ByteString.Char8 as Char8
import qualified Codec.Binary.UTF8.String   as UTF8Codec

-- text
import Data.Text  (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as TextIO
import qualified Data.Text.Encoding    as TextEnc
import qualified Data.Text.Lazy        as TextL
import qualified Data.Text.Lazy.IO     as TextLIO


-- Control
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans as Trans
import Control.Applicative ((<$>))


test = do
  doc <- HTML.readFile "out.html"
  putStrLn $ njeDescriptionFromDoc doc
  -- Monad.forM_ s $ print . njeId

-- njeDescriptionFromDoc :: XML.Document -> ByteString
njeDescriptionFromDoc doc =
  let [s] = XMLC.fromDocument doc
            $// XMLC.attributeIs "id" "view-honbun"
            &// XMLC.attributeIs "class" "m-bottom15"
      descs = XMLC.child s >>= XMLC.content
  in Text.unpack $ Text.concat descs

njeTagsFromDoc :: XML.Document -> [ByteString]
njeTagsFromDoc doc =
  let s = XMLC.fromDocument doc
          $// XMLC.attributeIs "id" "view-tag"
          &// XMLC.attributeIs "class" "tag"
          &// XMLC.attributeIs "class" "tag_name"
  in map (TextEnc.encodeUtf8 . toTagName) s
  where toTagName s =
          let (e:_) = s $// XMLC.element "a"
          in head $ XMLC.child e >>= XMLC.content

njeLinksFromThumbs :: NjeAPI -> XML.Document -> [NjeLink]
njeLinksFromThumbs api doc = go api
  where go (NjeLike _) = map njeLikeCursorToNjeLink $ cursor
                         $// XMLC.attributeIs "id" "main-left-main"
                         &// XMLC.attributeIs "class" "nijie mozamoza illust_list"
        go (NjeFav _)  = map njeBookmarkCursorToNjeLink $ cursor
                         $// XMLC.attributeIs "class" "nijie-bookmark"
        go (NjeRank _) = map njeOkazuCursorToNjeLink $ cursor
                         $// XMLC.attributeIs "id" "okazu_list"
                         &// XMLC.element "a"
        go (NjeUserIllust user) = map (njeUserIllustCursorToNjeLink user) $ cursor
                                  $// XMLC.attributeIs "class" "mem-index clearboth"
                                  &// XMLC.attributeIs "class" "nijie"
        go (NjeUserBookmark user) = map (njeUserIllustCursorToNjeLink empty) $ cursor
                                    $// XMLC.attributeIs "class" "mem-index clearboth"
                                    &// XMLC.attributeIs "class" "nijie"
          where empty = NjeUser "" "-1"
        go (NjeUserNuita _)    = map njeLikeCursorToNjeLink $ cursor
                                 $// XMLC.attributeIs "id" "main-left-main"
                                 &// XMLC.attributeIs "class" "nijie"
        go (NjeSearch _ _ _) = map njeLikeCursorToNjeLink $ cursor
                               $// XMLC.attributeIs "id" "main-left-main"
                               &// XMLC.attributeIs "class" "nijie mozamoza illust_list"
        cursor = XMLC.fromDocument doc


toUserId :: Text -> ByteString
toUserId text   = TextEnc.encodeUtf8 text =~ pattern
  where pattern = "([0-9]+)" :: ByteString

toAuthorId :: Text -> ByteString
toAuthorId text = TextEnc.encodeUtf8 text =~ pattern
  where pattern = "([0-9]+)" :: ByteString

toThumbUrl = ("http:"++) . Text.unpack

toNjeKind []     = NjeSingle
toNjeKind [kImg] = case XMLC.attribute "alt" kImg of
  ["同人"] -> NjeDoujin
  ["漫画"] -> NjeManga
  _        -> NjeSingle


njeUserIllustCursorToNjeLink author cursor =
  let [dao]   = cursor $// XMLC.attributeIs "class" "nijiedao"
      kImg    = cursor $// XMLC.attributeIs "class" "thumbnail-icon"
                       &// XMLC.element "img"
      kAnime  = cursor $// XMLC.attributeIs "class" "thumbnail-anime-icon"
      [link]  = dao $// XMLC.element "a"
      [thImg] = dao $// XMLC.element "img"
      [title] = XMLC.attribute "title" link
      [url]   = XMLC.attribute "href" link
      [thUrl] = XMLC.attribute "src" thImg
  in NjeLink { njeId = toUserId url
             , njeThumbUrl = toThumbUrl thUrl
             , njeTitle    = TextEnc.encodeUtf8 title
             , njeAuthor   = author
             , njeKind     = toNjeKind kImg
             , njeIsAnime  = not $ null kAnime }


njeLikeCursorToNjeLink :: XMLC.Cursor -> NjeLink
njeLikeCursorToNjeLink cursor =
  let [dao]   = cursor $// XMLC.attributeIs "class" "nijiedao"
      kImg    = cursor $// XMLC.attributeIs "class" "thumbnail-icon"
                       &// XMLC.element "img"
      anime   = cursor $// XMLC.attributeIs "class" "thumbnail-anime-icon"
      [auSpn] = cursor $// XMLC.element "span"
      [auIdA] = XMLC.parent auSpn
      [link]  = dao $// XMLC.element "a"
      [thImg] = dao $// XMLC.element "img"
      [title] = XMLC.attribute "title" link
      [url]   = XMLC.attribute "href" link
      [thUrl] = XMLC.attribute "src" thImg
      [auN]   = XMLC.content $ head $ XMLC.child auSpn
      [auId]  = XMLC.attribute "href" auIdA
  in NjeLink { njeId = toUserId url
             , njeThumbUrl = toThumbUrl thUrl
             , njeTitle    = TextEnc.encodeUtf8 title
             , njeAuthor   = NjeUser (TextEnc.encodeUtf8 auN) (toAuthorId auId)
             , njeKind     = toNjeKind kImg
             , njeIsAnime  = not $ null anime }

njeBookmarkCursorToNjeLink :: XMLC.Cursor -> NjeLink
njeBookmarkCursorToNjeLink cursor =
  let [dao]   = cursor $// XMLC.attributeIs "class" "picture"
                       &// XMLC.attributeIs "class" "nijiedao"
      kImg    = cursor $// XMLC.attributeIs "class" "bookmark-thumbnail-icon"
                       &// XMLC.element "img"
      kAnime  = cursor $// XMLC.attributeIs "class" "bookmark-thumbnail-anime-icon"
      [authP] = cursor $// XMLC.attributeIs "class" "kazu"
      [authA] = authP  $// XMLC.element "a"
      [tleP]  = cursor $// XMLC.attributeIs "class" "title"
      title = case XMLC.child tleP of
        [s] -> s
        (_:s:_) -> s
      [auId] = XMLC.attribute "href" authA
      [auName]  = concatMap XMLC.content $ XMLC.child authA
      [link]  = dao $// XMLC.element "a"
      [url]   = XMLC.attribute "href" link
      [thImg] = dao $// XMLC.element "img"
      [thUrl] = XMLC.attribute "src" thImg
  in NjeLink { njeId = toUserId url
             , njeThumbUrl = toThumbUrl thUrl
             , njeTitle    = TextEnc.encodeUtf8 $ head $ XMLC.content title
             , njeAuthor   = NjeUser (TextEnc.encodeUtf8 auName) (toAuthorId auId)
             , njeKind     = toNjeKind kImg
             , njeIsAnime  = not $ null kAnime }

njeOkazuCursorToNjeLink :: XMLC.Cursor -> NjeLink
njeOkazuCursorToNjeLink link =
  let id = head $ XMLC.attribute "href" link
      [titleC] = link $// XMLC.attributeIs "class" "rank"
                      &// XMLC.attributeIs "class" "title"
      [thumbs] = link $// XMLC.attributeIs "class" "rank"
                      &// XMLC.attributeIs "class" "illust_block"
                      &// XMLC.attributeIs "class" "illust"
                      &// XMLC.element "img"
      [title]  = XMLC.content =<< XMLC.child =<< titleC $// XMLC.element "strong"
      [author] = XMLC.content =<< XMLC.child =<< titleC $// XMLC.element "span"
      kind = link
             $// XMLC.attributeIs "class" "okazu-thumbnail-icon"
             &// XMLC.element "img"
      anime = link
              $// XMLC.attributeIs "class" "okazu-thumbnail-anime-icon"
  in NjeLink { njeId       = toUserId id
             , njeThumbUrl = toThumbUrl $ head $ XMLC.attribute "src" thumbs
             , njeTitle    = TextEnc.encodeUtf8 title
             , njeAuthor   = NjeUser (TextEnc.encodeUtf8 $ Text.drop 4 author)
                                     "-1"
             , njeKind     = toNjeKind kind
             , njeIsAnime  = not $ null anime }
