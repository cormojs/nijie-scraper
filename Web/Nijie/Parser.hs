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
import qualified Data.Text        as Text
import qualified Data.Text.IO     as TextIO
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Text.Lazy        as TextL
import qualified Data.Text.Lazy.IO     as TextLIO



njeCursorToNjeLink :: XMLC.Cursor -> NjeLink
njeCursorToNjeLink cursor =
  let [dao]   = cursor $// XMLC.attributeIs "class" "nijiedao"
      kImg    = cursor $// XMLC.attributeIs "class" "thumbnail-icon"
                       &// XMLC.element "img"
      [auSpn] = cursor $// XMLC.attributeIs "itemprop" "author"
      [auIdA] = XMLC.parent auSpn
      [link]  = dao $// XMLC.element "a"
      [thImg] = dao $// XMLC.element "img"
      [title] = XMLC.attribute "title" link
      [url]   = XMLC.attribute "href" link
      [thUrl] = XMLC.attribute "src" thImg
      [auN]   = XMLC.content $ head $ XMLC.child auSpn
      [auId]  = XMLC.attribute "href" auIdA
  in NjeLink { njeId = toId url
             , njeThumbUrl = toThUrl thUrl
             , njeTitle    = TextEnc.encodeUtf8 title
             , njeAuthor   = NjeUser (TextEnc.encodeUtf8 auN) (toAuId auId)
             , njeKind     = toKind kImg }
  where pattern = "([0-9]+)" :: ByteString
        toId text   = TextEnc.encodeUtf8 text =~ pattern
        toAuId text = TextEnc.encodeUtf8 text =~ pattern
        toThUrl = ("http:"++) . Text.unpack
        toKind []     = NjeSingle
        toKind [kImg] = case XMLC.attribute "alt" kImg of
          ["同人"] -> NjeDoujin
          ["漫画"] -> NjeManga
          _       -> NjeSingle
