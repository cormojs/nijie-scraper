{-# LANGUAGE OverloadedStrings #-}
module Web.Nijie
       ( njeStream
       , njeDoc
       , njeLinksFromThumbs
       , njeSaveUrl
       , njeSaveTopIllust
       , njeIllustUrls
       ) where

-- internal
import Web.Nijie.JSON
import Web.Nijie.Login
import Web.Nijie.Types
import Web.Nijie.Parser

-- conduit
import Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource (ResourceT(), MonadResource)
import qualified Control.Monad.Trans.Resource as Resource

-- http-conduit
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types   as Types

-- html-conduit
import Text.XML.Cursor (($//), (&//), (>=>))
import qualified Text.HTML.DOM   as HTML
import qualified Text.XML        as XML
import qualified Text.XML.Cursor as XMLC
import qualified Text.Blaze.Html as BlazeHtml (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- Control
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans as Trans
import Control.Applicative ((<$>))

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
import qualified Data.Text.Lazy        as TextL
import qualified Data.Text.Lazy.IO     as TextLIO

-- system
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath  as FilePath

-- regex-posix
import Text.Regex.Posix ((=~))


---
-- Save illust

njeSaveTopIllust :: FilePath -> NjeLink -> IO FilePath
njeSaveTopIllust saveDir link@(NjeLink { njeId = id }) = do
  (imageUrl:_) <- njeIllustUrls link
  njeSaveUrl (saveDir </> Char8.unpack id) imageUrl


njeIllustUrls :: NjeLink -> IO [String]
njeIllustUrls (NjeLink { njeId = id, njeKind = kind }) = do
  cursor <- XMLC.fromDocument <$> njeDoc "view" [("id", id)]
  let links = getLinks kind cursor
      urls  = map (XMLC.attribute "href") links
  return $ map (("http:"++) . Text.unpack . head) urls
  where getLinks NjeDoujin c =
          c $// XMLC.attributeIs "class" "dojin_gallery"
        getLinks _ c = c $// XMLC.attributeIs "id" "gallery_open"
                         &// XMLC.element "a"

test = do
  c <- XMLC.fromDocument <$> HTML.readFile "out.html"
  let links = c $// XMLC.attributeIs "id" "gallery_open"
                &// XMLC.element "a"
      urls  = map (XMLC.attribute "href") links
  print $ map (("http:"++) . Text.unpack . head) urls

njeSaveUrl :: FilePath -> String -> IO FilePath
njeSaveUrl filename url = Resource.runResourceT $ do
  let filename' = case FilePath.takeExtension url of
        "." -> filename ++ ".jpg"
        ext -> filename ++ ext
  exists <- Trans.liftIO $ Dir.doesFileExist $ filename'
  Monad.when (not exists) $ do
    cookie <- Trans.liftIO $ sessionCookie
    request <- Trans.liftIO $ HTTP.parseUrl url
    let request' = request { HTTP.cookieJar = Just cookie }
    manager <- Trans.liftIO $ HTTP.newManager HTTP.conduitManagerSettings
    body <- HTTP.responseBody <$> HTTP.http request' manager
    body $$+- CB.sinkFile filename'
  return filename'


---
-- API access

njeStream :: MonadResource m =>
             String -> Types.SimpleQuery ->
             m (C.ResumableSource m ByteString)
njeStream api query = do
  cookie <- Trans.liftIO sessionCookie
  request <- Trans.liftIO $ HTTP.parseUrl $ njeEndpoint api
  let request' = request { HTTP.cookieJar = Just cookie
                         , HTTP.queryString = Types.renderSimpleQuery True query }
  manager <- Trans.liftIO $ HTTP.newManager HTTP.conduitManagerSettings
  HTTP.responseBody <$> HTTP.http request' manager

njeDoc :: String -> Types.SimpleQuery -> IO XML.Document
njeDoc api query = Resource.runResourceT $ do
  body <- njeStream api query
  body $$+- HTML.sinkDoc

fetchFavsDoc :: Int -> IO XML.Document
fetchFavsDoc p = njeDoc "like_user_view" [("p", Char8.pack $ show p)]

render :: [XMLC.Cursor] -> TextL.Text
render cs = TextL.concat $ map (renderHtml . BlazeHtml.toHtml . XMLC.node) cs

renderFile filename doc =
  TextLIO.writeFile filename $ renderHtml $ BlazeHtml.toHtml doc

njeLinksFromThumbs :: String -> XML.Document -> [NjeLink]
njeLinksFromThumbs api doc =
  case api of
    "like_user_view" ->
      let njes = cursor
                   $// XMLC.attributeIs "id" "main-left-main"
                   &// XMLC.attributeIs "class" "nijie" in
      map njeCursorToNjeLink njes
    "okiniiri" ->
      let njeBookmarks = cursor
                           $// XMLC.attributeIs "id"    "main"
                           &// XMLC.attributeIs "class" "main-left2"
                           &// XMLC.attributeIs "class" "nijie-bookmark nijie" in
      map njeBookmarkCursorToNjeLink njeBookmarks
    _ -> undefined
  where cursor = XMLC.fromDocument doc
