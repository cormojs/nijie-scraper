{-# LANGUAGE OverloadedStrings #-}
module Web.Nijie
       ( njeStreamBS
       , njeDoc
       , njeLinksFromThumbs
       , njeSaveUrl
       , njeSaveTopIllust
       , njeIllustUrls
       , njeBookmarkAdd
       , njeNuitaAdd
       , njeRightAuthor
       , njeTagsFromDoc
       , njeDescriptionFromDoc
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
import qualified Network.HTTP.Client  as Client

-- html-conduit
import Text.XML.Cursor (($//), (&//), (>=>))
import qualified Text.HTML.DOM   as HTML
import qualified Text.XML        as XML
import qualified Data.XML.Types  as XMLTypes
import qualified Text.XML.Cursor as XMLC
import qualified Text.Blaze.Html as BlazeHtml (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- Date
import qualified Data.Time.Clock    as Clock
import qualified Data.Time.Calendar as Calendar

-- Control
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans as Trans
import Control.Applicative ((<$>))

-- ioref
import Data.IORef (IORef)
import qualified Data.IORef as IORef

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Lazy  as BSLazy
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as Char8Lazy
import qualified Codec.Binary.UTF8.String   as UTF8Codec

-- text
import Data.Text  (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as TextIO
import qualified Data.Text.Encoding    as TextEnc
import qualified Data.Text.Lazy        as TextL
import qualified Data.Text.Lazy.IO     as TextLIO

-- system
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath  as FilePath

-- regex-posix
import Text.Regex.Posix ((=~))


---
-- test
test' = do
  doc <- njeDoc $ NjeView "96144"
  renderFile "out.html" doc


test'' :: IO ()
test'' = Resource.runResourceT $ do
  let (api, query) = njeApiToQuery $ NjeFav 1
  event <- njeStreamEvent api query
  event $$+- sink
  where 
    sink = do
      mval <- await
      case mval of
        Nothing  -> return ()
        Just val -> do
          Trans.liftIO $ go val
          sink
    go (XMLTypes.EventBeginElement e _) = print e
    go _ = return ()

---
-- add an illust to your bookmarks
njeBookmarkAdd (NjeLink { njeId = id }) = postForm id $ NjeFavAdd  id
njeNuitaAdd    (NjeLink { njeId = id }) = postForm id $ NjeNuiAdd  id
njeGoodAdd     (NjeLink { njeId = id }) = postForm id $ NjeGoodAdd id

postForm id njeApi = do
  let (api, query) = njeApiToQuery njeApi
  request <- HTTP.parseUrl $ njeEndpoint api
  cookie  <- sessionCookie
  let request' = request { HTTP.cookieJar = Just cookie
                         , HTTP.requestBody =
                           HTTP.RequestBodyBS
                           $ Types.renderSimpleQuery False query
                         , HTTP.method = Types.methodPost
                         , HTTP.requestHeaders =
                           [ (Types.hContentType,
                              "application/x-www-form-urlencoded")
                           , ("X-Requested-With", "XMLHttpRequest")
                           , (Types.hConnection, "keep-alive")
                           , (Types.hReferer,
                              toUrl $ njeApiToQuery (NjeView id))]
                         }
  manager <- HTTP.newManager HTTP.conduitManagerSettings
  response <- HTTP.httpLbs request' manager
  where toUrl (url, q) = Char8.append
                         (Char8.pack $ njeEndpoint url)
                         (Types.renderSimpleQuery True q)



---
-- Save illust

njeSaveTopIllust :: FilePath -> NjeLink -> IO FilePath
njeSaveTopIllust saveDir link@(NjeLink { njeId = id }) = do
  (imageUrl:_) <- njeIllustUrls link
  njeSaveUrl (saveDir </> Char8.unpack id) imageUrl


njeRightAuthor :: NjeLink -> IO NjeUser
njeRightAuthor (NjeLink { njeId = id }) = do
  cursor <- XMLC.fromDocument <$> njeDoc (NjeView id)
  let [authE] = cursor
               $// XMLC.attributeIs "class" "user_icon"
               &// XMLC.element "a"
      [nameE] = cursor
               $// XMLC.attributeIs "class" "user_icon"
               &// XMLC.element "img"
  let userId = toAuthorId $ head $ XMLC.attribute "href" authE
      name = TextEnc.encodeUtf8 $ head $ XMLC.attribute "alt" nameE
  return $ NjeUser name userId

njeIllustUrls :: NjeLink -> IO [String]
njeIllustUrls (NjeLink { njeId = id, njeKind = kind }) = do
  cursor <- XMLC.fromDocument <$> njeDoc (NjeView id)
  let links = getLinks kind cursor
      urls  = map (XMLC.attribute "href") links
  return $ map (("http:"++) . Text.unpack . head) urls
  where getLinks NjeDoujin c = c $// XMLC.attributeIs "class" "dojin_gallery"
        getLinks _ c         = c $// XMLC.attributeIs "id" "gallery_open"
                                 &// XMLC.element "a"

njeSaveUrl :: FilePath -> String -> IO FilePath
njeSaveUrl filename url = Resource.runResourceT $ do
  let filename' = case FilePath.takeExtension url of
        "." -> filename ++ ".jpg"
        ext -> filename ++ ext
  exists <- Trans.liftIO $ Dir.doesFileExist filename'
  Monad.unless exists $ do
    (request, manager) <- reqman
    body <- HTTP.responseBody <$> HTTP.http request manager
    body $$+- CB.sinkFile filename'
  return filename'
    where
      reqman = Trans.liftIO $ do
        cookie  <- sessionCookie
        request <- HTTP.parseUrl url
        let request' = request { HTTP.cookieJar = Just cookie }
        manager <- HTTP.newManager HTTP.conduitManagerSettings
        return ( request', manager)


---
-- API access

njeStreamBS :: MonadResource m =>
             String -> Types.SimpleQuery ->
             m (C.ResumableSource m ByteString)
njeStreamBS api query = do
  cookie <- Trans.liftIO sessionCookie
  request <- Trans.liftIO $ HTTP.parseUrl $ njeEndpoint api
  let request' = request { HTTP.cookieJar = Just cookie
                         , HTTP.queryString = Types.renderSimpleQuery True query }
  manager  <- Trans.liftIO $ HTTP.newManager HTTP.conduitManagerSettings
  response <- HTTP.http request' manager
  Trans.liftIO $ update cookie request response
  return $ HTTP.responseBody response
  where
    update cookie request response = do
      time <- Clock.getCurrentTime
      let cookie' =
            fst $ Client.updateCookieJar response request time cookie
      saveJSONToFile "session.json" cookie'

njeStreamEvent :: MonadResource m =>
                  String -> Types.SimpleQuery ->
                  m (C.ResumableSource m XMLTypes.Event)
njeStreamEvent api query = do
  body <- njeStreamBS api query
  return $ body $=+ HTML.eventConduit

njeDoc :: NjeAPI -> IO XML.Document
njeDoc njeApi = Resource.runResourceT $ do
  let (api, query) = njeApiToQuery njeApi
  body <- njeStreamBS api query
  body $$+- HTML.sinkDoc

fetchFavsDoc :: Int -> IO XML.Document
fetchFavsDoc p = njeDoc (NjeLike p)

render :: [XMLC.Cursor] -> TextL.Text
render cs = TextL.concat $ map (renderHtml . BlazeHtml.toHtml . XMLC.node) cs

renderFile filename doc =
  TextLIO.writeFile filename $ renderHtml $ BlazeHtml.toHtml doc

