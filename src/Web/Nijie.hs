{-# LANGUAGE OverloadedStrings #-}
module Web.Nijie
       ( getRawStream
       , getDocument
       , getLinks
       , saveUrl
       , saveTopIllust
       , getIllustUrls
       , postBookmarkAdd
       , postNuitaAdd
       , getRightAuthor
       , getTags
       , getDescription
       , getNextPage
       , getSize
       , toggleSortAPI
       ) where



-- internal
import Web.Nijie.JSON
import Web.Nijie.Login
import Web.Nijie.Types
import Web.Nijie.Parser

-- conduit
import Data.Conduit as Conduit
import qualified Data.Conduit.Binary as ConduitBinary
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
-- api auxiliary functions

toggleSortAPI :: ListAPI -> ListAPI
toggleSortAPI (Search tag sort page) = Search tag (toggleSort sort) page
toggleSortAPI (Fav sort page) = Fav (toggleSort sort) page
toggleSortAPI api = api

---
-- add an illust to your bookmarks
postBookmarkAdd (Link { illustId = id }) = postForm id $ FavAdd  id
postNuitaAdd    (Link { illustId = id }) = postForm id $ NuiAdd  id
postGoodAdd     (Link { illustId = id }) = postForm id $ GoodAdd id

postForm :: ByteString -> PostAPI -> IO ()
postForm id njeApi = do
  let (api, query) = convertAPIToQuery njeApi
  request <- HTTP.parseUrl $ endpoint api
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
                              toUrl $ convertAPIToQuery (View id))]
                         }
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  response <- HTTP.httpLbs request' manager
  return ()
  where toUrl (url, q) = Char8.append
                         (Char8.pack $ endpoint url)
                         (Types.renderSimpleQuery True q)



---
-- Save illust

saveTopIllust :: FilePath -> Link -> IO FilePath
saveTopIllust saveDir link@(Link { illustId = id }) = do
  (imageUrl:_) <- getIllustUrls link
  saveUrl (saveDir </> Char8.unpack id) imageUrl


getRightAuthor :: Link -> IO User
getRightAuthor (Link { illustId = id }) = do
  cursor <- XMLC.fromDocument <$> getDocument (View id)
  let [authE] = cursor
               $// XMLC.attributeIs "class" "user_icon"
               &// XMLC.element "a"
      [nameE] = cursor
               $// XMLC.attributeIs "class" "user_icon"
               &// XMLC.element "img"
  let userId = toAuthorId $ head $ XMLC.attribute "href" authE
      name = TextEnc.encodeUtf8 $ head $ XMLC.attribute "alt" nameE
  return $ User name userId

getIllustUrls :: Link -> IO [String]
getIllustUrls (Link { illustId = id, kind = Doujin }) = do
  cursor <- XMLC.fromDocument <$> getDocument (View id)
  let links = cursor $// XMLC.attributeIs "class" "dojin_gallery"
      urls  = map (XMLC.attribute "href") links
  return $ map (("http:"++) . Text.unpack . head) urls
getIllustUrls (Link { illustId = id }) = do
  cursor <- XMLC.fromDocument <$> getDocument (ViewPopup id)
  let imgs = cursor $// XMLC.attributeIs "id" "img_window"
                    &// XMLC.attributeIs "class" "box-shadow999"
      urls  = map (XMLC.attribute "src") imgs
  return $ map (("http:"++) . Text.unpack . head) urls

saveUrl :: FilePath -> String -> IO FilePath
saveUrl filename url = Resource.runResourceT $ do
  let filename' = case FilePath.takeExtension url of
        "." -> filename ++ ".jpg"
        ext -> filename ++ ext
  exists <- Trans.liftIO $ Dir.doesFileExist filename'
  Monad.unless exists $ do
    (request, manager) <- reqman
    body <- HTTP.responseBody <$> HTTP.http request manager
    body $$+- ConduitBinary.sinkFile filename'
  return filename'
    where
      reqman = Trans.liftIO $ do
        cookie  <- sessionCookie
        request <- HTTP.parseUrl url
        let request' = request { HTTP.cookieJar = Just cookie }
        manager <- HTTP.newManager HTTP.tlsManagerSettings
        return ( request', manager)


---
-- API access

getRawStream :: MonadResource m =>
             String -> Types.SimpleQuery ->
             m (Conduit.ResumableSource m ByteString)
getRawStream api query = do
  cookie <- Trans.liftIO sessionCookie
  request <- Trans.liftIO $ HTTP.parseUrl $ endpoint api
  let request' = request { HTTP.cookieJar = Just cookie
                         , HTTP.queryString = Types.renderSimpleQuery True query }
  manager  <- Trans.liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  response <- HTTP.http request' manager
  Trans.liftIO $ update cookie request response
  return $ HTTP.responseBody response
  where
    update cookie request response = do
      time <- Clock.getCurrentTime
      let cookie' =
            fst $ Client.updateCookieJar response request time cookie
      saveJSONToFile "session.json" cookie'

getEventStream :: MonadResource m =>
                  String -> Types.SimpleQuery ->
                  m (Conduit.ResumableSource m XMLTypes.Event)
getEventStream api query = do
  body <- getRawStream api query
  return $ body $=+ HTML.eventConduit

getDocument :: API api => api -> IO XML.Document
getDocument njeApi = Resource.runResourceT $ do
  let (api, query) = convertAPIToQuery njeApi
  body <- getRawStream api query
  body $$+- HTML.sinkDoc


fetchFavsDoc :: Int -> IO XML.Document
fetchFavsDoc p = getDocument (Like p)

render :: [XMLC.Cursor] -> TextL.Text
render cs = TextL.concat $ map (renderHtml . BlazeHtml.toHtml . XMLC.node) cs

renderFile filename doc =
  TextLIO.writeFile filename $ renderHtml $ BlazeHtml.toHtml doc
