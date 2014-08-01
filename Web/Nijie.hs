{-# LANGUAGE OverloadedStrings #-}
module Web.Nijie () where

-- internal
import Web.Nijie.JSON
import Web.Nijie.Login
import Web.Nijie.Types
import Web.Nijie.Parser

-- conduit
import Data.Conduit as C
import qualified Data.Conduit.Binary as Binary
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

-- Data
import Data.ByteString (ByteString)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Lazy  as BSLazy
import qualified Data.ByteString.Char8 as Char8
import qualified Codec.Binary.UTF8.String   as UTF8Codec

import Data.Text  (Text)
import qualified Data.Text        as Text
import qualified Data.Text.IO     as TextIO
import qualified Data.Text.Lazy        as TextL
import qualified Data.Text.Lazy.IO     as TextLIO


njeStream :: MonadResource m =>
             String -> Types.SimpleQuery ->
             m (C.ResumableSource m ByteString)
njeStream api query = do
  cookie <- Trans.liftIO $ sessionCookie 
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

getMainLeft :: XML.Document -> [XMLC.Cursor]
getMainLeft doc =
  XMLC.fromDocument doc
  $// XMLC.attributeIs "id" "main-left-main"
  &// XMLC.attributeIs "class" "nijie"

render :: [XMLC.Cursor] -> TextL.Text
render cs = TextL.concat $ map (renderHtml . BlazeHtml.toHtml . XMLC.node) cs

getNjeLinks :: XML.Document -> [NjeLink]
getNjeLinks doc = map njeCursorToNjeLink $ getMainLeft doc


test n = do
  doc <- HTML.readFile "out.html"
  print $ take n $ getNjeLinks doc
  
