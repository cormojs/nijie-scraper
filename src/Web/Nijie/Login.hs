{-# LANGUAGE OverloadedStrings #-}
module Web.Nijie.Login where

-- internal
import Web.Nijie.JSON

-- http-conduit
import Network.HTTP.Conduit (Cookie(..))
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types   as Types
import qualified Network.HTTP.Client  as Client

-- Date
import qualified Data.Time.Clock    as Clock
import qualified Data.Time.Calendar as Calendar

-- Control
import qualified Control.Monad.Trans as Trans
import Control.Applicative ((<$>))

-- Data
import Data.ByteString (ByteString)
import Data.Text.Lazy  (Text)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Lazy  as BSLazy
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Lazy        as Text
import qualified Data.Text.Lazy.IO     as TextIO



endpoint :: String -> String
endpoint s = "http://nijie.info/" ++ s ++ ".php"



login :: ByteString -> ByteString ->
            IO (HTTP.CookieJar, HTTP.Response BSLazy.ByteString)
login email password = do
  let query  = [ ("email", email)
               , ("password", password)
               , ("save", "on")
               , ("ticket", "") ]
  cookie <- fetchCookie
  time     <- Clock.getCurrentTime
  manager  <- HTTP.newManager HTTP.tlsManagerSettings
  request  <- HTTP.parseUrl $ endpoint login
  response <- HTTP.httpLbs (request { HTTP.cookieJar = Just cookie
                                    , HTTP.requestHeaders =
                                      [ (Types.hContentType,
                                         "application/x-www-form-urlencoded")
                                      ]
                                    , HTTP.requestBody =
                                        HTTP.RequestBodyBS
                                        $ Types.renderSimpleQuery False query
                                    , HTTP.method = Types.methodPost })

               manager
  return $ Client.updateCookieJar response request time cookie
  where
    login = "login_int"
    fetchCookie = do
      request  <- HTTP.parseUrl $ endpoint "login"
      manager  <- HTTP.newManager HTTP.tlsManagerSettings
      response <- HTTP.httpLbs request manager
      time     <- Clock.getCurrentTime
      return $ fst
       $ Client.updateCookieJar response request time (HTTP.createCookieJar [])


loginSave :: FilePath -> ByteString -> ByteString -> IO ()
loginSave filename email password = do
  (cookie, response) <- login email password
  saveJSONToFile filename cookie

sessionCookie :: IO HTTP.CookieJar
sessionCookie = loadJSONFromFile "session.json"
