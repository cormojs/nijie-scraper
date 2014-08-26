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
import qualified Data.UnixTime      as UnixTime

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



njeEndpoint :: String -> String
njeEndpoint s = "http://nijie.info/" ++ s ++ ".php"



njeLogin :: ByteString -> ByteString ->
            IO (HTTP.CookieJar, HTTP.Response BSLazy.ByteString)
njeLogin email password = do
  let query  = [ ("email", email)
               , ("password", password)
               , ("save", "on")
               , ("ticket", "") ]
  cookie <- fetchCookie
  time     <- Clock.getCurrentTime
  manager  <- HTTP.newManager HTTP.conduitManagerSettings
  request  <- HTTP.parseUrl $ njeEndpoint login
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
      request  <- HTTP.parseUrl $ njeEndpoint "login"
      manager  <- HTTP.newManager HTTP.conduitManagerSettings
      response <- HTTP.httpLbs request manager
      time     <- Clock.getCurrentTime
      return $ fst
       $ Client.updateCookieJar response request time (HTTP.createCookieJar [])


njeLoginSave :: FilePath -> ByteString -> ByteString -> IO ()
njeLoginSave filename email password = do
  (cookie, response) <- njeLogin email password
  saveJSONToFile filename cookie

sessionCookie :: IO HTTP.CookieJar
sessionCookie = loadJSONFromFile "session.json"
