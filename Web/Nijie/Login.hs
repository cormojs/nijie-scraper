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


njeCk :: [Cookie]
njeCk = [ njeCkTemp { cookie_domain  = ".nijie.info"
                    , cookie_name    = "NIJIEIJIEID"
                    , cookie_value   = "8h5piofai37knoll587gok2506" }
        , njeCkTemp { cookie_domain  = ".nijie.info"
                    , cookie_name    = "R18"
                    , cookie_value   = "1" }
        , njeCkTemp { cookie_domain  = "nijie.info"
                    , cookie_name    = "nijie_login_hash"
                    , cookie_value   = "6db83674709be0a94df80ec3ab1a9d7131129dbd" }
        , njeCkTemp { cookie_domain  = "nijie.info"
                    , cookie_name    = "nijie_email_hash"
                    , cookie_value   = "515e44b4e37c429690c485302ef4e3f803297cba" }
        , njeCkTemp { cookie_domain  = "nijie.info"
                    , cookie_name    = "nijie_referer"
                    , cookie_value   = "nijie.info" }
        ]

njeCkTemp = Cookie { cookie_name = ""
                   , cookie_value = ""
                   , cookie_expiry_time = future
                   , cookie_domain = "nijie.info"
                   , cookie_path   = "/"
                   , cookie_creation_time    = past
                   , cookie_last_access_time = past
                   , cookie_persistent  = True
                   , cookie_host_only   = True
                   , cookie_secure_only = True
                   , cookie_http_only   = True }
  where past   = Clock.UTCTime (Calendar.ModifiedJulianDay 56200)
                               (Clock.secondsToDiffTime 0)
        future = Clock.UTCTime (Calendar.ModifiedJulianDay 562000)
                               (Clock.secondsToDiffTime 0)

njeEndpoint :: String -> String
njeEndpoint s = "https://nijie.info/" ++ s ++ ".php"

njeLogin :: ByteString -> ByteString ->
            IO (HTTP.CookieJar, HTTP.Response BSLazy.ByteString)
njeLogin email password = do
  tick <- Char8.pack <$> show <$> UnixTime.getUnixTime
  let tokens = [ njeCkTemp { cookie_name  = "nijie_token"
                           , cookie_value = tick }
               , njeCkTemp { cookie_name  = "nijie_token_secret"
                           , cookie_value = tick }]
      cookie = HTTP.createCookieJar $ tokens ++ njeCk
      query  = [ ("email", email)
               , ("password", password)
               , ("save", "on") ]
  time     <- Clock.getCurrentTime
  manager  <- HTTP.newManager HTTP.conduitManagerSettings
  request  <- HTTP.parseUrl $ njeEndpoint login
  response <- HTTP.httpLbs (request { HTTP.cookieJar = Just cookie
                                    , HTTP.method = Types.methodPost
                                    , HTTP.queryString =
                                         Types.renderSimpleQuery True query })
               manager
  return $ Client.updateCookieJar response request time cookie
  where login = "login_int"

njeLoginSave :: FilePath -> ByteString -> ByteString -> IO ()
njeLoginSave filename email password = do
  (cookie, response) <- njeLogin email password
  saveJSONToFile filename cookie

sessionCookie :: IO HTTP.CookieJar
sessionCookie = loadJSONFromFile "session.json"
