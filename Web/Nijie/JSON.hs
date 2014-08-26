{-# LANGUAGE TemplateHaskell #-}
module Web.Nijie.JSON where

-- internal
import Web.Nijie.Types

-- http
import Network.HTTP.Conduit (Cookie(), CookieJar())

-- Control
import qualified Control.Monad.Trans as Trans
import Control.Applicative ((<$>))

-- Data
import Data.ByteString (ByteString)
import Data.Maybe      (fromJust)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Encode.Pretty (encodePretty)

instance Aeson.FromJSON ByteString where
  parseJSON jsonstr = Char8.pack <$> Aeson.parseJSON jsonstr
instance Aeson.ToJSON ByteString where
  toJSON bstr = Aeson.toJSON $ Char8.unpack bstr

$(deriveJSON defaultOptions ''Cookie)
$(deriveJSON defaultOptions ''CookieJar)

$(deriveJSON defaultOptions ''NjeLogin)

loadJSONFromFile :: Aeson.FromJSON a => FilePath -> IO a
loadJSONFromFile filename = do
  result <- Aeson.fromJSON <$> loadJSONValue filename
  case result of
    Aeson.Success s -> return s
    Aeson.Error e   -> fail e
  where loadJSONValue :: FilePath -> IO Aeson.Value
        loadJSONValue filename = fromJust <$> Aeson.decode <$> LazyChar8.readFile filename

saveJSONToFile :: Aeson.ToJSON a => FilePath -> a -> IO ()
saveJSONToFile filename val =
  LazyChar8.writeFile filename $ encodePretty $ Aeson.toJSON val

