{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative (empty, (<$>), (<*>))

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default

import System.Directory (doesFileExist)

data Configuration = Configuration {
  port :: Int,
  aojUser :: String,
  aojPass :: String
  } deriving (Eq, Show)

instance Default Configuration where
  def = Configuration {
    port = 16384,
    aojUser = "",
    aojPass = ""
    }

instance AE.ToJSON Configuration where
  toJSON (Configuration port' user pass) =
    AE.object [
      "port" AE..= port',
      "aojUser" AE..= user,
      "aojPass" AE..= pass
      ]

instance AE.FromJSON Configuration where
  parseJSON (AE.Object v) = Configuration <$>
                            v AE..: "port" <*>
                            v AE..: "aojUser" <*>
                            v AE..: "aojPass"
  parseJSON _ = empty

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig filepath = do
  existp <- doesFileExist filepath
  if existp then loadConfig' filepath else return Nothing
  where loadConfig' fp = do
          content <- BL.readFile fp
          return $ AE.decode content
