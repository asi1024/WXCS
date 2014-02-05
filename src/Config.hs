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
  toJSON (Configuration port aojUser aojPass) =
    AE.object [
      "port" AE..= port,
      "aojUser" AE..= aojUser,
      "aojPass" AE..= aojPass
      ]

instance AE.FromJSON Configuration where
  parseJSON (AE.Object v) = Configuration <$>
                            v AE..: "port" <*>
                            v AE..: "aojUser" <*>
                            v AE..: "aojPass"
  parseJSON _ = empty

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig fp = do
  existp <- doesFileExist fp
  if existp then loadConfig' fp else return Nothing
  where loadConfig' fp = do
          content <- BL.readFile fp
          return $ AE.decode content
