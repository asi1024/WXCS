{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative (empty, (<$>), (<*>))

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import Data.Text (Text())

import System.Directory (doesFileExist)

data AojConf = AojConf {
  user :: String,
  pass :: String
  } deriving (Eq, Show)

data Configuration = Configuration {
  port :: Int,
  aoj :: AojConf,
  db :: Text,
  pass_prefix :: Text
  } deriving (Eq, Show)

instance Default Configuration where
  def = Configuration {
    port = 16384,
    aoj = AojConf "" "",
    db = "db.sqlite",
    pass_prefix = "/"
    }

instance AE.ToJSON AojConf where
  toJSON (AojConf user' pass') =
    AE.object ["user" AE..= user', "pass" AE..= pass']

instance AE.ToJSON Configuration where
  toJSON (Configuration port' aoj' db' prefix') =
    AE.object ["port" AE..= port', "aoj" AE..= aoj', "db" AE..= db',
               "prefix" AE..= prefix']

instance AE.FromJSON AojConf where
  parseJSON (AE.Object v) = AojConf <$>
                            v AE..: "user" <*>
                            v AE..: "pass"
  parseJSON _ = empty

instance AE.FromJSON Configuration where
  parseJSON (AE.Object v) = Configuration <$>
                            v AE..: "port" <*>
                            v AE..: "aoj" <*>
                            v AE..: "db" <*>
                            v AE..: "prefix"
  parseJSON _ = empty

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig filepath = do
  existp <- doesFileExist filepath
  if existp then loadConfig' filepath else return Nothing
  where loadConfig' fp = do
          content <- BL.readFile fp
          return $ AE.decode content
