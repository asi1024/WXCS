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

data CodeforcesConf = CodeforcesConf {
  x_user :: String,
  csrf_token :: String
  } deriving (Eq, Show)

data Configuration = Configuration {
  port :: Int,
  aoj :: AojConf,
  codeforces :: CodeforcesConf,
  db :: Text
  } deriving (Eq, Show)

instance Default Configuration where
  def = Configuration {
    port = 16384,
    aoj = AojConf "" "",
    codeforces = CodeforcesConf "" "",
    db = "db.sqlite"
    }

instance AE.ToJSON AojConf where
  toJSON (AojConf user' pass') =
    AE.object ["user" AE..= user', "pass" AE..= pass']

instance AE.ToJSON CodeforcesConf where
  toJSON (CodeforcesConf x_user' csrf_token') =
    AE.object ["x_user" AE..= x_user', "pass" AE..= csrf_token']

instance AE.ToJSON Configuration where
  toJSON (Configuration port' aoj' codeforces' db') =
    AE.object ["port" AE..= port', "aoj" AE..= aoj',
               "codeforces" AE..= codeforces', "db" AE..= db']

instance AE.FromJSON AojConf where
  parseJSON (AE.Object v) = AojConf <$>
                            v AE..: "user" <*>
                            v AE..: "pass"
  parseJSON _ = empty

instance AE.FromJSON CodeforcesConf where
  parseJSON (AE.Object v) = CodeforcesConf <$>
                            v AE..: "x_user" <*>
                            v AE..: "csrf_token"
  parseJSON _ = empty

instance AE.FromJSON Configuration where
  parseJSON (AE.Object v) = Configuration <$>
                            v AE..: "port" <*>
                            v AE..: "aoj" <*>
                            v AE..: "codeforces" <*>
                            v AE..: "db"
  parseJSON _ = empty

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig filepath = do
  existp <- doesFileExist filepath
  if existp then loadConfig' filepath else return Nothing
  where loadConfig' fp = do
          content <- BL.readFile fp
          return $ AE.decode content
