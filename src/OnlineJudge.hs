module OnlineJudge where

import Data.Time (UTCTime)

import qualified OnlineJudge.Aoj as Aoj

submit :: String -- Judge type
          -> UTCTime -- submit time
          -> String -- user name
          -> Int -- problem id
          -> String -- language
          -> String -- code
          -> IO ()
submit judge_type =
  if judge_type == "Aoj"
  then Aoj.submit
  else Aoj.submit -- TODO: error?
