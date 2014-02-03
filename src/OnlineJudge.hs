module OnlineJudge where

import Data.Time (UTCTime)

import qualified OnlineJudge.Aoj as Aoj

submit :: String -- Judge type
          -> Int -- problem id
          -> String -- language
          -> String -- code
          -> IO Bool
submit judge_type = do
  if judge_type == "Aoj"
    then Aoj.submit
    else false
  false
  where false _ _ _ = return False
