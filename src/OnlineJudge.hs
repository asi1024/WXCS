module OnlineJudge where

import Data.Time (UTCTime)

import qualified OnlineJudge.Aoj as Aoj

submit :: String -- Judge type
          -> Int -- problem id
          -> String -- language
          -> String -- code
          -> IO Bool
submit judgeType pid lang code = do
  putStrLn ("judge type = " ++ judgeType)
  if judgeType == "Aizu"
    then Aoj.submit pid lang code
    else return False

fetchResult :: String -- Judge type
               -> Int -- problem id
               -> IO (Maybe (String, String, String))
fetchResult judge pid =
  if judge == "Aizu"
  then Aoj.fetch pid
  else return $ Just ("Accept", "0.01", "10")