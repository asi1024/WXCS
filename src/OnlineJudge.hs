module OnlineJudge where

import qualified OnlineJudge.Aoj as Aoj

import Config

submit :: Configuration
          -> String -- Judge type
          -> String -- problem id
          -> String -- language
          -> String -- code
          -> IO Bool
submit conf judgeType pid lang code = do
  putStrLn ("judge type = " ++ judgeType)
  if judgeType == "Aizu"
    then Aoj.submit (aojUser conf) (aojPass conf) pid lang code
    else return False

fetchResult :: Configuration
               -> String -- Judge type
               -> String -- problem id
               -> IO (Maybe (String, String, String))
fetchResult conf judge pid =
  if judge == "Aizu"
  then Aoj.fetch (aojUser conf) pid
  else return $ Just ("Accept", "0.01", "10")
