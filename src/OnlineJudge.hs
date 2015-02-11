module OnlineJudge where

import qualified OnlineJudge.Aoj as Aoj

import Config
import ModelTypes

submit :: Configuration
          -> JudgeType -- Judge type
          -> String -- problem id
          -> String -- language
          -> String -- code
          -> IO Bool
submit conf Aizu = Aoj.submit (aoj conf)
submit conf Codeforces = undefined

fetchByRunId :: Configuration
                -> JudgeType -- judge type
                -> Int -- run id
                -> IO (Maybe (JudgeStatus, String, String))
fetchByRunId conf Aizu = Aoj.fetchByRunId (aoj conf)
fetchByRunId conf Codeforces = undefined

getLatestRunId :: Configuration
                  -> JudgeType -- judge type
                  -> IO Int
getLatestRunId conf Aizu = Aoj.getLatestRunId (aoj conf)
getLatestRunId conf Codeforces = undefined

getDescriptionURL :: JudgeType
                     -> String -- problem id
                     -> String
getDescriptionURL Aizu n =
  "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=" ++ n
getDescriptionURL Codeforces n =
  if length n < 2
  then undefined
  else "http://codeforces.com/contest/" ++ init n ++ "/problem/" ++ [last n]
