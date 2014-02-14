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
submit conf Aizu pid lang code = Aoj.submit (aoj conf) pid lang code

fetchResult :: Configuration
               -> JudgeType-- Judge type
               -> String -- problem id
               -> IO (Maybe (JudgeStatus, String, String))
fetchResult conf Aizu pid = Aoj.fetch (aoj conf) pid
