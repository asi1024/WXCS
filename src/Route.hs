{-# LANGUAGE OverloadedStrings #-}

module Route (
  SiteRoute(..),
  renderUrl
  ) where

import Data.Text (Text())
import qualified Data.Text as T

import Config

data SiteRoute =
  TopR
  | StatusR
  | StatisticsR
  | ContestR Text -- contest id
  | StandingsR Text -- contest id
  | RankingR
  | UserR String -- user
  | ProblemR String -- user
  | SubmitR
  | SetContestR
  | EditContestR Text -- contest id
  | SourceR Text -- source id
  | RejudgeR Text -- submit id
  | CssR Text -- file name

mkQueryString :: [(Text, Text)] -> Text
mkQueryString dic =
  let str = T.intercalate "&" $ map (\(key, value) -> T.concat [key, "=", value]) dic in
   if T.null str
   then ""
   else T.append "?" str

renderUrl :: Configuration -> SiteRoute -> [(Text, Text)] -> Text
renderUrl conf TopR _ = pass_prefix conf
renderUrl conf StatusR q = T.concat [pass_prefix conf, "status", mkQueryString q]
renderUrl conf (ContestR cid) _ = T.concat [pass_prefix conf, "contest/", cid]
renderUrl conf (StandingsR cid) _ = T.concat [pass_prefix conf, "standings/", cid]
renderUrl conf RankingR _ = T.concat [pass_prefix conf, "ranking"]
renderUrl conf (UserR user) _ = T.concat [pass_prefix conf, "user/", T.pack user]
renderUrl conf (ProblemR user) _ = T.concat [pass_prefix conf, "problem/", T.pack user]
renderUrl conf SubmitR _ = T.concat [pass_prefix conf, "submit"]
renderUrl conf SetContestR _ = T.concat [pass_prefix conf, "setcontest"]
renderUrl conf (EditContestR cid) _ = T.concat [pass_prefix conf, "setcontest/", cid]
renderUrl conf (SourceR sid) _ = T.concat [pass_prefix conf, "source/", sid]
renderUrl conf (RejudgeR sid) _ = T.concat [pass_prefix conf, "rejudge/", sid]
renderUrl conf (CssR file) _ = T.concat [pass_prefix conf, "css/", file]
