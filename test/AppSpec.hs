{-# LANGUAGE FlexibleInstances #-}
module AppSpec where

import ModelTypes
import Model
import App
import SpecHelper

spec :: Spec
spec = do
  describe "getUsers" $ do
    it "should return empty list with empty list" $ do
      getUsers [] `shouldBe` []

    it "should return user list" $ do
      getUsers [ def { submitUserId = "user" } ] `shouldBe` [ "user" ]

    it "should return unique user list" $ do
      getUsers [ def { submitUserId = "user" },
                 def { submitUserId = "user" } ] `shouldBe` [ "user" ]

    it "should return all unique users" $ do
      getUsers [ def { submitUserId = "user1" },
                 def { submitUserId = "user2" },
                 def { submitUserId = "user2" },
                 def { submitUserId = "user3" } ] `shouldBe` [ "user1", "user2", "user3" ]

  describe "getACTime" $ do
    context "user already have got AC" $ do
      it "should return the difference in minute between AC time and now" $ do
        pending

    context "user have not got AC yet" $ do
      let submits = [ def { submitUserId = "user", submitJudge = WrongAnswer, submitProblemId = "1" },
                      def { submitUserId = "user2", submitJudge = Accepted, submitProblemId = "1" },
                      def { submitUserId = "user", submitJudge = CompileError, submitProblemId = "1" } ]
      it "should return 0" $ do
        getACTime submits "user" "1" `shouldBe` 0

  describe "getWA" $ do
    context "user already have got AC" $ do
      it "should return the number of attempts which isn't accepted before getting AC." $ do
        pending

    context "user have not got AC yet" $ do
      it "should return the number of attempts which isn't accepted" $ do
        let submits = [ def { submitUserId = "user", submitJudge = WrongAnswer, submitProblemId = "1" },
                      def { submitUserId = "user2" },
                      def { submitUserId = "user", submitJudge = CompileError, submitProblemId = "1" },
                      def { submitUserId = "user", submitProblemId = "2" } ]
        getWA submits "user" "1" `shouldBe` 2
