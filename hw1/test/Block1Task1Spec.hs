module Block1Task1Spec where

import Test.Hspec (SpecWith, describe, shouldBe, it)
import Block1Task1
nextDayTest :: SpecWith()
nextDayTest =
  describe "Block1Task1.nextDay" $ do
  it "Returns the next day after given" $ nextDay Wednesday `shouldBe` Thursday
  it "Return the next day after Sunday (cycle week test)" $ nextDay Sunday `shouldBe` Monday

afterDaysTest :: SpecWith()
afterDaysTest =
  describe "Block1Task1.afterDays" $ do
  it "Returns the day after current in x days" $ afterDays Monday 5 `shouldBe` Saturday
  it "Returns the day in a week to check cycle" $ afterDays Monday 7 `shouldBe` Monday


isWeekendTest :: SpecWith()
isWeekendTest =
  describe "Block1Task1.isWeekend" $ do
    it "Test non weekend" $ isWeekend Monday  `shouldBe` False
    it "Test weekend" $ isWeekend Saturday `shouldBe` True


daysToPartyTest :: SpecWith()
daysToPartyTest =
  describe "Block1Task1.daysToParty" $ do
    it "returns the day after current in x days" $ daysToParty Monday  `shouldBe` 4
    it "returns the day in a week to check cycle" $ daysToParty Saturday  `shouldBe` 6
    it "Tests the same day (Friday)" $ daysToParty Friday  `shouldBe` 0
