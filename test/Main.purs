module Test.Main where

import Prelude

import Data.BigInt as BI
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import IntcodeBoost (runOutput, parseInput) as ICB
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "IntcodeBoost-spec" do
    describe "InputOutput" do
      describe "outputs" do
        it "can output referenced values" do
          let prog = "4,0,99"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [] testProg `shouldEqual` [(BI.fromInt 4)]
        it "can output immediate values" do
          let prog = "104,0,99"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [] testProg `shouldEqual` [(BI.fromInt 0)]
        it "can output relative values" do
          let prog = "204,2,99"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [] testProg `shouldEqual` [(BI.fromInt 99)]
        it "can output relative values with changed relative base" do
          let prog = "9,2,204,-2,99"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [] testProg `shouldEqual` [(BI.fromInt 9)]
      describe "inputs" do
        it "can store input to referenced location" do
          let prog = "3,5,4,5,99"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [BI.fromInt 42] testProg `shouldEqual`
            [(BI.fromInt 42)]
        it "can store input to relative location" do
          let prog = "9,2,203,10,204,10,99"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [BI.fromInt 42] testProg `shouldEqual`
            [(BI.fromInt 42)]
    describe "math" do
      describe "comparisons" do
        it "can test for equality using position mode" do
          let prog = "3,9,8,9,10,9,4,9,99,-1,8"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [BI.fromInt 42] testProg `shouldEqual`
            [(BI.fromInt 0)]
          ICB.runOutput [BI.fromInt 8] testProg `shouldEqual`
            [(BI.fromInt 1)]
        it "can test for equality using immediate mode" do
          let prog = "3,3,1108,-1,8,3,4,3,99"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [BI.fromInt 42] testProg `shouldEqual`
            [(BI.fromInt 0)]
          ICB.runOutput [BI.fromInt 8] testProg `shouldEqual`
            [(BI.fromInt 1)]
        it "can test for inequality using position mode" do
          let prog = "3,9,7,9,10,9,4,9,99,-1,8"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [BI.fromInt 42] testProg `shouldEqual`
            [(BI.fromInt 0)]
          ICB.runOutput [BI.fromInt 8] testProg `shouldEqual`
            [(BI.fromInt 0)]
          ICB.runOutput [BI.fromInt 7] testProg `shouldEqual`
            [(BI.fromInt 1)]
        it "can test for inequality using immediate mode" do
          let prog = "3,3,1107,-1,8,3,4,3,99"
          let testProg = fromMaybe [] (ICB.parseInput prog)
          ICB.runOutput [BI.fromInt 42] testProg `shouldEqual`
            [(BI.fromInt 0)]
          ICB.runOutput [BI.fromInt 8] testProg `shouldEqual`
            [(BI.fromInt 0)]
          ICB.runOutput [BI.fromInt 7] testProg `shouldEqual`
            [(BI.fromInt 1)]
    describe "arithmetic" do
        pending "can add numbers in reference mode"
