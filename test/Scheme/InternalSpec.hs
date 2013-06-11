module Scheme.InternalSpec where

import Test.Hspec

import Control.Arrow ((>>>))
import Control.Monad.Error (runErrorT, liftIO)
import Data.IORef
import Scheme.Internal
import Scheme.Internal.Environment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "environment" $ do
    describe "env / frame" $ do
      it "nullEnv has a frame" $ 
        nullEnv >>= readIORef >>= (length >>> shouldBe' 1)
      it "add a new frame to the environemt" $
        nullEnv >>= addFrame >>= readIORef >>= (length >>> shouldBe' 2)
    describe "variable (defineVar getVar setVar isBound)" $ do 
      it "After define variable by a symbol and a value, we can get the value by the symbol" $ do
        env <- nullEnv
        maybeVal <- runErrorT $ do
          defineVar env "id" sampleVal
          getVar env "id"
        maybeVal `shouldBe` Right sampleVal
      it "After difine variable in top frame and add a frame, we can get the variable" $ do
        env <- nullEnv
        maybeVal <- runErrorT $ do 
          defineVar env "id" sampleVal
          liftIO $ addFrame env
          getVar env "id"
        maybeVal `shouldBe` Right sampleVal
      it "After difine variable in top frame, it shadows other variables in other frames" $ do
        env <- nullEnv
        Right val <- runErrorT $ do 
          defineVar env "id" sampleVal1
          liftIO $ addFrame env
          defineVar env "id" sampleVal2
          getVar env "id"
        val `shouldBe` sampleVal2
      it "After define variable, we can change the value" $ do 
        env <- nullEnv
        Right val <- runErrorT $ do
          defineVar env "id" sampleVal1
          setVar env "id" sampleVal2
          getVar env "id"
        val `shouldBe` sampleVal2
      it "After define variable, re-defining overrides it" $ do 
        env <- nullEnv
        Right val <- runErrorT $ do
          defineVar env "id" sampleVal1
          defineVar env "id" sampleVal2
          getVar env "id"
        val `shouldBe` sampleVal2
      it "After define variable by a symbol, the sybmol should be bound" $ do
        env <- nullEnv
        runErrorT $ defineVar env "id" sampleVal
        isBound <- isBound env "id"
        isBound `shouldBe` True
  where
    shouldBe' = flip shouldBe
    sampleVal  = sampleVal1
    sampleVal1 = Number 1
    sampleVal2 = Number 2
