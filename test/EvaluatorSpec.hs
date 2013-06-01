-- file Spec.hs
import Scheme.Internal
import Scheme.Evaluator
import Test.Hspec hiding (shouldThrow)
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck hiding (Result, Success, Failure)

import Control.Monad.Error (runErrorT, liftIO)

main :: IO ()
main = hspec spec

shouldBeEvaluatedTo :: LispVal -> LispVal -> Expectation
shouldBeEvaluatedTo val expect = do 
  env <- nullEnv
  maybeVal <- runErrorT $ eval env val 
  case maybeVal of 
    Right ret -> if ret == expect then return () else assertFailure ("Expected: " ++ show expect ++ "\n" ++ " but got: " ++ show ret)
    Left err  -> assertFailure ("Expected: Right _\n but got: Left err (err: " ++ show err ++ ")")
    
-- shouldThrow :: LispVal -> Expectation
shouldThrow :: IOThrowsError LispVal -> () -> Expectation
shouldThrow execution _ = do
  maybeVal <- runErrorT $ execution
  case maybeVal of 
    Right ret -> assertFailure ("Expected: Left _\n but got: Right (" ++ show ret ++ ")")
    Left err  -> return ()


                                     
spec = do 
  describe "eval" $ do
    describe "(eval [self-evaluating-value]) returns self." $ do
      describe "<number>" $ do
        it "eval (Number 12) should be evaluated to (Number 12)" $
          Number 12 `shouldBeEvaluatedTo` Number 12
      describe "<boolean>" $ do
        it "eval (Bool True) should be evaluated to (Bool True)" $
          Bool True `shouldBeEvaluatedTo` Bool True
      describe "<character>" $ do
        it "eval (Character 'A') should be evaluated to (Character 'A')" $ 
          Character 'A' `shouldBeEvaluatedTo` Character 'A'
      describe "<string>" $ do
        it "eval (String \"test\") should be evaluated to (String \"test\")" $
          String "test" `shouldBeEvaluatedTo` String "test"
    describe "identifier" $ do
      describe "<symbol>" $ do 
        it "eval (Symbol \"id\") should throw error" $ do
          env <- nullEnv
          eval env sampleId `shouldThrow` () 
        it "eval (Symbol \"id\") should be evaluated to its content" $ do
          env <- nullEnv
          maybeVal <- runErrorT $ do
            eval env (List [(Symbol "define"), sampleId, sampleVal])
            eval env sampleId
          maybeVal `shouldBe` (Right sampleVal)
    describe "special forms" $ do
      describe "define" $ do
        it "define a symbol as a value" $ do
          env <- nullEnv
          maybeVal <- runErrorT $ do 
            eval env (List [(Symbol "define"), sampleId, sampleVal])
            eval env sampleId
          maybeVal `shouldBe` (Right sampleVal)
      describe "set!" $ do 
        it "change the value of the symbol" $ do
          env <- nullEnv
          maybeVal <- runErrorT $ do 
            eval env (List [(Symbol "define"), sampleId, sampleVal1])
            eval env (List [(Symbol "set!"), sampleId, sampleVal2])
            eval env sampleId
          maybeVal `shouldBe` (Right sampleVal2)
    describe "primitive functions" $ do 
      describe "+" $ do
        -- it "eval + should be an primitive function" $ 
        --   Symbol "+" `shouldBeEvaluatedTo` (case getEnv "+" of Right v -> v)
        it "eval (+ 1 1) should be evaluated to 2" $
          List [Symbol "+", Number 1, Number 1] `shouldBeEvaluatedTo` Number 2
        it "eval (+ 100 25) should be evaluated to 125" $
          List [Symbol "+", Number 100, Number 25] `shouldBeEvaluatedTo` Number 125
  where
    sampleId   = Symbol "id"
    sampleVal  = sampleVal1
    sampleVal1 = Number 1
    sampleVal2 = Number 2
