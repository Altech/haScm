-- file Spec.hs
import Scheme.Internal
import Scheme.Evaluator
import Test.Hspec
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck hiding (Result, Success, Failure)

main :: IO ()
main = hspec spec

shouldBeEvaluatedTo :: LispVal -> LispVal -> Expectation
val `shouldBeEvaluatedTo` expect = case eval val of 
                                     Right ret -> if ret == expect then return () else assertFailure ("Expected: " ++ show expect ++ "\n" ++ " but got: " ++ show ret)
                                     Left err  -> assertFailure ("Expected: Right _\n but got: Left err (err: " ++ show err ++ ")")
                                     
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
    describe "special forms" $ do
      return ()