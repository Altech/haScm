-- file Spec.hs
import Scheme.Internal
import Scheme.Evaluator
import Scheme.Parser (parse,parseDatum)
import Text.Trifecta.Result (Result (..))
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
    
shouldThrow :: IOThrowsError LispVal -> () -> Expectation
shouldThrow execution _ = do
  maybeVal <- runErrorT $ execution
  case maybeVal of 
    Right ret -> assertFailure ("Expected: Left _\n but got: Right (" ++ show ret ++ ")")
    Left err  -> return ()


expr :: String -> LispVal
expr s = case parse parseDatum s of Success exp -> exp

evalExpr :: String -> Env -> IOThrowsError LispVal
evalExpr str env = eval env (expr str)

evalExprs :: [String] -> Env -> IOThrowsError LispVal
evalExprs strs env = mapM (\str -> eval env (expr str)) strs >>= return . last

nullEnv' = liftIO nullEnv

runSample action = runErrorT action >>= return . extractValue
extractValue (Right val) = val
                                     
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
          (nullEnv' >>= evalExpr "id") `shouldThrow` () 
        it "eval (Symbol \"id\") should be evaluated to its content" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "id"]
          val `shouldBe` expr "1"
    describe "special forms" $ do
      describe "define" $ do
        it "define a symbol as a value" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "id"]
          val `shouldBe` expr "1"
      describe "set!" $ do 
        it "change the value of the symbol" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "(set! id 2)",
                 "id"]
          val `shouldBe` expr "2"
      describe "quote" $ do
        it "quote an symbol" $ do
          val <- runSample $ nullEnv' >>= evalExpr "'id"
          val `shouldBe` expr "id"
      describe "quasiquote" $ do
        it "quote an symbol" $ do
          val <- runSample $ nullEnv' >>= evalExpr "`id"
          val `shouldBe` expr "id"
      describe "quasiquote / unquote / unquote-splicing" $ do
        it "quote a list including an unquote" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "`(1 ,id)"]
          val `shouldBe` expr "(1 1)"
        it "quote a list including an double unquote(should throw)" $ do
           (nullEnv' >>= evalExprs [
            "(define id 1)",
            "`(1 ,,id)"]) `shouldThrow` ()
        it "quote a list twice including an unquote" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "``(1 ,id)"]
          val `shouldBe` (expr "`(1 ,id)")
        it "quote a list twice including an double quote" $ do 
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "`(a `(b ,,id))"]
          val `shouldBe` expr "(a `(b ,1))"
        it "quote a list including an unquote-splicing" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id '(1 2 3))",
                 "`(1 ,@id)"]
          val `shouldBe`  expr "(1 1 2 3)"
      describe "lambda" $ do 
        it "" pending
      describe "if" $ do
        it "if true then 1 else 2" $ do
          val <- runSample $ nullEnv' >>= evalExpr "(if #t 1 2)"
          val `shouldBe` expr "1"
        it "if false then 1 else 2" $ do
          val <- runSample $ nullEnv' >>= evalExpr "(if #f 1 2)"
          val `shouldBe` expr "2"
    -- describe "primitive functions" $ do 
    --   describe "+" $ do
    --     -- it "eval + should be an primitive function" $ 
    --     --   Symbol "+" `shouldBeEvaluatedTo` (case getEnv "+" of Right v -> v)
    --     it "eval (+ 1 1) should be evaluated to 2" $
    --       List [Symbol "+", Number 1, Number 1] `shouldBeEvaluatedTo` Number 2
    --     it "eval (+ 100 25) should be evaluated to 125" $
    --       List [Symbol "+", Number 100, Number 25] `shouldBeEvaluatedTo` Number 125
