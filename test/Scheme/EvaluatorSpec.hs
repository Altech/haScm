module Scheme.EvaluatorSpec where

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
    
shouldThrow :: String -> Expectation
shouldThrow expr = do
  env <- nullEnv
  maybeVal <- runErrorT $ evalExpr expr env
  case maybeVal of 
    Right ret -> assertFailure ("Expected: Left _\n but got: Right (" ++ show ret ++ ")")
    Left err  -> return ()

shouldThrow' :: [String] -> Expectation
shouldThrow' expr = do
  env <- nullEnv
  maybeVal <- runErrorT $ evalExprs expr env
  case maybeVal of 
    Right ret -> assertFailure ("Expected: Left _\n but got: Right (" ++ show ret ++ ")")
    Left err  -> return ()
    
(!) = flip ($)

expr :: String -> LispVal
expr s = case parse parseDatum s of Success exp -> exp

evalExpr :: String -> Env -> IOThrowsError LispVal
evalExpr str env = eval env (expr str)

evalExprs :: [String] -> Env -> IOThrowsError LispVal
evalExprs strs env = mapM (\str -> eval env (expr str)) strs >>= return . last

nullEnv' = liftIO nullEnv
defaultEnv' = liftIO defaultEnv

runSample action = runErrorT (trapError action) >>= return . extractValue

trapError action = catchError action (return . String . show)

extractValue (Right val) = val
                                     
spec = do 
  describe "Evaluator" $ do
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
          "id" ! shouldThrow
        it "eval (Symbol \"id\") should be evaluated to its content" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "id"]
          val `shouldBe` expr "1"
    describe "special forms" $ do
      describe "define" $ do
        it "define a value" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "id"]
          val `shouldBe` expr "1"
        it "define a function" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define (f x) x)",
                 "(f 1)"]
          val `shouldBe` expr "1"
        it "define a function" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define (f x . y) y)",
                 "(f 1 2 3 4)"]
          val `shouldBe` expr "(2 3 4)"
        it "when define a symbol as alias of another, changing the value should propagate." $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define a 1)",
                 "(define b a)",
                 "(set! a 2)",
                 "b"]
          val `shouldBe` expr "2"
      describe "define-macro" $ do
        it "define a macro without params" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define-macro (m1) '(1 2 3))",
                 "(macroexpand-1 '(m1))"]
          val `shouldBe` expr "(1 2 3)"
        it "define a macro with params" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define-macro (m1 a b) `(+ ,a ,b))",
                 "(macroexpand-1 '(m1 1 2))"]
          val `shouldBe` expr "(+ 1 2)"
        it "define a macro with dotted params" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define-macro (m1 a b . c) `(+ ,a ,b ,@c))",
                 "(macroexpand-1 '(m1 1 2 3 4))"]
          val `shouldBe` expr "(+ 1 2 3 4)"
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
      describe "quasiquote with unquote / unquote-splicing" $ do
        it "quote a list including an unquote" $ do
          val <- runSample $ nullEnv' >>= evalExprs [
                 "(define id 1)",
                 "`(1 ,id)"]
          val `shouldBe` expr "(1 1)"
        it "quote a list including an double unquote(should throw)" $ do
            ["(define id 1)", "`(1 ,,id)"] ! shouldThrow'
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
      describe "begin" $ do 
        it "evaluate each statement and return the last result"  $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define a 1)",
                 "(begin (set! a (+ a 1)) (set! a (+ a 1)) a)"]
          val `shouldBe` expr "3"
      describe "if" $ do
        it "if true then 1 else 2" $ do
          val <- runSample $ nullEnv' >>= evalExpr "(if #t 1 2)"
          val `shouldBe` expr "1"
        it "if false then 1 else 2" $ do
          val <- runSample $ nullEnv' >>= evalExpr "(if #f 1 2)"
          val `shouldBe` expr "2"
      describe "eq?" $ do
        describe "compare references, when receive symbols" $ do
          it "provided same references, return true" $ do
            val <- runSample $ nullEnv' >>= evalExprs [
                   "(define a 1)",
                   "(define b a)",
                   "(eq? a b)"   ]
            val `shouldBe`  expr "#t"
          it "provided different references, return false" $ do
            val <- runSample $ nullEnv' >>= evalExprs [
                   "(define a 1)",
                   "(define b 1)",
                   "(eq? a b)"   ]
            val `shouldBe`  expr "#f"
          it "provided unbound symbols, throw error" $ do
            ["(define a 1)", "(eq? a b)"] ! shouldThrow'
        it "return true, when receive a symbol and a non-symbol" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define a 1)",
                 "(eq? a 1)"   ]
          val `shouldBe`  expr "#t"
        it "compare values, when receive non-symbols" $ do
          val <- runSample $ defaultEnv' >>= evalExpr "(eq? '(1 2 3) '(1 2 3))"
          val `shouldBe`  expr "#t"
    describe "apply" $ do
      it "application + with (1 2 3) returns 6" $ do
        val <- runSample $ defaultEnv' >>= evalExpr "(apply + 1 2 3)"
        val `shouldBe` expr "6"
    describe "built-in functions" $ do
      describe "eval" $ do
        it "evaluate a quoted compound form and do application" $ do
          val <- runSample $ defaultEnv' >>= evalExpr "(eval '(+ 1 2))"
          val `shouldBe` expr "3"
        it "define a symbol by `eval`, it is visible in the outside environment" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(eval '(define a 1))",
                 "a" ]
          val `shouldBe` expr "1"
        it "define a symbol beforehand, it is visible in the `eval` environment" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define a 1)",
                 "(eval 'a)"]
          val `shouldBe` expr "1"
      describe "macroexpand-1" $ do
        it "expand a form of macro" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define-macro (m1) '(1 2 3))",
                 "(macroexpand-1 '(m1))"]
          val `shouldBe` expr "(1 2 3)"
        it "expand a form nesting a macro" $ do
          val <- runSample $ defaultEnv' >>= evalExprs [
                 "(define-macro (m1 a b) `(+ ,a ,b))",
                 "(macroexpand-1 '(+ (m1 1 2) 5))"]
          val `shouldBe` expr "(+ (+ 1 2) 5)"
