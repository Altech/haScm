module Scheme.Evaluator.PrimitivesSpec where

import Scheme.Internal
import Scheme.Evaluator
import Scheme.Parser (parse,parseDatum)
import Text.Trifecta.Result (Result (..))
import Test.Hspec hiding (shouldThrow)
import Test.HUnit.Lang (assertFailure)


main :: IO ()
main = hspec spec

shouldBeEvaluatedTo :: String -> String -> Expectation
shouldBeEvaluatedTo valExpr expectExpr = do 
  env <- defaultEnv
  maybeVal <- runErrorT $ eval env (expr valExpr)
  case maybeVal of 
    Right ret -> if ret == (expr expectExpr) then return () else assertFailure ("Expected: " ++ show (expr expectExpr) ++ "\n" ++ " but got: " ++ show ret)
    Left err  -> assertFailure ("Expected: Right _\n but got: Left err (err: " ++ show err ++ ")")
    
shouldThrow :: String -> Expectation
shouldThrow expr = do
  env <- defaultEnv
  maybeVal <- runErrorT $ evalExpr expr env
  case maybeVal of 
    Right ret -> assertFailure ("Expected: Left _\n but got: Right (" ++ show ret ++ ")")
    Left err  -> return ()

expr :: String -> LispVal
expr s = case parse parseDatum s of Success exp -> exp; Failure err -> String $ "error(" ++ show err ++ ")";

evalExpr :: String -> Env -> IOThrowsError LispVal
evalExpr str env = eval env (expr str)

evalExprs :: [String] -> Env -> IOThrowsError LispVal
evalExprs strs env = mapM (\str -> eval env (expr str)) strs >>= return . last

(!) = flip ($)

runSample action = runErrorT action >>= return . extractValue
extractValue (Right val) = val
                                     
spec = do 
  describe "core" $ do
    describe "equal?" $ do
      it "(equal? 1 1); => #t" $ 
        "(equal? 1 1)" `shouldBeEvaluatedTo` "#t"
      it "(equal? 1 2); => #f" $ 
        "(equal? 1 2)" `shouldBeEvaluatedTo` "#f"
      it "(equal? '(1 2 3) '(1 2 3)); => #t" $ 
        "(equal? '(1 2 3) '(1 2 3))" `shouldBeEvaluatedTo` "#t"
      it "(equal? '(1 100 3) '(1 2 3)); => #f" $ 
        "(equal? '(1 100 3) '(1 2 3))" `shouldBeEvaluatedTo` "#f"
    describe "eq?" $ do
      it "" $ 
        pending
  describe "predicate" $ do
    describe "number?" $ do
      it "(number? 2); => #t" $
        "(number? 2)" `shouldBeEvaluatedTo` "#t"
      it "(number? \"str\"); => #f" $
        "(number? \"str\")" `shouldBeEvaluatedTo` "#f"
    describe "boolean?" $ do
      it "(boolean? #t); => #t" $
        "(boolean? #t)" `shouldBeEvaluatedTo` "#t"
      it "(boolean? 1); => #f" $
        "(boolean? 1)" `shouldBeEvaluatedTo` "#f"
    describe "char?" $ do
      it "(char? #\\a); => #t" $
        "(char? #\\a)" `shouldBeEvaluatedTo` "#t"
      it "(char? 1); => #f" $
        "(char? 1)" `shouldBeEvaluatedTo` "#f"
    describe "string?" $ do
      it "(string? \"str\"); => #t" $
        "(string? \"str\")" `shouldBeEvaluatedTo` "#t"
      it "(string? 1); => #f" $
        "(string? 1)" `shouldBeEvaluatedTo` "#f"
    describe "symbol?" $ do
      it "(symbol? 'id); => #t" $
        "(symbol? 'id)" `shouldBeEvaluatedTo` "#t"
      it "(symbol? \"str\"); => #f" $
        "(symbol? \"str\")" `shouldBeEvaluatedTo` "#f"
    describe "procedure?" $ do
      it "(procedure? (lambda (x) x)); => #t" $
        "(procedure? (lambda (x) x))" `shouldBeEvaluatedTo` "#t"
      it "(procedure? 1); => #f" $
        "(procedure? 1)" `shouldBeEvaluatedTo` "#f"
    describe "list?" $ do
      it "(list? '(1 2 3)); => #t" $
        "(list? '(1 2 3))" `shouldBeEvaluatedTo` "#t"
      it "(list? 1); => #f" $
        "(list? 1)" `shouldBeEvaluatedTo` "#f"
    describe "null?" $ do
      it "(null? '()); => #t" $
        "(null? '())" `shouldBeEvaluatedTo` "#t"
      it "(null? '(1)); => #f" $
        "(null? '(1))" `shouldBeEvaluatedTo` "#f"
      it "(null? '(1 . 2)); => #f" $
        "(null? '(1 . 2))" `shouldBeEvaluatedTo` "#f"
    describe "pair?" $ do
      it "(pair? '()); => #f" $
        "(pair? '())" `shouldBeEvaluatedTo` "#f"
      it "(pair? '(1)); => #t" $
        "(pair? '(1))" `shouldBeEvaluatedTo` "#t"
      it "(pair? '(1 . 2)); => #t" $
        "(pair? '(1 . 2))" `shouldBeEvaluatedTo` "#t"
  describe "type casting" $ do
    describe "symbol->string" $ do
      it "(symbol->string 'id); => \"id\"" $ 
        "(symbol->string 'id)" `shouldBeEvaluatedTo` "\"id\""
      it "(symbol->string 2); => error" $ 
        "(symbol->string 2)" ! shouldThrow
    describe "string->symbol" $ do
      it "(string->symbol \"str\"); => 'str" $ 
        "(string->symbol \"str\")" `shouldBeEvaluatedTo` "str"
      it "(string->symbol 2); => error" $ 
        "(string->symbol 2)" ! shouldThrow
    describe "number->string" $ do
      it "(number->string 23); => \"23\"" $ 
        "(number->string 23)" `shouldBeEvaluatedTo` "\"23\""
      it "(number->string \"a\"); => error" $ 
        "(number->string  \"a\")" ! shouldThrow
    describe "string->number" $ do
      it "(string->number \"123\"); => 123" $ 
        "(string->number \"123\")" `shouldBeEvaluatedTo` "123"
      it "(string->number 2); => error" $ 
        "(string->number 2)" ! shouldThrow
  describe "numeric operator" $ do
    describe "+" $ do
      it "(+ 1 2); => 3" $
        "(+ 1 2)" `shouldBeEvaluatedTo` "3"
      it "(+ 1 2 3); => 6" $
        "(+ 1 2 3)" `shouldBeEvaluatedTo` "6"
      it "(+ 1 2 #t); => error" $
        "(+ 1 2 #t)" ! shouldThrow
    describe "-" $ do
      it "(- 2 1); => -1" $
        "(- 2 1)" `shouldBeEvaluatedTo` "1"
    describe "*" $ do
      it "(* 2 200); => -1" $
        "(* 2 200)" `shouldBeEvaluatedTo` "400"
    describe "/" $ do
      it "(/ 234 54); => 4" $
        "(/ 234 54)" `shouldBeEvaluatedTo` "4"
    describe "mod" $ do
      it "(mod 32 23); => 9" $
        "(mod 32 23)" `shouldBeEvaluatedTo` "9"
    describe "quotient" $ do
      it "(quotient 21 4); => 5" $
        "(quotient 21 4)" `shouldBeEvaluatedTo` "5"
    describe "remainder" $ do
      it "(remainder 21 4); => 1" $
        "(remainder 21 4)" `shouldBeEvaluatedTo` "1"
    describe "=" $ do
      it "(= 1 1); => #t" $
        "(= 1 1)" `shouldBeEvaluatedTo` "#t"
      it "(= 1 2); => #f" $
        "(= 1 2)" `shouldBeEvaluatedTo` "#f"
    describe ">" $ do
      it "(> 1 3); => #f" $
        "(> 1 3)" `shouldBeEvaluatedTo` "#f"
      it "(> 3 1); => #t" $
        "(> 3 1)" `shouldBeEvaluatedTo` "#t"
      it "(> 1 1); => #f" $
        "(> 1 1)" `shouldBeEvaluatedTo` "#f"
    describe "<" $ do
      it "(< 1 3); => #t" $
        "(< 1 3)" `shouldBeEvaluatedTo` "#t"
      it "(< 3 1); => #f" $
        "(< 3 1)" `shouldBeEvaluatedTo` "#f"
      it "(< 1 1); => #f" $
        "(< 1 1)" `shouldBeEvaluatedTo` "#f"
    describe ">=" $ do
      it "(>= 1 3); => #f" $
        "(>= 1 3)" `shouldBeEvaluatedTo` "#f"
      it "(>= 3 1); => #t" $
        "(>= 3 1)" `shouldBeEvaluatedTo` "#t"
      it "(>= 1 1); => #t" $
        "(>= 1 1)" `shouldBeEvaluatedTo` "#t"
    describe "<=" $ do
      it "(<= 1 3); => #t" $
        "(<= 1 3)" `shouldBeEvaluatedTo` "#t"
      it "(<= 3 1); => #f" $
        "(<= 3 1)" `shouldBeEvaluatedTo` "#f"
      it "(<= 1 1); => #t" $
        "(<= 1 1)" `shouldBeEvaluatedTo` "#t"
  describe "boolean operator" $ do
    describe "not" $ do
      it "(not #t); => #f" $
        "(not #t)" `shouldBeEvaluatedTo` "#f"
      it "(not #f); => #t" $
        "(not #f)" `shouldBeEvaluatedTo` "#t"
      it "(not 1); => error" $
        "(not 1)" ! shouldThrow
  describe "string operator" $ do
    describe "string-append" $ do
      it "(string-append \"foo\" \"bar\" \"baz\"); => \"foobarbaz\"" $
        "(string-append \"foo\" \"bar\" \"baz\")" `shouldBeEvaluatedTo` "\"foobarbaz\""
    -- describe "string>?" $ do
    --   it "(string>? \"foo\" \"bar\"); => #t" $
    --     "(string>? \"foo\" \"bar\")" `shouldBeEvaluatedTo` "#t"
    --   it "(string>? 3 1); => #t" $
    --     "(string>? 3 1)" `shouldBeEvaluatedTo` "#t"
    --   it "(string>? 1 1); => #f" $
    --     "(string>? 1 1)" `shouldBeEvaluatedTo` "#f"
    -- describe "<" $ do
    --   it "(< 1 3); => #t" $
    --     "(< 1 3)" `shouldBeEvaluatedTo` "#t"
    --   it "(< 3 1); => #f" $
    --     "(< 3 1)" `shouldBeEvaluatedTo` "#f"
    --   it "(< 1 1); => #f" $
    --     "(< 1 1)" `shouldBeEvaluatedTo` "#f"
    -- describe ">=" $ do
    --   it "(>= 1 3); => #f" $
    --     "(>= 1 3)" `shouldBeEvaluatedTo` "#f"
    --   it "(>= 3 1); => #t" $
    --     "(>= 3 1)" `shouldBeEvaluatedTo` "#t"
    --   it "(>= 1 1); => #t" $
    --     "(>= 1 1)" `shouldBeEvaluatedTo` "#t"
    -- describe "<=" $ do
    --   it "(<= 1 3); => #t" $
    --     "(<= 1 3)" `shouldBeEvaluatedTo` "#t"
    --   it "(<= 3 1); => #f" $
    --     "(<= 3 1)" `shouldBeEvaluatedTo` "#f"
    --   it "(<= 1 1); => #t" $
    --     "(<= 1 1)" `shouldBeEvaluatedTo` "#t"
  describe "list operator" $ do
    describe "cons" $ do
      it "(cons 1 2); => (1 . 2)" $
        "(cons 1 2)" `shouldBeEvaluatedTo` "(1 . 2)"
      it "(cons 1 '(2 3 4)); => (1 2 3 4)" $
        "(cons 1 '(2 3 4))" `shouldBeEvaluatedTo` "(1 2 3 4)"
      it "(cons '(1 2) '(3 4)); => ((1 2) 3 4)" $
        "(cons '(1 2) '(3 4))" `shouldBeEvaluatedTo` "((1 2) 3 4)"
    describe "car" $ do
      it "(car '(1 . 2)); => 1" $
        "(car '(1 . 2))" `shouldBeEvaluatedTo` "1"
      it "(car '(1 2)); => 1" $
        "(car '(1 2))" `shouldBeEvaluatedTo` "1"
    describe "cdr" $ do
      it "(cdr '(1 . 2)); => 2" $
        "(cdr '(1 . 2))" `shouldBeEvaluatedTo` "2"
      it "(cdr '(1 2)); => (2)" $
        "(cdr '(1 2))" `shouldBeEvaluatedTo` "(2)"
