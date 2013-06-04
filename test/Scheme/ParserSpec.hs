module Scheme.ParserSpec where

-- file Spec.hs
import Scheme.Internal
import Scheme.Parser
import Test.Hspec
import Test.QuickCheck hiding (Result, Success, Failure)
import Text.Trifecta (Result(Success, Failure))

instance Eq a => Eq (Result a) 
  where a == b = case (a,b) of
          (Success a, Success b) -> a == b
          (Failure _, Failure _) -> True
          _ -> False


main :: IO ()
main = hspec spec

p = parse parseDatum

spec = do 
  describe "parseDatum" $ do
    describe "<symple datum>" $ do
      it "simple boolean: \"#t\"" $ 
        p "#t" `shouldBe` (Success $ Bool True)

      it "simple natural numbrer: \"1\"" $ 
        p "1" `shouldBe` (Success $ Number 1)
  
      it "letter character \"#\\A\"" $
        p "#\\A" `shouldBe` (Success $ Character 'A')

      it "symple string: \"\\\"test\\\"\"" $
        p "\"test\"" `shouldBe` (Success $ String "test")
  
      it "escaped string: \"\\\"test\\n\\tend\\\"\"" $
        p "\"test\\n\\tend\"" `shouldBe` (Success $ String "test\n\tend")
        
    describe "<compound datum>" $ do
      it "empty list: \"()\"" $ 
        p "()" `shouldBe` (Success $ List [])
        
      it "symple list(syntax-sugar): \"(1 2)\"" $
        p "(1 2)" `shouldBe` (Success $ List [Number 1, Number 2])

      it "symple list(syntax-sugar): \"(\\\"test1\\\" \\\"test2\\\")\"" $
        p "(\"test1\" \"test2\")" `shouldBe` (Success $ List [String "test1", String "test2"])

      it "symple list: \"(1 . (2 . ()))\"" $
        p "(1 . (2 . ()))" `shouldBe` (Success $ List [Number 1, Number 2])

      it "nested list: \"(1 (2 3))\"" $
        p "(1 (2 3))" `shouldBe` (Success $ List [Number 1, List [Number 2, Number 3]])
      
      it "nested list: \"((1 2) 3)\"" $
        p "((1 2) 3)" `shouldBe` (Success $ List [List [Number 1, Number 2], Number 3])
      
      it "cons: \"(1 . 2)\"" $
        p "(1 . 2)" `shouldBe` (Success $ DottedList [Number 1] (Number 2))
      
      it "many cons: \"(1 . (2 . 3))\"" $
        p "(1 . (2 . 3))" `shouldBe` (Success $ DottedList [Number 1, Number 2] (Number 3))
        
      it "many cons(syntax-sugar): \"(1 2 . 3)\"" $
        p "(1 2 . 3)" `shouldBe` (Success $ DottedList [Number 1, Number 2] (Number 3))
      
      it "quoted number: \"'1\"" $ 
        p "'1" `shouldBe` (Success $ List [Symbol "quote", Number 1])
        
      it "quasiquoted number: \"`1\"" $ 
        p "`1" `shouldBe` (Success $ List [Symbol "quasiquote", Number 1])
        
      it "unquote symbol in a list: \"`(a ,b)\"" $ 
        p "`(a ,b)" `shouldBe` (Success $ List [Symbol "quasiquote", List [Symbol "a", List [Symbol "unquote", Symbol "b"]]])
        
      it "unquote-splicing symbol in a list: \"`(f ,@body)\"" $ 
        p "`(f ,@body)" `shouldBe` (Success $ List [Symbol "quasiquote", List [Symbol "f", List [Symbol "unquote-splicing", Symbol "body"]]])
