{-# LANGUAGE QuasiQuotes #-}

module LangSpec where

import Quote
import Lang

import Test.Hspec

langSpec :: Spec
langSpec = do
  describe "Testing the expression language" $ do
    it "variable" $
      [expr| v |] `shouldBe` (Binding $ V "v")
    it "function call" $
      [expr| f v |] `shouldBe` (Apply (Binding $ V "f") (Binding $ V "v"))
    it "lambda" $
      [expr| \v -> f v |] `shouldBe` (Lambda (V "v") (Apply (Binding $ V "f") (Binding $ V "v")))
    it "let" $
      [expr| let a = f b in g a |] `shouldBe` (Let (V "a")
                                                   (Apply (Binding $ V "f") (Binding $ V "b"))
                                                   (Apply (Binding $ V "g") (Binding $ V "a")))
    it "anti-quotes for var" $
      (let x = Binding $ V "v" in [expr| f $var:x |]) `shouldBe` (let x = Binding $ V "v" in (Apply (Binding $ V "f") x))
    it "anti-quotes for var (compare value directly)" $
      (let x = Binding $ V "v" in [expr| f $var:x |]) `shouldBe` (Apply (Binding $ V "f") (Binding $ V "v"))
