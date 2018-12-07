{-# LANGUAGE QuasiQuotes #-}

module LangSpec where

import One4All.Lang
import One4All.Quote

import Test.Hspec

langSpec :: Spec
langSpec = do
  describe "Testing the expression language" $ do
    it "variable" $ [o4a| v |] `shouldBe` (Binding $ V "v")
    it "function call" $
      [o4a| f v |] `shouldBe` (Apply (Binding $ V "f") (Binding $ V "v"))
    it "lambda" $
      [o4a| \v -> f v |] `shouldBe`
      (Lambda (V "v") (Apply (Binding $ V "f") (Binding $ V "v")))
    it "let" $
      [o4a| let a = f b in g a |] `shouldBe`
      (Let
         (V "a")
         (Apply (Binding $ V "f") (Binding $ V "b"))
         (Apply (Binding $ V "g") (Binding $ V "a")))
    it "nested let" $
      [o4a| let a = f b in
            let c = g a in
            c
        |] `shouldBe`
      (Let (V "a") (Apply (Binding $ V "f") (Binding $ V "b")) $
       Let (V "c") (Apply (Binding $ V "g") (Binding $ V "a")) $ Binding $ V "c")
    it "anti-quotes for var" $
      (let x = Binding $ V "v"
        in [o4a| f $var:x |]) `shouldBe`
      (let x = Binding $ V "v"
        in (Apply (Binding $ V "f") x))
    it "anti-quotes for var (compare value directly)" $
      (let x = Binding $ V "v"
        in [o4a| f $var:x |]) `shouldBe`
      (Apply (Binding $ V "f") (Binding $ V "v"))
    it "unit literal" $
      [o4a| let chan = channel () in chan |] `shouldBe`
      (Let
         (V "chan")
         (Apply (Binding $ V "channel") (Lit UnitLit))
         (Binding $ V "chan"))
    it "int literal" $
      [o4a| let chan = channel 0 in chan |] `shouldBe`
      (Let
         (V "chan")
         (Apply (Binding $ V "channel") (Lit $ IntegerLit 0))
         (Binding $ V "chan"))
