{-# LANGUAGE QuasiQuotes #-}

module RustSpec where

import One4All.Convert.Rust
import One4All.Lang
import One4All.Quote

import qualified Language.Rust.Data.Position as RustP
import qualified Language.Rust.Quote as RustQ
import qualified Language.Rust.Syntax as Rust

import Control.Monad (void)
import Test.Hspec

rustSpec :: Spec
rustSpec = do
  describe "Testing the conversion to Rust" $ do
    it "simple let expression" $
      let langExpr = [o4a| let b = foo a in bar b |]
          rustExpr = (convert langExpr) :: Rust.Expr ()
          expectedRustExpr = void [RustQ.expr| { let b = foo(a); bar(b) } |]
       in rustExpr `shouldBe` expectedRustExpr
-- it "Rust" $
--   (let p = [Rust.stmt| let y = f(a); |] in traceShowId p ) `shouldBe` ([Rust.stmt| let y = f(); |])
-- FIXME: our lang does not yet understand what a call to a function that takes no arguments!
-- expected:
--   BlockExpr [] (Block [Local (IdentP (ByValue Immutable) "a" Nothing ()) Nothing (Just (Call [] (PathExpr [] Nothing (Path False [PathSegment "foo" Nothing ()] ()) ()) [] ())) [] (),
--                        NoSemi (Call [] (PathExpr [] Nothing (Path False [PathSegment "bar" Nothing ()] ()) ()) [PathExpr [] Nothing (Path False [PathSegment "a" Nothing ()] ()) ()] ()) ()] Normal ()) ()
--
--  but got:
--  BlockExpr [] (Block [Local (IdentP (ByValue Immutable) "a" Nothing ()) Nothing (Just (PathExpr [] Nothing (Path False [PathSegment "foo" Nothing ()] ()) ())) [] (),
--                       NoSemi (Call [] (PathExpr [] Nothing (Path False [PathSegment "bar" Nothing ()] ()) ()) [PathExpr [] Nothing (Path False [PathSegment "a" Nothing ()] ()) ()] ()) ()] Normal ()) ()
