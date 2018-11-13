{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Quote
import Lang

-- import Language.Rust.Syntax
-- import Language.Rust.Quote as Rust

import Debug.Trace

main :: IO ()
main = hspec $ do
  exprSpec
  -- let e = [expr| let a = f b in g a |]
  -- in putStrLn $ "\nExpression: " ++ (show e)


exprSpec :: Spec
exprSpec = do
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

    -- it "Rust antiquotes" $
    --   (let p = [Rust.stmt| let y = f(a); |] in traceShowId p ) `shouldBe` ([Rust.stmt| let y = f(); |])

-- runtime code:
-- targetExpr =
--   [expr|
--     (* list of tasks *)
--     let v    = [] in
--
--     (* single communication channel *)
--     let c    = channel () in
--     let rcv  = nth 0 channel in
--     let snd  = nth 1 channel in
--
--     (* a single (tail-recursive) task *)
--     let task = (\() ->
--                   let i = rcv () in
--                   let o = $expr:f i in
--                   let j = snd o in
--                   task ()) in
--     let v = v ++ [task] in
--       runOhua v
--   |]

-- operator code:
-- TODO terminate recursion and send off size
-- [expr|
--   let smapFun = (\(dataInChan, dataOutChan, sizeOutChan) ->
--                     let data       = dataInChan ()
--                     let count      = 0 in
--                     let handleData = (\(d) -> dataOutChan d)
--                     let getData    = (\(xs, sentCount) -> let c   = get xs in
--                                                           let x   = nth 0 c in
--                                                           let xs' = nth 1 c in
--                                                           let _   = handleData x in
--                                                             handleData xs' (add sentCount 1))
--                       getData data 0
--                   )
-- |]
--
-- [expr|
--   let select = (\(selectInChan, trueBranchChan, falseBranchChan, outChan) ->
--                     let trueBranchExecuted = selectInChan () in
--                     let data = if trueBranchExecuted
--                                then trueBranchChan ()
--                                else falseBranchChan () in
--                       outChan data
--                   )
-- |]
