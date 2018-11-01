{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Quote
import Lang


main :: IO ()
main = hspec $ do
  exprSpec
  -- let e = [expr| let a = f b in g a |]
  -- in putStrLn $ "\nExpression: " ++ (show e)


exprSpec :: Spec
exprSpec = do
  describe "Testing the expression language" $ do
    it "variable" $
      [expr| v |] `shouldBe` (Var $ V "v")
    it "function call" $
      [expr| f v |] `shouldBe` (Apply (Var $ V "f") (Var $ V "v"))
    it "lambda" $
      [expr| \v -> f v |] `shouldBe` (Lambda (V "v") (Apply (Var $ V "f") (Var $ V "v")))
    it "let" $
      [expr| let a = f b in g a |] `shouldBe` (Let (V "a")
                                                   (Apply (Var $ V "f") (Var $ V "b"))
                                                   (Apply (Var $ V "g") (Var $ V "a")))
    it "anti-quotes for var" $
      (let x = Var $ V "v" in [expr| f $var:x |]) `shouldBe` (let x = Var $ V "v" in (Apply (Var $ V "f") x))
    it "anti-quotes for var (compare value directly)" $
      (let x = Var $ V "v" in [expr| f $var:x |]) `shouldBe` (Apply (Var $ V "f") (Var $ V "v"))

-- (
--   (Apply
--        (Var (V (((:) 'f') [])) )
--        )
--        x)

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
-- TODO
