import Test.Hspec

import LangSpec
import RustSpec

main :: IO ()
main = hspec $ do
  langSpec
  rustSpec

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
