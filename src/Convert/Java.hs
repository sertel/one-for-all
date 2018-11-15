module Convert.Java where

import qualified Lang as L
import           Language.Java.Syntax

instance L.ConvertUntypedLambda Stmt where

  supportsDirectLambdaApplication _ = False

  convert (L.Let (L.V v) e ie) =
    let ec   = case L.convert e of
                ExpStmt e -> e
                otherwise -> error $ "Java does not support this type of expression in an assigment: " ++ (show e)
        iec  = case ie of
                (L.Binding (L.V v)) -> Return $ Just $ ExpName $ Name [Ident v]
                otherwise           -> L.convert ie
        stmt = LocalVars [Final]
                         (RefType $ ClassRefType $ ClassType [(Ident "java", []), (Ident "lang", []), (Ident "Object", [])])
                         [VarDecl (VarId $ Ident v) (Just $ InitExp ec)]
    in
      case iec of
        (StmtBlock (Block stmts)) -> StmtBlock $ Block $ [stmt] ++ stmts
        otherwise -> StmtBlock $ Block [stmt]
  convert (L.Binding (L.V v)) = ExpStmt $ ExpName $ Name [Ident v]
  convert e@(L.Apply _ _) =
    let (f,args) = L.collectFnCall e
        argExprs = flip map args $ (\case (ExpStmt e) -> e) . L.convert
    in
      case f of
        Left lambdaApply ->
          -- -- it is not possible in Java to immediately call a closure, so we need bind it first
          error $ "Invariant broken: Java support requires to lift direct lambda applications, i.e., (\\a -> a) 5 => let id = (\\a -> a) in id 5. Offending call: " ++ (show lambdaApply)
        Right (L.Binding (L.V refApply)) ->
          ExpStmt $ MethodInv $ MethodCall (Name [Ident refApply]) argExprs
  convert (L.Lambda (L.V v) e) =
    ExpStmt $ Lambda (LambdaSingleParam $ Ident v)
                     ((LambdaBlock . (\case (StmtBlock s) -> s) . L.convert) e)
