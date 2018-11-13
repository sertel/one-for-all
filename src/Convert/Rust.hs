{-# LANGUAGE QuasiQuotes #-}

module Convert.Rust where

import           Language.Rust.Syntax
import           Language.Rust.Quote
import           Language.Rust.Data.Ident

import qualified Lang as L

instance L.ConvertUntypedLambda (Expr ()) where
  -- Rust is statement-oriented
  convert (L.Let (L.V v) e ie) =
    let ec   = L.convert e
        iec  = L.convert ie
        ecStmt = Local (IdentP (ByValue Immutable) (mkIdent v) Nothing ())
                       Nothing
                       (Just ec)
                       []
                       () in
      case iec of
        (BlockExpr attrs (Block stmts unsafety _) _) -> BlockExpr attrs (Block ([ecStmt] ++ stmts) unsafety ()) ()
        otherwise -> BlockExpr [] (Block [ecStmt, NoSemi iec ()] Normal ()) ()
  convert (L.Binding (L.V v)) =
    (PathExpr [] Nothing (Path False [PathSegment (mkIdent v) Nothing ()] ()) ())
  convert e@(L.Apply _ _) =
    let (f,args) = L.collectFnCall e
        argExprs = map L.convert args
    in
      case f of
        Left lambdaApply ->
          -- it is not possible in Rust to immediately call a closure, so we need bind it first
          error "Invariant broken: Rust support requires to lift direct lambda applications: (\a -> a) 5 => let f = (\a -> a) in f 5"
        Right (L.Binding (L.V refApply)) ->
          Call []
               (PathExpr [] Nothing (Path False [PathSegment (mkIdent refApply) Nothing ()] ()) ())
               argExprs
               ()
  convert (L.Lambda (L.V v) e) =
    Closure []
            Immovable -- no ownership
            Value
            (FnDecl [Arg (Just $ IdentP (ByValue Immutable) (mkIdent v) Nothing ())
                         (Infer ()) -- infer the type of the arguments for lambdas
                         ()]
                    Nothing
                    False
                    ()) -- no types (yet?)
            (L.convert e)
            ()
