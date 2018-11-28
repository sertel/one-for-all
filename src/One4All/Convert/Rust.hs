{-# LANGUAGE QuasiQuotes #-}

module One4All.Convert.Rust where

import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Pretty
import Language.Rust.Syntax

import qualified One4All.Lang as L

import Data.Text.Lazy (Text)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text (renderIO, renderLazy)
import System.IO

-- noSpan = Span NoPosition NoPosition
noSpan = ()

instance L.ConvertUntypedLambda (Expr ()) where
  supportsDirectLambdaApplication _ = False
  -- Rust is statement-oriented
  convert (L.Let (L.V v) e ie) =
    let ec = L.convert e
        iec = L.convert ie
        ecStmt =
          Local
            (IdentP (ByValue Immutable) (mkIdent v) Nothing noSpan)
            Nothing
            (Just ec)
            []
            noSpan
     in case iec of
          (BlockExpr attrs (Block stmts unsafety _) _) ->
            BlockExpr attrs (Block ([ecStmt] ++ stmts) unsafety noSpan) noSpan
          otherwise ->
            BlockExpr
              []
              (Block [ecStmt, NoSemi iec noSpan] Normal noSpan)
              noSpan
  convert (L.Binding (L.V v)) =
    (PathExpr
       []
       Nothing
       (Path False [PathSegment (mkIdent v) Nothing noSpan] noSpan)
       noSpan)
  convert e@(L.Apply _ _) =
    let (f, args) = L.collectFnCall e
        argExprs = map L.convert args
     in case f of
          Left lambdaApply
          -- it is not possible in Rust to immediately call a closure, so we need bind it first
           ->
            error $
            "Invariant broken: Rust support requires to lift direct lambda applications, i.e., (\\a -> a) 5 => let id = (\\a -> a) in id 5. Offending call: " ++
            (show lambdaApply)
          Right (L.Binding (L.V refApply)) ->
            Call
              []
              (PathExpr
                 []
                 Nothing
                 (Path
                    False
                    [PathSegment (mkIdent refApply) Nothing noSpan]
                    noSpan)
                 noSpan)
              argExprs
              noSpan
  convert (L.Lambda (L.V v) e) =
    Closure
      []
      Immovable -- no ownership
      Value
      (FnDecl
         [ Arg
             (Just $ IdentP (ByValue Immutable) (mkIdent v) Nothing noSpan)
             (Infer noSpan) -- infer the type of the arguments for lambdas
             noSpan
         ]
         Nothing
         False
         noSpan -- no types (yet?)
       )
      (L.convert e)
      noSpan

unparseToDocStream :: (Resolve a, Pretty a) => a -> PP.SimpleDocStream ann
unparseToDocStream = (PP.layoutPretty layoutOptions) . PP.unAnnotate . pretty'
  where
    pageWidth = 80
    layoutOptions =
      PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine pageWidth 1}

unparseToText :: (Resolve a, Pretty a) => a -> Text
unparseToText = renderLazy . unparseToDocStream

unparseToStdOut :: (Resolve a, Pretty a) => a -> IO ()
unparseToStdOut = (renderIO System.IO.stdout) . unparseToDocStream

unparseToFile :: (Resolve a, Pretty a) => String -> a -> IO ()
unparseToFile fileName expr =
  withFile fileName WriteMode (\h -> renderIO h $ unparseToDocStream expr)
