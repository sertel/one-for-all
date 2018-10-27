module Quote where

import           Data.Generics
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote

import           Parser (parseExpr)
import           Lang

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseExpr pos s
                      dataToExpQ (const Nothing `extQ` antiExprExp) expr

antiExprExp :: Expr -> Maybe (TH.Q TH.Exp)
-- an anti-quote is essentially a variable that exists in the scope of this
-- template. as such, turning this into the variable that references it, will
-- essentially splice this thing in. the function dataToExpQ will turn this into
-- Haskell AST.
antiExprExp  (AntiExpr v)     = Just $ TH.varE  (TH.mkName v)
antiExprExp  _                = Nothing

quoteExprPat :: String -> TH.PatQ
quoteExprPat s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseExpr pos s
                      dataToPatQ (const Nothing `extQ` antiExprPat) expr

antiExprPat :: Expr -> Maybe (TH.Q TH.Pat)
antiExprPat  (AntiExpr v)     = Just $ TH.varP  (TH.mkName v)
antiExprPat  _                = Nothing

expr  :: QuasiQuoter
expr  =  QuasiQuoter { quoteExp = quoteExprExp,
                       quotePat = quoteExprPat
                      -- with ghc >= 7.4, you could also
                      -- define quoteType and quoteDec for
                      -- quasiquotes in those places too
          }
