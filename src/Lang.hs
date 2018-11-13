{-# LANGUAGE DeriveDataTypeable #-}
module Lang where

import Data.Generics
import Control.Arrow (second)

data Var = V String
         -- TODO fn vars
         -- anti vars
         | AV String deriving (Show, Eq, Typeable, Data)

data Expr = Binding Var
          | Apply Expr Expr
          | Lambda Var Expr
          | Let Var Expr Expr
          -- anti quotes
          | AntiExpr String
          deriving (Show, Typeable, Data, Eq)


class ConvertUntypedLambda a where
  convert :: Expr -> a


-- helper functions for conversion
type InlinedLambdaApply = Expr -- (\a -> a) 7
type ReferencedLambdaApply = Expr -- id 7
type ActualArgs = [Expr]

collectFnCall :: Expr -> (Either InlinedLambdaApply ReferencedLambdaApply, ActualArgs)
collectFnCall (Apply l@(Lambda _ _) e) = (Left l, [e])
collectFnCall (Apply v@(Binding _) e) = (Right v, [e])
collectFnCall (Apply e1 e2) = second (++ [e2]) $ collectFnCall e1
collectFnCall _ = undefined
