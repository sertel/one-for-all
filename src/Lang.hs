{-# LANGUAGE DeriveDataTypeable #-}
module Lang where

import Data.Generics

data Var = V String
         -- TODO fn vars
         -- anti vars
         | AV String deriving (Show, Eq, Typeable, Data)

data Expr = Var Var
          | Apply Expr Expr
          | Lambda Var Expr
          | Let Var Expr Expr
          -- anti quotes
          | AntiExpr String
          deriving (Show, Typeable, Data)
