
module Parser where

import Data.Generics
import Text.ParserCombinators.Parsec

import Lang

-- Original code taken from paper "Why It’s Nice to be Quoted: Quasiquoting for Haskell"

parens p = between (symbol "(") (symbol ")") p

small = lower <|> char '_'
large = upper

idchar = small <|> large <|> digit <|> char '\''
lexeme p = do x <- p
              spaces
              return x

symbol name = lexeme $ string name

ident :: CharParser () String
ident = lexeme $
  do c <- small
     cs <- many idchar
     return $ c : cs

var :: CharParser () Var
var = do v <- ident
         return $ V v
  <|> do string "$var:"
         v <- ident
         return $ AV v

expr :: CharParser () Expr
expr = do es <- many1 aexp
          return $ foldl1 Apply es

lexpr :: CharParser () Expr
lexpr = do
  v <- lexeme var
  lexeme $ symbol "="
  e <- lexeme expr
  lexeme $ symbol "in"
  ie <- lexeme expr
  return $ Let v e ie

antiExpr :: CharParser () Expr
antiExpr = do
  string "$exp:"
  e <- ident -- just read the string in
  return $ AntiExpr e

aexp :: CharParser () Expr
aexp = -- (try $ var >>= (return . Var))
  (try $ do v <- var
            return $ Var v)
  <|> do symbol "\\"
         v <- lexeme var
         lexeme $ symbol "->"
         e <- expr
         return $ Lambda v e
  <|> lexpr
  <|> antiExpr
  <|> parens expr

parseExpr :: Monad m => (String, Int, Int) -> String -> m Expr
parseExpr (file, line, col) s =
   case runParser p () "" s of
     Left err  -> fail $ show err
     Right e   -> return e
 where
   p = do  pos <- getPosition
           setPosition $
             (flip setSourceName) file $
             (flip setSourceLine) line $
             (flip setSourceColumn) col $
             pos
           spaces
           e <- expr
           eof
           return e
