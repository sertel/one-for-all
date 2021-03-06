module One4All.Parser where

import Data.Generics
import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec

import Debug.Trace

import One4All.Lang

-- Original code taken from paper "Why It’s Nice to be Quoted: Quasiquoting for Haskell"
parens p = between (symbol "(") (symbol ")") p

small = lower <|> char '_'

large = upper

idchar = small <|> large <|> digit <|> char '\''

num :: CharParser () String
num = many1 digit

lexeme p = do
  x <- p
  spaces
  return x

symbol name = lexeme $ string name

ident :: CharParser () String
ident =
  lexeme $ do
    c <- small
    cs <- many idchar
    return $ c : cs

var :: CharParser () Var
var
      -- parserTrace $ "parsing var expresssion ..."
 =
  do v <- ident
      -- traceM $ "found var: " ++ (show v)
     return $ V v
     <|> do
    string "$var:"
    v <- ident
    return $ AV v

expr :: CharParser () Expr
expr
  -- parserTrace $ "parsing expresssion ..."
 = do
  es <- many1 aexp
  return $ foldl1 Apply es

lexpr :: CharParser () Expr
lexpr
  -- parserTrace "parsing let expresssion ..."
 = do
  symbol "let"
  -- parserTrace "found let ..."
  v <- lexeme var
  -- parserTrace "found var ..."
  symbol "="
  -- parserTrace "found = ..."
  -- lesson learned: this works only of the first parser uses a choice to parse
  -- the words. if it uses some sort of recursion and never dispatches then
  -- this "end"-parser is never being called!
  -- this is why `manyTill expr (try $ symbol "in")` did not work!
  e <- foldl1 Apply <$> manyTill aexp (try $ symbol "in")
  -- parserTrace "found e!"
  -- e <- lexeme expr
  -- parserTrace $ "found expr ..."
  ie <- lexeme expr
  -- parserTrace "found in-expression ..."
  return $ Let v e ie

litExpr :: CharParser () Expr
litExpr =
  (try $ do
     symbol "()"
     return $ Lit UnitLit) <|>
  (fmap (Lit . IntegerLit . read) $ lexeme num)

antiExpr :: CharParser () Expr
antiExpr
  -- parserTrace "parsing anti-expresssion ..."
 = do
  string "$exp:"
  e <- ident -- just read the string in
  return $ AntiExpr e

aexp :: CharParser () Expr
aexp -- (try $ var >>= (return . Var))
 =
  (try lexpr) <|>
  (do v <- var
      return $ Binding v) <|>
  (do symbol "\\"
      v <- lexeme var
      symbol "->"
      e <- expr
      return $ Lambda v e) <|>
  antiExpr <|>
  litExpr <|>
  parens expr

parseExpr :: Monad m => (String, Int, Int) -> String -> m Expr
parseExpr (file, line, col) s =
  case runParser p () "" s of
    Left err -> fail $ show err
    Right e -> return e
  where
    p = do
      pos <- getPosition
      setPosition $
        (flip setSourceName) file $
        (flip setSourceLine) line $ (flip setSourceColumn) col $ pos
      spaces
      e <- expr
      eof
      return e
