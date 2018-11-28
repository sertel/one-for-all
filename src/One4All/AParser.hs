module One4All.AParser where
-- import Text.Parsec
-- import qualified Text.Parsec.Token as P
-- import Text.Parsec.Language (haskellStyle)
--
--
-- callByNeedLambdaCalculus :: TokenParser st
-- callByNeedLambdaCalculus = makeTokenParser callByNeedDef
--
-- callByNeedDef :: LanguageDef st
-- callByNeedDef = haskellStyle { reservedOpNames= ["=","\\","->"]
--                              , reservedNames  = ["let","in"]
--                              }
--
-- -- The parser
-- -- ...
-- -- expr  =   parens expr
-- --        <|> identifier
-- --        -- <|> ...
--
--
--  -- The lexer
--  lever ::  GenLanguageDef s u m -> GenTokenParser s u m
--  lexer = P.makeTokenParser callByNeedDef
--
--  parens      = P.parens     lexer
--  braces      = P.braces     lexer
--  identifier  = P.identifier lexer
--  reserved    = P.reserved   lexer
--  reservedOp  = P.reservedOp lexer
--  whiteSpace  = P.whiteSpace lexer
--  -- ...
--
--  -- -- ident :: CharParser () String
--  -- ident = lexeme $
--  --   do c <- small
--  --      cs <- many idchar
--  --      return $ c : cs
--
--  -- var :: CharParser () Var
--  var = do
--        parserTrace $ "parsing var expresssion ..."
--        v <- identifier
--        -- traceM $ "found var: " ++ (show v)
--        return $ V v
--      <|> do
--        string "$var:"
--        v <- ident
--        return $ AV v
--
--  -- expr :: CharParser () Expr
--  expr = do
--    parserTrace $ "parsing expresssion ..."
--    es <- many1 aexp
--    return $ foldl1 Apply es
--
--  -- lexpr :: CharParser () Expr
--  lexpr = do
--    parserTrace "parsing let expresssion ..."
--
--    reserved "let"
--    parserTrace "found let ..."
--
--    v <- lexeme var
--    parserTrace "found var ..."
--
--    reservedOp "="
--
--    e <- expr
--
--    parserTrace "found e!"
--
--    reserved "in"
--
--    ie <-  expr
--    parserTrace "found in-expression ..."
--
--    return $ Let v e ie
--
--  antiExpr :: CharParser () Expr
--  antiExpr = do
--    parserTrace "parsing anti-expresssion ..."
--    string "$exp:"
--    e <- ident -- just read the string in
--    return $ AntiExpr e
--
--  aexp :: CharParser () Expr
--  aexp = -- (try $ var >>= (return . Var))
--    (try lexpr)
--    <|> do v <- var
--           return $ Var v
--    <|> do symbol "\\"
--           v <- lexeme var
--           symbol "->"
--           e <- expr
--           return $ Lambda v e
--    <|> antiExpr
--    <|> parens expr
--
-- parseExpr :: Monad m => (String, Int, Int) -> String -> m Expr
-- parseExpr (file, line, col) s =
--   case runParser p () "" s of
--     Left err  -> fail $ show err
--     Right e   -> return e
--   where
--     p = do  pos <- getPosition
--             setPosition $
--               (flip setSourceName) file $
--               (flip setSourceLine) line $
--               (flip setSourceColumn) col $
--               pos
--             spaces
--             e <- expr
--             eof
--             return e
--
-- -- https://hackage.haskell.org/package/parsec-3.1.13.0/docs/src/Text.Parsec.Language.html#haskellDef
-- --
-- -- -- | The language definition for the Haskell language.
-- --
-- -- haskellDef  :: LanguageDef st
-- -- haskellDef   = haskell98Def
-- --                 { identLetter    = identLetter haskell98Def <|> char '#'
-- --                 , reservedNames  = reservedNames haskell98Def ++
-- --                                    ["foreign","import","export","primitive"
-- --                                    ,"_ccall_","_casm_"
-- --                                    ,"forall"
-- --                                    ]
-- --                 }
-- --
-- -- haskell98Def :: LanguageDef st
-- -- haskell98Def = haskellStyle
-- --                 { reservedOpNames= ["::","..","=","\\","|","<-","->","@","~","=>"]
-- --                 , reservedNames  = ["let","in","case","of","if","then","else",
-- --                                     "data","type",
-- --                                     "class","default","deriving","do","import",
-- --                                     "infix","infixl","infixr","instance","module",
-- --                                     "newtype","where",
-- --                                     "primitive"
-- --                                     -- "as","qualified","hiding"
-- --                                    ]
-- --                 }
