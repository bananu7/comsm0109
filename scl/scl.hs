import Data.List
import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Data.Functor
import Control.Applicative
import Control.Monad.Combinators.Expr

data Unops = Neg | Not | PoInc | PoDec | PreInc | PreDec | Deref | Addrof deriving Show

data Binops = Add | Sub | Mul | Div | Euc | And | Or | Lshift | Rshift
            | Eq | Neq | Gt | Lt | Gte | Lte
            deriving Show

data Expr = Lit Int
          | Ident String
          | Unop Unops Expr
          | Binop Binops Expr Expr
          | CallExpr String [Expr]
          deriving Show

data Ptys = Tint | Tfloat | Tbool deriving Show

data Ty = Pty Ptys
        | Pointer Ty
        | UserTy String deriving Show

data VarDecl = VarDecl String Ty Expr deriving Show

data Stmt = VarStmt VarDecl
          | Return (Maybe Expr)
          | CallStmt String [Expr]
          | If Expr Stmt [(Expr, Stmt)] (Maybe Stmt)
          | Block [Stmt]
          deriving Show

data FnDecl = FnDecl String [(String, Ty)] Ty [Stmt] deriving Show

type Module = [Either FnDecl VarDecl]

type Parser = Parsec Void String

pSc :: Parser ()
pSc = Text.Megaparsec.Char.Lexer.space space1 lineComment blockComment
  where
    lineComment = skipLineComment "//"
    blockComment = skipBlockComment "/*" "*/"

pLexeme :: Parser a -> Parser a
pLexeme = Text.Megaparsec.Char.Lexer.lexeme pSc

pSymbol :: String -> Parser String
pSymbol = Text.Megaparsec.Char.Lexer.symbol pSc

pParens :: Parser a -> Parser a
pParens = between (pSymbol "(") (pSymbol ")")

pBrackets :: Parser a -> Parser a
pBrackets = between (pSymbol "{") (pSymbol "}")

reserved :: [String]
reserved = ["fn", "if", "else", "let", "return"]

pIdent :: Parser String
pIdent = do
  candidate <- (:) <$> letterChar <*> many alphaNumChar
  pSc
  if candidate `elem` reserved then
    fail $ "keyword " ++ candidate ++ " cannot be an identifier"
  else return candidate

pCall :: (String -> [Expr] -> a) -> Parser a
pCall ctor = ctor <$> pIdent <*> pParens (pExpr `sepBy` pSymbol ",") <* pSc 

pExpr = makeExprParser term ops
  where
    parseSomeUnary p = foldr1 (.) <$> some p
    pLit = pLexeme $ Lit <$> read <$> some digitChar
    pIdentExpr = Ident <$> pIdent
    term = pLit <|> try (pCall CallExpr) <|> pIdentExpr <|> pParens pExpr
    ops = [ [ Postfix (Unop PoInc <$ pSymbol "++")
            , Postfix (Unop PoDec <$ pSymbol "--") ]
          , [ Prefix (Unop PreInc <$ pSymbol "++")
            , Prefix (Unop PreDec <$ pSymbol "--")
            , Prefix (Unop Neg <$ pSymbol "-")
            , Prefix (Unop Not <$ pSymbol "!")
            , Prefix (parseSomeUnary (Unop Deref <$ pSymbol "*"))
            , Prefix (parseSomeUnary (Unop Addrof <$ pSymbol "&")) ]
          , [ InfixL (Binop Mul <$ pSymbol "*")
            , InfixL (Binop Div <$ pSymbol "/")
            , InfixL (Binop Euc <$ pSymbol "%") ]
          , [ InfixL (Binop Add <$ pSymbol "+")
            , InfixL (Binop Sub <$ pSymbol "-") ]
          , [ InfixL (Binop Lshift <$ pSymbol "<<")
            , InfixL (Binop Rshift <$ pSymbol ">>") ]
          , [ InfixL (Binop Gt  <$ pSymbol "<")
            , InfixL (Binop Gte <$ pSymbol "<=")
            , InfixL (Binop Lt  <$ pSymbol ">")
            , InfixL (Binop Lte <$ pSymbol ">=") ]
          , [ InfixL (Binop Eq  <$ pSymbol "==")
            , InfixL (Binop Neq <$ pSymbol "!=") ]
          , [ InfixL (Binop And <$ pSymbol "&&")
            , InfixL (Binop Or  <$ pSymbol "||") ] ]

pPty :: Parser Ptys
pPty =  try (Tint <$ pSymbol "int")
    <|> try (Tfloat <$ pSymbol "float")
    <|> Tbool <$ pSymbol "bool"

pTy :: Parser Ty
pTy =  Pty <$> pPty
   <|> (UserTy <$> pIdent) 

pVarDecl :: Parser VarDecl
pVarDecl = VarDecl <$> (pSymbol "let" *>) pIdent
                   <*> (pSymbol ":" *>) pTy
                   <*> (pSymbol "=" *>) pExpr

p1Stmt :: Parser Stmt
p1Stmt =  try (VarStmt <$> pVarDecl)
      <|> try (Return <$> (pSymbol "return" *>) (option Nothing (Just <$> pExpr)))
      <|> try (pCall CallStmt)
      <|> (If <$> (pSymbol "if" *>) (pParens pExpr)
              <*> (pBrackets pStmt)
              <*> try (many $ (pSymbol "else if" *>) $ (,) <$> (pParens pExpr) <*> (pBrackets pStmt))
              <*> (option Nothing (Just <$> (pSymbol "else" *>) (pBrackets pStmt))))

pStmt :: Parser Stmt
pStmt = Block <$> p1Stmt `sepEndBy` (pSymbol ";")

pFnDecl :: Parser FnDecl
pFnDecl = FnDecl <$> (pSymbol "fn" *>) pIdent <*> (pParens pParams) <*> (pSymbol ":" *>) pTy <*> ((:[]) <$> pStmt) where
  pParams :: Parser [(String, Ty)]
  pParams = ((,) <$> pIdent <*> (pSymbol ":" *>) pTy) `sepBy` (pSymbol ",")

-- data FnDecl = FnDecl String [(String, Ty)] Ty [Stmt] deriving Show

-- type Module = [Either FnDecl VarDecl]

prettyShowUnops :: Unops -> String
prettyShowUnops Neg = "-"
prettyShowUnops Not = "!"
prettyShowUnops Deref = "*"

prettyShowBinops :: Binops -> String
prettyShowBinops Add = "+"
prettyShowBinops Sub = "-"
prettyShowBinops Mul = "*"
prettyShowBinops Div = "/"
prettyShowBinops Euc = "%"
prettyShowBinops And = "&&"
prettyShowBinops Or  = "||"
prettyShowBinops Lshift = "<<"
prettyShowBinops Rshift = ">>"
prettyShowBinops Eq  = "=="
prettyShowBinops Neq = "!="
prettyShowBinops Gt  = ">"
prettyShowBinops Lt  = "<"
prettyShowBinops Gte = ">="
prettyShowBinops Lte = "<="

prettyShowExpr :: Expr -> String
prettyShowExpr (Lit x) = show x
prettyShowExpr (Ident x) = x
prettyShowExpr (Unop PoInc ex) = "(" ++ prettyShowExpr ex ++ ")++"
prettyShowExpr (Unop PoDec ex) = "(" ++ prettyShowExpr ex ++ ")--"
prettyShowExpr (Unop PreInc ex) = "++(" ++ prettyShowExpr ex ++ ")"
prettyShowExpr (Unop PreDec ex) = "--(" ++ prettyShowExpr ex ++ ")"
prettyShowExpr (Unop op ex) = prettyShowUnops op ++ "(" ++ prettyShowExpr ex ++ ")"
prettyShowExpr (Binop op lhs rhs) = prettyShowExpr lhs ++ " " ++ prettyShowBinops op ++ " " ++ prettyShowExpr rhs
prettyShowExpr (CallExpr f args) = f ++ "(" ++ (intercalate "," (fmap prettyShowExpr args)) ++ ")"

prettyShowTy :: Ty -> String
prettyShowTy (Pty Tint) = "int"
prettyShowTy (Pointer ty) = prettyShowTy ty ++ "*"
prettyShowTy (UserTy s) = s

prettyShowVarDecl (VarDecl n ty val) = "let " ++ n ++ " : " ++ (prettyShowTy ty) ++ " = " ++ (prettyShowExpr val)

prettyShowStmt :: Stmt -> String
prettyShowStmt (VarStmt decl) = prettyShowVarDecl decl
prettyShowStmt (Return val) = "return" ++ (maybe "" ((" " ++) . prettyShowExpr) val) ++ ";"
prettyShowStmt (CallStmt f args) = f ++ "(" ++ (intercalate "," (fmap prettyShowExpr args)) ++ ")"
prettyShowStmt (If cond then1 elses elsesEnd) =
  "if (" ++ (prettyShowExpr cond) ++ ") {\n" ++
  (prettyShowStmt then1) ++
  "\n}" ++
  (foldr (++) "" (prettyShowElseIf <$> elses)) ++ 
  (maybe "" ((" else {\n" ++) . (++ "\n}") . prettyShowStmt) elsesEnd)
    where
      prettyShowElseIf (cond, then2) = " else if (" ++ (prettyShowExpr cond) ++ ") {\n" ++ (prettyShowStmt then2) ++ "\n}"
prettyShowStmt (Block sts) = intercalate ";\n" (fmap prettyShowStmt sts)
