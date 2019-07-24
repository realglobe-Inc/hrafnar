{
module Hrafnar.Recipe.Parser where

import Hrafnar.Recipe.Lexer
import Hrafnar.Recipe.Types
import Hrafnar.Recipe.Annotation
import Hrafnar.Recipe.AST

import qualified Data.Map as MA
import qualified Data.Text as ST
}

%name parser program
%name lineParser line
%name exprParser expr
%name declParser decl
%error { parseError }
%lexer { lexwrap } { TkEof }
%monad { Alex }
%tokentype { Token }

%token
-- | Symbols
';'     { TkSep _ }
'='     { TkEqual _ }
'('     { TkLParen $$ }
')'     { TkRParen _ }
'{'     { TkLBrace _ }
'}'     { TkRBrace _ }
'\\'    { TkLambda $$ }
'->'    { TkArrow _ }
'|'     { TkVLine _ }
','     { TkComma _ }
':'     { TkColon _ }
'_'     { TkUnderscore $$ }

-- | Literals.
NUM     { TkInt $$ }
VARID   { TkVarId $$ }
CONID   { TkConId $$ }
VARSYM  { TkVarSym $$ }

-- | Reserved keywords.
'let'   { TkLet $$ }
'in'    { TkIn _ }
'if' { TkIf $$ }
'then' { TkThen _ }
'else' { TkElse _ }
'data'  { TkData _ }
'case'  { TkCase $$ }
'of'  { TkOf _ }
'do'  { TkDo $$ }

%right IF
%left APPLY
%nonassoc DECS

%%
-- | Top level declaretions.
program :: { [Decl] }
program:        decls        { $1 }

-- | For repl.
line :: { Line }
line
  : expr { ExprLine $1 }
  | decl { DeclLine $1 }

-- | Declarations.
decls :: { [Decl] }
decls
  : decl ';' decls %prec DECS { $1 : $3 }
  | decl ';' { [$1] }
  | decl { [$1] }

-- | A declaration.
decl :: { Decl }
decl
  : VARID '=' expr { ExprDecl (fst $1) $3 }
  | datadecl { $1 }
  | tyanno { $1 }

-- | Type annotation.
tyanno :: { Decl }
tyanno
  : varids ':' tylit { TypeAnno $1 $3 }

-- | Identifiers of variables.
varids :: { [Name] }
varids
  : VARID ',' varids { fst $1 : $3 }
  | VARID { [fst $1] }

-- | Type literature.
tylit :: { Type }
tylit
  : tyfun { $1 }
  | tycon { $1 }
  | tyvar { $1 }
  | '(' tylit ')'  { $2 }

tyfun :: { Type }
tyfun
  : CONID '->' tylit { TyFun (TyCon $ fst $1) $3 }

tycon :: { Type }
tycon
  : CONID { TyCon (fst $1) }

tyvar :: { Type }
tyvar
  : VARID { TyVar . TV $ fst $1 }

-- | Data constructor declaration.
datadecl :: { Decl }
datadecl
  : 'data' CONID varids '=' datacons { DataDecl (fst $2) $3 (shrinkTypes (fst $2) $5) }
  | 'data' CONID '=' datacons { DataDecl (fst $2) [] (shrinkTypes (fst $2) $4) }

datacons :: { [(Name, [Type])] }
datacons
  : datacon '|' datacons { $1 : $3 }
  | datacon { [$1] }

tylits :: { [Type] }
tylits
  : tylit tylits { $1 : $2 }
  | tylit { [$1] }
  | {- empty -} { [] }

datacon :: { (Name, [Type]) }
datacon
  : CONID tylits { (fst $1, $2) }


-- | Expression.
expr :: { Expr }
expr
  : 'if' expr 'then' expr 'else' expr %prec IF { At $1 $ If $2 $4 $6 }
  | 'case' expr 'of' '{' branches '}'  { At $1 $ Case $2 $5 }
  | term actualArgs %prec APPLY { mkApp $1 $2 }
  | term { $1 }
  | lambda { $1 }
  | expr VARSYM expr { binop $1 $2 $3 }
  | expr VARSYM expr VARSYM expr { assoc $1 $2 $3 $4 $5 }
  | 'let' decls 'in' expr { At $1 $ Let $2 $4 }
  | 'do' '{' exprs '}' { At $1 $ Do $3 }

exprs :: { [Expr] }
exprs
  : expr ';' exprs { $1 : $3 }
  | expr { [$1] }

term :: { Expr }
term
  : varid       { $1 }
  | conid       { $1 }
  | num         { $1 }
  | tuple       { $1 }
  | '(' expr ')' { $2 }

varid :: { Expr }
varid
  : VARID { At (snd $1) (Var (fst $ $1)) }

conid :: { Expr }
conid
  : CONID { At (snd $1) (Var (fst $ $1)) }

-- | For "case" exspression.
branches :: { [(Pat, Expr)] }
branches
  : branch ';' branches { $1 : $3 }
  | branch { [$1] }

branch :: { (Pat, Expr) }
branch
  : casepat '->' expr { ($1, $3) }

casepat :: { Pat }
casepat
  : VARID { At (snd $1) (PVar $ fst $1) }
  | '_' { At $1 PWildcard }
  -- TODO: temporary implementations
  | NUM { At (snd $1) (PLit . Int $ fst $1) }
  | CONID { At (snd $1) (PCon (fst $1) []) }
  | CONID casepats { At (snd $1) (PCon (fst $1) $2) }

casepats :: { [Pat] }
casepats
  : casepat casepats { $1 : $2 }
  | '(' casepat ')' casepats { $2 : $4 }
  | casepat { [$1] }
  | '(' casepat ')' { [$2] }

-- | Literals.
--literal :: { Lit }
--literal
--  : NUM { (Int $ fst $1) }

num :: { Expr }
num
  : NUM { At (snd $1) (Lit . Int . fst $ $1) }

-- | Tuple.
tuple :: { Expr }
tuple
  : '(' ')' { At $1 (Lit $ Tuple []) }
  | '(' tuple_member ')' { At $1 (Lit $ Tuple $2) }

tuple_member :: { [Expr] }
tuple_member
  : expr ',' tuple_member { $1 : $3 }
  | expr ',' expr { [$1, $3] }


lambda :: { Expr }
lambda
  : '\\' formalArgs '->' expr { mkLambda $4 $2 }

actualArgs :: { [Expr] }
actualArgs
  : term { [$1] }
  | term actualArgs { $1 : $2 }

formalArgs :: { [Expr] }
formalArgs
  : varid { [$1] }
  | varid formalArgs { $1 : $2 }
{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)


parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

-- parseHML s = runAlex s $ alexSetUserState (AlexUserState operators MA.empty) >> parser
}
