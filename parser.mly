/***********************************************************************/

%token PLUS MINUS TIMES DIV MOD
%token NOT AND LOR
%token BITAND BITOR BITXOR BITNOT
%token LE LEQ EQ GEQ GR
%token LPAR RPAR
%token BACK BAR
%token TRUE FALSE INFINTE

/***********************************************************************/

%token VAR CONST
%token INT BOOL PARA
%token COMMA DOTDOT 
%token DOT REF BANG
%token LSQ RSQ
%token LBR RBR

/***********************************************************************/

%token SEMICOL 
%token SKIP STOP SKIPIF SKIPASN
%token ASSIGN RANDOM
%token IF THEN ELSE FI
%token WHILE DO OD
%token FOR IN ROF
%token REPEAT UNTIL
%token CHOOSE OR COLON RO
%token CASE OF DEFAULT ESAC
%token BEGIN END
%token OUTPUT MKARRAY
%token GOTO

/***********************************************************************/

%token <int> NUM
%token <string> ID

%token EOF

/***********************************************************************/

%right COMMA
%right SEMICOL

%left AND LOR
%left BITAND BITOR BITXOR
%left LE LEQ EQ GEQ GR
%left PLUS MINUS
%left TIMES DIV MOD

%nonassoc UMINUS
%nonassoc NOT
%nonassoc BITNOT
%nonassoc LABEL

/***********************************************************************/

%start prog
%type <Declaration.decls*Statement.stmt> prog

/***********************************************************************/
%%
/***********************************************************************/

prog:
  block                         { ([], $1) }
| VAR decls block               { ($2, $3) }
;

/***********************************************************************/

decls:
  decl SEMICOL			{ $1 }
| decl SEMICOL decls           	{ List.append $1 $3 }
;

decl:
  ID COLON range		{ [($1, Declaration.Var($3))] }
;

/***********************************************************************/

range:
  BOOL                          { [0;1] }  
| INT				{ Global.interval 
                                  Declaration.minInt 
                                  Declaration.maxInt }
| LBR consts RBR 		{ List.sort compare $2 }
;

consts:
  const				{ [$1] }
| const DOTDOT const 		{ (Global.interval $1 $3) }
| consts COMMA consts 		{ List.append $1 $3 }
;

const:
  NUM				{  $1 }
| MINUS NUM %prec UMINUS 	{ -$2 } 
;

/***********************************************************************/

block:
  BEGIN END                     { Statement.Skip }
| BEGIN stmt END                { $2 }
;

/***********************************************************************/

aexpr:
  LPAR aexpr RPAR		{ $2 }
| NUM			        { Expression.Const($1) }
| id                            { $1 }
| MINUS aexpr %prec UMINUS      { Expression.Minus($2) }
| aexpr PLUS aexpr	        { Expression.Sum($1,$3) }
| aexpr MINUS aexpr	        { Expression.Diff($1,$3) }
| aexpr TIMES aexpr	        { Expression.Prod($1,$3) }
| aexpr DIV aexpr		{ Expression.Div($1,$3) }
| aexpr MOD aexpr		{ Expression.Mod($1,$3) }
| aexpr BITXOR aexpr		{ Expression.BXor($1,$3) }
| aexpr BITAND aexpr		{ Expression.BAnd($1,$3) }
| aexpr BITOR aexpr		{ Expression.BOr($1,$3) }
;

bexpr:
  LPAR bexpr RPAR		{ $2 }
| TRUE			        { Expression.True }
| FALSE			        { Expression.False }
| NOT bexpr		        { Expression.Not($2) }
| bexpr AND bexpr		{ Expression.And($1,$3) }
| bexpr LOR bexpr		{ Expression.Or($1,$3) }
| aexpr LE aexpr		{ Expression.Lesser($1,$3) }
| aexpr LEQ aexpr		{ Expression.LeEqual($1,$3) }
| aexpr EQ aexpr		{ Expression.Equal($1,$3) }
| aexpr GEQ aexpr		{ Expression.GrEqual($1,$3) }
| aexpr GR aexpr		{ Expression.Greater($1,$3) }
;

cond:
  bexpr                         { Expression.Not($1) }
;

id:
  ID                            { Expression.Var($1) }
;

/***********************************************************************/

stmt:
  BEGIN stmt END		{ $2 }
| stmt SEMICOL		        { $1 }
| ID COLON stmt %prec LABEL     { Statement.TaggedStmt($1,$3) }
| STOP			        { Statement.Stop }
| SKIP			        { Statement.Skip }
| id ASSIGN aexpr	        { Statement.Assign($1,$3) }
| id RANDOM range	        { Statement.Random($1,$3) }
| stmt SEMICOL stmt	        { Statement.Sequence($1,$3) }
| IF bexpr THEN stmt ELSE stmt FI { Statement.If($2,$4,$6) }
| WHILE bexpr DO stmt OD        { Statement.While($2,$4) }
| FOR stmt SEMICOL bexpr SEMICOL stmt 
  DO stmt OD                    { Statement.For($2,$4,$6,$8) }
| CASE aexpr cases default ESAC { Statement.Case($2,$3,$4) }
| REPEAT stmt UNTIL cond        { Statement.Repeat($2,$4) }
| CHOOSE alts RO                { Statement.Choose($2) }
| GOTO ID                       { Statement.Goto($2) }
;

cases:
  case                          { [$1] }
| case cases                    { $1::$2 }

case:
  OF NUM COLON stmt             { ($2,$4) }

default:
  DEFAULT COLON stmt            { $3 }

alts:
  alt OR alt                    { [$1;$3] }
| alt OR alts		        { $1::$3 }
;

alt:
  NUM COLON stmt		{ ($1,$3) }
;

/***********************************************************************/
