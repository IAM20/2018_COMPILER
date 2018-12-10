/****************************************************/
/* File: tiny.y                                     */
/* The TINY Yacc/Bison specification file           */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/
%{
#define YYPARSER /* distinguishes Yacc output from other code files */

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

#define YYSTYPE TreeNode *
static char * savedName; /* for use in assignments */
static int savedNum;
static int savedLineNo;  /* ditto */
static TreeNode * savedTree; /* stores syntax tree for later return */
static int yylex(void);
int yyerror(char *);

%}

%token IF WHILE RETURN INT VOID
%token ID NUM
%token ASSIGN EQ NE LT LE GT GE PLUS MINUS TIMES OVER LPAREN RPAREN
%token LBRACE RBRACE LCURLY RCURLY SEMI COMMA
%token ERROR

/*for dangling else*/
%nonassoc DANGLE_ELSE
%nonassoc ELSE

/* discarded */
%token THEN END REPEAT UNTIL READ WRITE

%% /* Grammar for TINY */

program     : dcl-list
                 { savedTree = $1;} 
            ;
saveID      : ID
                 { savedName = copyString(tokenString);
                   savedLineNo = lineno;
                 }
            ;
saveNUM     : NUM
                 { savedNum = atoi(tokenString);
                   savedLineNo = lineno;
                 }
            ;
dcl-list    : dcl-list dcl
                 { YYSTYPE t = $1;
                   if (t != NULL)
                   { while (t->sibling != NULL)
                        t = t->sibling;
                     t->sibling = $2;
                     $$ = $1; }
                     else $$ = $2;
                 }
            | dcl { $$ = $1; }
            ;
dcl         : var-dcl { $$ = $1; }
            | fun-dcl { $$ = $1; }
            ;
var-dcl     : type-spec saveID SEMI
                 { $$ = newDclNode(VarK);
                   $$->child[0] = $1;
                   $$->lineno = lineno;
                   $$->attr.name = savedName;
                   $$->type = $1->type;
                 }
            | type-spec saveID LBRACE saveNUM RBRACE SEMI
                 { $$ = newDclNode(ArrVarK);
                   $$->child[0] = $1;
                   $$->lineno = lineno;
                   $$->attr.arr.name = savedName;
                   $$->attr.arr.size = savedNum;
                   $$->type = $1->type;
                 }
            ;
type-spec   : INT
                 { $$ = newTypeNode(TypenameK);
                   $$->type = Integer;
                 }
            | VOID
                 { $$ = newTypeNode(TypenameK);
                   $$->attr.name = "Not_null";
                   $$->type = Void;
                 }
            ;
fun-dcl     : type-spec saveID
                 { $$ = newDclNode(FuncK);
                   $$->lineno = lineno;
                   $$->attr.name = savedName;
                 }
              LPAREN params RPAREN comp-stmt
                 { $$ = $3;
                   $$->child[0] = $1;
                   $$->child[1] = $5;
                   $$->child[2] = $7;
                   $$->type = $1->type;
                 }
            ;
params      : param-list { $$ = $1; }
            | VOID
                 { $$ = newParamNode(ParK);
                   $$->type = Void;
                 }
            ;
param-list  : param-list COMMA param
                 { YYSTYPE t = $1;
                   if (t != NULL)
                   { while (t->sibling != NULL)
                        t = t->sibling;
                     t->sibling = $3;
                     $$ = $1; }
                     else $$ = $3;
                 }
            | param { $$ = $1; }
            ;
param       : type-spec saveID
                 { $$ = newParamNode(ParK);
                   $$->child[0] = $1;
                   $$->attr.name = savedName;
                   $$->type = $1->type;
                 }
            | type-spec saveID LBRACE RBRACE
                 { $$ = newParamNode(ArrParK);
                   $$->child[0] = $1;
                   $$->attr.arr.name = savedName;
                   $$->type = $1->type;
                 }
            ;
comp-stmt   : LCURLY local-dcl stmt-list RCURLY
                 { $$ = newStmtNode(CompK);
                   $$->child[0] = $2;
                   $$->child[1] = $3;
                 }
            ;
local-dcl   : local-dcl var-dcl
                 { YYSTYPE t = $1;
                   if (t != NULL)
                   { while (t->sibling != NULL)
                        t = t->sibling;
                     t->sibling = $2;
                     $$ = $1; }
                     else $$ = $2;
                 }
            | { $$ = NULL; }
            ;
stmt-list   : stmt-list stmt
                 { YYSTYPE t = $1;
                   if (t != NULL)
                   { while (t->sibling != NULL)
                        t = t->sibling;
                     t->sibling = $2;
                     $$ = $1; }
                     else $$ = $2;
                 }
            | { $$ = NULL; }
            ;
stmt        : exp-stmt { $$ = $1; }
            | comp-stmt { $$ = $1; }
            | sel-stmt { $$ = $1; }
            | iter-stmt { $$ = $1; }
            | ret-stmt { $$ = $1; }
            ;
exp-stmt    : exp SEMI { $$ = $1;}
            | SEMI { $$ = $1;}
            ;
sel-stmt    : IF LPAREN exp RPAREN stmt %prec DANGLE_ELSE
                 { $$ = newStmtNode(IfK);
                   $$->child[0] = $3;
                   $$->child[1] = $5;
                 }
            | IF LPAREN exp RPAREN stmt ELSE stmt
                 { $$ = newStmtNode(IfK);
                   $$->child[0] = $3;
                   $$->child[1] = $5;
                   $$->child[2] = $7;
                 }
            ;
iter-stmt   : WHILE LPAREN exp RPAREN stmt
                 { $$ = newStmtNode(IterK);
                   $$->child[0] = $3;
                   $$->child[1] = $5;
                 }
            ;
ret-stmt    : RETURN SEMI
                 { $$ = newStmtNode(RetK);
                   $$->child[0] = NULL;
                 }
            | RETURN exp SEMI
                 { $$ = newStmtNode(RetK);
                   $$->child[0] = $2;
                 }
            ;
exp         : var ASSIGN exp
                 { $$ = newExpNode(AssignK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                 }
            | simple-exp { $$ = $1; }
            ;
var         : saveID
                 { $$ = newExpNode(IdK);
                   $$->attr.name = savedName;
                 }
            | saveID
                 { $$ = newExpNode(ArrIdK);
                   $$->attr.arr.name = savedName;
                 }
              LBRACE exp RBRACE { $$ = $2; $$->child[0] = $4; }
            ;
simple-exp  : add-exp EQ add-exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = EQ;
                 }
            | add-exp NE add-exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = NE;
                 }
            | add-exp LT add-exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = LT;
                 }
            | add-exp LE add-exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = LE;
                 }
            | add-exp GT add-exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = GT;
                 }
            | add-exp GE add-exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = GE;
                 }
            | add-exp { $$ = $1; }
            ;
add-exp     : add-exp PLUS term
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = PLUS;
                 }
            | add-exp MINUS term
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = MINUS;
                 }
            | term { $$ = $1; }
            ;
term        : term TIMES factor
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = TIMES;
                 }
            | term OVER factor
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = OVER;
                 }
            | factor { $$ = $1; }
            ;
factor      : LPAREN exp RPAREN { $$= $2; }
            | var { $$ = $1; }
            | call { $$ = $1; }
            | saveNUM
                 { $$ = newExpNode(ConstK);
                   $$->attr.val = savedNum;
                   $$->type = Integer;
                 }
            ;
call        : saveID
                 { $$ = newExpNode(CallK);
                   $$->attr.name = savedName;
                 }
              LPAREN args RPAREN { $$ = $2; $$->child[0] = $4; }
            ;
args        : arg-list { $$ = $1; }
            | { $$ = NULL; }
            ;
arg-list    : arg-list COMMA exp
                 { YYSTYPE t = $1;
                   if (t != NULL)
                   { while (t->sibling != NULL)
                        t = t->sibling;
                     t->sibling = $3;
                     $$ = $1; }
                   else $$ = $3;
                 }
            | exp { $$ = $1; }
            ;
%%

int yyerror(char * message)
{ fprintf(listing,"Syntax error at line %d: %s\n",lineno,message);
  fprintf(listing,"Current token: ");
  printToken(yychar,tokenString);
  Error = TRUE;
  return 0;
}

/* yylex calls getToken to make Yacc/Bison output
 * compatible with ealier versions of the TINY scanner
 */
static int yylex(void)
{ return getToken(); }

TreeNode * parse(void)
{ yyparse();
  return savedTree;
}

