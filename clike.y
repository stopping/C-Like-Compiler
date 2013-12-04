%{
	#include <stdio.h>
	#include <string.h>
	#include "functions.h"
	
	extern int lineNumber;
	int returnType = NONE_TYPE;
	bool hasReturn = false;
%}

%locations
%expect 1

%union {
	symbolNode *snode_ptr;
	idList *idlist_ptr;
	astNode *astnode_ptr;
	int iVal;
	float fVal;
	PrimitiveType primType;
	char *name;
}

%destructor { free( $$ ); } <name>
%destructor { destroyIdList( $$ ); } <idList>
%destructor { destroySymbolNode( $$ ); } <snode_ptr>

%token <name> ID;
%token <type> VOID CHAR INT FLOAT;
%token <fVal> FLOATCON;
%token <iVal> INTCON;
%token LPAREN RPAREN LBRACK RBRACK LCURLY RCURLY COMMA SEMICOLON ASSIGN
%token WHILE FOR RETURN IF ELSE
%token UNRECOGNIZED

%type <snode_ptr> dclr f_prot;
%type <idlist_ptr> type_list comma_type_star;
%type <idlist_ptr> id_list comma_id_star;
%type <idlist_ptr> expr_list comma_expr_star expr_list_opt;
%type <astnode_ptr> expr expr_opt stmt;
%type <litType> type void_type;

%left OR
%left AND
%nonassoc LESS LEQ GREATER GEQ EQ NEQ
%left PLUS MINUS
%left SLASH STAR
%right NOT UMINUS

%%

prog
: prog dcl_or_func
| /* eps */

dcl_or_func
: dcl SEMICOLON
| func

dcl
: type dclr comma_dclr_star
	{ addSymbol( globalTable, $2 ); returnType = NONE_TYPE; }
| void_type f_prot comma_f_prot_star
	{ addSymbol( globalTable, $2 ); returnType = NONE_TYPE; }

comma_dclr_star
: comma_dclr_star COMMA dclr
	{ addSymbol( globalTable, $3 ); }
| /* eps */

comma_f_prot_star
: comma_f_prot_star COMMA f_prot
	{ addSymbol( globalTable, $3 ); }
| /* eps */

dclr
: f_prot
	{ $$ = $1; }
| ID 
	{ $$ = createSymbolNode( $1, LITERAL, createLiteralType( returnType ) ); }
| ID LBRACK INTCON RBRACK
	{ $$ = createSymbolNode( $1, ARRAY, createArrayType( returnType, $3 ) ); }

f_prot
: ID LPAREN type_list RPAREN
	{ $$ = createSymbolNode( $1, FUN_PROT, createFprotType( returnType, $3 ) ); }
| ID LPAREN RPAREN
	{ $$ = createSymbolNode( $1, FUN_PROT, createFprotType( returnType, NULL ) ); }

type_list
: type comma_type_star
	{ $$ = createIdList( NULL, $1, $2 ); }

comma_type_star
: comma_type_star COMMA type
	{ $$ = createIdList( NULL, $3, $1 ); }
| /* eps */
	{ $$ = NULL; }

type
: CHAR
	{ $$ = CHAR_TYPE; if( returnType == NONE_TYPE ) returnType = CHAR_TYPE; }
| INT
	{ $$ = INT_TYPE; if( returnType == NONE_TYPE ) returnType = INT_TYPE; }
| FLOAT
	{ $$ = FLOAT_TYPE; if( returnType == NONE_TYPE ) returnType = FLOAT_TYPE; }

void_type
: VOID
	{ $$ = VOID_TYPE; if( returnType == NONE_TYPE ) returnType = VOID_TYPE; }

func
: type ID LPAREN 
	{ currType = NONE_TYPE; }
  id_list RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( $5, localTable );
	addSymbol( globalTable, createSymbolNode( $2, FUNCTION, createFunctionType( $1, $5 ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ destroyTable( localTable ); returnType = NONE_TYPE; checkReturn(); }
| type ID LPAREN RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( NULL, localTable );
	addSymbol( globalTable, createSymbolNode( $2, FUNCTION, createFunctionType( $1, NULL ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ destroyTable( localTable ); returnType = NONE_TYPE; checkReturn(); }
| void_type ID LPAREN
	{ currType = NONE_TYPE; }
  id_list RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( $5, localTable );
    addSymbol( globalTable, createSymbolNode( $2, FUNCTION, createFunctionType( $1, $5 ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ destroyTable( localTable ); returnType = NONE_TYPE; }
| void_type ID LPAREN RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( NULL, localTable );
    addSymbol( globalTable, createSymbolNode( $2, FUNCTION, createFunctionType( $1, NULL ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ destroyTable( localTable ); returnType = NONE_TYPE; }
| ID LPAREN
	{ currType = NONE_TYPE; returnType = INT_TYPE; }
  id_list RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( $4, localTable );
    addSymbol( globalTable, createSymbolNode( $1, FUNCTION, createFunctionType( INT_TYPE, $4 ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ destroyTable( localTable ); returnType = NONE_TYPE; checkReturn(); }
| ID LPAREN RPAREN
	{ localTable = makeSymbolTable(); returnType = INT_TYPE; }
  loc_dcl_star
	{ updateTypes( NULL, localTable );
    addSymbol( globalTable, createSymbolNode( $1, FUNCTION, createFunctionType( INT_TYPE, NULL ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ destroyTable( localTable ); returnType = NONE_TYPE; checkReturn(); }

loc_dcl_star
: loc_dcl_star loc_dcl
| /* eps */

stmt_semicolon_star
: stmt_semicolon_star stmt SEMICOLON
| stmt_semicolon_star error SEMICOLON
	{ yyerror("Invalid statement"); }
| /* eps */

loc_dcl
: type
	{ currType = $1; }
  id_list 
	{ addIdList( localTable, $3 ); }
  SEMICOLON

id_list
: ID comma_id_star
	{ $$ = createIdList( $1, currType, $2 ); }

comma_id_star
: comma_id_star COMMA ID
	{ $$ = createIdList( $3, currType, $1 ); }
| /* eps */
	{ $$ = NULL; }

stmt
: IF LPAREN expr RPAREN stmt 
	{ areCompatible( $3, BOOL_TYPE ); }
| IF LPAREN expr RPAREN stmt ELSE stmt
	{ areCompatible( $3, BOOL_TYPE ); }
| WHILE LPAREN expr RPAREN stmt_opt
	{ areCompatible( $3, BOOL_TYPE ); }
| FOR LPAREN assg_opt SEMICOLON expr_opt SEMICOLON assg_opt RPAREN stmt_opt
	{ areCompatible( $5, BOOL_TYPE ); }
| RETURN
	{ areCompatible( VOID_TYPE, returnType ); hasReturn = true; }
| RETURN expr
	{ areCompatible( $2, returnType ); hasReturn = true; }
| assg
| ID LPAREN expr_list_opt RPAREN
	{ isValidFunctionCall( $1, $3 ); isSymbolType( $1, FUNCTION ); areCompatible( literalTypeOf( $1 ), VOID_TYPE ); destroyIdList( $3 ); free($1); }
| LCURLY stmt_semicolon_star RCURLY

stmt_opt
: stmt
| /* eps */

assg_opt
: assg
| /* eps */

expr_opt
: expr
	{ $$ = $1; }
| /* eps */
	{ $$ = NONE_TYPE; }

expr_list_opt
: expr_list
	{ $$ = $1; }
| /* eps */
	{ $$ = NULL; }

expr_list
: expr comma_expr_star
	{ $$ = createIdList( NULL, $1, $2 ); }

comma_expr_star
: comma_expr_star COMMA expr
	{ $$ = createIdList( NULL, $3, $1 ); }
| /* eps */
	{ $$ = NULL; }

assg
: ID ASSIGN expr
	{ areCompatible( literalTypeOf( $1 ), $3 ); isSymbolType( $1, LITERAL ); free($1); }
| ID LBRACK expr RBRACK ASSIGN expr
	{ areCompatible( literalTypeOf( $1 ), $6 ); isSymbolType( $1, ARRAY ); areCompatible( $3, INT_TYPE ); free($1); }

expr
: NOT expr { $$ = makeASTNode( NOT_EX, $2 ); }
| MINUS expr %prec UMINUS { $$ = makeASTNode( UNARYMINUS_EX, $2 ); }
| expr AND expr { $$ = makeASTNode( AND_EX, $1, $3 ); }
| expr OR expr { $$ = makeASTNode( OR_EX, $1, $3 ); }
| expr EQ expr { $$ = makeASTNode( EQ_EX, $1, $3 ); }
| expr NEQ expr { $$ = makeASTNode( NEQ_EX, $1, $3 );; }
| expr PLUS expr { $$ = makeASTNode( PLUS_EX, $1, $3 ); }
| expr MINUS expr { $$ = makeASTNode( MINUS_EX, $1, $3 ); }
| expr STAR expr { $$ = makeASTNode( STAR_EX, $1, $3 ); }
| expr SLASH expr { $$ = makeASTNode( SLASH_EX, $1, $3 ); }
| expr LEQ expr { $$ = makeASTNode( LEQ_EX, $1, $3 ); }
| expr LESS expr { $$ = makeASTNode( LESS_EX, $1, $3  ); }
| expr GEQ expr { $$ = makeASTNode( GEQ_EX, $1, $3 ); }
| expr GREATER expr { $$ = makeASTNode( GREATER_EX, $1, $3 ); }
| ID { $$ = makeIdNode(getSymbol( localTable, $1, PRIMITIVE )); isSymbolType( $1, PRIMITIVE ); free($1); }
| ID LBRACK expr RBRACK { $$ = makeIdNode(getSymbol( localTable, $1, ARRAY )); isSymbolType( $1, ARRAY ); areCompatible( $3, INT_TYPE ); free($1); }
| ID LPAREN expr_list_opt RPAREN { $$ = makeIdNode(getSymbol( globalTable, $1, FUNCTION )); isSymbolType( $1, FUNCTION ); isValidFunctionCall( $1, $3 ); destroyIdList( $3 ); free($1); }
| LPAREN expr RPAREN { $$ = $2; }
| INTCON { $$ = makeIntconNode($1); }
| FLOATCON { $$ = makeFloatconNode($1); }

%%

int main( int argc, char **argv ) {
	globalTable = makeSymbolTable();
	int err = 0;

	err = yyparse();
	yylex_destroy();
	
	destroyTable( globalTable );
	
	return err;
}