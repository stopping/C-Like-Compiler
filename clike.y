%{
	#include <stdio.h>
	#include <string.h>
	#include "functions.h"
	
	extern int lineNumber;
	int returnType = NONE_TYPE;
	bool hasReturn = false;
	astNode *root;
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

/*
%destructor { free( $$ ); } <name>
%destructor { destroyIdList( $$ ); } <idList>
%destructor { destroySymbolNode( $$ ); } <snode_ptr>
%destructor { freeTree( $$ ); } <astnode_ptr>
*/

%token <name> ID;
%token <primType> VOID CHAR INT FLOAT;
%token <fVal> FLOATCON;
%token <iVal> INTCON;
%token LPAREN RPAREN LBRACK RBRACK LCURLY RCURLY COMMA SEMICOLON ASSIGN
%token WHILE FOR RETURN IF ELSE
%token UNRECOGNIZED

%type <snode_ptr> dclr f_prot;
%type <idlist_ptr> type_list comma_type_star;
%type <idlist_ptr> id_list comma_id_star;
%type <astnode_ptr> prog dcl_or_func dcl func comma_dclr_star comma_f_prot_star;
%type <astnode_ptr> expr expr_opt expr_list comma_expr_star expr_list_opt stmt stmt_opt stmt_semicolon_star assg_opt assg;
%type <primType> type void_type;

%left OR
%left AND
%nonassoc LESS LEQ GREATER GEQ EQ NEQ
%left PLUS MINUS
%left SLASH STAR
%right NOT UMINUS

%%

prog
: prog dcl_or_func { $$ = makeASTNode2( PROGRAM, $1, $2 ); root = $$; }
| /* eps */ { $$ = NULL; }

dcl_or_func
: dcl SEMICOLON { $$ = $1; }
| func { $$ = $1; }

dcl
: type dclr comma_dclr_star
	{ $$ = makeASTNode2( PROGRAM, makeGlobalNode( $2 ), $3 ); addSymbol( globalTable, $2 ); returnType = NONE_TYPE; }
| void_type f_prot comma_f_prot_star
	{ $$ = makeASTNode2( PROGRAM, makeGlobalNode( $2 ), $3 ); addSymbol( globalTable, $2 ); returnType = NONE_TYPE; }

comma_dclr_star
: COMMA dclr comma_dclr_star
	{ $$ = makeASTNode2( PROGRAM, makeGlobalNode( $2 ), $3 ); addSymbol( globalTable, $2 ); }
| /* eps */
	{ $$ = NULL; }

comma_f_prot_star
: COMMA f_prot comma_f_prot_star
	{ $$ = makeASTNode2( PROGRAM, makeGlobalNode( $2 ), $3 ); addSymbol( globalTable, $2 ); }
| /* eps */
	{ $$ = NULL; }

dclr
: f_prot
	{ $$ = $1; }
| ID 
	{ $$ = createSymbolNode( $1, PRIMITIVE, createLiteralType( returnType ) ); free($1); }
| ID LBRACK INTCON RBRACK
	{ $$ = createSymbolNode( $1, ARRAY, createArrayType( returnType, $3 ) ); free($1); }

f_prot
: ID LPAREN type_list RPAREN
	{ $$ = createSymbolNode( $1, FUN_PROT, createFprotType( returnType, $3 ) ); free($1); }
| ID LPAREN RPAREN
	{ $$ = createSymbolNode( $1, FUN_PROT, createFprotType( returnType, NULL ) ); free($1); }

type_list
: type comma_type_star
	{ $$ = createIdList( NULL, $1, $2 ); }

comma_type_star
// I changed this, may break things.
: COMMA type comma_type_star
	{ $$ = createIdList( NULL, $2, $3 ); }
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
	{ $$ = makeFunNode( getSymbol(globalTable, $2, FUNCTION), localTable, $12);
	if( getSymbol(globalTable, $2, FUNCTION) != NULL ) ((functionType *) (getSymbol(globalTable, $2, FUNCTION)->typeInfo))->symTable = localTable;
	returnType = NONE_TYPE; checkReturn(); tempNum = 0; free($2); }
| type ID LPAREN RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( NULL, localTable );
	addSymbol( globalTable, createSymbolNode( $2, FUNCTION, createFunctionType( $1, NULL ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ $$ = makeFunNode( getSymbol(globalTable, $2, FUNCTION), localTable, $10);
	if( getSymbol(globalTable, $2, FUNCTION) != NULL ) ((functionType *) (getSymbol(globalTable, $2, FUNCTION)->typeInfo))->symTable = localTable;
	returnType = NONE_TYPE; checkReturn(); tempNum = 0; free($2); }
| void_type ID LPAREN
	{ currType = NONE_TYPE; }
  id_list RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( $5, localTable );
    addSymbol( globalTable, createSymbolNode( $2, FUNCTION, createFunctionType( $1, $5 ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ $$ = makeFunNode( getSymbol(globalTable, $2, FUNCTION), localTable, $12);
	if( getSymbol(globalTable, $2, FUNCTION) != NULL ) ((functionType *) (getSymbol(globalTable, $2, FUNCTION)->typeInfo))->symTable = localTable;
	returnType = NONE_TYPE; tempNum = 0; free($2); }
| void_type ID LPAREN RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( NULL, localTable );
    addSymbol( globalTable, createSymbolNode( $2, FUNCTION, createFunctionType( $1, NULL ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ $$ = makeFunNode( getSymbol(globalTable, $2, FUNCTION), localTable, $10);
	if( getSymbol(globalTable, $2, FUNCTION) != NULL ) ((functionType *) (getSymbol(globalTable, $2, FUNCTION)->typeInfo))->symTable = localTable;
	returnType = NONE_TYPE; tempNum = 0; free($2); }
| ID LPAREN
	{ currType = NONE_TYPE; returnType = INT_TYPE; }
  id_list RPAREN
	{ localTable = makeSymbolTable(); }
  loc_dcl_star
	{ updateTypes( $4, localTable );
    addSymbol( globalTable, createSymbolNode( $1, FUNCTION, createFunctionType( INT_TYPE, $4 ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ $$ = makeFunNode( getSymbol(globalTable, $1, FUNCTION), localTable, $11);
	if( getSymbol(globalTable, $1, FUNCTION) != NULL ) ((functionType *) (getSymbol(globalTable, $1, FUNCTION)->typeInfo))->symTable = localTable;
	returnType = NONE_TYPE; checkReturn(); tempNum = 0; free($1); }
| ID LPAREN RPAREN
	{ localTable = makeSymbolTable(); returnType = INT_TYPE; }
  loc_dcl_star
	{ updateTypes( NULL, localTable );
    addSymbol( globalTable, createSymbolNode( $1, FUNCTION, createFunctionType( INT_TYPE, NULL ) ) ); }
  LCURLY loc_dcl_star stmt_semicolon_star RCURLY
	{ $$ = makeFunNode( getSymbol(globalTable, $1, FUNCTION), localTable, $9);
	if( getSymbol(globalTable, $1, FUNCTION) != NULL ) ((functionType *) (getSymbol(globalTable, $1, FUNCTION)->typeInfo))->symTable = localTable;
	returnType = NONE_TYPE; checkReturn(); tempNum = 0; free($1); }

loc_dcl_star
: loc_dcl_star loc_dcl
| /* eps */

stmt_semicolon_star
// I changed this, may break things.
: stmt SEMICOLON stmt_semicolon_star
	{ $$ = makeASTNode2( LIST_ST, $1, $3 ); }
| /* eps */
	{ $$ = NULL; }

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
// I changed this, may break things.
: COMMA ID comma_id_star
	{ $$ = createIdList( $2, currType, $3 ); }
| /* eps */
	{ $$ = NULL; }

stmt
: IF LPAREN expr RPAREN stmt 
	{ $$ = makeASTNode3( IF_ST, $3, $5, NULL ); areCompatible( $3->primType, BOOL_TYPE ); }
| IF LPAREN expr RPAREN stmt ELSE stmt
	{ $$ = makeASTNode3( IF_ST, $3, $5, $7 ); areCompatible( $3->primType, BOOL_TYPE ); }
| WHILE LPAREN expr RPAREN stmt_opt
	{ $$ = makeASTNode2( WHILE_ST, $3, $5 ); areCompatible( $3->primType, BOOL_TYPE ); }
| FOR LPAREN assg_opt SEMICOLON expr_opt SEMICOLON assg_opt RPAREN stmt_opt
	{ $$ = makeASTNode4( FOR_ST, $3, $5, $7, $9 ); areCompatible( $5->primType, BOOL_TYPE ); }
| RETURN
	{ $$ = makeASTNode1( RETURN_ST, NULL ); areCompatible( VOID_TYPE, returnType ); hasReturn = true; }
| RETURN expr
	{ $$ = makeASTNode1( RETURN_ST, $2 ); areCompatible( $2->primType, returnType ); hasReturn = true; }
| assg
	{ $$ = $1; }
| ID LPAREN expr_list_opt RPAREN
	{ if(getSymbol( globalTable, $1, FUNCTION )) $$ = makeASTNode2( FUNCALL_ST, makeIdNode(getSymbol( globalTable, $1, FUNCTION )), $3 );
	else if(getSymbol( globalTable, $1, FUN_PROT )) $$ = makeASTNode2( FUNCALL_ST, makeIdNode(getSymbol( globalTable, $1, FUN_PROT )), $3);
	else $$ = NULL;
	isValidFunctionCall( $1, $3 ); areCompatible( literalTypeOf( $1 ), VOID_TYPE ); free($1); }
| LCURLY stmt_semicolon_star RCURLY
	{ $$ = $2; }

stmt_opt
: stmt
	{ $$ = $1; }
| /* eps */
	{ $$ = NULL; }

assg_opt
: assg
	{ $$ = $1; }
| /* eps */
	{ $$ = NULL; }

expr_opt
: expr
	{ $$ = $1; }
| /* eps */
	{ $$ = NULL; }

expr_list_opt
: expr_list
	{ $$ = $1; }
| /* eps */
	{ $$ = NULL; }

expr_list
: expr comma_expr_star
	{ $$ = makeASTNode2( COMMA_EX, $1, $2 ); }
	//{ $$ = createIdList( NULL, $1, $2 ); }

comma_expr_star
// I changed this, may break things.
: COMMA expr comma_expr_star
	{ $$ = makeASTNode2( COMMA_EX, $2, $3 ); }
	//{ $$ = createIdList( NULL, $3, $1 ); }
| /* eps */
	{ $$ = NULL; }

assg
: ID ASSIGN expr
	{ $$ = makeASTNode2( ASSIGN_ST, makeIdNode( getSymbol2( $1, PRIMITIVE) ), $3); areCompatible( literalTypeOf( $1 ), $3->primType ); isSymbolType( $1, PRIMITIVE ); free($1); }
| ID LBRACK expr RBRACK ASSIGN expr
	{ $$ = makeASTNode2( ASSIGN_ST, makeASTNode2( ARRAY_EX, makeIdNode(getSymbol2( $1, ARRAY )), $3 ), $6); areCompatible( literalTypeOf( $1 ), $6->primType ); isSymbolType( $1, ARRAY ); areCompatible( INT_TYPE, $3->primType ); free($1); }

expr
: NOT expr { $$ = makeASTNode1( NOT_EX, $2 ); }
| MINUS expr %prec UMINUS { $$ = makeASTNode1( UNARYMINUS_EX, $2 ); }
| expr AND expr { $$ = makeASTNode2( AND_EX, $1, $3 ); }
| expr OR expr { $$ = makeASTNode2( OR_EX, $1, $3 ); }
| expr EQ expr { $$ = makeASTNode2( EQ_EX, $1, $3 ); }
| expr NEQ expr { $$ = makeASTNode2( NEQ_EX, $1, $3 );; }
| expr PLUS expr { $$ = makeASTNode2( PLUS_EX, $1, $3 ); }
| expr MINUS expr { $$ = makeASTNode2( MINUS_EX, $1, $3 ); }
| expr STAR expr { $$ = makeASTNode2( STAR_EX, $1, $3 ); }
| expr SLASH expr { $$ = makeASTNode2( SLASH_EX, $1, $3 ); }
| expr LEQ expr { $$ = makeASTNode2( LEQ_EX, $1, $3 ); }
| expr LESS expr { $$ = makeASTNode2( LESS_EX, $1, $3  ); }
| expr GEQ expr { $$ = makeASTNode2( GEQ_EX, $1, $3 ); }
| expr GREATER expr { $$ = makeASTNode2( GREATER_EX, $1, $3 ); }
| ID { $$ = makeIdNode( getSymbol2( $1, PRIMITIVE ) ); isSymbolType( $1, PRIMITIVE ); free($1); }
| ID LBRACK expr RBRACK { $$ = makeASTNode2( ARRAY_EX, makeIdNode(getSymbol2( $1, ARRAY )), $3 ); isSymbolType( $1, ARRAY ); areCompatible( INT_TYPE, $3->primType ); free($1); }
| ID LPAREN expr_list_opt RPAREN
	{ if(getSymbol( globalTable, $1, FUNCTION )) $$ = makeASTNode2( FUNCALL_EX, makeIdNode(getSymbol( globalTable, $1, FUNCTION )), $3 );
	else if(getSymbol( globalTable, $1, FUN_PROT )) $$ = makeASTNode2( FUNCALL_EX, makeIdNode(getSymbol( globalTable, $1, FUN_PROT )), $3 );
	isValidFunctionCall( $1, $3 ); free($1); }
| LPAREN expr RPAREN { $$ = $2; }
| INTCON { $$ = makeIntconNode($1); }
| FLOATCON { $$ = makeFloatconNode($1); }

%%

int main( int argc, char **argv ) {

	if( argc > 1 && strcmp(argv[1],"-im") == 0 ) {
		intermediate = true;
	} else {
		intermediate = false;
	}
	
	globalTable = makeSymbolTable();
	int err = 0;
	error = 0;
	tempNum = 0;
	labelNum = 0;

	err = yyparse();
	if(error == 0) {
		updateOffsets(globalTable);
		genCode(root);
		if(!intermediate) printDataSection();
		printCode(root->code);
		if(!intermediate) printStandardLibrary();
	}

	yylex_destroy();
	if(root != NULL && root->code != NULL) freeCode( root->code );
	freeTree( root );
	destroyTable( globalTable );
	
	return err;
}
