#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "functions.h"
#include "clike.tab.h"

int error;
int currType;
int returnType;
int tempNum; // Indicates the next temporary variable which can be used
int labelNum; // Indicates the next label number to use (for ifs, loops, etc)
int paramNum;
int currOffset;

bool paramStart = false;
bool intermediate;
int paramNum;
char *funName;

bool debug = false;
int depth = 0;

symbolTable *globalTable;
symbolTable *localTable;
quad *quadList;

idList *createIdList( char *symbol, PrimitiveType type, idList *nextID ) {
	idList *list = malloc(sizeof(idList));
	list->symbol = symbol;
	list->type = type;
	list->nextID = nextID;
	return list;
}

void destroyIdList( idList *list ) {
	if( list == NULL ) return;
	idList *temp;
	while( list != NULL ) {
		if( list->symbol != NULL ) free( list->symbol );
		temp = list;
		list = list->nextID;
		free( temp );
	}
}

symbolTable *makeSymbolTable() {
	symbolTable *st = malloc(sizeof(symbolTable));
	int i;
	for(i = 0; i < TABLESIZE; i++)
		st->array[i] = NULL;
	return st;
}

void destroyTable( symbolTable *t ) {
	if( t == NULL ) return;
	symbolNode *temp;
	int i;
	for(i = 0; i < TABLESIZE; i++) {
		symbolNode *curr = t->array[i];
		while( curr != NULL ) {
			temp = curr;
			curr = curr->nextSymNode;
			destroySymbolNode( temp );
		}
	}
	free(t);
}

symbolNode *createSymbolNode( char *symbol, SymbolType type, void *typeInfo ) {
	symbolNode *symNode = malloc(sizeof(symbolNode));
	symNode->symbol = strdup(symbol);
	symNode->type = type;
	symNode->typeInfo = typeInfo;
	return symNode;
}

void destroySymbolNode( symbolNode *symNode ) {
	if( symNode == NULL ) return;
	idList *list;
	symbolTable *table;
	switch( symNode->type ) {
		case PRIMITIVE:
			break;
		case ARRAY:
			break;
		case FUNCTION:
			list = ((functionType*) (symNode->typeInfo))->paramList;
			table = ((functionType*) (symNode->typeInfo))->symTable;
			destroyIdList(list);
			destroyTable(table);
			break;
		case FUN_PROT:
			list = ((fprotType*) (symNode->typeInfo))->paramList;
			destroyIdList(list);
			break;
	}
	if(symNode->symbol != NULL) {
		free(symNode->symbol);
	}
	free(symNode->typeInfo);
	free(symNode);
}

literalType *createLiteralType( PrimitiveType dataType ) {
	literalType *type = malloc(sizeof(literalType));
	type->dataType = dataType;
	return type;
}

arrayType *createArrayType( PrimitiveType dataType, int size ) {
	arrayType *type = malloc(sizeof(arrayType));
	type->dataType = dataType;
	type->size = size;
	return type;
}

functionType *createFunctionType( PrimitiveType returnType, idList *paramList ) {
	functionType *type = malloc(sizeof(functionType));
	type->returnType = returnType;
	type->paramList = paramList;
	return type;
}

int numParams( symbolNode *functionNode ) {
	if(functionNode->type == FUNCTION) {
		functionType *typeInfo = (functionType *) functionNode->typeInfo;
		idList *param = typeInfo->paramList;
		return listLength(param);
	} else if(functionNode->type == FUN_PROT) {
		fprotType *typeInfo = (fprotType *) functionNode->typeInfo;
		idList *param = typeInfo->paramList;
		return listLength(param);
	} else {
		return 0;
	}

}

int listLength( idList *list ) {
	int i = 0;
	for(i = 0; list != NULL; i++) {
		list = list->nextID;
	}
	return i;
}

fprotType *createFprotType( PrimitiveType returnType, idList *paramList ) {
	fprotType *type = malloc(sizeof(fprotType));
	type->returnType = returnType;
	type->paramList = paramList;
	return type;
}

bool symbolExists( symbolTable *t, char *symbol ) {
	int c = hash(symbol);
	symbolNode *curr = t->array[c % TABLESIZE];
	while( curr != NULL ) {
		if( strcmp(curr->symbol, symbol) == 0 ) {
			return true;
		}
		curr = curr->nextSymNode;
	}
	return false;
}

symbolNode *getSymbol( symbolTable *t, char *symbol, SymbolType type ) {
	int c = hash(symbol);
	symbolNode *curr = t->array[c % TABLESIZE];
	while( curr != NULL ) {
		if( curr->type == type && strcmp(curr->symbol, symbol) == 0 ) {
			return curr;
		}
		curr = curr->nextSymNode;
	}
	return NULL;
}

symbolNode *getSymbol2( char *symbol, SymbolType type ) {
	symbolNode *local = getSymbol(localTable, symbol, type);
	if( local != NULL ) return local;
	return getSymbol(globalTable, symbol, type);
}

void addSymbol( symbolTable *t, symbolNode *symNode ) {
	if( symbolExists( t, symNode->symbol ) ) {
		if( symNode->type == FUNCTION ) {
			symbolNode *prototype = getSymbol( t, symNode->symbol, FUN_PROT );
			symbolNode *function = getSymbol( t, symNode->symbol, FUNCTION );
			if( function != NULL ) {
				destroySymbolNode( symNode );
				yyerror("Function already defined");
			} else if ( prototype != NULL ) {
				fprotType *prototypeInfo = (fprotType*) prototype->typeInfo;
				functionType *functionInfo = (functionType*) symNode->typeInfo;
				bool parametersCorrect = idListsEqual( prototypeInfo->paramList, functionInfo->paramList );
				bool returnTypeCorrect = prototypeInfo->returnType == functionInfo->returnType;
				if( parametersCorrect && returnTypeCorrect ) {
					
				} else if( !parametersCorrect ){
					yyerror("Argument mismatch with prototype");
				} else {
					yyerror("Return type mismatch with prototype");
				}
				int c = hash(symNode->symbol);
				symNode->nextSymNode = t->array[c % TABLESIZE];
				t->array[c % TABLESIZE] = symNode;
			} else {
				destroySymbolNode( symNode );
				yyerror("Symbol already declared as non-function");
			}	
		} else {
			destroySymbolNode( symNode );
			yyerror("Symbol already declared");
		}
	} else {
		int c = hash(symNode->symbol);
		symNode->nextSymNode = t->array[c % TABLESIZE];
		t->array[c % TABLESIZE] = symNode;
	}
}

void addIdList( symbolTable *t, idList *list ) {
	idList *head = list;
	while( list != NULL ) {
		if( symbolExists( t, list->symbol ) ) {
			yyerror("Symbol already declared in table");
		} else {
			char *symbol = list->symbol;
			int type = list->type;
			
			literalType *lt = createLiteralType( type );
			symbolNode *curr = createSymbolNode( symbol, PRIMITIVE, lt );
			
			addSymbol( t, curr );
		}
		list = list->nextID;
	}
	destroyIdList( head );
}

bool idListsEqual( idList *list1, idList *list2 ) {
	bool flag = true;
	while( list1 != NULL && list2 != NULL ) {
		if( list1->type != list2->type ) {
			flag = false;
		}
		list1 = list1->nextID;
		list2 = list2->nextID;
	}
	
	if( list1 != NULL || list2 != NULL ) {
		flag = false;
	}
	
	return flag;
}

bool idListsCompatible( idList *list1, idList *list2 ) {
	bool flag = true;
	while( list1 != NULL && list2 != NULL ) {
		if( !areCompatible( list1->type, list2->type ) ) {
			flag = false;
		}
		list1 = list1->nextID;
		list2 = list2->nextID;
	}
	
	if( list1 != NULL || list2 != NULL ) {
		flag = false;
	}
	
	return flag;
}

void updateOffsets( symbolTable *t ) {
	if( t == NULL ) return;
	currOffset = 0; // Offset from stack pointer
	if( t == globalTable ) {
		symbolNode *temp;
		int i;
		for(i = 0; i < TABLESIZE; i++) {
			symbolNode *curr = t->array[i];
			while( curr != NULL ) {
				curr->offset = currOffset;
				curr->isGlobal = true;
				switch(curr->type) {
				case PRIMITIVE:
					if(((literalType *) curr->typeInfo)->dataType == FLOAT_TYPE)
						currOffset += 8;
					else
						currOffset += 4;
					break;
				case ARRAY: {
					int size = ((arrayType *) curr->typeInfo)->size;
					if(((arrayType *) curr->typeInfo)->dataType == FLOAT_TYPE)
						currOffset += 8*size;
					else if(((arrayType *) curr->typeInfo)->dataType == INT_TYPE)
						currOffset += 4*size;
					else
						currOffset += 1*size;
					break;
				}
				default: break;
				}
				curr = curr->nextSymNode;
			}
		}
		t->byteSize = currOffset;
	} else {
		symbolNode *temp;
		int i;
		for(i = 0; i < TABLESIZE; i++) {
			symbolNode *curr = t->array[i];
			while( curr != NULL ) {
				curr->offset = currOffset;
				curr->isGlobal = false;
				if(((literalType *) curr->typeInfo)->dataType == FLOAT_TYPE)
					currOffset += 8; // Change this based on data structure type
				else
					currOffset += 4; // Change this based on data structure type
				curr = curr->nextSymNode;
			}
		}
		t->byteSize = currOffset;
	}

}

void updateTypes( idList *list, symbolTable *t ) {
	idList *head = list;

	while( list != NULL ) {
		char *symbol = list->symbol;
		int c = hash(symbol);
		
		symbolNode *curr = t->array[c % TABLESIZE];
		while( curr != NULL ) {
			if( strcmp(curr->symbol, symbol) == 0 ) {
				list->type = ((literalType*) (curr->typeInfo))->dataType;
				break;
			}
			curr = curr->nextSymNode;
		}
		if( curr == NULL ) {
			yyerror("Formal type not declared");
		}
		list = list->nextID;
	}
	
	int i;
	for(i = 0; i < TABLESIZE; i++) {
		symbolNode *curr = t->array[i];
		while( curr != NULL ) {
			list = head;
			while( list != NULL ) {
				if( strcmp(curr->symbol, list->symbol) == 0 ) {
					break;
				}
				list = list->nextID;
			}
			if( list == NULL ) {
				yyerror("Formal type not in parameter list");
				break;
			}
			curr = curr->nextSymNode;
		}
	}
}

PrimitiveType literalTypeOf( char *symbol ) {

	symbolNode *curr;
	if( symbolExists( localTable, symbol ) ) {
		if( ( curr = getSymbol( localTable, symbol, FUNCTION ) ) != NULL )
			return ((functionType*) (curr->typeInfo))->returnType;
		else if( ( curr = getSymbol( localTable, symbol, ARRAY ) ) != NULL )
			return ((arrayType*) (curr->typeInfo))->dataType;
		else if( ( curr = getSymbol( localTable, symbol, PRIMITIVE ) ) != NULL )
			return ((literalType*) (curr->typeInfo))->dataType;
		else {
			//printf("Error searching local table");
			return ERROR_TYPE;
		}
	} else if ( symbolExists( globalTable, symbol ) ) {
		if( ( curr = getSymbol( globalTable, symbol, FUNCTION ) ) != NULL )
			return ((functionType*) (curr->typeInfo))->returnType;
		else if( ( curr = getSymbol( globalTable, symbol, FUN_PROT ) ) != NULL )
			return ((fprotType*) (curr->typeInfo))->returnType;
		else if( ( curr = getSymbol( globalTable, symbol, ARRAY ) ) != NULL )
			return ((arrayType*) (curr->typeInfo))->dataType;
		else if( ( curr = getSymbol( globalTable, symbol, PRIMITIVE ) ) != NULL )
			return ((literalType*) (curr->typeInfo))->dataType;
		else {
			//printf("Error searching global table");
			return ERROR_TYPE;
		}
	} else {
		//printf("Could not find symbol in any table");
		return ERROR_TYPE;
	}

}

PrimitiveType binaryExpressionType( PrimitiveType t1, PrimitiveType t2, OperatorType op ) {
	if( t1 == ERROR_TYPE || t2 == ERROR_TYPE ) {
		return ERROR_TYPE;
	}
	switch(op) {
	case COMPARISON:
		if( areCompatible( t1, t2 ) && ( t1 == CHAR_TYPE || t1 == INT_TYPE || t1 == FLOAT_TYPE ) )
			return BOOL_TYPE;
		else {
			char *buf = malloc(sizeof(char)*100);
			snprintf(buf,100,"Incompatible types for comparison: %d %d",t1,t2);
			yyerror(buf);
			free(buf);
			return ERROR_TYPE;
		}
	case ARITHMETIC:
		if( areCompatible( t1, t2 ) && ( t1 == CHAR_TYPE || t1 == INT_TYPE || t1 == FLOAT_TYPE ) )
			if( t1 == CHAR_TYPE || t1 == INT_TYPE ) return INT_TYPE;
			else return FLOAT_TYPE;
		else {
			char *buf = malloc(sizeof(char)*100);
			snprintf(buf,100,"Incompatible types for arithmetic: %d %d",t1,t2);
			yyerror(buf);
			free(buf);
			return ERROR_TYPE;
		}
	case LOGICAL:
		if( t1 == BOOL_TYPE && t2 == BOOL_TYPE )
			return BOOL_TYPE;
		else {
			char *buf = malloc(sizeof(char)*100);
			snprintf(buf,100,"Incompatible types for boolean expression: %d %d",t1,t2);
			yyerror(buf);
			free(buf);
			return ERROR_TYPE;
		}
	default:
		return ERROR_TYPE;
	}
}

PrimitiveType unaryExpressionType( PrimitiveType t, OperatorType op ) {
	if( op == UNARYMINUS && (t == FLOAT_TYPE || t == INT_TYPE || t == CHAR_TYPE) )
		return t;
	else if( op == BOOLNOT && t == BOOL_TYPE )
		return t;
	else
		return ERROR_TYPE;
}

bool areCompatible( PrimitiveType t1, PrimitiveType t2 ) {
	if( t1 == BOOL_TYPE && t2 == BOOL_TYPE )
		return true;
	else if( t1 == FLOAT_TYPE && t2 == FLOAT_TYPE )
		return true;
	else if( ( t1 == CHAR_TYPE || t1 == INT_TYPE ) && ( t2 == CHAR_TYPE || t2 == INT_TYPE ) )
		return true;
	else if( t1 == VOID_TYPE && t2 == VOID_TYPE )
		return true;
	else {
		char *buf = malloc(sizeof(char)*100);
		snprintf(buf,100,"Incompatible types: %d %d",t1,t2);
		yyerror(buf);
		free(buf);
		return false;
	}
}

bool isValidFunctionCall( char *symbol, astNode *exprTree ) {

	if( getSymbol( globalTable, symbol, FUNCTION ) ) {
		symbolNode *function = getSymbol( globalTable, symbol, FUNCTION );
		functionType *functionInfo = (functionType*) function->typeInfo;
		idList *list = exprTreeToList( exprTree );
		if( idListsCompatible( functionInfo->paramList, list ) ) {
			destroyIdList(list);
			return true;
		} else {
			destroyIdList(list);
			yyerror("Argument mismatch in function call");
			return false;
		}
	} else if( getSymbol( globalTable, symbol, FUN_PROT ) ) {
		symbolNode *function = getSymbol( globalTable, symbol, FUN_PROT );
		fprotType *functionInfo = (fprotType*) function->typeInfo;
		idList *list = exprTreeToList( exprTree );
		if( idListsCompatible( functionInfo->paramList, list ) ) {
			destroyIdList(list);
			return true;
		} else {
			destroyIdList(list);
			yyerror("Argument mismatch in function call");
			return false;
		}
	} else {
		yyerror("Invalid function call");
		return false;
	}

}

bool isSymbolType( char *symbol, SymbolType t ) {
	symbolNode *curr;
	if( ( curr = getSymbol( localTable, symbol, t ) ) != NULL )
		return true;
	else if( ( curr = getSymbol( globalTable, symbol, t ) ) != NULL )
		return true;
	else {
		yyerror("Symbol type mismatch");
		return false;
	}
}

idList *exprTreeToList( astNode *exprTree ) {
	if( exprTree == NULL ) {
		return NULL;
	}

	astNode2 *tree2 = (astNode2 *) exprTree;

	PrimitiveType t;
	if(tree2->child0->type == ID_EX)
		t = ((idNode *) tree2->child0)->primType;
	else
		t = tree2->child0->primType;
	return createIdList( NULL, t, exprTreeToList(tree2->child1) );
}

void checkReturn() {
	if( !hasReturn )
		yyerror("No return statement in non-void function");
	hasReturn = false;
}

int hash( char *str ) {
	char c;
	int sum = 0;
	int i;
	for(i = 0; str[i] != 0; i++) {
		sum += str[i];
	}
	return sum;
}

void yyerror(char *s) {
	error = 1;
	fprintf(stderr, "Error from %d:%d to %d:%d: %s\n", yylloc.first_line, yylloc.first_column, yylloc.last_line, yylloc.last_column, s);
}

// AST Construction

OperatorType toOpType(NodeType type) {
	switch(type) {
	case UNARYMINUS_EX:
		return UNARYMINUS;
	case NOT_EX:
		return BOOLNOT;
	case AND_EX:
	case OR_EX:
		return LOGICAL;
	case EQ_EX:
	case NEQ_EX:
	case LEQ_EX:
	case LESS_EX:
	case GEQ_EX:
	case GREATER_EX:
		return COMPARISON;
	case PLUS_EX:
	case MINUS_EX:
	case STAR_EX:
	case SLASH_EX:
	default:
		return ARITHMETIC;
	}
}

astNode *makeASTNode(NodeType type) {
	astNode *n = malloc(sizeof(astNode));
	n->type = type;
	n->primType = VOID_TYPE;
	n->location = NULL;
	n->code = NULL;
	return n;
}

astNode *makeASTNode1(NodeType type, astNode *n1) {
	astNode1 *n = malloc(sizeof(astNode1));
	n->type = type;
	if(UNARYMINUS_EX <= type && type <= NOT_EX) {
		n->primType = unaryExpressionType( n1->primType, toOpType(type) );
		char *tempName = nextTempName();
		n->location =  createSymbolNode( tempName, PRIMITIVE, createLiteralType(n->primType) );
		free(tempName);
		addSymbol( localTable, n->location );
	} else {
		n->primType = VOID_TYPE;
		n->location = NULL;
	}
	n->child0 = n1;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeASTNode2(NodeType type, astNode *n1, astNode *n2) {
	astNode2 *n = malloc(sizeof(astNode2));
	n->type = type;
	if(CONSTANT_EX <= type && type <= GREATER_EX) {
		if( type == ARRAY_EX) {
			n->primType = n1->primType;
		} else if( type == FUNCALL_EX ) {
			n->primType = ((idNode *) n1)->primType;
		} else {
			n->primType = binaryExpressionType( n1->primType, n2->primType, toOpType(type) );
		}
		char *tempName = nextTempName();
		n->location = createSymbolNode( tempName, PRIMITIVE, createLiteralType(n->primType) );
		free(tempName);
		addSymbol( localTable, n->location );
	} else {
		n->primType = VOID_TYPE;
		n->location = NULL;
	}
	n->child0 = n1;
	n->child1 = n2;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeASTNode3(NodeType type, astNode *n1, astNode *n2, astNode *n3) {
	astNode3 *n = malloc(sizeof(astNode3));
	n->type = type;
	n->primType = VOID_TYPE;
	n->location = NULL;
	n->child0 = n1;
	n->child1 = n2;
	n->child2 = n3;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeASTNode4(NodeType type, astNode *n1, astNode *n2, astNode *n3, astNode *n4) {
	astNode4 *n = malloc(sizeof(astNode4));
	n->type = type;
	n->primType = VOID_TYPE;
	n->location = NULL;
	n->child0 = n1;
	n->child1 = n2;
	n->child2 = n3;
	n->child3 = n4;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeIdNode(symbolNode *symNode) {
	idNode *n = malloc(sizeof(idNode));
	n->type = ID_EX;
	if (symNode == NULL) {
		n->primType = ERROR_TYPE;
	} else if(symNode->type == FUNCTION) {
		n->primType = ((functionType *) symNode->typeInfo)->returnType;
	} else if(symNode->type == FUN_PROT) {
		n->primType = ((fprotType *) symNode->typeInfo)->returnType;
	} else if(symNode->type == ARRAY) {
		n->primType = ((arrayType *) symNode->typeInfo)->dataType;
	} else {
		n->primType = ((literalType *) symNode->typeInfo)->dataType;
	}
	n->location = symNode;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeGlobalNode(symbolNode *symNode) {
	idNode *n = malloc(sizeof(idNode));
	n->type = GLOBAL_DCL;
	if (symNode == NULL) {
		n->primType = ERROR_TYPE;
	} else if(symNode->type == FUNCTION) {
		n->primType = ((functionType *) symNode->typeInfo)->returnType;
	} else if(symNode->type == FUN_PROT) {
		n->primType = ((fprotType *) symNode->typeInfo)->returnType;
	} else if(symNode->type == ARRAY) {
		n->primType = ((arrayType *) symNode->typeInfo)->dataType;
	} else {
		n->primType = ((literalType *) symNode->typeInfo)->dataType;
	}
	n->location = symNode;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeFunNode(symbolNode *symNode, symbolTable *table, astNode *tree) {
	funNode *n = malloc(sizeof(funNode));
	n->type = FUNCTION_DEF;
	if(symNode == NULL)
		n->primType = ERROR_TYPE;
	else if(symNode->type != FUNCTION)
		n->primType = ERROR_TYPE;
	else
		n->primType = ((functionType*) symNode->typeInfo)->returnType;
	n->location = symNode;
	n->symTable = table;
	n->codeTree = tree;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeIntconNode(int val) {
	intconNode *n = malloc(sizeof(intconNode));
	n->type = INT_EX;
	n->primType = INT_TYPE;
	char *tempName = nextTempName();
	n->location =  createSymbolNode( tempName, PRIMITIVE, createLiteralType(INT_TYPE) );
	free(tempName);
	addSymbol( localTable, n->location );
	n->value = val;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeFloatconNode(double val) {
	floatconNode *n = malloc(sizeof(floatconNode));
	n->type = FLOAT_EX;
	n->primType = FLOAT_TYPE;
	char *tempName = nextTempName();
	n->location =  createSymbolNode( tempName, PRIMITIVE, createLiteralType(FLOAT_TYPE) );
	free(tempName);
	addSymbol( localTable, n->location );
	n->value = val;
	n->code = NULL;
	return (astNode *) n;
}

astNode *makeCharconNode(char val) {
	charconNode *n = malloc(sizeof(charconNode));
	n->type = CHAR_EX;
	char *tempName = nextTempName();
	n->location =  createSymbolNode( tempName, PRIMITIVE, createLiteralType(CHAR_TYPE) );
	free(tempName);
	addSymbol( localTable, n->location );
	n->value = val;
	n->code = NULL;
	return (astNode *) n;
}

char *nextTempName() {
	int bufsize = 10;
	char *buf = malloc(sizeof(char)*bufsize);
	snprintf(buf,bufsize,"_T%d",tempNum++);
	return buf;
}

char *nextLabelName() {
	int bufsize = 16;
	char *buf = malloc(sizeof(char)*bufsize);
	snprintf(buf,bufsize,"_L%d",labelNum++);
	return buf;
}

void genCode(astNode *n) {

	if(n == NULL) return;

	depth++;
	if(debug) {
		char buf[10];
		snprintf(buf,10,"%d",n->type);
		printDebug(buf);
	}

	astNode1 *n1;
	astNode2 *n2;
	astNode3 *n3;
	astNode4 *n4;

	char *op = NULL;
	switch(n->type) {
	case PROGRAM: {
		if(debug) printDebug("Program tree");
		n2 = (astNode2 *) n;
		genCode(n2->child0);
		genCode(n2->child1);
		if(n2->child0 == NULL && n2->child1 == NULL)
			n2->code = NULL;
		else if(n2->child1 == NULL)
			n2->code = n2->child0->code;
		else if(n2->child0 == NULL)
			n2->code = n2->child1->code;
		else
			n2->code = concatCode( n2->child0->code, n2->child1->code );
		break;
	}
	case FUNCTION_DEF: {
		if(debug) printDebug("Function def");
		funNode *fn = (funNode *) n;
		updateOffsets(fn->symTable);
		quad *q1 = newInstr(ENTER,ST_ENTRY,UNUSED,UNUSED,fn->location,NULL,NULL);
		genCode(fn->codeTree);
		quad *q2 = NULL;
		/*
		switch(fn->primType) {
		case VOID_TYPE: q2 = NULL; break;
		default: q2 = newInstr(RETVAL,ST_ENTRY,UNUSED,UNUSED,fn->location,NULL,NULL); break;
		}
		*/
		quad *q3 = newInstr(LEAVE,ST_ENTRY,UNUSED,UNUSED,fn->location,NULL,NULL);
		quad *i1;
		if(fn->codeTree == NULL) i1 = q1;
		else i1 = concatCode( q1,fn->codeTree->code );

		fn->code = concatCode( i1, concatCode(q2,q3) );
		if(debug) printDebug("Function done");
		break;
	}
	case PLUS_EX:
	case MINUS_EX:
	case STAR_EX:
	case SLASH_EX: {
		if(debug) printDebug("Binary arithmetic");
		n2 = (astNode2 *) n;
		genCode(n2->child0);
		genCode(n2->child1);
		quad *q1 = newInstr( n2->type, ST_ENTRY, ST_ENTRY, ST_ENTRY, n2->child0->location, n2->child1->location, n2->location );
		n2->code = concatCode( concatCode( n2->child0->code, n2->child1->code ), q1 );
		if(debug) printDebug("Finishing binary arithmetic");
		break;
	}
	case ARRAY_EX: {
		if(debug) printDebug("Array expression");
		n2 = (astNode2 *) n;
		genCode(n2->child1);
		quad *q1 = newInstr( ARRAY_EX, ST_ENTRY, ST_ENTRY, ST_ENTRY, n2->child0->location, n2->child1->location, n2->location );
		n2->code = concatCode( n2->child1->code, q1 );
		break;
	}
	case UNARYMINUS_EX: {
		if(debug) printDebug("Unary minus");
		n1 = (astNode1 *) n;
		genCode(n1->child0);
		quad *q1 = newInstr( n1->type, ST_ENTRY, UNUSED, ST_ENTRY, n1->child0->location, NULL, n1->location );
		n1->code = concatCode( n1->child0->code, q1 );
		break;
	}
	case FUNCALL_EX: {
		if(debug) printDebug("Function call in expression");
		n2 = (astNode2 *) n;
		symbolNode *function = ((idNode *) n2->child0)->location;
		int paramNumOld = paramNum;
		paramNum = numParams( function );
		//printf("Number of parameters: %d\n",paramNum);
		genCode(n2->child1);
		quad *q1 = newInstr( CALL, ST_ENTRY, UNUSED, UNUSED, function, NULL, NULL );
		quad *q2 = newInstr( RETRIEVE, ST_ENTRY, UNUSED, UNUSED, n2->location, NULL, NULL );
		if(n2->child1 == NULL)
			n2->code = concatCode( q1 , q2 );
		else
			n2->code = concatCode( concatCode( n2->child1->code, q1 ), q2 );
		paramNum = paramNumOld;
		break;
	}
	case FUNCALL_ST: {
		if(debug) printDebug("Function call as statement");
		n2 = (astNode2 *) n;
		symbolNode *function = ((idNode *) n2->child0)->location;
		int paramNumOld = paramNum;
		paramNum = numParams( function );
		genCode(n2->child1);
		quad *q1 = newInstr( CALL, ST_ENTRY, UNUSED, UNUSED, function, NULL, NULL );
		if(n2->child1 == NULL)
			n2->code = q1;
		else
			n2->code = concatCode( n2->child1->code, q1 );
		paramNum = paramNumOld;
		break;
	}
	case RETURN_ST: {
		if(debug) printDebug("Return statement");
		n1 = (astNode1 *) n;
		if(n1->child0 == NULL) {
			if(debug) printDebug("Void return");
			n1->code = newInstr( RETURN_FUN, UNUSED, UNUSED, UNUSED, NULL, NULL, NULL );
			break;
		} else {
			if(debug) printDebug("Type return");
			genCode(n1->child0);
			quad *q1 = newInstr( RETURN_FUN, ST_ENTRY, UNUSED, UNUSED, n1->child0->location, NULL, NULL );
			n1->code = concatCode( n1->child0->code, q1 );
			break;
		}
	}
	case ASSIGN_ST: {
		if(debug) printDebug("Assignment statement");
		n2 = (astNode2 *) n;
		genCode(n2->child1);
		astNode *leftside = n2->child0;
		quad *q1 = NULL;
		switch(leftside->type) {
		case ARRAY_EX:
			genCode(((astNode2 *) leftside)->child1);
			q1 = (((astNode2 *) leftside)->child1)->code;
			quad *q2 = newInstr( ARRAY_ASSN, ST_ENTRY, ST_ENTRY, ST_ENTRY, n2->child1->location, ((astNode2 *) leftside)->child1->location, ((astNode2 *) leftside)->child0->location );
			q1 = concatCode( q1, q2 );
			break;
		case ID_EX:
			q1 = newInstr( ASSIGN_ST, ST_ENTRY, UNUSED, ST_ENTRY, n2->child1->location, NULL, n2->child0->location );
			break;
		}
		n2->code = concatCode( n2->child1->code, q1 );
		break;
	}
	case IF_ST: {
		if(debug) printDebug("If statement");
		n3 = (astNode3 *) n;
		char *labelThen = nextLabelName();
		char *labelElse = nextLabelName();
		char *labelAfter = nextLabelName();

		genCodeBool(n3->child0, labelThen, labelElse);
		quad *q1 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelThen, NULL, NULL );
		genCode(n3->child1);
		quad *q2 = newInstr( GOTO, STRING, UNUSED, UNUSED, labelAfter, NULL, NULL );
		quad *q3 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelElse, NULL, NULL );
		genCode(n3->child2);
		quad *q4 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelAfter, NULL, NULL );

		quad *i1 = concatCode( n3->child0->code, q1 );
		quad *i2 = concatCode( n3->child1->code, q2 );
		quad *i3;
		if(n3->child2 == NULL)
			i3 = q3;
		else
			i3 = concatCode( q3, n3->child2->code );
		n3->code = concatCode( concatCode( i1, i2 ), concatCode( i3, q4 ) );
		break;
	}
	case WHILE_ST: {
		if(debug) printDebug("While loop");
		n2 = (astNode2 *) n;
		char *labelTop = nextLabelName();
		char *labelBody = nextLabelName();
		char *labelAfter = nextLabelName();

		quad *q1 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelTop, NULL, NULL );
		genCodeBool(n2->child0, labelBody, labelAfter);
		quad *q2 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelBody, NULL, NULL );
		genCode(n2->child1);
		quad *q3 = newInstr( GOTO, STRING, UNUSED, UNUSED, labelTop, NULL, NULL );
		quad *q4 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelAfter, NULL, NULL );

		quad *i1 = concatCode( q1, n2->child0->code );
		quad *i2;
		if(n2->child1 == NULL) i2 = q2;
		else i2 = concatCode( q2, n2->child1->code );
		quad *i3 = concatCode( q3, q4 );
		n2->code = concatCode( concatCode( i1, i2 ), i3 );
		break;
	}
	case FOR_ST: {
		if(debug) printDebug("For loop");
		n4 = (astNode4 *) n;
		char *labelTop = nextLabelName();
		char *labelBody = nextLabelName();
		char *labelAfter = nextLabelName();

		genCode(n4->child0);
		quad *q1 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelTop, NULL, NULL );
		genCodeBool(n4->child1, labelBody, labelAfter);
		quad *q2 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelBody, NULL, NULL );
		genCode(n4->child3);
		genCode(n4->child2);
		quad *q3 = newInstr( GOTO, STRING, UNUSED, UNUSED, labelTop, NULL, NULL );
		quad *q4 = newInstr( LABEL, STRING, UNUSED, UNUSED, labelAfter, NULL, NULL );

		quad *i1;
		quad *i2;
		quad *i3;
		if(n4->child0 ==  NULL) i1 = q1;
		else i1 = concatCode( n4->child0->code, q1 );
		if(n4->child1 == NULL) i2 = q2;
		else i2 = concatCode( n4->child1->code, q2 );
		if(n4->child2 == NULL && n4->child3 == NULL) i3 = NULL;
		else if(n4->child2 == NULL) i3 = n4->child3->code;
		else if(n4->child3 == NULL) i3 = n4->child2->code;
		else i3 = concatCode( n4->child3->code, n4->child2->code );
		quad *i4 = concatCode( q3, q4 );
		n4->code = concatCode( concatCode( i1, i2 ), concatCode( i3, i4 ) );
		break;
	}
	case LIST_ST: {
		if(debug) printDebug("List of statements");
		n2 = (astNode2 *) n;
		genCode(n2->child0);
		genCode(n2->child1);
		if(n2->child1 == NULL)
			n2->code = n2->child0->code;
		else
			n2->code = concatCode( n2->child0->code, n2->child1->code );
		break;
	}
	case COMMA_EX: {
		if(debug) printDebug("Expression in list");
		n2 = (astNode2 *) n;
		genCode(n2->child0);
		genCode(n2->child1);

		int *iPtr = malloc(sizeof(int));
		*iPtr = paramNum--;
		quad *q1 = newInstr( PARAM, ST_ENTRY, CONSTANT, UNUSED, n2->child0->location, iPtr, NULL );
		if(n2->child1 == NULL)
			n2->code = concatCode( n2->child0->code, q1 );
		else
			n2->code = concatCode( concatCode( n2->child0->code, n2->child1->code ), q1 );
		break;
	}
	case INT_EX: {
		if(debug) printDebug("Integer Constant");
		intconNode *iNode = (intconNode *) n;
		int *ptr;
		ptr = &(iNode->value);
		iNode->code = newInstr( ASSIGN_ST, CONSTANT, UNUSED, ST_ENTRY, ptr, NULL, iNode->location );
		break;
	}
	case FLOAT_EX: {
		if(debug) printDebug("Float Constant");
		floatconNode *fNode = (floatconNode *) n;
		double *ptr;
		ptr = &(fNode->value);
		fNode->code = newInstr( ASSIGN_ST, CONSTANT, UNUSED, ST_ENTRY, ptr, NULL, fNode->location );
		break;
	}
	case GLOBAL_DCL: {
		if(debug) printDebug("Global Declaration");
		idNode *iNode = (idNode *) n;
		iNode->code = newInstr( GLOBAL_DCL, ST_ENTRY, UNUSED, UNUSED, iNode->location, NULL, NULL );
		break;
	}
	default:
		if(debug) printDebug("Unrecognized");
		break;
	}

	depth--;
}

void genCodeBool(astNode *n, char *trueDest, char *falseDest) {

	if(n == NULL) return;

	depth++;

	if(debug) {
		char buf[10];
		snprintf(buf,10,"%d",n->type);
		printDebug(buf);
	}

	char *op = NULL;
	astNode2 *condition = (astNode2 *) n;
	astNode *r0 = condition->child0;
	astNode *r1 = condition->child1;
	genCode(r0);
	genCode(r1);

	switch(condition->type) {
	case EQ_EX:
	case NEQ_EX:
	case LEQ_EX:
	case LESS_EX:
	case GEQ_EX:
	case GREATER_EX: {
		if(debug) printDebug("Relational expression");
		quad *q1 = newInstr( condition->type, ST_ENTRY, ST_ENTRY, STRING, r0->location, r1->location, trueDest );
		quad *q2 = newInstr( GOTO, STRING, UNUSED, UNUSED, falseDest, NULL, NULL );
		condition->code = concatCode( concatCode( r0->code, r1->code ), concatCode( q1, q2 ) );
		break;
	}
	case AND_EX: {
		if(debug) printDebug("AND operator");
		char *label = nextLabelName();
		genCodeBool(r0, label, falseDest);
		quad *q1 = newInstr( LABEL, STRING, UNUSED, UNUSED, label, NULL, NULL );
		genCodeBool(r1, trueDest, falseDest);
		condition->code = concatCode( concatCode( r0->code, q1 ), r1->code );
		break;
	}
	case OR_EX: {
		if(debug) printDebug("OR operator");
		char *label = nextLabelName();
		genCodeBool(r0, trueDest, label);
		quad *q1 = newInstr( LABEL, STRING, UNUSED, UNUSED, label, NULL, NULL );
		genCodeBool(r1, trueDest, falseDest);
		condition->code = concatCode( concatCode( r0->code, q1 ), r1->code );
		break;
	}
	}

	depth--;
}

quad *concatCode( quad *q1, quad *q2 ) {
	if(debug) printDebug("Now concatenating code");
	if(q1 == NULL && q2 == NULL) {
		if(debug) printDebug("Both were null");
		return NULL;
	}
	if(q1 == NULL){
		if(debug) printDebug("q1 was null");
		return q2;
	}
	if(q2 == NULL){
		if(debug) printDebug("q2 was null");
		return q1;
	}
	if(debug) printDebug("Neither is null, concatenating");
	quad *q1last = q1->prevQuad;
	quad *q2last = q2->prevQuad;
	q1last->nextQuad = q2;
	q2->prevQuad = q1last;
	q2last->nextQuad = q1;
	q1->prevQuad = q2last;
	return q1;
}

quad *newLabel() {
	return newInstr( LABEL, STRING, UNUSED, UNUSED, nextLabelName, NULL, NULL );
}

quad *newInstr(NodeType type, SourceType s1Type, SourceType s2Type, SourceType dType, void* s1, void* s2, void* d) {
	quad *q = malloc(sizeof(quad));
	q->type = type;
	q->nextQuad = q;
	q->prevQuad = q;
	q->src1Type = s1Type;
	q->src2Type = s2Type;
	q->destType = dType;
	q->src1 = s1;
	q->src2 = s2;
	q->dest = d;
	return q;
}

void printCode( quad *q ) {

	quad *curr = q->nextQuad;

	printQuadToAssembly( q );
	while( curr != q ) {
		printQuadToAssembly( curr );
		curr = curr->nextQuad;
	}
}

void printDebug( char *message ) {
	int i;
	for(i = 0; i < depth; i++) {
		printf("|>");
	}
	printf(" %s\n",message);
}

void printQuadToAssembly( quad *q ) {
	if(q == NULL) return;
	char *instr = NULL;

	if(debug) {
		char buf[10];
		snprintf(buf,10,"%d",q->type);
		printDebug(buf);
	}
	if(!intermediate) {
		switch(q->type) {
		case PLUS_EX:
		case MINUS_EX:
		case STAR_EX:
		case SLASH_EX:

			switch(q->type) {
			case PLUS_EX:
				printf("# %s = %s + %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol); break;
			case MINUS_EX:
				printf("# %s = %s - %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol); break;
			case STAR_EX:
				printf("# %s = %s * %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol); break;
			case SLASH_EX:
				printf("# %s = %s / %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol); break;
			}

			if(((literalType *) ((symbolNode *) q->src1)->typeInfo)->dataType == FLOAT_TYPE) {

				if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lwc1    $f0, %d($t0)\n", ((symbolNode *) q->src1)->offset);
				printf("lwc1    $f1, %d($t0)\n", ((symbolNode *) q->src1)->offset + 4);

				if(((symbolNode *) q->src2)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lwc1    $f2, %d($t0)\n", ((symbolNode *) q->src2)->offset);
				printf("lwc1    $f3, %d($t0)\n", ((symbolNode *) q->src2)->offset + 4);

				switch(q->type) {
				case PLUS_EX:
					printf("add.d   $f0, $f0, $f2\n"); break;
				case MINUS_EX:
					printf("sub.d   $f0, $f0, $f2\n"); break;
				case STAR_EX:
					printf("mul.d   $f0, $f0, $f2\n"); break;
				case SLASH_EX:
					printf("div.d   $f0, $f0, $f2\n"); break;
				}

				if(((symbolNode *) q->dest)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("swc1    $f0, %d($t0)\n", ((symbolNode *) q->dest)->offset);
				printf("swc1    $f1, %d($t0)\n", ((symbolNode *) q->dest)->offset + 4);

			} else {

				if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lw      $t1, %d($t0)\n", ((symbolNode *) q->src1)->offset);

				if(((symbolNode *) q->src2)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lw      $t2, %d($t0)\n", ((symbolNode *) q->src2)->offset);

				switch(q->type) {
				case PLUS_EX:
					printf("add     $t1, $t1, $t2\n"); break;
				case MINUS_EX:
					printf("sub     $t1, $t1, $t2\n"); break;
				case STAR_EX:
					printf("mult    $t1, $t2\n");
					printf("mflo    $t1\n"); break;
				case SLASH_EX:
					printf("div     $t1, $t2\n");
					printf("mflo    $t1\n"); break;
				}

				if(((symbolNode *) q->dest)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("sw      $t1, %d($t0)\n", ((symbolNode *) q->dest)->offset);

			}
			break;
		case UNARYMINUS_EX:
			printf("# %s = -%s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol);

			if(((literalType *) ((symbolNode *) q->src1)->typeInfo)->dataType == FLOAT_TYPE) {

				if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lwc1    $f0, %d($t0)\n", ((symbolNode *) q->src1)->offset);
				printf("lwc1    $f1, %d($t0)\n", ((symbolNode *) q->src1)->offset + 4);

				printf("neg.d   $f0, $f0\n");

				if(((symbolNode *) q->dest)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("swc1    $f0, %d($t0)\n", ((symbolNode *) q->dest)->offset);
				printf("swc1    $f1, %d($t0)\n", ((symbolNode *) q->dest)->offset + 4);


			} else {

				if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lw      $t1, %d($t0)\n", ((symbolNode *) q->src1)->offset);

				printf("addi    $t2, $zero, -1\n");
				printf("mult    $t1, $t2\n");
				printf("mflo    $t1\n");

				if(((symbolNode *) q->dest)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("sw      $t1, %d($t0)\n", ((symbolNode *) q->dest)->offset);

			}
			break;
		case LABEL:
			printf("%s:\n", (char *) q->src1);
			break;
		case GOTO:
			printf("j       %s\n", (char *) q->src1);
			break;
		case EQ_EX:
		case NEQ_EX:
		case LEQ_EX:
		case LESS_EX:
		case GEQ_EX:
		case GREATER_EX:

			switch(q->type) {
			case EQ_EX:
				printf("# if %s == %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest); break;
			case NEQ_EX:
				printf("# if %s != %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest); break;
			case LEQ_EX:
				printf("# if %s <= %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest); break;
			case LESS_EX:
				printf("# if %s < %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest); break;
			case GEQ_EX:
				printf("# if %s >= %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest); break;
			case GREATER_EX:
				printf("# if %s > %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest); break;
			}

			if(((literalType *) ((symbolNode *) q->src1)->typeInfo)->dataType == FLOAT_TYPE) {

				if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lwc1    $f0, %d($t0)\n", ((symbolNode *) q->src1)->offset);
				printf("lwc1    $f1, %d($t0)\n", ((symbolNode *) q->src1)->offset + 4);

				if(((symbolNode *) q->src2)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lwc1    $f2, %d($t0)\n", ((symbolNode *) q->src2)->offset);
				printf("lwc1    $f3, %d($t0)\n", ((symbolNode *) q->src2)->offset + 4);

				switch(q->type) {
				case EQ_EX:
					printf("c.eq.d  $f0, $f2\n");
					printf("bc1t    %s\n", (char *) q->dest); break;
				case NEQ_EX:
					printf("c.eq.d  $f0, $f2\n");
					printf("bc1f    %s\n", (char *) q->dest); break;
				case LEQ_EX:
					printf("c.le.d  $f0, $f2\n");
					printf("bc1t    %s\n", (char *) q->dest); break;
				case LESS_EX:
					printf("c.lt.d  $f0, $f2\n");
					printf("bc1t    %s\n", (char *) q->dest); break;
				case GEQ_EX:
					printf("c.le.d  $f2, $f0\n");
					printf("bc1t    %s\n", (char *) q->dest); break;
				case GREATER_EX:
					printf("c.lt.d  $f2, $f0\n");
					printf("bc1t    %s\n", (char *) q->dest); break;
				}

			} else {

				if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lw      $t1, %d($t0)\n", ((symbolNode *) q->src1)->offset);

				if(((symbolNode *) q->src2)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("lw      $t2, %d($t0)\n", ((symbolNode *) q->src2)->offset);

				switch(q->type) {
				case EQ_EX:
					printf("beq     $t1, $t2, %s\n", (char *) q->dest); break;
				case NEQ_EX:
					printf("bne     $t1, $t2, %s\n", (char *) q->dest); break;
				case LEQ_EX:
					printf("beq     $t1, $t2, %s\n", (char *) q->dest);
					printf("blt     $t1, $t2, %s\n", (char *) q->dest); break;
				case LESS_EX:
					printf("blt     $t1, $t2, %s\n", (char *) q->dest); break;
				case GEQ_EX:
					printf("beq     $t1, $t2, %s\n", (char *) q->dest);
					printf("blt     $t2, $t1, %s\n", (char *) q->dest); break;
				case GREATER_EX:
					printf("blt     $t2, $t1, %s\n", (char *) q->dest); break;
				}
			}

			break;
		case ASSIGN_ST:

			if(((literalType *) ((symbolNode *) q->dest)->typeInfo)->dataType == FLOAT_TYPE) {

				if(q->src1Type == CONSTANT) {
					double *dPtr = (double *) q->src1;
					printf("# %s = %f\n", ((symbolNode *) q->dest)->symbol, *dPtr);
					printf("li.d    $f0, %f\n", *dPtr);
				} else {
					printf("# %s = %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol);
					if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
					else printf("addi    $t0, $sp, 0\n");
					printf("lwc1    $f0, %d($t0)\n", ((symbolNode *) q->src1)->offset);
					printf("lwc1    $f1, %d($t0)\n", ((symbolNode *) q->src1)->offset + 4);
				}

				if(((symbolNode *) q->dest)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("swc1    $f0, %d($t0)\n", ((symbolNode *) q->dest)->offset);
				printf("swc1    $f1, %d($t0)\n", ((symbolNode *) q->dest)->offset + 4);

			} else {

				if(q->src1Type == CONSTANT) {
					int *iPtr = (int *) q->src1;
					printf("# %s = %d\n", ((symbolNode *) q->dest)->symbol, *iPtr);
					printf("li      $t1, %d\n", *iPtr);
				} else {
					printf("# %s = %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol);
					if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
					else printf("addi    $t0, $sp, 0\n");
					printf("lw      $t1, %d($t0)\n", ((symbolNode *) q->src1)->offset);
				}

				if(((symbolNode *) q->dest)->isGlobal) printf("la      $t0, global_memory\n");
				else printf("addi    $t0, $sp, 0\n");
				printf("sw      $t1, %d($t0)\n", ((symbolNode *) q->dest)->offset);
			}
			break;
		case ARRAY_EX: {
			printf("# %s = %s[%s]\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol);

			if(((symbolNode *) q->src2)->isGlobal) printf("la      $t0, global_memory\n");
			else printf("addi    $t0, $sp, 0\n");
			printf("lw      $t1, %d($t0)\n", ((symbolNode *) q->src2)->offset);

			printf("la      $t0, global_memory\n");
			printf("addi    $t0, $t0, %d\n", ((symbolNode *) q->src1)->offset);

			switch(((literalType *) ((symbolNode *) q->dest)->typeInfo)->dataType) {
			case FLOAT_TYPE:
				printf("sll     $t1, $t1, %d\n", 3);
				printf("add     $t0, $t0, $t1\n");
				printf("lw      $t1, 0($t0)\n");
				printf("lw      $t2, 4($t0)\n"); break;
			case INT_TYPE:
				printf("sll     $t1, $t1, %d\n", 2);
				printf("add     $t0, $t0, $t1\n");
				printf("lw      $t1, 0($t0)\n"); break;
			case CHAR_TYPE:
				printf("add     $t0, $t0, $t1\n");
				printf("lb      $t1, 0($t0)\n"); break;
			}

			if(((symbolNode *) q->dest)->isGlobal) printf("la      $t0, global_memory\n");
			else printf("addi    $t0, $sp, 0\n");

			switch(((literalType *) ((symbolNode *) q->dest)->typeInfo)->dataType) {
			case FLOAT_TYPE:
				printf("sw      $t1, %d($t0)\n", ((symbolNode *) q->dest)->offset);
				printf("sw      $t2, %d($t0)\n", ((symbolNode *) q->dest)->offset + 4); break;
			case INT_TYPE:
			case CHAR_TYPE:
				printf("sw      $t1, %d($t0)\n", ((symbolNode *) q->dest)->offset); break;
			}

			break;
		}
		case ARRAY_ASSN: {
			printf("# %s[%s] = %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src2)->symbol, ((symbolNode *) q->src1)->symbol);

			if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
			else printf("addi    $t0, $sp, 0\n");

			switch(((literalType *) ((symbolNode *) q->src1)->typeInfo)->dataType) {
			case FLOAT_TYPE:
				printf("lw      $t2, %d($t0)\n", ((symbolNode *) q->src1)->offset);
				printf("lw      $t3, %d($t0)\n", ((symbolNode *) q->src1)->offset + 4); break;
			case INT_TYPE:
				printf("lw      $t2, %d($t0)\n", ((symbolNode *) q->src1)->offset); break;
			case CHAR_TYPE:
				printf("lb      $t2, %d($t0)\n", ((symbolNode *) q->src1)->offset); break;
			}

			if(((symbolNode *) q->src2)->isGlobal) printf("la      $t0, global_memory\n");
			else printf("addi    $t0, $sp, 0\n");
			printf("lw      $t1, %d($t0)\n", ((symbolNode *) q->src2)->offset);

			printf("la      $t0, global_memory\n");
			printf("addi    $t0, $t0, %d\n", ((symbolNode *) q->dest)->offset);

			switch(((arrayType *) ((symbolNode *) q->dest)->typeInfo)->dataType) {
			case FLOAT_TYPE:
				printf("sll     $t1, $t1, %d\n", 3);
				printf("add     $t0, $t0, $t1\n");
				printf("sw      $t2, 0($t0)\n");
				printf("sw      $t3, 4($t0)\n"); break;
			case INT_TYPE:
				printf("sll     $t1, $t1, %d\n", 2);
				printf("add     $t0, $t0, $t1\n");
				printf("sw      $t2, 0($t0)\n"); break;
			case CHAR_TYPE:
				printf("add     $t0, $t0, $t1\n");
				printf("sb      $t2, 0($t0)\n"); break;
			}
			break;
		}
		case ENTER: {
			functionType *funType = (functionType *) ((symbolNode *) q->src1)->typeInfo;
			symbolTable *table = funType->symTable;
			idList *paramList = funType->paramList;

			funName = ((symbolNode *) q->src1)->symbol;
			int tableSize = table->byteSize;
			int numParams = listLength( paramList );
			int paramSize = numParams * 4;
			if( paramSize < 16 ) paramSize = 16;

			/*
			 * Stack Structure:
			 *
			 * Input Arguments
			 * $ra
			 * $fp
			 * Local Variables
			 */

			int stackSpace = tableSize + 16 + paramSize;
			printf("%s:\n\n", ((symbolNode *) q->src1)->symbol);
			printf("# Prologue for %s\n", ((symbolNode *) q->src1)->symbol);
			printf("subu    $sp, $sp, %d\n", stackSpace);
			printf("sw      $a3, %d($sp)\n", tableSize + 28);
			printf("sw      $a2, %d($sp)\n", tableSize + 24);
			printf("sw      $a1, %d($sp)\n", tableSize + 20);
			printf("sw      $a0, %d($sp)\n", tableSize + 16);
			printf("sw      $fp, %d($sp)\n", tableSize + 8);
			printf("sw      $ra, %d($sp)\n", tableSize + 0);
			printf("addiu   $fp, $sp, %d\n\n", stackSpace);

			idList *param = paramList;
			int paramOffset = tableSize + 16;
			int currParamNum;

			printf("# Begin storing arguments in local variables\n\n");
			for(currParamNum = 0; currParamNum < numParams; currParamNum++ ) {
				symbolNode *symNode = getSymbol( table, param->symbol, PRIMITIVE );
				printf("# Store argument %d in %s\n", currParamNum + 1, param->symbol);

				switch(((literalType *) symNode->typeInfo)->dataType){
				case CHAR_TYPE:
					printf("lb      $t0, %d($sp)\n", paramOffset + currParamNum * 4);
					printf("sb      $t0, %d($sp)\n\n", symNode->offset); break;
				case INT_TYPE:
					printf("lw      $t0, %d($sp)\n", paramOffset + currParamNum * 4);
					printf("sw      $t0, %d($sp)\n\n", symNode->offset); break;
				case FLOAT_TYPE:
					printf("lw      $t0, %d($sp)\n", paramOffset + currParamNum * 4);
					printf("lw      $t1, 0($t0)\n");
					printf("lw      $t2, 4($t0)\n");
					printf("sw      $t1, %d($sp)\n\n", symNode->offset);
					printf("sw      $t2, %d($sp)\n\n", symNode->offset + 4); break;
				}

				param = param->nextID;
			}
			printf("# End storing arguments in local variables\n\n");

			break;
		}
		case LEAVE: {
			functionType *funType = (functionType *) ((symbolNode *) q->src1)->typeInfo;
			symbolTable *table = funType->symTable;
			idList *paramList = funType->paramList;

			int tableSize = table->byteSize;
			int paramSize = listLength( paramList ) * 4;
			if( paramSize < 16 ) paramSize = 16;
			int stackSpace = tableSize + 16 + paramSize;

			printf("%s_done:\n\n", ((symbolNode *) q->src1)->symbol);
			printf("# Epilogue for %s\n", ((symbolNode *) q->src1)->symbol);
			printf("lw      $fp, %d($sp)\n", tableSize + 8);
			printf("lw      $ra, %d($sp)\n", tableSize + 0);
			printf("addiu   $sp, $sp, %d\n", stackSpace);
			printf("jr      $ra\n");
			break;
		}
		case PARAM: {
			int *iPtr = (int *) q->src2;
			int i = *iPtr;
			if(!paramStart) {
				paramStart = true;
				paramNum = i;
			}

			printf("# Argument %d = %s\n", i, ((symbolNode *) q->src1)->symbol);
			if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
			else printf("addi    $t0, $sp, 0\n");

			if(((literalType *) ((symbolNode *) q->src1)->typeInfo)->dataType == FLOAT_TYPE) {
				if(i <= 4) {
					printf("addi    $t0, $t0, %d\n", ((symbolNode *) q->src1)->offset);
					printf("addi    $a%d, $t0, 0\n", i - 1);
				} else {
					printf("addi    $t0, $t0, %d\n", ((symbolNode *) q->src1)->offset);
					printf("sw      $t0, %d($sp)\n", (-paramNum + i - 1)*4);
				}
			} else {
				if(i <= 4) {
					printf("lw      $t0, %d($t0)\n", ((symbolNode *) q->src1)->offset);
					printf("addi    $a%d, $t0, 0\n", i - 1);
				} else {
					printf("lw      $t0, %d($t0)\n", ((symbolNode *) q->src1)->offset);
					printf("sw      $t0, %d($sp)\n", (-paramNum + i - 1)*4);
				}
			}

			if(i == 1) paramStart = false;
			break;
		}
		case CALL: {
			printf("# Call %s\n", ((symbolNode *) q->src1)->symbol);
			printf("jal     %s\n", ((symbolNode *) q->src1)->symbol);
			break;
		}
		case RETURN_FUN: {
			if(q->src1Type == UNUSED) {
				printf("# Return from %s\n", funName);
				printf("j       %s_done\n", funName);
			} else {
				printf("# Return %s from %s\n", ((symbolNode *) q->src1)->symbol, funName);
				printf("lw      $t0, %d($sp)\n", ((symbolNode *) q->src1)->offset);
				printf("lw      $t1, %d($sp)\n", ((symbolNode *) q->src1)->offset + 4);
				printf("addi    $v0, $t0, 0\n");
				printf("addi    $v1, $t1, 0\n");
				printf("j       %s_done\n", funName);
			}
			break;
		}
		case RETRIEVE: {
			printf("# Retrieve return value, place in %s\n", ((symbolNode *) q->src1)->symbol);

			if(((symbolNode *) q->src1)->isGlobal) printf("la      $t0, global_memory\n");
			else printf("addi    $t0, $sp, 0\n");

			if(((literalType *) ((symbolNode *) q->src1)->typeInfo)->dataType == FLOAT_TYPE) {
				printf("sw      $v0, %d($t0)\n", ((symbolNode *) q->src1)->offset);
				printf("sw      $v1, %d($t0)\n", ((symbolNode *) q->src1)->offset + 4);
			} else {
				printf("sw      $v0, %d($t0)\n", ((symbolNode *) q->src1)->offset);
			}
			break;
		}
		case GLOBAL_DCL: {
			break;
		}
		default:
			break;
		}
		printf("\n");
	} else {
		switch(q->type) {
		case PLUS_EX:
			printf("%s = %s + %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol);
			break;
		case MINUS_EX:
			printf("%s = %s - %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol);
			break;
		case STAR_EX:
			printf("%s = %s * %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol);
			break;
		case SLASH_EX:
			printf("%s = %s / %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol);
			break;
		case UNARYMINUS_EX:
			printf("%s = -%s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol);
			break;
		case LABEL:
			printf("label %s\n", (char *) q->src1);
			break;
		case GOTO:
			printf("goto %s\n", (char *) q->src1);
			break;
		case EQ_EX:
			printf("if %s == %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest);
			break;
		case NEQ_EX:
			printf("if %s != %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest);
			break;
		case LEQ_EX:
			printf("if %s <= %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest);
			break;
		case LESS_EX:
			printf("if %s < %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest);
			break;
		case GEQ_EX:
			printf("if %s >= %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest);
			break;
		case GREATER_EX:
			printf("if %s > %s, goto %s\n", ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol, (char *) q->dest);
			break;
		case ASSIGN_ST:
			if(q->src1Type == CONSTANT) {
				int *iPtr = (int *) q->src1;
				printf("%s = %d\n", ((symbolNode *) q->dest)->symbol, *iPtr);
			} else {
				printf("%s = %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol);
			}
			break;
		case ARRAY_EX: {
			printf("%s = %s[%s]\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src1)->symbol, ((symbolNode *) q->src2)->symbol);
			break;
		}
		case ARRAY_ASSN: {
			printf("%s[%s] = %s\n", ((symbolNode *) q->dest)->symbol, ((symbolNode *) q->src2)->symbol, ((symbolNode *) q->src1)->symbol);
			break;
		}
		case ENTER: {
			printf("enter %s:\n\n", ((symbolNode *) q->src1)->symbol);
			break;
		}
		case LEAVE: {
			printf("leave %s\n\n", ((symbolNode *) q->src1)->symbol);
			break;
		}
		case PARAM: {
			int *iPtr = (int *) q->src2;
			if(!paramStart) {
				paramStart = true;
				paramNum = *iPtr;
			}

			printf("param %s, %d\n", ((symbolNode *) q->src1)->symbol, *iPtr);

			if(*iPtr == 1) paramStart = false;
			break;
		}
		case CALL: {
			printf("call %s\n", ((symbolNode *) q->src1)->symbol);
			break;
		}
		case RETURN_FUN: {
			if(q->src1Type == UNUSED) {
				printf("return\n", funName);
			} else {
				printf("return %s\n", ((symbolNode *) q->src1)->symbol, funName);
			}
			break;
		}
		case RETRIEVE: {
			printf("retrieve %s\n", ((symbolNode *) q->src1)->symbol);
			break;
		}
		case GLOBAL_DCL: {
			symbolNode *symNode = (symbolNode *) q->src1;

			if(symNode->type == ARRAY) {
				arrayType *arrType = (arrayType *) symNode->typeInfo;
				int size = arrType->size;
				PrimitiveType dataType = arrType->dataType;

				int elSize;
				if(dataType == CHAR_TYPE)
					printf("global char[%d] %s\n\n", size, ((symbolNode *) q->src1)->symbol);
				else if(dataType == INT_TYPE)
					printf("global int[%d] %s\n\n", size, ((symbolNode *) q->src1)->symbol);
				else if(dataType == FLOAT_TYPE)
					printf("global double[%d] %s\n\n", size, ((symbolNode *) q->src1)->symbol);
				else
					printf("global int[%d] %s\n\n", size, ((symbolNode *) q->src1)->symbol);


			} else if(symNode->type == PRIMITIVE) {
				literalType *litType = (literalType *) symNode->typeInfo;
				PrimitiveType dataType = litType->dataType;

				int elSize;
				if(dataType == CHAR_TYPE)
					printf("global char %s \n\n", ((symbolNode *) q->src1)->symbol);
				else if(dataType == INT_TYPE)
					printf("global int %s \n\n", ((symbolNode *) q->src1)->symbol);
				else if(dataType == FLOAT_TYPE)
					printf("global double %s \n\n", ((symbolNode *) q->src1)->symbol);
				else
					printf("global int %s \n\n", ((symbolNode *) q->src1)->symbol);
			}
			break;
		}
		default:
			break;
		}
	}

}

void printStandardLibrary() {
	printf("# Standard Library\n\n");

	printf("printint:\n\n");

	printf("subu    $sp, $sp, 32\n");
	printf("sw      $fp, 0($sp)\n");
	printf("sw      $ra, 8($sp)\n");
	printf("addiu   $fp, $sp, 32\n\n");

	printf("addi    $v0, $zero, 1\n");
	printf("syscall\n\n");

	printf("lw      $ra, 8($sp)\n");
	printf("lw      $fp, 0($sp)\n");
	printf("addiu   $sp, $sp, 32\n");
	printf("jr      $ra\n\n");

	printf("printchar:\n\n");

	printf("subu    $sp, $sp, 40\n");
	printf("sw      $fp, 8($sp)\n");
	printf("sw      $ra, 16($sp)\n");
	printf("addiu   $fp, $sp, 40\n\n");

	printf("sb      $a0, 0($sp)\n");
	printf("addi    $t0, $zero, 0\n");
	printf("sb      $t0, 1($sp)\n");
	printf("addi    $a0, $sp, 0\n");
	printf("addi    $v0, $zero, 4\n");
	printf("syscall\n\n");

	printf("lw      $ra, 16($sp)\n");
	printf("lw      $fp, 8($sp)\n");
	printf("addiu   $sp, $sp, 40\n");
	printf("jr      $ra\n\n");

	printf("printdouble:\n\n");

	printf("subu    $sp, $sp, 32\n");
	printf("sw      $fp, 0($sp)\n");
	printf("sw      $ra, 8($sp)\n");
	printf("addiu   $fp, $sp, 32\n\n");

	printf("lwc1    $f12, 0($a0)\n");
	printf("lwc1    $f13, 4($a0)\n");
	printf("addi    $v0, $zero, 3\n");
	printf("syscall\n\n");

	printf("lw      $ra, 8($sp)\n");
	printf("lw      $fp, 0($sp)\n");
	printf("addiu   $sp, $sp, 32\n");
	printf("jr      $ra\n\n");

	printf("toint:\n\n");

	printf("subu    $sp, $sp, 32\n");
	printf("sw      $fp, 0($sp)\n");
	printf("sw      $ra, 8($sp)\n");
	printf("addiu   $fp, $sp, 32\n\n");

	printf("lwc1        $f0, 0($a0)\n");
	printf("lwc1        $f1, 4($a0)\n");
	printf("floor.w.d   $f0, $f0\n");
	printf("mfc1        $v0, $f0\n\n");

	printf("lw      $ra, 8($sp)\n");
	printf("lw      $fp, 0($sp)\n");
	printf("addiu   $sp, $sp, 32\n");
	printf("jr      $ra\n\n");

	printf("todouble:\n\n");

	printf("subu    $sp, $sp, 32\n");
	printf("sw      $fp, 0($sp)\n");
	printf("sw      $ra, 8($sp)\n");
	printf("addiu   $fp, $sp, 32\n\n");

	printf("mtc1    $a0, $f0\n");
	printf("cvt.d.w $f0, $f0\n");
	printf("mfc1    $v0, $f0\n");
	printf("mfc1    $v1, $f1\n\n");

	printf("lw      $ra, 8($sp)\n");
	printf("lw      $fp, 0($sp)\n");
	printf("addiu   $sp, $sp, 32\n");
	printf("jr      $ra\n\n");

}

void printDataSection() {
	printf(".data\n");
	printf("global_memory: .space %d\n\n", globalTable->byteSize);

	printf(".text\n");
}

void freeTree( astNode *n ) {
	if(n == NULL) return;

	astNode1 *n1;
	astNode2 *n2;
	astNode3 *n3;
	astNode4 *n4;

	switch(n->type) {
	case UNARYMINUS_EX:
	case RETURN_ST: {
		n1 = (astNode1 *) n;
		freeTree(n1->child0);
		break;
	}
	case PROGRAM:
	case PLUS_EX:
	case MINUS_EX:
	case STAR_EX:
	case SLASH_EX:
	case EQ_EX:
	case NEQ_EX:
	case LEQ_EX:
	case LESS_EX:
	case GEQ_EX:
	case GREATER_EX:
	case AND_EX:
	case OR_EX:
	case ARRAY_EX:
	case FUNCALL_EX:
	case FUNCALL_ST:
	case ASSIGN_ST:
	case WHILE_ST:
	case LIST_ST:
	case COMMA_EX: {
		n2 = (astNode2 *) n;
		freeTree(n2->child0);
		freeTree(n2->child1);
		break;
	}
	case IF_ST: {
		n3 = (astNode3 *) n;
		freeTree(n3->child0);
		freeTree(n3->child1);
		freeTree(n3->child2);
		break;
	}
	case FOR_ST: {
		n4 = (astNode4 *) n;
		freeTree(n4->child0);
		freeTree(n4->child1);
		freeTree(n4->child2);
		freeTree(n4->child3);
		break;
	}
	case FUNCTION_DEF: {
		funNode *fn = (funNode *) n;
		freeTree(fn->codeTree);
		break;
	}
	case INT_EX: {
		intconNode *iNode = (intconNode *) n;
		break;
	}
	case FLOAT_EX: {
		floatconNode *fNode = (floatconNode *) n;
		break;
	}
	case GLOBAL_DCL: {
		idNode *iNode = (idNode *) n;
		break;
	}
	default:
		break;
	}

	free(n);
}

void freeCode( quad *q ) {
	quad *start = q;
	quad *temp;
	while(q != NULL) {
		temp = q;
		q = q->nextQuad;

		switch(temp->type) {
		case LABEL: free(temp->src1); break;
		case PARAM: free(temp->src2); break;
		}

		free(temp);
		if(q == start) break;
	}
}








