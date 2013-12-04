#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "functions.h"
#include "clike.tab.h"

/*
#define LITERAL 0;
#define ARRAY 1;
#define FUNCTION 2;
#define FUN_PROT 3;

#define VOID_TYPE 0;
#define CHAR_TYPE 1;
#define INT_TYPE 2;
#define FLOAT_TYPE 3;
#define BOOL_TYPE 4;
#define NONE_TYPE 5;
*/

int currType;
int returnType;
bool hasReturn;

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
	symNode->symbol = symbol;
	symNode->type = type;
	symNode->typeInfo = typeInfo;
	return symNode;
}

void destroySymbolNode( symbolNode *symNode ) {
	if( symNode == NULL ) return;
	idList *list;
	switch( symNode->type ) {
		case PRIMITIVE:
			break;
		case ARRAY:
			break;
		case FUNCTION:
			list = ((functionType*) (symNode->typeInfo))->argIdList;
			destroyIdList(list);
			break;
		case FUN_PROT:
			list = ((fprotType*) (symNode->typeInfo))->argIdList;
			destroyIdList(list);
			break;
	}
	if(symNode->symbol != NULL) free(symNode->symbol);
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

functionType *createFunctionType( PrimitiveType returnType, idList *argIdList ) {
	functionType *type = malloc(sizeof(functionType));
	type->returnType = returnType;
	type->argIdList = argIdList;
	return type;
}

fprotType *createFprotType( PrimitiveType returnType, idList *argIdList ) {
	fprotType *type = malloc(sizeof(fprotType));
	type->returnType = returnType;
	type->argIdList = argIdList;
	return type;
}

bool symbolExists( symbolTable *t, char *symbol ) {
	char c = symbol[0];
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
	char c = symbol[0];
	symbolNode *curr = t->array[c % TABLESIZE];
	while( curr != NULL ) {
		if( curr->type == type && strcmp(curr->symbol, symbol) == 0 ) {
			return curr;
		}
		curr = curr->nextSymNode;
	}
	return NULL;
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
				bool parametersCorrect = idListsEqual( prototypeInfo->argIdList, functionInfo->argIdList );
				bool returnTypeCorrect = prototypeInfo->returnType == functionInfo->returnType;
				if( parametersCorrect && returnTypeCorrect ) {
					
				} else if( !parametersCorrect ){
					yyerror("Argument mismatch with prototype");
				} else {
					yyerror("Return type mismatch with prototype");
				}
				char c = symNode->symbol[0];
				symNode->nextSymNode = t->array[c % TABLESIZE];
				t->array[c % TABLESIZE] = symNode;
			} else {
				destroySymbolNode( symNode );
				yyerror("Symbol already declared");
			}	
		} else {
			destroySymbolNode( symNode );
			yyerror("Symbol already declared");
		}
	} else {
		char c = symNode->symbol[0];
		
		symNode->nextSymNode = t->array[c % TABLESIZE];
		t->array[c % TABLESIZE] = symNode;
	}
}

void addIdList( symbolTable *t, idList *list ) {
	idList *head = list;
	while( list != NULL ) {
		if( symbolExists( t, list->symbol ) ) {
			yyerror("Symbol already declared");
		} else {
			char *symbol = strdup(list->symbol);
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

void updateTypes( idList *list, symbolTable *t ) {
	idList *head = list;

	while( list != NULL ) {
		char *symbol = list->symbol;
		char c = symbol[0];
		
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
		else
			return ERROR_TYPE;
	} else if ( symbolExists( globalTable, symbol ) ) {
		if( ( curr = getSymbol( globalTable, symbol, FUNCTION ) ) != NULL )
			return ((functionType*) (curr->typeInfo))->returnType;
		else if( ( curr = getSymbol( globalTable, symbol, ARRAY ) ) != NULL )
			return ((arrayType*) (curr->typeInfo))->dataType;
		else if( ( curr = getSymbol( globalTable, symbol, PRIMITIVE ) ) != NULL )
			return ((literalType*) (curr->typeInfo))->dataType;
		else
			return ERROR_TYPE;
	} else {
		return ERROR_TYPE;
	}

}

PrimitiveType binaryExpressionType( PrimitiveType t1, PrimitiveType t2, OperatorType op ) {
	if( t1 == ERROR_TYPE || t2 == ERROR_TYPE ) {
		return ERROR_TYPE;
	} else if( op == COMPARISON ) {
		if( areCompatible( t1, t2 ) && ( t1 == CHAR_TYPE || t1 == INT_TYPE || t1 == FLOAT_TYPE ) )
			return BOOL_TYPE;
		else {
			yyerror("Incompatible types");
			return ERROR_TYPE;
		}
	} else if ( op == ARITHMETIC ) {
		if( areCompatible( t1, t2 ) && ( t1 == CHAR_TYPE || t1 == INT_TYPE || t1 == FLOAT_TYPE ) )
			if( t1 == CHAR_TYPE || t1 == INT_TYPE )
				return INT_TYPE;
			else
				return FLOAT_TYPE;
		else {
			yyerror("Incompatible types");
			return ERROR_TYPE;
		}
	} else if ( op == LOGICAL ) {
		if( t1 == BOOL_TYPE && t2 == BOOL_TYPE )
			return BOOL_TYPE;
		else {
			yyerror("Incompatible types");
			return ERROR_TYPE;
		}
	} else {
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
		yyerror("Incompatible types");
		return false;
	}
}

bool isValidFunctionCall( char *symbol, idList *list ) {
	symbolNode *function = getSymbol( globalTable, symbol, FUNCTION );
	if( function == NULL ) {
		yyerror("Invalid function call");
		return false;
	} else { 
		functionType *functionInfo = (functionType*) function->typeInfo;
		if( idListsCompatible( functionInfo->argIdList, list ) )
			return true;
		else {
			yyerror("Argument mismatch in function call");
			return false;
		}
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

void checkReturn() {
	if( !hasReturn )
		yyerror("No return statement in non-void function");
	hasReturn = false;
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

astNode makeASTNode(NodeType type) {
	astNode *n = malloc(sizeof(astNode));
	n->type = type;
	n->primType = VOID_TYPE;
	return n;
}

astNode makeASTNode(NodeType type, astNode n1) {
	astNode1 *n = malloc(sizeof(astNode1));
	n->type = type;
	if(UNARYMINUS_EX <= type && type <= NOT_EX) {
		n->primType = unaryExpressionType( n1->primType, toOpType(type) );
	} else {
		n->primType = VOID_TYPE;
	}
	n->child0 = n1;
	return (astNode *) n;
}

astNode makeASTNode(NodeType type, astNode n1, astNode n2) {
	astNode2 *n = malloc(sizeof(astNode2));
	n->type = type;
	if(CONSTANT_EX <= type && type <= GREATER_EX) {
		n->primType = binaryExpressionType( n1->primType, n2->primType, toOpType(type) );
	} else {
		n->primType = VOID_TYPE;
	}
	n->child0 = n1;
	n->child1 = n2;
	return (astNode *) n;
}

astNode makeASTNode(NodeType type, astNode n1, astNode n2, astNode n3) {
	astNode3 *n = malloc(sizeof(astNode3));
	n->type = type;
	n->primType = VOID_TYPE;
	n->child0 = n1;
	n->child1 = n2;
	n->child2 = n3;
	return (astNode *) n;
}

astNode makeIdNode(symbolNode symNode) {
	idNode *n = malloc(sizeof(idNode));
	n->type = ID_EX;
	n->id = symNode;
	return (astNode *) n;
}

astNode makeIntconNode(int val) {
	intconNode *n = malloc(sizeof(intconNode));
	n->type = INT_EX;
	n->value = val;
	return (astNode *) n;
}

astNode makeFloatconNode(double val) {
	floatconNode *n = malloc(sizeof(floatconNode));
	n->type = FLOAT_EX;
	n->value = val;
	return (astNode *) n;
}

astNode makeCharconNode(char val) {
	charconNode *n = malloc(sizeof(charconNode));
	n->type = CHAR_EX;
	n->value = val;
	return (astNode *) n;
}


















