#ifndef STRUCTURES_H
#define STRUCTURES_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define TABLESIZE 26

typedef enum {
	PRIMITIVE,
	ARRAY,
	FUNCTION,
	FUN_PROT
} SymbolType;

typedef enum {
	VOID_TYPE,
	CHAR_TYPE,
	INT_TYPE,
	FLOAT_TYPE,
	BOOL_TYPE,
	NONE_TYPE,
	ERROR_TYPE
} PrimitiveType;

typedef enum {
	ARITHMETIC,
	COMPARISON,
	LOGICAL,
	ASSIGNMENT,
	UNARYMINUS,
	BOOLNOT
} OperatorType;

typedef enum {
	// Expressions
	COMMA_EX,
	CONSTANT_EX,
	ID_EX,
	INT_EX,
	FLOAT_EX,
	CHAR_EX,
	ARRAY_EX,
	FUNCALL_EX,
	UNARYMINUS_EX,
	NOT_EX,
	AND_EX,
	OR_EX,
	EQ_EX,
	NEQ_EX,
	PLUS_EX,
	MINUS_EX,
	STAR_EX,
	SLASH_EX,
	LEQ_EX,
	LESS_EX,
	GEQ_EX,
	GREATER_EX,
	// Statements
	IF_ST,
	WHILE_ST,
	FOR_ST,
	RETURN_ST,
	ASSIGN_ST,
	FUNCALL_ST,
	LIST_ST
} NodeType;

typedef enum {

} StatementType;

int error = 0;
int currType;
int returnType;
int tempNum = 0; // Indicates the next temporary variable which can be used

typedef struct symbolNode symbolNode;
struct symbolNode {
	char *symbol;
	SymbolType type; // Type of the symbol (primitive, function, etc)
	void *typeInfo; // Generic pointer to 'type' structure
	symbolNode *nextSymNode;
};

typedef struct idList idList;
struct idList {
	char *symbol; // Formal argument symbol, NULL if in prototype
	PrimitiveType type; // int, char, etc
	idList *nextID;
};

typedef struct symbolTable {
	symbolNode* array[TABLESIZE];
} symbolTable;

typedef struct literalType {
	PrimitiveType dataType; // int, char, etc
} literalType;

typedef struct arrayType {
	PrimitiveType dataType; // int, char etc
	int size;
} arrayType;

typedef struct functionType {
	PrimitiveType returnType; // int, char, etc
	idList *argIdList; // 
} functionType;

typedef struct fprotType {
	PrimitiveType returnType; // int, char, etc
	idList *argIdList; // 
} fprotType;

typedef struct astNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
} astNode;

typedef struct astNode1 {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	astNode *child0;
} astNode1;

typedef struct astNode2 {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	astNode *child0;
	astNode *child1;
} astNode2;

typedef struct astNode3 {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	astNode *child0;
	astNode *child1;
	astNode *child2;
} astNode3;

typedef struct astNode4 {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	astNode *child0;
	astNode *child1;
	astNode *child2;
	astNode *child3;
} astNode4;

typedef struct idNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
} idNode;

typedef struct intconNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	int value;
} intconNode;

typedef struct floatconNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	double value;
} floatconNode;

typedef struct charconNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	char value;
} charconNode;

typedef struct quad {
	NodeType type;
	symbolNode *src1;
	symbolNode *src2;
	symbolNode *dest;
	quad *prevQuad;
	quad *nextQuad;
} quad;

symbolTable *globalTable;
symbolTable *localTable;
quad *quadList;

idList *createIdList( char *symbol, PrimitiveType type, idList *nextID );
void destroyIdList( idList *list );

symbolTable *makeSymbolTable();
void destroyTable( symbolTable *t );

symbolNode *createSymbolNode( char *symbol, SymbolType type, void *typeInfo );
void destroySymbolNode( symbolNode *symNode );

literalType *createLiteralType( PrimitiveType dataType );

arrayType *createArrayType( PrimitiveType dataType, int size );

functionType *createFunctionType( PrimitiveType returnType, idList *argIdList );

fprotType *createFprotType( PrimitiveType returnType, idList *argIdList );

bool symbolExists( symbolTable *t, char* symbol );

symbolNode *getSymbol( symbolTable *t, char *symbol, SymbolType type );

void addSymbol( symbolTable *t, symbolNode *symNode );

void addIdList( symbolTable *t, idList *list );

bool idListsEqual( idList *list1, idList *list2 );
bool idListsCompatible( idList *list1, idList *list2 );

void updateTypes( idList *list, symbolTable *t );

PrimitiveType literalTypeOf( char *symbol );
PrimitiveType binaryExpressionType( PrimitiveType t1, PrimitiveType t2, OperatorType op );
PrimitiveType unaryExpressionType( PrimitiveType t, OperatorType op );
bool areCompatible( PrimitiveType t1, PrimitiveType t2 );
bool isValidFunctionCall( char *symbol, idList *list );
bool isSymbolType( char *symbol, SymbolType t );
void checkReturn();
int hash( char *str );

void yyerror(char *s);

// AST Construction

OperatorType toOpType(NodeType type);
astNode makeASTNode(NodeType type);
astNode makeASTNode(NodeType type, astNode n1);
astNode makeASTNode(NodeType type, astNode n1, astNode n2);
astNode makeASTNode(NodeType type, astNode n1, astNode n2, astNode n3);
astNode makeASTNode(NodeType type, astNode n1, astNode n2, astNode n3, astNode n4);
astNode makeIdNode(symbolNode symNode);
astNode makeIntconNode(int val);
astNode makeFloatconNode(double val);
astNode makeCharconNode(char val);
char *nextTempName();

// Quad List Construction



#endif
