#ifndef FUNCTIONS_H
#define FUNCTIONS_H

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
	COMMA_EX, // <-- Expressions
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
	IF_ST, // <-- Statements
	WHILE_ST,
	FOR_ST,
	RETURN_ST,
	ASSIGN_ST,
	FUNCALL_ST,
	ARRAY_ASSN,
	LIST_ST,
	PROGRAM, // <-- Other
	FUNCTION_DEF,
	GLOBAL_DCL,
	LABEL,
	GOTO,
	PARAM,
	RETVAL,
	CALL,
	ENTER,
	LEAVE,
	RETURN_FUN,
	RETRIEVE,
	DEREF,
	REF
} NodeType;

typedef enum {
	CONSTANT,
	ST_ENTRY,
	STRING,
	QUAD,
	UNUSED
} SourceType;

extern int error;
extern int currType;
extern int returnType;
extern int tempNum; // Indicates the next temporary variable which can be used
extern int labelNum; // Indicates the next label number to use (for ifs, loops, etc)
extern int paramNum;
extern int currOffset;

extern bool hasReturn;
extern bool paramStart;
extern bool intermediate;
extern int paramNum;
extern char *funName;

extern bool debug;
extern int depth;

typedef struct symbolNode symbolNode;
struct symbolNode {
	char *symbol;
	SymbolType type; // Type of the symbol (primitive, function, etc)
	int offset; // Offset from frame pointer
	bool isGlobal;
	void *typeInfo; // Generic pointer to 'type' structure
	symbolNode *nextSymNode;
};

typedef struct idList idList;
struct idList {
	char *symbol; // Formal argument symbol, NULL if in prototype
	PrimitiveType type; // int, char, etc
	idList *nextID;
};

typedef struct quad quad;
struct quad {
	NodeType type;
	quad *prevQuad;
	quad *nextQuad;
	SourceType src1Type;
	SourceType src2Type;
	SourceType destType;
	void *src1;
	void *src2;
	void *dest;
};

typedef struct astNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
} astNode;

typedef struct symbolTable {
	symbolNode* array[TABLESIZE];
	int byteSize;
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
	idList *paramList; //
	symbolTable *symTable;
} functionType;

typedef struct fprotType {
	PrimitiveType returnType; // int, char, etc
	idList *paramList; //
} fprotType;

typedef struct astNode1 {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
	astNode *child0;
} astNode1;

typedef struct astNode2 {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
	astNode *child0;
	astNode *child1;
} astNode2;

typedef struct astNode3 {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
	astNode *child0;
	astNode *child1;
	astNode *child2;
} astNode3;

typedef struct astNode4 {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
	astNode *child0;
	astNode *child1;
	astNode *child2;
	astNode *child3;
} astNode4;

typedef struct idNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
} idNode;

typedef struct funNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
	symbolTable *symTable;
	astNode *codeTree;
} funNode;

typedef struct intconNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
	int value;
} intconNode;

typedef struct floatconNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
	double value;
} floatconNode;

typedef struct charconNode {
	NodeType type;
	PrimitiveType primType;
	symbolNode *location;
	quad *code;
	char value;
} charconNode;

extern symbolTable *globalTable;
extern symbolTable *localTable;
extern quad *quadList;

// Functions

idList *createIdList( char *symbol, PrimitiveType type, idList *nextID );
void destroyIdList( idList *list );

symbolTable *makeSymbolTable();
void destroyTable( symbolTable *t );

symbolNode *createSymbolNode( char *symbol, SymbolType type, void *typeInfo );
void destroySymbolNode( symbolNode *symNode );

literalType *createLiteralType( PrimitiveType dataType );

arrayType *createArrayType( PrimitiveType dataType, int size );

functionType *createFunctionType( PrimitiveType returnType, idList *paramList );
int numParams( symbolNode *functionNode );
int listLength( idList *list );

fprotType *createFprotType( PrimitiveType returnType, idList *paramList );

bool symbolExists( symbolTable *t, char* symbol );

symbolNode *getSymbol( symbolTable *t, char *symbol, SymbolType type );
symbolNode *getSymbol2( char *symbol, SymbolType type );

void addSymbol( symbolTable *t, symbolNode *symNode );
void addIdList( symbolTable *t, idList *list );

bool idListsEqual( idList *list1, idList *list2 );
bool idListsCompatible( idList *list1, idList *list2 );

void updateTypes( idList *list, symbolTable *t );
void updateOffsets( symbolTable *t );

PrimitiveType literalTypeOf( char *symbol );
PrimitiveType binaryExpressionType( PrimitiveType t1, PrimitiveType t2, OperatorType op );
PrimitiveType unaryExpressionType( PrimitiveType t, OperatorType op );
bool areCompatible( PrimitiveType t1, PrimitiveType t2 );
bool isValidFunctionCall( char *symbol, astNode *exprList );
bool isSymbolType( char *symbol, SymbolType t );
idList *exprTreeToList( astNode *exprTree );
void checkReturn();
int hash( char *str );

void yyerror(char *s);

// AST Construction

OperatorType toOpType(NodeType type);
astNode *makeASTNode(NodeType type);
astNode *makeASTNode1(NodeType type, astNode *n1);
astNode *makeASTNode2(NodeType type, astNode *n1, astNode *n2);
astNode *makeASTNode3(NodeType type, astNode *n1, astNode *n2, astNode *n3);
astNode *makeASTNode4(NodeType type, astNode *n1, astNode *n2, astNode *n3, astNode *n4);
astNode *makeIdNode(symbolNode *symNode);
astNode *makeGlobalNode(symbolNode *symNode);
astNode *makeFunNode(symbolNode *symNode, symbolTable *table, astNode *codeTree);
astNode *makeIntconNode(int val);
astNode *makeFloatconNode(double val);
astNode *makeCharconNode(char val);
char *nextTempName();
char *nextLabelName();

// Quad List Construction

void genCode(astNode *n);
void genCodeBool(astNode *n, char *trueDest, char *falseDest);
quad *concatCode( quad *q1, quad *q2 );
quad *newLabel();
quad *newInstr(NodeType type, SourceType s1Type, SourceType s2Type, SourceType dType, void* s1, void* s2, void* d);

void printCode( quad *q );
void printDebug( char *message );
void printQuadToAssembly( quad *q );
void printStandardLibrary();
void printDataSection();
void freeTree( astNode *n );
void freeCode( quad *q );

#endif
