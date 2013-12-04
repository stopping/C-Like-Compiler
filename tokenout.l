%option yylineno

%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "structures.h"
#include "clike.tab.h"

int lineNumber = 1;
int status = 0;

int yycolumn = 1;

#define YY_USER_ACTION yylloc.first_line = yylloc.last_line = yylineno; \
	yylloc.first_column = yycolumn; yylloc.last_column = yycolumn+yyleng-1; \
	yycolumn += yyleng;
%}

SPACE			[ \t\v\f]
NEWLINE			[\n\r]
KW1				auto|_Bool|break|case|char|_Complex|const|continue|default
KW2				restrict|do|double|else|enum|extern|float|for|goto|if
KW3				_Imaginary|inline|int|long|register|return|short|signed
KW4				sizeof|static|struct|switch|typedef|union|unsigned|void
KW5				volatile|while
KEYWORD			{KW1}|{KW2}|{KW3}|{KW4}|{KW5}
SNGLCOMMENT		\/\/.*[\r\n]
COMMENTSTART	\/\*
COMMENTEND		\*\/
ID				[a-zA-Z_][0-9a-zA-Z_]*
STRING			"[0-9a-zA-Z]
INTSUF			(l|L|ll|LL)[uU]?|[uU](l|L|ll|LL)?	
DEC				[1-9][0-9]*{INTSUF}?
OCT				0[0-7]*{INTSUF}?
HEX				0[Xx][A-Fa-f0-9]+{INTSUF}?
DIGSEQ			[0-9]+
DOTDIGS			({DIGSEQ}\.)|({DIGSEQ}\.{DIGSEQ})|(\.{DIGSEQ})
EXP				[eE][\+-]?{DIGSEQ}
FLOATSUF		[fFlL]
FLOAT			({DIGSEQ}{EXP}{FLOATSUF}?)|({DOTDIGS}{EXP}?{FLOATSUF}?)
CHARESCAPE		[ntbrfv\\\'\"a\?]
OCTESCAPE		[0-7]{1,3}
HEXESCAPE		x[A-Fa-f0-9]+
ESCAPECODE		\\({CHARESCAPE}|{OCTESCAPE}|{HEXESCAPE})
CHARSEQ			({ESCAPECODE}|[^\'])+
CHAR			L?\'{CHARSEQ}\'
STRSEQ			({ESCAPECODE}|[^\"])+
STR				L?\"{STRSEQ}?\"
SIMPOPS			[%\^&~\|\.\?]
COMPOPS1		\+=|\-=|\*=|\/=|%=|<<=|>>=|&=|\^=|\|=
COMPOPS2		->|\+\+|\-\-|<<|>>
SEPOPS			\.\.\.
ALTOPS			<%|%>|<:|:>|%:|%:%:
OPERATOR		{SIMPOPS}|{COMPOPS1}|{COMPOPS2}|{SEPOPS}|{ALTOPS}
QUOTES			[\'\"]
VALIDSINGLE		[^\'\" \n\r\t\v\f]

%x MULTICOMMENT

%%

<INITIAL>{COMMENTSTART} { BEGIN(MULTICOMMENT); }

<MULTICOMMENT><<EOF>> {
		fprintf( stderr, "%d, Unterminated Comment\n", lineNumber);
		return(1);
	}

<MULTICOMMENT>[\n\r] { yycolumn = 1; BEGIN(MULTICOMMENT); }

<MULTICOMMENT>. { BEGIN(MULTICOMMENT) ; }

<MULTICOMMENT>{COMMENTEND} { BEGIN(INITIAL); }

"void"		{ return VOID; }
"char"		{ return CHAR; }
"int"		{ return INT; }
"float"		{ return FLOAT; }
"double"	{ return FLOAT; }
"while"		{ return WHILE; }
"for"		{ return FOR; }
"return"	{ return RETURN; }
"if"		{ return IF; }
"else"		{ return ELSE; }

"="		{ return ASSIGN; }
"-"		{ return MINUS; }
"!"		{ return NOT; }
"&&"	{ return AND; }
"||"	{ return OR; }
"=="	{ return EQ; }
"!="	{ return NEQ; }
"+"		{ return PLUS; }
"*"		{ return STAR; }
"/"		{ return SLASH; }
"<="	{ return LEQ; }
"<"		{ return LESS; }
">="	{ return GEQ; }
">"		{ return GREATER; }
\(		{ return LPAREN; }
\)		{ return RPAREN; }
\[		{ return LBRACK; }
\]		{ return RBRACK; }
\{		{ return LCURLY; }
\}		{ return RCURLY; }
","		{ return COMMA; }
";"		{ return SEMICOLON; }

{SNGLCOMMENT} { yycolumn = 1; }

{ID} { yylval.name = strdup(yytext); return ID; }

{DEC} { yylval.iVal = atoi(yytext); return INTCON; }

{OCT} { yylval.iVal = atoi(yytext); return INTCON; }

{HEX} { yylval.iVal = atoi(yytext); return INTCON; }

{FLOAT} { yylval.fVal = atof(yytext); return FLOATCON; }

{CHAR} { yyerror("Unknown token"); 
		status = (1);
	}

{STR} { yyerror("Unknown token"); 
		status = (1);
	}

{QUOTES} { yyerror("Unknown token"); 
		status = (1);
	}
	
{OPERATOR} { yyerror("Operator not in grammar");
		status = (1);
	}

{NEWLINE} { yycolumn = 1; }

{SPACE}

%%
