void yyerror(char *s) {
	//fprintf(stderr,"Error(%d):\n",lineNumber);
}

void main( char *argv, int argc ) {
	int err;

	err = yyparse();
}