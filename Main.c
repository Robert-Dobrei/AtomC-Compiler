#include <stdio.h>
#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "at.h"
#include "vm.h"

int main() {
	Token* tokens;
	char* inbuf = loadFile("tests/testgc.c");
	//showTokens(tokenize(inbuf));
	tokens = tokenize(inbuf);
	pushDomain();
	vmInit();
	parse(tokens);
	//showDomain(symTable, "global"); 
	//Instr* testCode = genTestProgram(); 
	//run(testCode);
	Symbol *symMain=findSymbolInDomain(symTable,"main");
	if (!symMain)err("missing main function");
	Instr* entryCode = NULL;
	addInstr(&entryCode, OP_CALL)->arg.instr = symMain->fn.instr;
	addInstr(&entryCode, OP_HALT);
	run(entryCode);
	dropDomain();
	return 0;
}