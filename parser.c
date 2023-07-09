#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "utils.h"
#include "ad.h"
#include "at.h"
#include "vm.h"
#include "gc.h"


Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token
Symbol* owner;

void tkerr(const char *fmt,...){
	fprintf(stderr,"error in line %d: ",iTk->line);
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}

const char* tkCodeName(int code) {
	static const char* const code_names[] = {
		"ID", "INT", "DOUBLE", "CHAR", "STRING",
		"TYPE_CHAR", "TYPE_INT", "TYPE_DOUBLE", "ELSE", "IF",
		"RETURN", "STRUCT", "VOID", "WHILE",
		"COMMA", "END", "SEMICOLON", "LPAR", "RPAR",
		"LBRACKET", "RBRACKET", "LACC", "RACC",
		"ASSIGN", "EQUAL", "ADD", "SUB", "MUL", "DIV",
		"DOT", "AND", "OR", "NOT", "NOTEQ", "LESS",
		"LESSEQ", "GREATER", "GREATEREQ"
	};
	// The array size is the number of codes in the enumeration.
	const size_t num_codes = sizeof(code_names) / sizeof(*code_names);

	if (code >= 0 && code < num_codes) {
		return code_names[code];
	}
	else {
		return "UNKNOWN"; // Return a default value for invalid codes.
	}
}

//const char *tkCodeName(int code) - o funcție care primește ca parametru un cod de
//atom și îi returnează numele
bool consume(int code){
	//printf("consume(%s)",tkCodeName(code));
	if(iTk->code==code){
		consumedTk=iTk;
		iTk=iTk->next;
		//printf(" => consumed\n");
		return true;
		}
	//printf(" => found %s\n",tkCodeName(iTk->code));
	return false;
	}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type* t) {
	t->n = -1;
	Token* start = iTk;
	if (consume(TYPE_INT)) {
		t->tb = TB_INT;
		return true;
	}
	if (consume(TYPE_DOUBLE)) {
		t->tb = TB_DOUBLE;
		return true;
	}
	if (consume(TYPE_CHAR)) {
		t->tb = TB_CHAR;
		return true;
	}
	if (consume(STRUCT)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if (!t->s)tkerr("undefined structure: %s", tkName->text);
			return true;
		}
		else tkerr("structure name missing.");
	}
	iTk = start;
	return false;
}

bool expr(Ret* r);

//exprPrimary: ID(LPAR(expr(COMMA expr)*) ? RPAR) ?
//| INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(Ret* r) {
	//printf("#exprPrimary\n");
	Token* start = iTk;
	if (consume(ID)) {
		Token* tkName = consumedTk;
		Symbol* s = findSymbol(tkName->text);
		if (!s)tkerr("undefined id: %s", tkName->text);
		if (consume(LPAR)) {
			if (s->kind != SK_FN)tkerr("only a function can be called");
			Ret rArg;
			Symbol* param = s->fn.params;
			if (expr(&rArg)) {
				if (!param)tkerr("too many arguments in function call");
				if (!convTo(&rArg.type, &param->type))tkerr("in call, cannot convert the argument type to the parameter type");
				addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
				param = param->next;
				for (;;) {
					if (consume(COMMA)) {
						if (expr(&rArg)) {
							if (!param)tkerr("too many arguments in function call");
							if (!convTo(&rArg.type, &param->type))tkerr("in call, cannot convert the argument type to the parameter type");
							addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
							insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
							param = param->next;
						}
					}
					else break;
				}
			}
			if (consume(RPAR)) {
				if (param)tkerr("too few arguments in function call");
				*r = (Ret){ s->type,false,true };
				if (s->fn.extFnPtr) {
					addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
				}
				else {
					addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
				}
			}
			else tkerr(") missing after the expression");
		}
		if (s->kind == SK_FN)tkerr("a function can only be called");
		*r = (Ret){ s->type,true,s->type.n >= 0 };
		if (s->kind == SK_VAR) {
			if (s->owner == NULL) { // global variables
				addInstr(&owner->fn.instr, OP_ADDR)->arg.p = s->varMem;
			}
			else { // local variables
				switch (s->type.tb) {
				case TB_INT:addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->varIdx + 1); break;
				case TB_DOUBLE:addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->varIdx + 1); break;
				}
			}
		}
		if (s->kind == SK_PARAM) {
			switch (s->type.tb) {
			case TB_INT:
				addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->paramIdx - symbolsLen(s->owner->fn.params) -
					1); break;
			case TB_DOUBLE:
				addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->paramIdx - symbolsLen(s->owner->fn.params) -
					1); break;
			}
		
		}
		return true;
	}else if(consume(INT)) {
		Token* ct = consumedTk;
		*r = (Ret){ {TB_INT,NULL,-1},false,true }; 
		addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);
		return true; 
	}
	else if(consume(DOUBLE)){ 
		Token* ct = consumedTk;
		*r = (Ret){ {TB_DOUBLE,NULL,-1},false,true }; 
		addInstrWithDouble(&owner->fn.instr, OP_PUSH_F, ct->d);
		return true; }
	else if (consume(CHAR)) { Token* ct = consumedTk; *r = (Ret){ {TB_CHAR,NULL,-1},false,true }; return true; }
	else if (consume(STRING)) { Token* ct = consumedTk; *r = (Ret){ {TB_CHAR,NULL,0},false,true }; return true; }
	else if (consume(LPAR)) {
		if (expr(r)) {
			if (consume(RPAR)) {
				return true;
			}
			else tkerr(") missing after the expression");
		}
	}
	iTk = start;
	return false;
}

//exprPostfix: exprPostfix LBRACKET expr RBRACKET
//| exprPostfix DOT ID
//| exprPrimary
//A: A α1 | … | A αm | β1 | … | βn
//A = exprPostfix
//a1 = LBRACKET expr RBRACKET
//a2 = DOT ID
//β1 = exprPrimary

//A: β1 A’ | … | βn A’
//exprPostfix: exprPrimary exprPostfixPrim
//A’: α1 A’ | … | αm A’ | ε
//exprPostfixPrim: LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim | ε
bool exprPostfixPrim(Ret* r) {
	//printf("#exprPostfixPrim\n");
	Token* start = iTk;
	if (consume(LBRACKET)) {
		Ret idx;
		if (expr(&idx)) {
			if (consume(RBRACKET)) {
				if (r->type.n < 0)tkerr("only an array can be indexed");
				Type tInt = { TB_INT,NULL,-1 };
				if (!convTo(&idx.type, &tInt))tkerr("the index is not convertible to int");
				r->type.n = -1;
				r->lval = true;
				r->ct = false;
				if (exprPostfixPrim(r)) {
					return true;
				}
			}
			else tkerr("] missing after the expression");
		}
		else tkerr("index expression is missing");
	}
	else if (consume(DOT)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;
			if (r->type.tb != TB_STRUCT)tkerr("a field can only be selected from a struct");
			Symbol* s = findSymbolInList(r->type.s->structMembers, tkName->text);
			if (!s)tkerr("the structure %s does not have a field % s",r->type.s->name,tkName->text);
				*r = (Ret){ s->type,true,s->type.n >= 0 };
			if (exprPostfixPrim(r)) {
				return true;
			}
		}
		else tkerr("the field after . is missing");
	}
	iTk = start;
	return true;
}

bool exprPostfix(Ret* r) {
	//printf("#exprPostfix\n");
	Token* start = iTk;
	if (exprPrimary(r)) {
		if (exprPostfixPrim(r)) {
			return true;
		}
	}
	iTk = start;
	return false;
}


//exprUnary: (SUB | NOT) exprUnary | exprPostfix
bool exprUnary(Ret* r) {
	//printf("#exprUnary\n");
	Token* start = iTk;
	if (consume(SUB)) {
		if (exprUnary(r)) { 
			if (!canBeScalar(r))tkerr("unary - or ! must have a scalar operand");
			r->lval = false;
			r->ct = true;
			return true; 
		}
	}
	else if (consume(NOT)) {
		if (exprUnary(r)) { 
			if (!canBeScalar(r))tkerr("unary - or ! must have a scalar operand");
			r->lval = false;
			r->ct = true;
			return true; 
		}
	}
	iTk = start;
	if (exprPostfix(r)) { return true; }
	iTk = start;
	return false;
}

bool arrayDecl(Type* t);

//exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(Ret* r) {
	Type t;
	//printf("#exprCast\n");
	Token* start = iTk;
	if (consume(LPAR)) {
		Type t; Ret op;
		if (typeBase(&t)) {
			if(arrayDecl(&t)){}
			if (consume(RPAR)) {
				if (exprCast(&op)) {
					if (t.tb == TB_STRUCT)tkerr("cannot convert to a struct type");
					if (op.type.tb == TB_STRUCT)tkerr("cannot convert a struct");
					if (op.type.n >= 0 && t.n < 0)tkerr("an array can be converted only to another array");
					if (op.type.n < 0 && t.n >= 0)tkerr("a scalar can be converted only to another scalar");
					*r = (Ret){ t,false,true };
					return true;
				}
			}
			else tkerr(") missing after cast");
		}
	}
	iTk = start;
	if (exprUnary(r)) {
		return true;
	}
	iTk = start;
	return false;
}

//exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
bool exprMulPrim(Ret* r) {
	//printf("#exprMulPrim\n");
	Token* start = iTk;
	Token* op;
	if (consume(MUL)) {
		Ret right;
		op = consumedTk;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprCast(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for * or /");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case MUL:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_MUL_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_MUL_F); break;
				}
				break;
			case DIV:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_DIV_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_DIV_F); break;
				}
				break;
				*r = (Ret){ tDst,false,true };
				if (exprMulPrim(r)) {
					return true;
				}
			}
		}
		else if (consume(DIV)) {
			Ret right;
			op = consumedTk;
			Instr* lastLeft = lastInstr(owner->fn.instr);
			addRVal(&owner->fn.instr, r->lval, &r->type);
			if (exprCast(&right)) {
				Type tDst;
				if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for * or /");
				addRVal(&owner->fn.instr, right.lval, &right.type);
				insertConvIfNeeded(lastLeft, &r->type, &tDst);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
				switch (op->code) {
				case MUL:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_MUL_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_MUL_F); break;
					}
					break;
				case DIV:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_DIV_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_DIV_F); break;
					}
					break;
					*r = (Ret){ tDst,false,true };
					if (exprMulPrim(r)) {
						return true;
					}
				}
			}
			iTk = start;
			return true;
		}
	}
}

bool exprMul(Ret* r) {
	Token* op;
	//printf("#exprMul\n");
	Token* start = iTk;
	if (exprCast(r)) {
		if (exprMulPrim(r)) {
			return true;
		}
	}
	iTk = start;
	return false;
}

//exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
bool exprAddPrim(Ret* r) {
	//printf("#exprAddPrim\n");
	Token* start = iTk;
	Token* op;
	if (consume(ADD)) {
		Ret right;
		op = consumedTk;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprMul(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for + or -");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case ADD:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_ADD_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_ADD_F); break;
				}
				break;
			case SUB:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_SUB_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_SUB_F); break;
				}
				break;
			}
			*r = (Ret){ tDst,false,true };
			if (exprAddPrim(r)) {
				return true;
			}
		}
	}
	else if (consume(SUB)) {
		Ret right;
		op = consumedTk;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprMul(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for + or -");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case ADD:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_ADD_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_ADD_F); break;
				}
				break;
			case SUB:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_SUB_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_SUB_F); break;
				}
				break;
			}
			*r = (Ret){ tDst,false,true };
			if (exprAddPrim(r)) {
				return true;
			}
		}
	}
	iTk = start;
	return true;
}

bool exprAdd(Ret* r) {
	//printf("#exprAdd\n");
	Token* op;
	Token* start = iTk;
	if (exprMul(r)) {
		if (exprAddPrim(r)) {
			return true;
		}
	}
	iTk = start;
	return false;
}

//exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
bool exprRelPrim(Ret* r) {
	//printf("#exprRelPrim\n");
	Token* start = iTk;
	Token* op;
	if (consume(LESS)) {
		op = consumedTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprAdd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >= ");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case LESS:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F); break;
				}
				break;
			}
			* r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprRelPrim(r)) {
				return true;
			}
		}
		else tkerr("expression missing after <");
	}
	else if (consume(LESSEQ)) {
		op = consumedTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		if (exprAdd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >= ");	
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case LESS:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F); break;
				}
				break;
			}
			* r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprRelPrim(r)) {
				return true;
			}
		}else tkerr("expression missing after <=");
	}
	else if (consume(GREATER)) {
		op = consumedTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		if (exprAdd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >= ");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case LESS:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F); break;
				}
				break;
			}
			* r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprRelPrim(r)) {
				return true;
			}
		}
		else tkerr("expression missing after >");
	}
	else if (consume(GREATEREQ)) {
		op = consumedTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		if (exprAdd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >= ");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case LESS:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F); break;
				}
				break;
			}
			* r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprRelPrim(r)) {
				return true;
			}
		}else tkerr("expression missing after >=");
	}
	iTk = start;
	return true;
}

bool exprRel(Ret* r) {
	Token* op;
	//printf("#exprRel\n");
	Token* start = iTk;
	if (exprAdd(r)) {
		if (exprRelPrim(r)) {
			return true;
		}
	}
	iTk = start;
	return false;
}

//exprEq: exprEq (EQUAL | NOTEQ) exprRel | exprRel
bool exprEqPrim(Ret* r) {
	//printf("#exprEqPrim\n");
	Token* start = iTk;
	if (consume(EQUAL)) {
		Ret right;
		if (exprRel(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for == or != ");
				* r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprEqPrim(r)) {
				return true;
			}
		}
	}
	else if (consume(NOTEQ)) {
		Ret right;
		if (exprRel(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for == or != ");
				*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprEqPrim(r)) {
				return true;
			}
		}
	}
	iTk = start;
	return true;
}

bool exprEq(Ret* r) {
	//printf("#exprEq\n");
	Token* start = iTk;
	if (exprRel(r)) {
		if (exprEqPrim(r)) {
			return true;
		}
	}
	iTk = start;
	return false;
}

//exprAnd: exprAnd AND exprEq | exprEq
//A: A α1 | … | A αm | β1 | … | βn
//A = exprAnd
//a1 = AND exprEq
//β1 = exprEq

//A: β1 A’ | … | βn A’
//exprAnd: exprEq exprAndPrim
//A’: α1 A’ | … | αm A’ | ε
//exprAndPrim: AND exprEq exprAndPrim | ε

bool exprAndPrim(Ret* r) {
	//printf("#exprAndPrim\n");
	Token* start = iTk;
	if (consume(AND)) {
		Ret right;
		if (exprEq(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for &&");
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprAndPrim(r)) {
				return true;
			}
		}
	}
	iTk = start;
	return true;
}

bool exprAnd(Ret* r) {
	//printf("#exprAnd\n");
	Token* start = iTk;
	if (exprEq(r)) {
		if (exprAndPrim(r)) {
			return true;
		}
	}
	iTk = start;
	return false;
}

//exprOr: exprOr OR exprAnd | exprAnd
//A: A α1 | … | A αm | β1 | … | βn
//A = exprOr
//a1 = OR exprAnd
//β1 = exprAnd

//A: β1 A’ | … | βn A’
//exprOr: exprAnd exprOrPrim
//A’: α1 A’ | … | αm A’ | ε
//exprOrPrim: OR exprAnd exprOrPrim | ε  adica  (OR exprAnd exprOrPrim)?

bool exprOrPrim(Ret* r) {
	//printf("#exprOrPrim\n");
	Token* start = iTk;
	if (consume(OR)) {
		Ret right;
		if (exprAnd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for ||");
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprOrPrim(r)) {
				return true;
			}
		}
	}
	iTk = start;
	return true; //de la epsilon
}

bool exprOr(Ret* r) {
	//printf("#exprOr\n");
	Token* start = iTk;
	if (exprAnd(r)) {
		if (exprOrPrim(r)) {
			return true;
		}
	}
	iTk = start;
	return false;
}



//exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(Ret* r) {
	//printf("#exprAssign\n");
	Ret rDst;
	Token* start = iTk;
	if (exprUnary(&rDst)) {
		if (consume(ASSIGN)) {
			if (exprAssign(r)) {
				addRVal(&owner->fn.instr, r->lval, &r->type);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);
				switch (rDst.type.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_STORE_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_STORE_F); break;
				}
				if (!rDst.lval)tkerr("the assign destination must be a left-value");
				if (rDst.ct)tkerr("the assign destination cannot be constant");
				if (!canBeScalar(&rDst))tkerr("the assign destination must be scalar");
				if (!canBeScalar(r))tkerr("the assign source must be scalar");
				if (!convTo(&r->type, &rDst.type))tkerr("the assign source cannot be converted to destination");
				r->lval = false;
				r->ct = true;

				return true; 
			}
			else tkerr("expression missing after =");
		}
	}
	iTk = start;
	if (exprOr(r)) { return true; }
	iTk = start;
	return false;
}

//expr: exprAssign
bool expr(Ret* r) {
	//printf("#expr\n");
	Token* start = iTk;
	if (exprAssign(r)) {
		return true;
	}
	iTk = start;
	return false;
}

//arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(Type *t){
	//printf("#arrayDecl\n");
	Token* start = iTk;
	if (consume(LBRACKET)) {
		if (consume(INT)) {
			Token* tkSize = consumedTk;
			t->n = tkSize->i; 
		}else {
			t->n = 0; // array fara dimensiune: int v[]
		}
		if (consume(RBRACKET)) {
			return true;
		}
		else tkerr("] missing on array declaration");
	}
	iTk = start;
	return false;
}

//varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef() {
	Type t;
	//printf("#varDef\n");
	Token* start = iTk;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;
			if (arrayDecl(&t)) { if (t.n == 0)tkerr("a vector variable must have a specified dimension"); }
			if (consume(SEMICOLON)) {
				Symbol* var = findSymbolInDomain(symTable, tkName->text);
				if (var)tkerr("symbol redefinition: %s", tkName->text);
				var = newSymbol(tkName->text, SK_VAR);
				var->type = t;
				var->owner = owner;
				addSymbolToDomain(symTable, var);
				if (owner) {
					switch (owner->kind) {
					case SK_FN:
						var->varIdx = symbolsLen(owner->fn.locals);
						addSymbolToList(&owner->fn.locals, dupSymbol(var));
						break;
					case SK_STRUCT:
						var->varIdx = typeSize(&owner->type);
						addSymbolToList(&owner->structMembers, dupSymbol(var));
						break;
					}
				}
				else {
					var->varMem = safeAlloc(typeSize(&t));
				}

				return true;
			}
			else tkerr("; missing after variable def.");
		}
	}
	iTk = start;
	return false;
}

//structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef() {
	//printf("#structDef\n");
	Token* start=iTk;
	if (consume(STRUCT)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;
			if (consume(LACC)) {
				Symbol* s = findSymbolInDomain(symTable, tkName->text);
				if (s)tkerr("symbol redefinition: %s", tkName->text);
				s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
				s->type.tb = TB_STRUCT;
				s->type.s = s;
				s->type.n = -1;
				pushDomain();
				owner = s;

				for (;;) {
					if (varDef()) {}
					else break;
				}
				if (consume(RACC)) {
					if (consume(SEMICOLON)) {
						owner = NULL;
						dropDomain();
						return true;
					}
					else tkerr("; missing after structure");
				}
				else tkerr("} missing after structure");
			}			
		}
		else tkerr("name of structure is missing");
	}
	iTk = start;
	return false;
}

//fnParam: typeBase ID arrayDecl?
bool fnParam() {
	Type t;
	//printf("#fnParam\n");
	Token* start = iTk;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;
			if(arrayDecl(&t)){ t.n = 0; }
			Symbol* param = findSymbolInDomain(symTable, tkName->text);
			if (param)tkerr("symbol redefinition: %s", tkName->text);
			param = newSymbol(tkName->text, SK_PARAM);
			param->type = t;
			param->owner = owner;
			param->paramIdx = symbolsLen(owner->fn.params);
			// parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
			addSymbolToDomain(symTable, param);
			addSymbolToList(&owner->fn.params, dupSymbol(param));
			return true;
		}
		else tkerr("name of parameter is missing.");
	}
	//else tkerr("Lipseste tipul parametrului");
	iTk = start;
	return false;
}

bool stm();

//stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound(bool newDomain) {
	//printf("#stmCompound\n");
	Token* start = iTk;
	if (consume(LACC)) {
		if (newDomain)pushDomain();
		for (;;) {
			if (varDef()) {}
			else if (stm()) {}
			else break;
		}
		if (consume(RACC)) {
			if (newDomain)dropDomain();
			return true;
		}
		else tkerr("} missing at the end of the function");
	}
	iTk = start;
	return false;
}

//stm: stmCompound
//| IF LPAR expr RPAR stm(ELSE stm) ?
//| WHILE LPAR expr RPAR stm
//| RETURN expr ? SEMICOLON
//| expr ? SEMICOLON
bool stm() {
	//printf("#stm\n");
	Ret rCond, rExpr;
	Token* start = iTk;
	if (stmCompound(true)) {
		return true;
	}
	else if (consume(IF)) {
		if (consume(LPAR)) {
			if (expr(&rCond)) {
				if (!canBeScalar(&rCond))tkerr("the if condition must be a scalar value");
				if (consume(RPAR)) {
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
					Type intType = { TB_INT,NULL,-1 };
					insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
					Instr* ifJF = addInstr(&owner->fn.instr, OP_JF);
					//printf("RPAR in stm");
					if (stm()) {
						if (consume(ELSE)) {
							Instr* ifJMP = addInstr(&owner->fn.instr, OP_JMP);
							ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
							if (stm()) {
								ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
								return true;
							}
						}
						ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
						return true;
					}
				}
				else tkerr(") missing after if");
			}
		}
		else tkerr("( missing after if");
	}
	else if (consume(WHILE)) {
		Instr* beforeWhileCond = lastInstr(owner->fn.instr);
		if (consume(LPAR)) {
			if (expr(&rCond)) {
				if (!canBeScalar(&rCond))tkerr("the while condition must be a scalar value");
				if (consume(RPAR)) {
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
					Type intType = { TB_INT,NULL,-1 };
					insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
					Instr* whileJF = addInstr(&owner->fn.instr, OP_JF);
					if (stm()) {
						addInstr(&owner->fn.instr, OP_JMP)->arg.instr = beforeWhileCond->next;
						whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
						return true;
					}
				}
				else tkerr(") missing after while");
			}
		}
		else tkerr("( missing after while");
	}
	else if (consume(RETURN)) {
		if (expr(&rExpr)) {
			addRVal(&owner->fn.instr, rExpr.lval, &rExpr.type);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type, &owner->type);
			addInstrWithInt(&owner->fn.instr, OP_RET, symbolsLen(owner->fn.params));
			if (owner->type.tb == TB_VOID)tkerr("a void function cannot return a value");
			if (!canBeScalar(&rExpr))tkerr("the return value must be a scalar value");
			if (!convTo(&rExpr.type, &owner->type))tkerr("cannot convert the return expression type to the function return type");
		}else{
			addInstr(&owner->fn.instr, OP_RET_VOID);
			if (owner->type.tb != TB_VOID)tkerr("a non-void function must return a value"); }
		if (consume(SEMICOLON)) {
			return true;
		}
		else tkerr("; missing after return");
	}
	else {
		if (expr(&rExpr)) { if (rExpr.type.tb != TB_VOID)addInstr(&owner->fn.instr, OP_DROP); }
		if (consume(SEMICOLON)) {
			return true;
		};
	}
	iTk = start;
	return false;
}


//fnDef: (typeBase | VOID) ID
//				LPAR(fnParam(COMMA fnParam)*) ? RPAR
//				stmCompound
bool fnDef() {
	Type t;
	//printf("#fnDef\n");
	Token* start = iTk;
	if (typeBase(&t)) {
		if(consume(ID)){
			Token* tkName = consumedTk;
			if (consume(LPAR)) {
				Symbol* fn = findSymbolInDomain(symTable, tkName->text);
				if (fn)tkerr("symbol redefinition: %s", tkName->text);
				fn = newSymbol(tkName->text, SK_FN);
				fn->type = t;
				addSymbolToDomain(symTable, fn);
				owner = fn;
				pushDomain();
				if (fnParam()) {
					for (;;) {
						if (consume(COMMA)) {
							if (fnParam());
						}
						else break;
					}
				}
				if (consume(RPAR)) {
					//puts("#1");
					addInstr(&fn->fn.instr, OP_ENTER);
					//puts("#2");
					if (stmCompound(false)) {
						fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
						if (fn->type.tb == TB_VOID)
							addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
						dropDomain();
						owner = NULL;
						return true;
					}
				}
				else tkerr(") missing on function def.");
			}
		}
		else tkerr("name of function is missing");
	}
	else if (consume(VOID)) {
		t.tb = TB_VOID;
		if (consume(ID)) {
			Token* tkName = consumedTk;
			if (consume(LPAR)) {
				Symbol* fn = findSymbolInDomain(symTable, tkName->text);
				if (fn)tkerr("symbol redefinition: %s", tkName->text);
				fn = newSymbol(tkName->text, SK_FN);
				fn->type = t;
				addSymbolToDomain(symTable, fn);
				owner = fn;
				pushDomain();
				if (fnParam()) {
					for (;;) {
						if (consume(COMMA)) {
							if (fnParam());
						}
						else break;
					}
				}
				if (consume(RPAR)) {
					addInstr(&fn->fn.instr, OP_ENTER);
					if (stmCompound(false)) {
						fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
						if (fn->type.tb == TB_VOID)
							addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
						dropDomain();
						owner = NULL;
						return true;
					}
				}
				else tkerr(") missing on function def.");
			}
		}
		else tkerr("name of function is missing");
	}
	iTk = start;
	return false;
}


// unit: ( structDef | fnDef | varDef )* END
bool unit(){
	//printf("#unit %d\n", tkName(iTk->name));
	//printf("#unit\n");
	for(;;){
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break;
		}
	if(consume(END)){
		return true;
		}
	return false;
	}

void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
}