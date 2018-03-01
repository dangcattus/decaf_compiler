#include "decafexpr-defs.h"
#include <list>
#include <ostream>
#include <iostream>
#include <sstream>
#include <string>
#include <map>
#include <fstream>
#include <cstdlib>
#include <stdexcept>
#include <iterator>
#include <algorithm>
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"


#ifndef YYTOKENTYPE
#include "decafexpr.tab.h"
#endif

using namespace std;
using namespace llvm;

typedef enum { voidTy, intTy, boolTy, stringTy, } decafType;

typedef struct{
	int lineno;
	string type;
	string* reg_dest; // Register Destination
	llvm::AllocaInst* mem_loca; // Memory Address Location
	bool spilled; // Spilled "indicates if the value is to be found in a register or in memory"
	llvm::Value* Val = NULL;
} descriptor;

string TyString(decafType x) {
	switch (x) {
		case voidTy: return string("VoidType");
		case intTy: return string("IntType");
		case boolTy: return string("BoolType");
		case stringTy: return string("StringType");
		default: throw runtime_error("unknown type in TyString call");
	}
}

string TyString_cerr(decafType x) {
  switch (x) {
    case voidTy: return string("void");
    case intTy: return string("int");
    case boolTy: return string("bool");
    case stringTy: return string("string");
    default: throw runtime_error("unknown type in TyString call");
  }
}

string BinaryOpString(int Op) {
	switch (Op) {
		case T_PLUS: return string("Plus");
  		case T_MINUS: return string("Minus");
  		case T_MULT: return string("Mult");
  		case T_DIV: return string("Div");
  		case T_LEFTSHIFT: return string("Leftshift");
  		case T_RIGHTSHIFT: return string("Rightshift");
  		case T_MOD: return string("Mod");
  		case T_LT: return string("Lt");
  		case T_GT: return string("Gt");
  		case T_LEQ: return string("Leq");
  		case T_GEQ: return string("Geq");
  		case T_EQ: return string("Eq");
  		case T_NEQ: return string("Neq");
  		case T_AND: return string("And");
  		case T_OR: return string("Or");
		default: throw runtime_error("unknown type in BinaryOpString call");
	}
}

string UnaryOpString(int Op) {
	switch (Op) {
  		case T_MINUS: return string("UnaryMinus");
  		case T_NOT: return string("Not");
		default: throw runtime_error("unknown type in UnaryOpString call");
	}
}

string convertInt(int number) {
	stringstream ss;
	ss << number;
	return ss.str();
}

/// decafAST - Base class for all abstract syntax tree nodes.
class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
	virtual llvm::Value *Codegen() = 0;
};

string getString(decafAST *d) {
	if (d != NULL) {
		return d->str();
	} else {
		return string("None");
	}
}

string buildString1(const char *Name, decafAST *a) {
	return string(Name) + "(" + getString(a) + ")";
}

string buildString1(const char *Name, string a) {
	return string(Name) + "(" + a + ")";
}

string buildString2(const char *Name, decafAST *a, decafAST *b) {
	return string(Name) + "(" + getString(a) + "," + getString(b) + ")";
}

string buildString2(const char *Name, string a, decafAST *b) {
	return string(Name) + "(" + a + "," + getString(b) + ")";
}

string buildString2(const char *Name, string a, string b) {
	return string(Name) + "(" + a + "," + b + ")";
}

string buildString3(const char *Name, decafAST *a, decafAST *b, decafAST *c) {
	return string(Name) + "(" + getString(a) + "," + getString(b) + "," + getString(c) + ")";
}

string buildString3(const char *Name, string a, decafAST *b, decafAST *c) {
	return string(Name) + "(" + a + "," + getString(b) + "," + getString(c) + ")";
}

string buildString3(const char *Name, string a, string b, decafAST *c) {
	return string(Name) + "(" + a + "," + b + "," + getString(c) + ")";
}

string buildString3(const char *Name, string a, string b, string c) {
	return string(Name) + "(" + a + "," + b + "," + c + ")";
}

string buildString4(const char *Name, string a, decafAST *b, decafAST *c, decafAST *d) {
	return string(Name) + "(" + a + "," + getString(b) + "," + getString(c) + "," + getString(d) + ")";
}

string buildString4(const char *Name, string a, string b, decafAST *c, decafAST *d) {
	return string(Name) + "(" + a + "," + b + "," + getString(c) + "," + getString(d) + ")";
}

string buildString4(const char *Name, decafAST *a, decafAST *b, decafAST *c, decafAST *d) {
	return string(Name) + "(" + getString(a) + "," + getString(b) + "," + getString(c) + "," + getString(d) + ")";
}

template <class T>
string commaList(list<T> vec) {
	string s("");
	for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
		s = s + (s.empty() ? string("") : string(",")) + (*i)->str();
	}
	if (s.empty()) {
		s = string("None");
	}
	return s;
}

template <class T>
llvm::Value *listCodegen(list<T> vec) {
	llvm::Value *val = NULL;
	for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
		llvm::Value *j = (*i)->Codegen();
		if (j != NULL) { val = j; }
	}
	return val;
}

vector<Value*> AllArgs(list<decafAST*> vec){
	vector<Value*> AllArgs;
	for (typename list<decafAST*>::iterator i = vec.begin(); i != vec.end(); i++) {
		llvm::Value *j = (*i)->Codegen();
		if (j != NULL) { AllArgs.push_back(j); }
	}
	return AllArgs;
}

class TypedSymbol {
public:
	string Sym;
	decafType Ty;
	TypedSymbol(string s, decafType t) : Sym(s), Ty(t) {}
	string str() {
		if (Sym.empty()) {
			return "VarDef(" + TyString(Ty) + ")";
		} else {
			return "VarDef(" + Sym + "," + TyString(Ty) + ")";
		}
	}
	//virtual string getName();
};

class TypedSymbolListAST : public decafAST {
	list<class TypedSymbol *> arglist;
	decafType listType; // this variable is used if all the symbols in the list share the same type
public:
	TypedSymbolListAST() {}
	TypedSymbolListAST(string sym, decafType ty) {
		TypedSymbol *s = new TypedSymbol(sym, ty);
		arglist.push_front(s);
		listType = ty;
	}
	~TypedSymbolListAST() {
		for (list<class TypedSymbol *>::iterator i = arglist.begin(); i != arglist.end(); i++) {
			delete *i;
		}
	}
	void push_front(string sym, decafType ty) {
		TypedSymbol *s = new TypedSymbol(sym, ty);
		arglist.push_front(s);
	}
	void push_back(string sym, decafType ty) {
		TypedSymbol *s = new TypedSymbol(sym, ty);
		arglist.push_back(s);
	}
	void new_sym(string sym) {
		if (arglist.empty()) {
			throw runtime_error("Error in AST creation: insertion into empty typed symbol list\n");
		}
		TypedSymbol *s = new TypedSymbol(sym, listType);
		arglist.push_front(s);
	}
	void FillOutArgs(list<Value*> &NeededForFunctionArgs, vector<string> &FunctionArgsName) { // Passing by reference, hence return type is void
		for (typename list<class TypedSymbol *>::iterator i = arglist.begin(); i != arglist.end(); i++) {
			if(TyString((*i)->Ty)=="IntType"){
				NeededForFunctionArgs.push_back(ConstantInt::get(getGlobalContext(), APInt(32, 0)));
				FunctionArgsName.push_back((*i)->Sym);
			}
			else if(TyString((*i)->Ty)=="BoolType"){
				NeededForFunctionArgs.push_back(ConstantInt::get(getGlobalContext(), APInt(1, 0)));
				FunctionArgsName.push_back((*i)->Sym);
			}
		}
	}
	string retrieve_front(){
		TypedSymbol* temp = arglist.front();
		string retrieval = temp->Sym;
		return retrieval;
	}
	string retrieve_front_type(){
		TypedSymbol* temp = arglist.front();
		string retrieval = TyString(temp->Ty);
		return retrieval;
	}
	bool check_empty(){ return arglist.empty(); }
	virtual Value* Codegen();
	string str() { return commaList<class TypedSymbol *>(arglist); }
};

/// decafStmtList - List of Decaf statements
class decafStmtList : public decafAST {
	list<decafAST *> stmts;
public:
	decafStmtList() {}
	~decafStmtList() {
		for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) {
			delete *i;
		}
	}
	int size() { return stmts.size(); }
	void push_front(decafAST *e) { stmts.push_front(e); }
	void push_back(decafAST *e) { stmts.push_back(e); }
	vector<Value*> VectorOfArgs(){
		return AllArgs(stmts);
	}
	virtual llvm::Value *Codegen() {
		return listCodegen<decafAST *>(stmts);
	}
	string str() { return commaList<class decafAST *>(stmts); }
};

/// NumberExprAST - Expression class for integer numeric literals like "12".
class NumberExprAST : public decafAST {
	int Val;
public:
	NumberExprAST(int val) : Val(val) {}
	virtual Value* Codegen();
	string str() { return buildString1("NumberExpr", convertInt(Val)); }
};

/// StringConstAST - string constant
class StringConstAST : public decafAST {
	string StringConst;
public:
	StringConstAST(string s) : StringConst(s) {}
	virtual Value* Codegen();
	string str() { return buildString1("StringConstant", "\"" + StringConst + "\""); }
};

/// BoolExprAST - Expression class for boolean literals: "true" and "false".
class BoolExprAST : public decafAST {
	bool Val;
public:
	BoolExprAST(bool val) : Val(val) {}
	virtual Value* Codegen();
	string str() { return buildString1("BoolExpr", Val ? string("True") : string("False")); }
};

/// VariableExprAST - Expression class for variables like "a".
class VariableExprAST : public decafAST {
	string Name;
public:
	VariableExprAST(string name) : Name(name) {}
	virtual Value* Codegen();
	string str() { return buildString1("VariableExpr", Name); }
	//const std::string &getName() const { return Name; }
};

/// MethodCallAST - call a function with some arguments
class MethodCallAST : public decafAST {
	string Name;
	decafStmtList *Args;
public:
	MethodCallAST(string name, decafStmtList *args) : Name(name), Args(args) {}
	~MethodCallAST() { delete Args; }
	virtual Value* Codegen();
	decafStmtList* returnArgs(){
		return Args;
	}
	string str() { return buildString2("MethodCall", Name, Args); }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public decafAST {
	int Op; // use the token value of the operator
	decafAST *LHS, *RHS;
public:
	BinaryExprAST(int op, decafAST *lhs, decafAST *rhs) : Op(op), LHS(lhs), RHS(rhs) { }
	~BinaryExprAST() { delete LHS; delete RHS; }
	virtual Value* Codegen();
	string str() { return buildString3("BinaryExpr", BinaryOpString(Op), LHS, RHS); }
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public decafAST {
	int Op; // use the token value of the operator
	decafAST *Expr;
public:
	UnaryExprAST(int op, decafAST *expr) : Op(op), Expr(expr) {}
	~UnaryExprAST() { delete Expr; }
	virtual Value* Codegen();

	string str() { return buildString2("UnaryExpr", UnaryOpString(Op), Expr); }
};

/// AssignVarAST - assign value to a variable
class AssignVarAST : public decafAST {
	string Name; // location to assign value
	decafAST *Expr;
	llvm::Value* Val;
public:
	AssignVarAST(string name, decafAST *expr) : Name(name), Expr(expr) {}
	AssignVarAST(string name, llvm::Value* val) : Name(name), Val(val) {}
	~AssignVarAST() {
		if (Expr != NULL) { delete Expr; }
	}
	virtual llvm::Value* Codegen();
	void setValue(llvm::Value* V){
		Val = V;
	}
	string str() { return buildString2("AssignVar", Name, Expr); }
};

/// AssignArrayLocAST - assign value to a variable
class AssignArrayLocAST : public decafAST {
	string Name; // name of array variable
    decafAST *Index;  // index for assignment of value
	decafAST *Value;
public:
	AssignArrayLocAST(string name, decafAST *index, decafAST *value) : Name(name), Index(index), Value(value) {}
	~AssignArrayLocAST() { delete Index; delete Value; }
	virtual llvm::Value* Codegen();
	string str() { return buildString3("AssignArrayLoc", Name, Index, Value); }
};

/// ArrayLocExprAST - access an array location
class ArrayLocExprAST : public decafAST {
	string Name;
    decafAST *Expr;
public:
	ArrayLocExprAST(string name, decafAST *expr) : Name(name), Expr(expr) {}
	~ArrayLocExprAST() {
		if (Expr != NULL) { delete Expr; }
	}
	virtual Value* Codegen();
	string str() { return buildString2("ArrayLocExpr", Name, Expr); }
};

/// BlockAST - block
class BlockAST : public decafAST {
	decafStmtList *Vars;
	decafStmtList *Statements;
public:
	BlockAST(decafStmtList *vars, decafStmtList *s) : Vars(vars), Statements(s) {}
	~BlockAST() {
		if (Vars != NULL) { delete Vars; }
		if (Statements != NULL) { delete Statements; }
	}
	virtual Value* Codegen();
	decafStmtList *getVars() { return Vars; }
	decafStmtList *getStatements() { return Statements; }
	string str() { return buildString2("Block", Vars, Statements); }
};

/// MethodBlockAST - block for methods
class MethodBlockAST : public decafAST {
	decafStmtList *Vars;
	decafStmtList *Statements;
public:
	MethodBlockAST(decafStmtList *vars, decafStmtList *s) : Vars(vars), Statements(s) {}
	~MethodBlockAST() {
		if (Vars != NULL) { delete Vars; }
		if (Statements != NULL) { delete Statements; }
	}
	virtual Value* Codegen();
	string str() { return buildString2("MethodBlock", Vars, Statements); }
};

/// IfStmtAST - if statement
class IfStmtAST : public decafAST {
	decafAST *Cond;
	BlockAST *IfTrueBlock;
	BlockAST *ElseBlock;
public:
	IfStmtAST(decafAST *cond, BlockAST *iftrue, BlockAST *elseblock) : Cond(cond), IfTrueBlock(iftrue), ElseBlock(elseblock) {}
	~IfStmtAST() {
		delete Cond;
		delete IfTrueBlock;
		if (ElseBlock != NULL) { delete ElseBlock; }
	}
	virtual Value* Codegen();
	string str() { return buildString3("IfStmt", Cond, IfTrueBlock, ElseBlock); }
};

/// WhileStmtAST - while statement
class WhileStmtAST : public decafAST {
	decafAST *Cond;
	BlockAST *Body;
public:
	WhileStmtAST(decafAST *cond, BlockAST *body) : Cond(cond), Body(body) {}
	~WhileStmtAST() { delete Cond; delete Body; }
	virtual Value* Codegen();
	string str() { return buildString2("WhileStmt", Cond, Body); }
};

/// ForStmtAST - for statement
class ForStmtAST : public decafAST {
	decafStmtList *InitList;
	decafAST *Cond;
	decafStmtList *LoopEndList;
	BlockAST *Body;
public:
	ForStmtAST(decafStmtList *init, decafAST *cond, decafStmtList *end, BlockAST *body) :
		InitList(init), Cond(cond), LoopEndList(end), Body(body) {}
	~ForStmtAST() {
		delete InitList;
		delete Cond;
		delete LoopEndList;
		delete Body;
	}
	virtual Value* Codegen();
	string str() { return buildString4("ForStmt", InitList, Cond, LoopEndList, Body); }
};

/// ReturnStmtAST - return statement
class ReturnStmtAST : public decafAST {
	decafAST *Value;
public:
	ReturnStmtAST(decafAST *value) : Value(value) {}
	~ReturnStmtAST() {
		if (Value != NULL) { delete Value; }
	}
	virtual llvm::Value* Codegen();
	string str() { return buildString1("ReturnStmt", Value); }
};

/// BreakStmtAST - break statement
class BreakStmtAST : public decafAST {
public:
	BreakStmtAST() {}
	virtual Value* Codegen();
	string str() { return string("BreakStmt"); }
};

/// ContinueStmtAST - continue statement
class ContinueStmtAST : public decafAST {
public:
	ContinueStmtAST() {}
	virtual Value* Codegen();
	string str() { return string("ContinueStmt"); }
};

/// MethodDeclAST - function definition
class MethodDeclAST : public decafAST {
	decafType ReturnType;
	string Name;
	TypedSymbolListAST *FunctionArgs;
	MethodBlockAST *Block;
public:
	MethodDeclAST(decafType rtype, string name, TypedSymbolListAST *fargs, MethodBlockAST *block)
		: ReturnType(rtype), Name(name), FunctionArgs(fargs), Block(block) {}
	~MethodDeclAST() {
		delete FunctionArgs;
		delete Block;
	}
	virtual Value* Codegen();
	string getName() {
		return Name;
	}
	string str() { return buildString4("Method", Name, TyString(ReturnType), FunctionArgs, Block); }
};

/// AssignGlobalVarAST - assign value to a global variable
class AssignGlobalVarAST : public decafAST {
	decafType Ty;
	string Name; // location to assign value
	decafAST *Value;
public:
	AssignGlobalVarAST(decafType ty, string name, decafAST *value) : Ty(ty), Name(name), Value(value) {}
	~AssignGlobalVarAST() {
		if (Value != NULL) { delete Value; }
	}
	virtual llvm::Value* Codegen();
	string str() { return buildString3("AssignGlobalVar", Name, TyString(Ty), Value); }
};

/// FieldDecl - field declaration aka Decaf global variable
class FieldDecl : public decafAST {
	string Name;
	decafType Ty;
	int Size; // -1 for scalars and size value for arrays, size 0 array is an error
public:
	FieldDecl(string name, decafType ty, int size) : Name(name), Ty(ty), Size(size) {}
	virtual llvm::Value* Codegen();
	string str() { return buildString3("FieldDecl", Name, TyString(Ty), (Size == -1) ? "Scalar" : "Array(" + convertInt(Size) + ")"); }
};

class FieldDeclListAST : public decafAST {
	list<class decafAST *> arglist;
	decafType listType; // this variable is used if all the symbols in the list share the same type
	int size; // this variable is used if all the symbols in the list share the same type, -1 for scalar, array size otherwise
public:
	FieldDeclListAST() {}
	FieldDeclListAST(string sym, decafType ty, int sz) {
		FieldDecl *s = new FieldDecl(sym, ty, sz);
		arglist.push_front(s);
		listType = ty;
		size = sz;
	}
	~FieldDeclListAST() {
		for (list<class decafAST *>::iterator i = arglist.begin(); i != arglist.end(); i++) {
			delete *i;
		}
	}
	void push_front(string sym, decafType ty, int sz) {
		FieldDecl *s = new FieldDecl(sym, ty, sz);
		arglist.push_front(s);
	}
	void push_back(string sym, decafType ty, int sz) {
		FieldDecl *s = new FieldDecl(sym, ty, sz);
		arglist.push_back(s);
	}
	void new_sym(string sym) {
		if (arglist.empty()) {
			throw runtime_error("Error in AST creation: insertion into empty field list\n");
		}
		FieldDecl *s = new FieldDecl(sym, listType, size);
		arglist.push_front(s);
	}
	void new_sym(string sym, int sz) {
		if (arglist.empty()) {
			throw runtime_error("Error in AST creation: insertion into empty field list\n");
		}
		FieldDecl *s = new FieldDecl(sym, listType, sz);
		arglist.push_back(s);
	}
	virtual Value* Codegen();
	string str() { return commaList<class decafAST *>(arglist); }
};

class PackageAST : public decafAST {
	string Name;
	FieldDeclListAST *FieldDeclList;
	decafStmtList *MethodDeclList;
public:
	PackageAST(string name, FieldDeclListAST *fieldlist, decafStmtList *methodlist)
		: Name(name), FieldDeclList(fieldlist), MethodDeclList(methodlist) {}
	~PackageAST() {
		if (FieldDeclList != NULL) { delete FieldDeclList; }
		if (MethodDeclList != NULL) { delete MethodDeclList; }
	}
	virtual Value* Codegen();
	string str() { return buildString3("Package", Name, FieldDeclList, MethodDeclList); }
};

/// ExternAST - extern function definition
class ExternAST : public decafAST {
	decafType ReturnType;
	string Name;
	TypedSymbolListAST *FunctionArgs;
public:
	ExternAST(decafType r, string n, TypedSymbolListAST *fargs) : ReturnType(r), Name(n), FunctionArgs(fargs) {}
	~ExternAST() {
		if (FunctionArgs != NULL) { delete FunctionArgs; }
	}
	virtual Value* Codegen();
	string str() { return buildString3("ExternFunction", Name, TyString(ReturnType), FunctionArgs); }
};

/// ProgramAST - the decaf program
class ProgramAST : public decafAST {
	decafStmtList *ExternList;
	PackageAST *ClassDef;
public:
	ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), ClassDef(c) {}
	~ProgramAST() {
		if (ExternList != NULL) { delete ExternList; }
		if (ClassDef != NULL) { delete ClassDef; }
	}
	virtual Value* Codegen();
	string str() { return buildString2("Program", ExternList, ClassDef); }
};

class symboltable{
	public:
		symboltable() {
		}

		void new_symtbl() {
			symbol_table *new_symtbl = new symbol_table();
			symtbl.push_front(new_symtbl);
		}

		void pop_symtbl() {
			if (symtbl.empty())
				throw runtime_error("no symbol table to remove here!");
			symtbl.pop_front();
		}

		void remove_symtbl() {
			symbol_table *tbl;
			if (symtbl.empty())
				throw runtime_error("no symbol table to remove here!");
			else
				tbl = symtbl.front();
		tbl->clear();
		delete(tbl);
			symtbl.pop_front();
		}

		void enter_symtbl(string ident, descriptor *d) {
			symbol_table *tbl;
			symbol_table::iterator find_ident;

			if (symtbl.empty())
				throw runtime_error("no symbol table created yet!");

			tbl = symtbl.front();
			if ((find_ident = tbl->find(ident)) != tbl->end()) {
				if(SymTblErr){ cerr << "Warning: redefining previously defined identifier: " << ident << endl; }
				delete(find_ident->second);
				tbl->erase(ident);
			}
			(*tbl)[ident] = d;
		}

		descriptor* access_symtbl(string ident) {
			for (symbol_table_list::iterator i = symtbl.begin(); i != symtbl.end(); ++i) {
				symbol_table::iterator find_ident;
				if ((find_ident = (*i)->find(ident)) != (*i)->end()) return find_ident->second;
			}
			return NULL;
		}

	private:
		typedef map<string, descriptor* > symbol_table;
		typedef list<symbol_table* > symbol_table_list;
		symbol_table_list symtbl;
};
