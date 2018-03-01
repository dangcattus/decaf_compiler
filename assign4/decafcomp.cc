#include "decafcomp-defs.h"
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
#include "decafcomp.tab.h"
#endif

using namespace std;
using namespace llvm;

typedef enum { voidTy, intTy, boolTy, stringTy, } decafType;

typedef struct{
	int lineno;
	string type;
	string* reg_dest; // Register Destination
	llvm::AllocaInst* mem_loca = NULL; // Memory Address Location
	llvm::GlobalVariable* globvar_mem_loca = NULL;
	llvm::ArrayType* array_type = NULL;
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
	vector<Value*> Args;
	for (typename list<decafAST*>::iterator i = vec.begin(); i != vec.end(); i++) {
		llvm::Value *j = (*i)->Codegen();
		if (j != NULL) { Args.push_back(j); }
	}
	return Args;
}

class VarDefAST {
public:
	string Name;
	string VarType;
	VarDefAST( string name, string methodvartype ) : Name(name), VarType(methodvartype) {}
	string str() {return string("VarDef") + "(" + Name + "," + VarType + ")" ;}
	string getType() {return VarType; }
};

class MethodVarListAST : public decafAST {
  list <class VarDefAST *> MethodVars;
  string MethodVarType;

public:
	MethodVarListAST(){}
  MethodVarListAST(string name, string type){
    MethodVarType = type;
    VarDefAST *temp = new VarDefAST(name, type);
    MethodVars.push_front(temp);
  }
  void push_back(string name, string type){
    VarDefAST *temp = new VarDefAST(name, type);
    MethodVars.push_back(temp);
  }
  void push_front(string name, string type){
    VarDefAST *temp = new VarDefAST(name, type);
    MethodVars.push_front(temp);
  }
  void addVar(string name){
    VarDefAST *temp = new VarDefAST(name, MethodVarType);
    MethodVars.push_front(temp);
  }
  void addVar (string name, string type){
		if (MethodVars.empty()) {
			throw runtime_error("Error in AST creation: insertion into empty \n");
		}
    VarDefAST *temp = new VarDefAST(name, type);
    MethodVars.push_front(temp);
  }
	void FillOutArgs(list<Value*> &ArgType, vector<string> &ArgName) { // Passing by reference, hence return type is void
		for (typename list<class VarDefAST *>::iterator i = MethodVars.begin(); i != MethodVars.end(); i++) {
			string temp = (*i)->VarType;
			if( temp == "IntType"){
				ArgType.push_back(ConstantInt::get(getGlobalContext(), APInt(32, 0)));
				if (Maxx) cout << "Fill out Args (method), push_back " << (*i)->Name << " IntType" << endl;
				ArgName.push_back((*i)->Name);
			}
			else if( temp == "BoolType"){
				ArgType.push_back(ConstantInt::get(getGlobalContext(), APInt(1, 0)));
				if (Maxx) cout << "Fill out Args (method), push_back " + (*i)->Name + " BoolType" << endl;
				ArgName.push_back((*i)->Name);
			}
		}
	}

VarDefAST* getFront(){
	return MethodVars.front();
}
	string retrieve_front(){
		VarDefAST* temp = MethodVars.front();
		string retrieval = temp->Name;
		return retrieval;
	}
	string retrieve_front_type(){
		VarDefAST* temp = MethodVars.front();
		string retrieval = temp->VarType;
		return retrieval;
	}
	bool check_empty(){ return MethodVars.empty(); }
	virtual Value* Codegen();
  string str() { return commaList<class VarDefAST *> (MethodVars);}
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

class NumberExprAST : public decafAST {
	int Value;
	public:
	NumberExprAST( int value ) : Value(value) {}
  ~NumberExprAST() {}
	string str() { return string("NumberExpr") + "(" + to_string(Value) + ")"; }
	virtual llvm::Value* Codegen();
};

class StringConstantAST : public decafAST{
	string StringConstant;
	public:
	StringConstantAST(string stringconstant) : StringConstant(stringconstant) {}
	virtual Value* Codegen();
	string str() { return string("StringConstant") + "(" + StringConstant + ")"; }
};

class BoolExprAST : public decafAST{
	string BoolExpr;
  int Val;

	public:
	BoolExprAST( string boolexpr ) : BoolExpr(boolexpr) {}
	BoolExprAST(int boolnum) : Val(boolnum){}
	virtual Value* Codegen();
	string str() { return string("BoolExpr") + "(" + BoolExpr + ")"; }
};

class VariableExprAST : public decafAST {
	string Value;
	public:
	VariableExprAST( string value ) : Value(value) {}
  ~VariableExprAST() {}
	virtual llvm::Value* Codegen();
	string str() { return string ("VariableExpr") + "(" + Value + ")"; }

};

class MethodCallAST : public decafAST{
	string MethodName;
	decafStmtList* Expression;
	public:
	MethodCallAST(string methodname, decafStmtList* expression) : MethodName(methodname), Expression(expression) {}
	~MethodCallAST(){
		if (Expression != NULL){
				delete Expression;
		}
	}
	virtual Value* Codegen();
	decafStmtList* returnArgs(){
		return Expression;
	}
	string str() { return string("MethodCall") + "(" + MethodName + "," + getString(Expression) + ")"; }
};

class BinaryExprAST : public decafAST {
	int Operator;
	decafAST* Expression1;
	decafAST* Expression2;
	public:
	BinaryExprAST( int op, decafAST* expression1, decafAST* expression2 ) : Operator(op), Expression1(expression1), Expression2(expression2) {}
  ~BinaryExprAST(){}
	virtual Value* Codegen();
  string str() { return string("BinaryExpr") + "(" + BinaryOpString(Operator) + "," + getString(Expression1) + "," + getString(Expression2) + ")"; }
};

class UnaryExprAST : public decafAST{
	int UnaryType;
	decafAST* Argument; // May need to change type
	public:
	UnaryExprAST(int unarytype, decafAST* argument) : UnaryType(unarytype), Argument(argument) {}
	virtual Value* Codegen();
	string str() { return string("UnaryExpr") + "(" + UnaryOpString(UnaryType) + "," + getString(Argument) + ")"; }
};

class AssignVarAST : public decafAST {
	string VarName;
	decafAST *Expression;
	llvm::Value* Val;
	public:
	AssignVarAST(string varname, decafAST *expression) : VarName(varname), Expression(expression) {}
	~AssignVarAST() {
		if (Expression != NULL) { delete Expression; }
	}
	virtual llvm::Value* Codegen();
	void setValue(llvm::Value* V){
		Val = V;
	}
	string RetVarName(){ return VarName; }
	string str() { return string("AssignVar") + "(" + VarName + "," + getString(Expression) + ")"; }
};

class AssignArrayLocAST : public decafAST {
    string ArrayName;
    decafAST *Index;
    decafAST *Value;

  public:
    AssignArrayLocAST(string name, decafAST* i, decafAST* val) : ArrayName(name), Index(i), Value(val){}
    ~AssignArrayLocAST(){}
		virtual llvm::Value* Codegen();
    string str(){ return string("AssignArrayLoc") + "(" + ArrayName + "," + getString(Index) + "," + getString(Value) +  ")";
    }
};

class ArrayLocExprAST : public decafAST {
    string Name;
    decafAST* Index;
  public:
    ArrayLocExprAST(string name, decafAST* i) : Name(name), Index(i) {}
    ~ArrayLocExprAST(){}
		virtual llvm::Value* Codegen();
    string str() {
      return string("ArrayLocExpr") + "(" + Name + "," + getString(Index) + ")";
    }
};

class BlockAST : public decafAST {
    decafStmtList *BlockVarDefs;
    decafStmtList *BlockStatementList;
public :
    BlockAST( decafStmtList *blockvardefs, decafStmtList *blockstatementlist ) : BlockVarDefs(blockvardefs), BlockStatementList(blockstatementlist) {}
		virtual Value* Codegen();
    string str() {return string("Block") + "(" + getString(BlockVarDefs) + "," + getString(BlockStatementList) + ")";}
};

class MethodBlockAST : public decafAST {
    decafStmtList *MethodBlockVarDefs;
    decafStmtList *MethodBlockStatementList;
public :
    MethodBlockAST( decafStmtList *methodblockvardefs, decafStmtList *methodblockstatementlist ) : MethodBlockVarDefs(methodblockvardefs), MethodBlockStatementList(methodblockstatementlist) {}
		virtual Value* Codegen();
    string str() {return string("MethodBlock") + "(" + getString(MethodBlockVarDefs) + "," + getString(MethodBlockStatementList) + ")";}
};

class IfStmtAST : public decafAST{
  decafAST* IfCondition;
	BlockAST* IfStmt;
  BlockAST* ElseStmt;
	public:
	IfStmtAST( decafAST* ifcond, BlockAST* blockexpr, BlockAST* elsestmt ) : IfCondition(ifcond), IfStmt(blockexpr), ElseStmt(elsestmt) {}
  IfStmtAST(decafAST* ifcond, BlockAST* blockexpr) : IfCondition(ifcond), IfStmt(blockexpr){
    ElseStmt = NULL;
  }
	virtual Value* Codegen();
  string str() { return string("IfStmt") + "(" + getString(IfCondition) + "," + getString(IfStmt) + "," + getString(ElseStmt)+ ")" ;}
};

class WhileStmtAST : public decafAST{
	decafAST* BinaryExpr;
	BlockAST* BlockExpr;
	public:
	WhileStmtAST(decafAST* binaryexpr, BlockAST* blockexpr) : BinaryExpr(binaryexpr), BlockExpr(blockexpr) {}
	virtual Value* Codegen();
	string str() { return string("WhileStmt") + "(" + getString(BinaryExpr) + "," + getString(BlockExpr) + ")"; }
};

class ForStmtAST : public decafAST{
	AssignVarAST *AssignVar1;
	decafAST *BinaryExpr;
	decafAST* AssignVar2;
	BlockAST* BlockExpr;
	public:
	ForStmtAST( AssignVarAST* assignvar1, BinaryExprAST* binaryexpr, AssignVarAST* assignvar2, BlockAST* blockexpr ) : AssignVar1(assignvar1), BinaryExpr(binaryexpr), AssignVar2(assignvar2), BlockExpr(blockexpr) {}
	virtual Value* Codegen();
	string str() { return string("ForStmt") + "(" + getString(AssignVar1) + "," + getString(BinaryExpr) + "," + getString(AssignVar2) + "," + getString(BlockExpr) + ")" ;}
};

class ReturnStmtAST : public decafAST{
	decafAST* Expression;
	public:
	ReturnStmtAST(decafAST* expression) : Expression(expression) {}
	~ReturnStmtAST() {
		if (Expression != NULL) { delete Expression; }
	}
	virtual llvm::Value* Codegen();
	string str() { return string("ReturnStmt") + "(" + getString(Expression) + ")"; }
};

class BreakStmtAST : public decafAST{
	string BreakStmt;
	public:
	BreakStmtAST(string breakstmt) : BreakStmt(breakstmt) {}
	virtual llvm::Value* Codegen();
	string str() { return BreakStmt; }
};

class ContinueStmtAST : public decafAST{
	string ContinueStmt;
	public:
	ContinueStmtAST(string continuestmt) : ContinueStmt(continuestmt) {}
	virtual llvm::Value* Codegen();
	string str() { return ContinueStmt; }
};

class MethodDeclAST: public decafAST {
  string Name;
  string methodType;
	MethodVarListAST *MethodVarsList;
  MethodBlockAST *MBlock;
public:
  MethodDeclAST( string name, string mtype, MethodVarListAST *methodvarslist, MethodBlockAST* mblock ) : Name(name), methodType(mtype), MethodVarsList(methodvarslist), MBlock(mblock) {}
	virtual Value* Codegen();
  string str() {return string("Method") + "(" + Name + "," + methodType + "," + getString(MethodVarsList) + "," + getString(MBlock) + ")"; }
};

class AssignGlobalVarAST : public decafAST {
  string Name;
  string Type;
  decafAST* ExprValue;
	llvm::Value* Val;
public:
  AssignGlobalVarAST(string name, string type, decafAST* val) : Name(name), Type(type), ExprValue(val){}
  ~AssignGlobalVarAST() {
		if (ExprValue != NULL) { delete ExprValue; }
	}
	virtual llvm::Value* Codegen();
	void setValue(llvm::Value* V){
		Val = V;
	}
  string str() {return string("AssignGlobalVar") + "(" + Name + "," + Type + "," + getString(ExprValue) + ")";}
};

class FieldDeclAST : public decafAST {
  string Name;
	string VarType;
  int Size;

public:
	FieldDeclAST(string name, string vartype, int size) : Name(name), VarType(vartype), Size(size) {}
  ~FieldDeclAST(){}
  string str() {
    string s;
    if (Size == -1){
      s = "Scalar";
    }
    else{
      s = "Array(" + to_string(Size) + ")";;
    }
    return string("FieldDecl") + "(" + Name + "," + VarType + "," + s + ")";
   }
	 virtual llvm::Value* Codegen();
 };

 class IdList : public decafAST {
    list<class decafAST *> Vars;
    string Type;
    int Size;

  public:
    IdList(){}
    IdList(string name, string type, int size){
      Type = type;
      Size = size;
      FieldDeclAST *temp = new FieldDeclAST(name, Type, Size);
      Vars.push_front(temp);
    }
    ~IdList(){}

    void push_front(string name, string type, int size){
      FieldDeclAST *temp = new FieldDeclAST(name, type, size);
      Vars.push_front(temp);
    }
    void push_back(string name, string type, int size){
      FieldDeclAST *temp = new FieldDeclAST(name, type, size);
      Vars.push_back(temp);
    }
    void addVar(string name) {
      FieldDeclAST *temp = new FieldDeclAST(name, Type, Size);
      Vars.push_front(temp);
    }
		virtual llvm::Value* Codegen();
    string str(){ return commaList<class decafAST *>(Vars);}
};

class PackageAST : public decafAST {
	string Name;
	IdList *FieldDeclList;
	decafStmtList *MethodDeclList;
public:
	PackageAST(string name, IdList *fieldlist, decafStmtList *methodlist) : Name(name), FieldDeclList(fieldlist), MethodDeclList(methodlist) {}
	~PackageAST() {
		if (FieldDeclList != NULL) { delete FieldDeclList; }
		if (MethodDeclList != NULL) { delete MethodDeclList; }
	}
	virtual Value* Codegen();
	string str() {
			return string("Package") + "(" + Name + "," + getString(FieldDeclList) + "," + getString(MethodDeclList) + ")";
		}
};

class ExternFunctionAST : public decafAST {
	string VariableName;
	string Type;
	MethodVarListAST* VarDefType;
	public:
	ExternFunctionAST(string variablename, string type, MethodVarListAST* vardeftype) : VariableName(variablename), Type(type), VarDefType(vardeftype) {}
	~ExternFunctionAST() {}
	virtual Value* Codegen();
	string str() { return string("ExternFunction") + "(" + VariableName + "," + Type + "," + getString(VarDefType) + ")"; }
};

class ProgramAST : public decafAST {
	decafStmtList *ExternList;
	PackageAST *PackageDef;
public:
	ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), PackageDef(c) {}
	~ProgramAST() {
		if (ExternList != NULL) { delete ExternList; }
		if (PackageDef != NULL) { delete PackageDef; }
	}
	virtual Value* Codegen();
	string str() { return string("Program") + "(" + getString(ExternList) + "," + getString(PackageDef) + ")"; }
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
