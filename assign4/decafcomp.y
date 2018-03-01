%{
#include <iostream>
#include <ostream>
#include <string>
#include <cstdlib>
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "decafcomp-defs.h"

int yylex(void);
int yyerror(char *);

using namespace std;
using namespace llvm;

// print AST?
bool printAST = false;
// print to StdErr SymTbl Errors?
bool SymTblErr = false;
bool Maxx = false;
int tablenumber = 1;

#include "decafcomp.cc"

symboltable* symtable = new symboltable();
symboltable* globalsymtable = new symboltable();
static std::map<std::string, llvm::Function*> FunctionProtos;
Function* CurrentFunction;
MethodDeclAST* SavingMain = NULL;
// this global variable contains all the generated code
static llvm::Module *TheModule;

// this is the method used to construct the LLVM intermediate code (IR)
static llvm::IRBuilder<> Builder(llvm::getGlobalContext());
// the calls to getGlobalContext() in the init above and in the
// following code ensures that we are incrementally generating
// instructions in the right order

void errstr(string s){
  if (Maxx){
    cout << s << " " << lineno << endl;
  }
}

void setFuncArgs(llvm::Function *f, vector<string> funargs){
  llvm::Function::arg_iterator I,J;
  unsigned Idx = 0;
  for (I = f->arg_begin(), J = f->arg_end(); I != J; ++I, ++Idx){
    I->setName(funargs[Idx]);
  }
}

static llvm::AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, string VarName){
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Type::getDoubleTy(getGlobalContext()), 0, VarName);
}

/*void createArgAlloca(llvm::Function* F, string arg){
  //llvm::Function::arg_iterator AI = F->arg_begin();
//  for (unsigned Idx = 0, e = Args.size(); Idx != e; ++Idx, ++AI){
    //Create alloca for this variable
    AllocaInst *Alloca = CreateEntryBlockAlloca(F, arg);
    //Store the initial Value into the Alloca
    Builder.CreateStore(AI, Alloca);
    //add args to variable symboltable
    //NamedValues[Args[Idx]] = Alloca;
    descriptor *temp = new descriptor;
  //  llvm::AllocaInst *Alloca = Builder.CreateAlloca(llvmTy, 0, id);
    temp->lineno = lineno;
    temp->mem_loca = Alloca;
    symtable->enter_symtbl(id, temp);
}
*/

llvm::AllocaInst *defineVariable(llvm::Type *llvmTy, string id){
  descriptor *temp = new descriptor;
  llvm::AllocaInst *Alloca = Builder.CreateAlloca(llvmTy, 0, id);
  temp->lineno = lineno;
  temp->mem_loca = Alloca;
  symtable->enter_symtbl(id, temp);
  return Alloca;
}


llvm::Type* getLLVMType(string s){
  llvm::Type *ft;
  if (s == "VoidType"){
    errstr("get_arg_type VoidType");
    ft =  Builder.getVoidTy();
    return ft;
  }
  else if (s == "IntType") {
    errstr("get_arg_type IntType");
    ft =  Builder.getInt32Ty();
return ft;
  }
  else if (s == "BoolType") {
    errstr("get_arg_type BoolType");
    ft =  Builder.getInt1Ty();
return ft;
  }
  else if (s == "StringType") {
    errstr("get_arg_type StringType");
    ft =  Builder.getInt8PtrTy();
    return ft;
  }
  else {
    throw runtime_error("Type is not recognized");
  }
}
llvm::FunctionType* get_function_type(string dt, vector<Type*> args){
  //string s = TyString(dt);
  FunctionType *ft;
  if (dt == "VoidType"){
    ft =  llvm::FunctionType::get(Type::getVoidTy(getGlobalContext()), args, false);
    return ft;
  }
  else if (dt == "IntType") {
    ft =  llvm::FunctionType::get(Type::getInt32Ty(getGlobalContext()), args, false);
return ft;
  }
  else if (dt == "BoolType") {
    ft =  llvm::FunctionType::get(Type::getInt1Ty(getGlobalContext()), args, false);
return ft;
  }
  else if (dt == "StringType") {
    ft =  llvm::FunctionType::get(Type::getInt8PtrTy(getGlobalContext()), args, false);
    return ft;
  }
  else {
    throw runtime_error("empty function type");
  }
}

Function *gen_read_int_def() {
  // create a extern definition for print_int
  FunctionType *read_int_type = FunctionType::get(IntegerType::get(getGlobalContext(), 32), false);
  return Function::Create(read_int_type, Function::ExternalLinkage, "read_int", TheModule);
}

Function *gen_print_int_def() {
  // create a extern definition for print_int
  std::vector<Type*> args;
  args.push_back(IntegerType::get(getGlobalContext(), 32)); // print_int takes one integer argument
  FunctionType *print_int_type = FunctionType::get(IntegerType::get(getGlobalContext(), 32), args, false);
  return Function::Create(print_int_type, Function::ExternalLinkage, "print_int", TheModule);
}

Function *gen_print_string_def(){
	std::vector<Type*> args;
  args.push_back(PointerType::getUnqual(IntegerType::get(getGlobalContext(), 8))); // print_string takes one string argument
  FunctionType *print_int_type = FunctionType::get(Type::getVoidTy(getGlobalContext()), args, false);
  return Function::Create(print_int_type, Function::ExternalLinkage, "print_string", TheModule);
}

llvm::Function *gen_main_def() {
  // create the top-level definition for main
  errstr("gen_main_def");
  llvm::FunctionType *FT = llvm::FunctionType::get(llvm::IntegerType::get(llvm::getGlobalContext(), 32), false);
  llvm::Function *TheFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", TheModule);
  if (TheFunction == 0) {
    throw runtime_error("empty function block");
  }
  // Create a new basic block which contains a sequence of LLVM instructions
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", TheFunction);
  // All subsequent calls to IRBuilder will place instructions in this location
  Builder.SetInsertPoint(BB);
  Builder.CreateRet(ConstantInt::get(getGlobalContext(), APInt(32, 1)));
  Builder.SetInsertPoint(BB, TheFunction->getEntryBlock().begin());
	CurrentFunction = TheFunction;
  return TheFunction;
}

llvm::Function *gen_function_def(list<Value*> &FunctionArgs, string FunctionName, string FunctionType, vector<string> &FunctionArgsName, MethodBlockAST* mblock){
  llvm::FunctionType *FT;
	vector<Type*> FunctionArgTypes;
  vector<Value*> FuncVals;
  int i = 0;
  errstr("gen_function_def : " + FunctionName);
	while( !FunctionArgs.empty() ){
    //changing all function args to back temp
		FunctionArgTypes.push_back(FunctionArgs.front()->getType());
    FuncVals.push_back(FunctionArgs.front());
    //not certain if we need to do the alloca here
    AllocaInst* Alloca;
  //  descriptor *temp = new descriptor;
    llvm::Type* tempType = FunctionArgs.front()->getType();
    //Alloca = defineVariable(tempType, FunctionArgsName[i]);
    errstr("Gen_function_def: Put Symbol " + FunctionArgsName[i] + " in table");
    FunctionArgs.pop_front();
    i++;
	}
  FT = get_function_type(FunctionType, FunctionArgTypes);
/*FUNCTION DECLARATION*/
  llvm::Function *TheFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, FunctionName, TheModule);
  if (TheFunction->getName() != FunctionName){
    TheFunction->eraseFromParent();
    TheFunction = TheModule->getFunction(FunctionName);
    if (!TheFunction->empty()){
      throw runtime_error("ERROR: redefinition of function");
      //return 0;
    }
  }
  if (TheFunction == 0) {
    throw runtime_error("empty function block");
  }
  setFuncArgs(TheFunction, FunctionArgsName);
  // Create a new basic block which contains a sequence of LLVM instructions
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", TheFunction);
  // All subsequent calls to IRBuilder will place instructions in this location
  Builder.SetInsertPoint(BB);
  for (unsigned i = 0; i < FuncVals.size(); ++i){

  }
  //FROM LECTURE NOTES
  llvm::BasicBlock *CurBB = Builder.GetInsertBlock();
  //gives you a link to the current basic block
  llvm::Function *func = Builder.GetInsertBlock()->getParent();
  //gives pointer to the function deintion
/*
  if (Value *RetVal = mblock->Codegen()){
    Builder.CreateRet(RetVal);
    verifyFunction(*TheFunction);
    return TheFunction;
  }
  TheFunction->eraseFromParent();
  return 0;
  */
//llvm::Type *Tryme = func->getReturnType();
//  Builder.CreateRet(Tryme);
  verifyFunction(*TheFunction);
  return TheFunction;
}

%}

%union{
    class decafAST *ast;
    std::string *sval;
    int number;
    int decaftype;
 }

%token T_AND
%token T_ASSIGN
%token <sval> T_BOOLTYPE
%token T_BREAK
%token <number> T_CHARCONSTANT
%token T_COMMA
%token T_COMMENT
%token T_CONTINUE
%token T_DIV
%token T_DOT
%token T_ELSE
%token T_EQ
%token T_EXTERN
%token <number> T_FALSE
%token T_FOR
%token T_FUNC
%token T_GEQ
%token T_GT
%token <sval> T_ID
%token T_IF
%token <number> T_INTCONSTANT
%token <sval> T_INTTYPE
%token T_LCB
%token T_LEFTSHIFT
%token T_LEQ
%token T_LPAREN
%token T_LSB
%token T_LT
%token T_MINUS
%token T_MOD
%token T_MULT
%token T_NEQ
%token T_NOT
%token T_NULL
%token T_OR
%token T_PACKAGE
%token T_PLUS
%token T_RCB
%token T_RETURN
%token T_RIGHTSHIFT
%token T_RPAREN
%token T_RSB
%token T_SEMICOLON
%token <sval> T_STRINGCONSTANT
%token <sval> T_STRINGTYPE
%token <number> T_TRUE
%token T_VAR
%token <sval> T_VOID
%token T_WHILE
%token T_WHITESPACE

%type <ast> lvalue expr constant bool_constant method_call method_arg method_arg_list assign number_expr
%type <ast> block method_block statement statement_list var_decl_list var_decl method_var_list
%type <ast> method_decl method_decl_list field_decl_list field_decl extern_type_list extern_defn
%type <ast> extern_list decafpackage while_stmt for_stmt return_stmt valid_return break_stmt continue_stmt id_list
%type <sval> decaf_type method_type extern_type

%left T_OR
%left T_AND
%left T_EQ T_NEQ T_LT T_LEQ T_GEQ T_GT
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%left T_NOT
%right UMINUS

%%

start: program

program: extern_list decafpackage
    {
        ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2);
		if (printAST) {
			cout << getString(prog) << endl;
		}
        delete prog;
    }


extern_list: extern_list extern_defn
    { decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | /* extern_list can be empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

extern_defn: T_EXTERN T_FUNC T_ID T_LPAREN extern_type_list T_RPAREN method_type T_SEMICOLON
    {
		$$ = new ExternFunctionAST( *$3, *$7, (MethodVarListAST *)$5 );
    //extern_defn Codegen()
    $$->Codegen();
		delete $3; }
    | T_EXTERN T_FUNC T_ID T_LPAREN T_RPAREN method_type T_SEMICOLON
    { $$ = new ExternFunctionAST( *$3, *$6, NULL); $$->Codegen(); delete $3; }
    ;

extern_type_list: extern_type
    { $$ = new MethodVarListAST(string(""), *$1); }
    | extern_type T_COMMA extern_type_list
    {
        MethodVarListAST *tlist = (MethodVarListAST *)$3;
        tlist->push_front(string(""), *$1);
        $$ = tlist;
    }
    ;

decafpackage: T_PACKAGE T_ID T_LCB field_decl_list method_decl_list T_RCB
    { $$ = new PackageAST(*$2, (IdList *)$4, (decafStmtList *)$5);
			symtable->new_symtbl(); // For FieldDeclList (Global Sym Table)
      tablenumber++;
			$4->Codegen();
			$5->Codegen();
			if(SavingMain != NULL){SavingMain->Codegen();}
			symtable->remove_symtbl();
      tablenumber--;
			delete $2;
		}
    | T_PACKAGE T_ID T_LCB field_decl_list T_RCB
    { $$ = new PackageAST(*$2, (IdList *)$4, new decafStmtList());
			symtable->new_symtbl(); // For FieldDeclList
      tablenumber ++;
			$4->Codegen();
			symtable->remove_symtbl();
      tablenumber --;
			delete $2; }
    ;

field_decl_list: field_decl_list field_decl
    { decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | /* empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

field_decl: T_VAR id_list T_SEMICOLON {$$ = $2;}
  | T_VAR T_ID decaf_type T_ASSIGN constant T_SEMICOLON {
    $$ = new AssignGlobalVarAST(*$2, *$3, $5);
    }

id_list: T_ID decaf_type {
    $$ = new IdList(*$1, *$2, -1);}
  | T_ID T_COMMA id_list { IdList *temp = (IdList*)$3;
      temp->addVar(*$1); $$ = temp;}
  | T_ID T_LSB T_INTCONSTANT T_RSB decaf_type {
      $$ = new IdList(*$1, *$5, $3);}
      ;

method_decl_list: method_decl_list method_decl
    { decafStmtList *slist = (decafStmtList *)$1;
			if(SavingMain == NULL){
				slist->push_back($2);
			}
			else if($2 != SavingMain) {slist->push_back($2);}
			$$ = slist;
		}
    | method_decl {
			decafStmtList *slist = new decafStmtList();
			if(SavingMain == NULL){
				slist->push_back($1);
			}
			else if($1 != SavingMain){slist->push_back($1);}
			$$ = slist;
		}
    ;

method_decl: T_FUNC T_ID T_LPAREN method_var_list T_RPAREN method_type method_block {
      if(*$2 == "main"){
				SavingMain = new MethodDeclAST(*$2, *$6, (MethodVarListAST *)$4, (MethodBlockAST *)$7);
				$$ = SavingMain;
			}
			else{
				$$ = new MethodDeclAST(*$2, *$6, (MethodVarListAST *)$4, (MethodBlockAST *)$7);
			}
			// $$->Codegen(); // Was here for Assignment 3
			delete $2;
		}
		;

block: T_LCB var_decl_list statement_list T_RCB
    { $$ = new BlockAST((decafStmtList *)$2, (decafStmtList *)$3); }


statement_list: statement statement_list
    { decafStmtList *slist = (decafStmtList *)$2; slist->push_front($1); $$ = slist; }
    | /* empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

statement: assign T_SEMICOLON
    { $$ = $1; }
    | method_call T_SEMICOLON
    { $$ = $1; }
    | T_IF T_LPAREN expr T_RPAREN block T_ELSE block
    { $$ = new IfStmtAST($3, (BlockAST *)$5, (BlockAST *)$7); }
    | T_IF T_LPAREN expr T_RPAREN block
    { $$ = new IfStmtAST($3, (BlockAST *)$5, NULL); }
    | while_stmt { $$ = $1; }
		| for_stmt { $$ = $1; }
		| return_stmt { $$ = $1; }
    | break_stmt { $$ = $1; }
		| continue_stmt { $$ = $1; }
    | block { $$ = $1; }
    ;

while_stmt: T_WHILE T_LPAREN expr T_RPAREN block { $$ = new WhileStmtAST( (decafAST *)$3, (BlockAST *)$5); }
    ;

for_stmt: T_FOR T_LPAREN assign T_SEMICOLON expr T_SEMICOLON assign T_RPAREN block { $$ = new ForStmtAST( (AssignVarAST *)$3, (BinaryExprAST *)$5, (AssignVarAST *)$7, (BlockAST *)$9 ); }

return_stmt: T_RETURN T_SEMICOLON { $$ = new ReturnStmtAST(NULL); }
    | T_RETURN valid_return T_SEMICOLON { $$ = new ReturnStmtAST($2); }
    ;

valid_return: T_LPAREN expr T_RPAREN { $$ = $2; }
    | T_LPAREN T_RPAREN {$$ = NULL;}
    ;

break_stmt: T_BREAK T_SEMICOLON { $$ = new BreakStmtAST("break"); }

continue_stmt: T_CONTINUE T_SEMICOLON { $$ = new ContinueStmtAST("continue"); }

assign: T_ID T_ASSIGN expr {  $$ = new AssignVarAST(*$1, $3); }
  | T_ID T_LSB expr T_RSB T_ASSIGN expr { $$ = new AssignArrayLocAST(*$1, $3, $6 );}
    ;

method_call: T_ID T_LPAREN method_arg_list T_RPAREN { $$ = new MethodCallAST(*$1, (decafStmtList *)$3 ); }
    | T_ID T_LPAREN T_RPAREN {$$ = new MethodCallAST(*$1, (decafStmtList *)NULL);}
    ;

method_arg_list: method_arg {decafStmtList *slist = new decafStmtList(); slist->push_front($1); $$ = slist;}

    | method_arg T_COMMA method_arg_list {decafStmtList *slist = (decafStmtList *)$3; slist->push_front($1); $$ = slist;}
    ;

method_arg: expr { errstr("method_arg = expression"); $$ = $1;}
		| T_STRINGCONSTANT { $$ = new StringConstantAST(*$1); }
		;

lvalue: T_ID {errstr("lvalue T_ID = " + *$1); $$ = new VariableExprAST(*$1); delete $1; }
    | T_ID T_LSB expr T_RSB { $$ = new ArrayLocExprAST(*$1, $3); delete $1; }
    ;

expr: lvalue { $$ = $1; }
		| constant { $$ = $1; }
    | expr T_OR expr {$$ = new BinaryExprAST(T_OR, $1, $3);}
    | expr T_AND expr {$$ = new BinaryExprAST(T_AND, $1, $3);}
    | expr T_EQ expr {$$ = new BinaryExprAST(T_EQ, $1, $3);}
    | expr T_NEQ expr {$$ = new BinaryExprAST(T_NEQ, $1, $3);}
    | expr T_LT expr {$$ = new BinaryExprAST(T_LT, $1, $3);}
    | expr T_GT expr {$$ = new BinaryExprAST(T_GT, $1, $3);}
    | expr T_GEQ expr {$$ = new BinaryExprAST(T_GEQ, $1, $3);}
    | expr T_LEQ expr {$$ = new BinaryExprAST(T_LEQ, $1, $3);}
    | expr T_PLUS expr { $$ = new BinaryExprAST(T_PLUS, $1, $3);}
    | expr T_MINUS expr { $$ = new BinaryExprAST(T_MINUS, $1, $3);}
    | expr T_MULT expr {$$ = new BinaryExprAST(T_MULT, $1, $3);}
    | expr T_DIV expr {$$ = new BinaryExprAST(T_DIV, $1, $3);}
    | expr T_MOD expr {$$ = new BinaryExprAST(T_MOD, $1, $3);}
    | expr T_LEFTSHIFT expr {$$ = new BinaryExprAST(T_LEFTSHIFT, $1, $3);}
    | expr T_RIGHTSHIFT expr {$$ = new BinaryExprAST(T_RIGHTSHIFT, $1, $3);}
    | T_NOT expr {$$ = new UnaryExprAST(T_NOT, $2);}
    | T_MINUS expr %prec UMINUS { $$ = new UnaryExprAST(T_MINUS, $2);}
    | method_call {$$ = $1;}
    | T_LPAREN expr T_RPAREN { $$ = $2;}
		;

method_block: T_LCB var_decl_list statement_list T_RCB
        { $$ = new MethodBlockAST((decafStmtList *)$2, (decafStmtList *)$3); }

var_decl_list: /*empty string*/ {
    $$ = new decafStmtList();}
    | var_decl var_decl_list {
    decafStmtList* temp = (decafStmtList*)$2;
      temp->push_front($1); $$ = temp;}
		;

var_decl: T_VAR method_var_list T_SEMICOLON {
    $$ = $2;}
		;

method_var_list: /*empty string*/  { $$ = new MethodVarListAST(); }
  | T_ID decaf_type { errstr("method_var_list addVar " + *$1); $$ = new MethodVarListAST(*$1, *$2); }
  | T_ID T_COMMA method_var_list {
      MethodVarListAST *temp = (MethodVarListAST *)$3;
      errstr("method_var_list addVar " + *$1);
      temp->addVar(*$1); $$ = temp;}
  | T_ID decaf_type T_COMMA method_var_list {
      MethodVarListAST *temp = (MethodVarListAST *)$4;
      errstr("method_var_list addVar " + *$1);
      temp->addVar(*$1, *$2); $$ = temp;
      }
  ;

extern_type: T_STRINGTYPE { $$ = $1; }
  | decaf_type { $$ = $1; }
  ;

method_type: T_VOID { $$ = $1; }
  | decaf_type { $$ = $1; }
  ;

decaf_type: T_BOOLTYPE {$$ = $1;}
  | T_INTTYPE {$$ = $1;}
  ;

constant: number_expr
		| bool_constant

number_expr: T_INTCONSTANT { $$ = new NumberExprAST($1); }
		| T_CHARCONSTANT { $$ = new NumberExprAST($1); }
		;

bool_constant: T_TRUE {$$ = new BoolExprAST($1);}
    | T_FALSE {$$ = new BoolExprAST($1);}
    ;

%%


/***********/
/**CODEGEN**/
/***********/

Value* MethodVarListAST::Codegen() {
	AllocaInst *Alloca;
  errstr("MethodVarList, MethodVars.size() = " + to_string(MethodVars.size()));
	while(MethodVars.size() != 0){
    VarDefAST* vd = getFront();
    errstr("MethodVarListAST VarDef: Name = " + vd->Name + ", Type: " + vd->VarType);
		if(retrieve_front_type()=="IntType"){
      errstr("MethodVarList IntType Store Alloca in table " + to_string(tablenumber));
			Alloca = Builder.CreateAlloca(Type::getInt32Ty(getGlobalContext()), 0, retrieve_front());
			Builder.CreateStore(Builder.getInt32(0), Alloca); // Zero initialize
		}
		else if(retrieve_front_type()=="BoolType"){
			Alloca = Builder.CreateAlloca(Type::getInt1Ty(getGlobalContext()), 0, retrieve_front());
			Builder.CreateStore(Builder.getInt1(0), Alloca); // Zero initialize
      errstr("MethodVarList BoolType Store Alloca in table " + to_string(tablenumber));
		}
		else{
			throw runtime_error("This type is not recognised.");
		}
		descriptor* temp = new descriptor;
		temp->lineno = lineno;
		temp->mem_loca = Alloca;
		symtable->enter_symtbl(retrieve_front(), temp);
    errstr("Added symbol " + retrieve_front() + " in table " + to_string(tablenumber));
		MethodVars.pop_front();
	}
}
/*Value* MethodVarListAST::Codegen() {
	AllocaInst *Alloca;
  errstr("MethodVarList, MethodVars.size() = " + to_string(MethodVars.size()));
	while(MethodVars.size() != 0){
    VarDefAST* vd = getFront();
    errstr("MethodVarListAST VarDef: Name = " + vd->Name + ", Type: " + vd->VarType);
		if(retrieve_front_type()=="IntType"){
      errstr("MethodVarList IntType Store Alloca in table " + to_string(tablenumber));
			Alloca = Builder.CreateAlloca(Type::getInt32Ty(getGlobalContext()), nullptr, retrieve_front());
			Builder.CreateStore(Builder.getInt32(0), Alloca); // Zero initialize
			descriptor* AlreadyDeclared = symtable->access_symtbl(retrieve_front());
			if(AlreadyDeclared != NULL && AlreadyDeclared->Val != NULL){

        Builder.CreateStore(symtable->access_symtbl(retrieve_front())->Val, Alloca);
			}
		}
		else if(retrieve_front_type()=="BoolType"){
			Alloca = Builder.CreateAlloca(Type::getInt1Ty(getGlobalContext()), nullptr, retrieve_front());
			Builder.CreateStore(Builder.getInt1(0), Alloca); // Zero initialize
      errstr("MethodVarList BoolType Store Alloca in table " + to_string(tablenumber));
      descriptor* AlreadyDeclared = symtable->access_symtbl(retrieve_front());
			if(AlreadyDeclared != NULL && AlreadyDeclared->Val != NULL){
				Builder.CreateStore(symtable->access_symtbl(retrieve_front())->Val, Alloca);
			}
		}
		else{
			throw runtime_error("This type is not recognised.");
		}
		descriptor* temp = new descriptor;
		temp->lineno = lineno;
		temp->mem_loca = Alloca;
		symtable->enter_symtbl(retrieve_front(), temp);
    errstr("Added symbol " + retrieve_front() + " in table " + to_string(tablenumber));
		MethodVars.pop_front();
	}
}*/

Value* NumberExprAST::Codegen() {
	return ConstantInt::get(getGlobalContext(), APInt(32, Value));
}

Value* StringConstantAST::Codegen() {
	llvm::GlobalVariable *GS = Builder.CreateGlobalString(StringConstant, "globalstring");
	return Builder.CreateConstGEP2_32(GS->getValueType(), GS, 0, 0, "cast");
}

Value* BoolExprAST::Codegen() {
	return ConstantInt::get(getGlobalContext(), APInt(1, Val));
}

Value* VariableExprAST::Codegen() {
  descriptor* access = symtable->access_symtbl(Value);
	if( access != NULL ){
		llvm::Value *V = access->mem_loca;
    if(access->mem_loca != NULL){
      V = access->mem_loca;
    }
    else{
      V = access->globvar_mem_loca;
    }
    errstr("VariableExpr create Load " + Value);
		return Builder.CreateLoad(V,Value);
	}
	else{
		throw runtime_error("VariableExprAST: This variable has not been defined inside the Symbol Table");
	}
}

Value* MethodCallAST::Codegen() {
//  Function *CallFunc = FunctionProtos[MethodName];
  Function *CallFunc = TheModule->getFunction(MethodName);
  errstr("MethodCall Codegen() " + MethodName);
  if (CallFunc == 0){
    throw runtime_error("could not find the function requested.");
  }

  vector<Value*> FuncArgs;
  if(Expression != NULL){ // An empty DecafStmtList is registered as NULL
		if(Expression->size() != 0){
      errstr("MethodCallAST Codegen() Expression Codegen()");
			FuncArgs = Expression->VectorOfArgs(); //return AllArts(stmts), vector of Values
  /*    for (unsigned i = 0, e = Expression->size(); i != e; ++i){
        errstr("Expression arg " + to_string(i) + " Codegen()");
        FuncArgs.push_back(Expression[i].Codegen());
        if(FuncArgs.back() == 0){
          return 0;
        }
      }
      */
      errstr("FuncArgs Size = " + to_string(FuncArgs.size()));
      //Vector of args, takes list and push_back
    }
	}
  else{
    errstr("MethodCall Args NULL");
  }
//check if the function has a return type void
Value *V;
if (Expression != NULL){
  V = Expression->Codegen();
}
if(MethodName == "print_int" && V->getType()->isIntegerTy(1)){

  V = Builder.CreateZExt(V, Builder.getInt32Ty(), "zexttmp");
  return Builder.CreateCall(CallFunc, V, "calltmp");
}
  bool isVoid = CallFunc->getReturnType()->isVoidTy();
  errstr("Builder Create Call " + MethodName);
	return Builder.CreateCall(FunctionProtos[MethodName], FuncArgs, isVoid ? "" : "calltmp");
}

Value* BinaryExprAST::Codegen() {
	if(Operator != 258 && Operator != 292){
		Value *L = Expression1->Codegen();
		Value *R = Expression2->Codegen();
		if (L == 0 || R == 0) return 0;

		switch (Operator) {
		case 294: return Builder.CreateAdd(L, R, "addtmp");
		case 286: return Builder.CreateSub(L, R, "subtmp");
		case 258: return Builder.CreateAnd(L, R, "andtmp");
		case 288: return Builder.CreateMul(L,R,"multemp");
		case 266 : return Builder.CreateSDiv(L,R,"divtemp");
		case 281 : return Builder.CreateShl(L,R,"shltemp"); //leftshift
		case 297 : return Builder.CreateLShr(L,R,"lshrtemp"); //Rightshift
		case 287 : return Builder.CreateSRem(L,R,"sremtemp"); //mod
		case 285 : return Builder.CreateICmpSLT(L,R,"lttemp");
		case 275 : return Builder.CreateICmpSGT(L,R,"gttemp");
		case 282 : return Builder.CreateICmpSLE(L,R,"leqtemp");
		case 274 : return Builder.CreateICmpSGE(L,R,"geqtemp");
		case 292 : return Builder.CreateOr(L,R,"ortemp");
		case 269 : return Builder.CreateICmpEQ(L,R,"eqtemp");
		case 289 : return Builder.CreateICmpNE(L,R,"neqtemp");
		default: throw runtime_error("what operator is this? never heard of it.");
		}
	}
	else{
		llvm::BasicBlock* EntryInsertBlockBB = Builder.GetInsertBlock();
		llvm::BasicBlock* PrevInsertBlockBB = Builder.GetInsertBlock(); // Saves the Insert Block previous to the one currently in.
		if(Operator == 258){ // And
			llvm::BasicBlock *AndTrueBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "andtrue", CurrentFunction);
			llvm::BasicBlock *AndEndBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "andend", CurrentFunction);

			llvm::Value* L = Expression1->Codegen();
			Builder.CreateCondBr(L, AndTrueBB, AndEndBB);

			Builder.SetInsertPoint(AndTrueBB);
			llvm::Value* R = Expression2->Codegen();
			PrevInsertBlockBB = Builder.GetInsertBlock();
			Builder.CreateBr(AndEndBB);

			Builder.SetInsertPoint(AndEndBB);
			llvm::PHINode *val = Builder.CreatePHI(L->getType(), 2, "phival");
			val->addIncoming(L, EntryInsertBlockBB);
			val->addIncoming(R, PrevInsertBlockBB);
			PrevInsertBlockBB = Builder.GetInsertBlock();
			return val;
		}
		else{
			llvm::BasicBlock *OrFalseBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "orfalse", CurrentFunction);
			llvm::BasicBlock *OrEndBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "orend", CurrentFunction);

			llvm::Value* L = Expression1->Codegen();
			Builder.CreateCondBr(L, OrEndBB, OrFalseBB);

			Builder.SetInsertPoint(OrFalseBB);
			llvm::Value* R = Expression2->Codegen();
			PrevInsertBlockBB = Builder.GetInsertBlock();
			Builder.CreateBr(OrEndBB);

			Builder.SetInsertPoint(OrEndBB);
			llvm::PHINode *val = Builder.CreatePHI(L->getType(), 2, "phival");
			val->addIncoming(L, EntryInsertBlockBB);
			val->addIncoming(R, PrevInsertBlockBB);
			PrevInsertBlockBB = Builder.GetInsertBlock();
			return val;
		}
	}

}

Value* UnaryExprAST::Codegen() {
  Value* itemp = Argument->Codegen();
  switch (UnaryType){
    case 286: return Builder.CreateNeg(itemp, "uminustmp");
		case 290: return Builder.CreateNot(itemp, "notstmp");
    default: throw runtime_error("what operator is this? never heard of it.");
  }
  //return ConstantInt::get(getGlobalContext(), APInt(32, Val));
}

Value* AssignVarAST::Codegen() {
	descriptor* temp = NULL;
  llvm::Value *V = Expression->Codegen();
  setValue(V);
	temp = symtable->access_symtbl(VarName);
	if(temp == NULL){throw runtime_error("AssignVarAST::This variable has not been declared.");}
	llvm::AllocaInst* Alloca = temp->mem_loca;
	Builder.CreateStore(Val, Alloca);
  errstr("AssignVarAST CreateStore " + VarName);
}

Value* AssignArrayLocAST::Codegen() {
	descriptor* temp = NULL;
	temp = symtable->access_symtbl(ArrayName);
	if(temp == NULL) { throw runtime_error("AssignArrayLocAST::Codegen() - This variable was not declared in the symbol table"); }
	if(temp->globvar_mem_loca == NULL){ throw runtime_error("AssignArrayLocAST::Codegen() - This variable was not assigned as a Global Variable"); }
	if(temp->array_type == NULL){ throw runtime_error("AssignArrayLocAST::Codegen() - This array was not given a type"); }
	llvm::GlobalVariable* GlobVar = temp->globvar_mem_loca;
	llvm::ArrayType* array_type = temp->array_type;
	if(temp->type == "IntType"){
		llvm::Value *ArrayLoc = Builder.CreateStructGEP(array_type, GlobVar, 0, "arrayloc");
		llvm::Value *RetIndex = Index->Codegen();
		llvm::Value *ArrayIndex = Builder.CreateGEP(Builder.getInt32Ty(), ArrayLoc, RetIndex, "arrayindex");
		llvm::Value *RetValue = Value->Codegen();
		llvm::Value *ArrayStore = Builder.CreateStore(RetValue, ArrayIndex);
	}
	else if(temp->type == "BoolType"){
		llvm::Value *ArrayLoc = Builder.CreateStructGEP(array_type, GlobVar, 0, "arrayloc");
		llvm::Value *RetIndex = Index->Codegen();
		llvm::Value *ArrayIndex = Builder.CreateGEP(Builder.getInt1Ty(), ArrayLoc, RetIndex, "arrayindex");
		llvm::Value *RetValue = Value->Codegen();
		llvm::Value *ArrayStore = Builder.CreateStore(RetValue, ArrayIndex);
	}
	else{
		throw runtime_error("AssignArrayLocAST::Codegen() - The type inside the symbol table was not defined.");
	}
}

Value* ArrayLocExprAST::Codegen() {
	descriptor* temp = NULL;
	temp = symtable->access_symtbl(Name);
	if(temp == NULL) { throw runtime_error("AssignArrayLocAST::Codegen() - This variable was not declared in the symbol table"); }
	if(temp->globvar_mem_loca == NULL){ throw runtime_error("AssignArrayLocAST::Codegen() - This variable was not assigned as a Global Variable"); }
	if(temp->array_type == NULL){ throw runtime_error("AssignArrayLocAST::Codegen() - This array was not given a type"); }
	llvm::GlobalVariable* GlobVar = temp->globvar_mem_loca;
	llvm::ArrayType* array_type = temp->array_type;
	if(temp->type == "IntType"){
		llvm::Value *ArrayLoc = Builder.CreateStructGEP(array_type, GlobVar, 0, "arrayloc");
		llvm::Value *RetIndex = Index->Codegen();
		llvm::Value *ArrayIndex = Builder.CreateGEP(Builder.getInt32Ty(), ArrayLoc, RetIndex, "arrayindex");
		llvm::Value *Value = Builder.CreateLoad(ArrayIndex, "loadtmp");
		return Value;
	}
	else if(temp->type == "BoolType"){
		llvm::Value *ArrayLoc = Builder.CreateStructGEP(array_type, GlobVar, 0, "arrayloc");
		llvm::Value *RetIndex = Index->Codegen();
		llvm::Value *ArrayIndex = Builder.CreateGEP(Builder.getInt1Ty(), ArrayLoc, RetIndex, "arrayindex");
		llvm::Value *Value = Builder.CreateLoad(ArrayIndex, "loadtmp");
		return Value;
	}
	else{
		throw runtime_error("AssignArrayLocAST::Codegen() - The type inside the symbol table was not defined.");
	}
}

Value* BlockAST::Codegen() {
	  symtable->new_symtbl();
    tablenumber++;
		BlockVarDefs->Codegen();
		BlockStatementList->Codegen();
		symtable->remove_symtbl();
    tablenumber--;
}

Value* MethodBlockAST::Codegen() {
		// The new symbol table is created and destroyed inside of MethodDecl
    MethodBlockVarDefs->Codegen();
    MethodBlockStatementList->Codegen();
  }

Value* IfStmtAST::Codegen(){
//  llvm::BasicBlock* PrevInsertBlockBB = Builder.GetInsertBlock(); // Might need the previous insertion point later on?
//	llvm::BasicBlock::iterator PrevInsertPoint = Builder.GetInsertPoint();
  Value *Condtn = IfCondition->Codegen(); //Generate the Value for the if condition
  if (!Condtn){
    return nullptr;
  }
  Condtn = Builder.CreateICmpNE(Condtn, Builder.getInt1(0), "if-cond");
  errstr("BuilderICMP Complete");
  llvm::Function* Func = Builder.GetInsertBlock()->getParent();
  //NOTE: change from currentfunction to the above
//emit val for the contition, then compare it to 0 to get truth value as a 1-bit bool
//  Function* TheFunction = Builder.GetInsertBlock()->getParent();
  //Create Blocks for then and else clauses. Insert the 'then' block at the end of Function
  BasicBlock *ThenBranch = BasicBlock::Create(llvm::getGlobalContext(),  "then", Func);
  BasicBlock *ElseBranch = BasicBlock::Create(llvm::getGlobalContext(),  "else");//, CurrentFunction);
  //BasicBlock *IfCont = BasicBlock::Create(llvm::getGlobalContext(),  "ifcont"); //, CurrentFunction);

//Create blocks for our statements
  Builder.CreateCondBr(Condtn, ThenBranch, ElseBranch);

  //Set Insert for then to emit Value
  Builder.SetInsertPoint(ThenBranch);

  Value *ThenVal = IfStmt->Codegen();
  if (!ThenVal) {return nullptr;}
  //Create branch to continue if statement
  //Builder.CreateBr(IfCont);
  //Codegen of then can change current block, update ThenBranch for the PHI
  ThenBranch = Builder.GetInsertBlock(); //This is done since the then stmt can change the block (nested)
//Conditional Branch is now inserted, move builder to insert then block

  //Emit else block, add block to function
  Func->getBasicBlockList().push_back(ElseBranch);
  Builder.SetInsertPoint(ElseBranch);

  Value *ElseVal = ElseStmt->Codegen();
  if (!ElseVal) { return nullptr;}

  //Builder.CreateBr(IfCont);
  ElseBranch = Builder.GetInsertBlock();

  //Merge if and else (ifcont)
//  Func->getBasicBlockList().push_back(IfCont);

//  Builder.SetInsertPoint(IfCont);

  //PHINode *PN = Builder.CreatePHI(Type::getInt1Ty(llvm::getGlobalContext()), 2, "iftmp");
  //PN->addIncoming(ThenVal, ThenBranch);
  //PN->addIncoming(ElseVal, ElseBranch);
  errstr("If Codegen COMPLETE");
  //return PN;

}


Value* WhileStmtAST::Codegen() {
	Value *Condtn = BinaryExpr->Codegen();
	llvm::BasicBlock* PrevInsertBlockBB = Builder.GetInsertBlock(); // Need to save the entry BasicBlock
	llvm::Function* Func = Builder.GetInsertBlock()->getParent();
	BasicBlock *WhileLoopBranch = BasicBlock::Create(llvm::getGlobalContext(),  "whileloop", Func);
  BasicBlock *WhileAfterLoopBranch = BasicBlock::Create(llvm::getGlobalContext(),  "whileafterloop", Func);
	Builder.CreateCondBr(Condtn, WhileLoopBranch, WhileAfterLoopBranch); // If the initial condition fails, then we never want to go into LoopBranch

	Builder.SetInsertPoint(WhileLoopBranch);
	PHINode *PN = Builder.CreatePHI(Type::getInt1Ty(llvm::getGlobalContext()), 1, "phi-val");
  PN->addIncoming(Condtn, PrevInsertBlockBB);
	Value *Block = BlockExpr->Codegen();
	Condtn = BinaryExpr->Codegen();
	PN->addIncoming(Condtn, WhileLoopBranch);
	Builder.CreateCondBr(Condtn, WhileLoopBranch, WhileAfterLoopBranch);

	Builder.SetInsertPoint(WhileAfterLoopBranch);
}

Value* ForStmtAST::Codegen() {
	symtable->new_symtbl(); // Creating a new symbol table for the variable defined inside the for loop
	AllocaInst* Alloca = Builder.CreateAlloca(Type::getInt32Ty(getGlobalContext()), 0, AssignVar1->RetVarName());
	Builder.CreateStore(Builder.getInt32(0), Alloca); // Zero initialize
	descriptor *temp = new descriptor;
	temp->mem_loca = Alloca;
	symtable->enter_symtbl(AssignVar1->RetVarName(), temp); // Now when Assign1 is Codegen(), there is allocated memory for it
	Value *Assign1 = AssignVar1->Codegen();
	Value *Condtn = BinaryExpr->Codegen();
	llvm::BasicBlock* PrevInsertBlockBB = Builder.GetInsertBlock(); // Need to save the entry BasicBlock
	llvm::Function* Func = Builder.GetInsertBlock()->getParent();
	BasicBlock *LoopBranch = BasicBlock::Create(llvm::getGlobalContext(),  "loop", Func);
  BasicBlock *AfterLoopBranch = BasicBlock::Create(llvm::getGlobalContext(),  "afterloop", Func);

	Builder.CreateCondBr(Condtn, LoopBranch, AfterLoopBranch); // If the initial condition fails, then we never want to go into LoopBranch

	Builder.SetInsertPoint(LoopBranch);
	PHINode *PN = Builder.CreatePHI(Type::getInt1Ty(llvm::getGlobalContext()), 1, "phi-val");
  PN->addIncoming(Condtn, PrevInsertBlockBB);
	Value *Block = BlockExpr->Codegen();
	Value *Assign2  = AssignVar2->Codegen();
	Condtn = BinaryExpr->Codegen();
	PN->addIncoming(Condtn, LoopBranch);
	Builder.CreateCondBr(Condtn, LoopBranch, AfterLoopBranch);

	Builder.SetInsertPoint(AfterLoopBranch);
	symtable->remove_symtbl(); // Removing the symbol table created at the beginning of the for loop
}

Value* ReturnStmtAST::Codegen() {
	if(Expression == NULL){
		Builder.CreateRet(nullptr);
	}
	else if(Expression != NULL){
		llvm::Value* temp = Expression->Codegen();
		Builder.CreateRet(temp);
	}
	else{
		throw runtime_error("ReturnStmtAST was not set up to handle this return type in ReturnStmtAST::Codegen()");
	}
}

Value* BreakStmtAST::Codegen() {}

Value* ContinueStmtAST::Codegen() {}

Value* MethodDeclAST::Codegen() {
	symtable->new_symtbl();
  errstr("MethodDecl Codegen() " + Name);
  if (Name == "main"){
    gen_main_def();
  }
  else{
    list<Value*> ArgTypes;
    vector<string> ArgNames;
		MethodVarsList->FillOutArgs(ArgTypes, ArgNames);
    //needed for functionArgs push_back Type* (either bool or int)

		FunctionProtos[Name] = gen_function_def(ArgTypes, Name, methodType, ArgNames, MBlock); // NeededForFunctionArgs & FunctionArgsName is passed by reference, so we don't need to empty it
		//MethodVarsList->Codegen();
  }
  Value* ReturnTy = MBlock->Codegen();
	//if(Name == "main"){
	//	Builder.CreateRet(ConstantInt::get(getGlobalContext(), APInt(32, 1)));
	//}
	//symtable->remove_symtbl();
  //
  if(methodType == "VoidType"){
    Builder.CreateRet(NULL);
  }
  symtable->remove_symtbl();
  //
	Builder.ClearInsertionPoint();
}

Value* AssignGlobalVarAST::Codegen() {

	descriptor* temp = new descriptor;
	temp->lineno = lineno;
	temp->type = Type;
	// We need a way to store the GlobalVariable* into the Symbol Table
	llvm::Value* VarValue = ExprValue->Codegen();
	int constantIntValue = 0;
	if(ConstantInt* CI = dyn_cast<ConstantInt>(VarValue)){
		if(CI->getBitWidth() <= 32){
			constantIntValue = CI->getSExtValue();
		}
	}
	if(Type=="IntType"){ // i32
			llvm::GlobalVariable *GlobVar = new llvm::GlobalVariable(
			*TheModule,
			Builder.getInt32Ty(),
			false,  // variable is mutable
			llvm::GlobalValue::InternalLinkage,
			Builder.getInt32(constantIntValue),
			Name
			);
			temp->globvar_mem_loca = GlobVar;
	}
	else if(Type=="BoolType"){ // i1
			llvm::GlobalVariable *GlobVar = new llvm::GlobalVariable(
			*TheModule,
			Builder.getInt1Ty(),
			false,  // variable is mutable
			llvm::GlobalValue::InternalLinkage,
			Builder.getInt32(0), // Zero Initializing All Scalars
			Name
			);
			temp->globvar_mem_loca = GlobVar;
	}
  errstr("AssignGlbalVar = " + Name);
	symtable->enter_symtbl(Name, temp);
 }




Value* FieldDeclAST::Codegen() {
	if(Size == -1){ // Global Scalar Declaration
      errstr("FieldDecl Codegen Alloca " + Name);
      llvm::Type *varType = getLLVMType(VarType);
      llvm::AllocaInst *Alloca = defineVariable(varType, Name);
    }
	else { //(Size > -1) // Global Array Declaration
		if(VarType=="IntType"){ // i32
			llvm::ArrayType *array = llvm::ArrayType::get(Builder.getInt32Ty(), Size);
			// zeroinitalizer: initialize array to all zeroes
			llvm::Constant *zeroInit = llvm::Constant::getNullValue(array);
			// declare a global variable
			llvm::GlobalVariable *GlobVar = new llvm::GlobalVariable(*TheModule, array, false, llvm::GlobalValue::ExternalLinkage, zeroInit, Name);
			// 3rd parameter to GlobalVariable is false because it is not a constant variable
		}
		else if(VarType=="BoolType"){ // i1
			llvm::ArrayType *array = llvm::ArrayType::get(Builder.getInt1Ty(), Size);
			// zeroinitalizer: initialize array to all zeroes
			llvm::Constant *zeroInit = llvm::Constant::getNullValue(array);
			// declare a global variable
			llvm::GlobalVariable *GlobVar = new llvm::GlobalVariable(*TheModule, array, false, llvm::GlobalValue::ExternalLinkage, zeroInit, Name);
			// 3rd parameter to GlobalVariable is false because it is not a constant variable
		}
		else{
			throw runtime_error("This type is not recognised by FieldDeclAST::Codegen() - Array ");
		}
	}
}

/*	if(VarType == "IntType"){ // i32
    llvm::GlobalVariable *GlobVar = new llvm::GlobalVariable(
    *TheModule,
    Builder.getInt32Ty(),
    false,  // variable is mutable
    llvm::GlobalValue::InternalLinkage,
    Builder.getInt32(0), // Zero Initializing All Scalars
    Name
    );
  }
  else if(VarType=="BoolType"){ // i1
    llvm::GlobalVariable *GlobVar = new llvm::GlobalVariable(
    *TheModule,
    Builder.getInt1Ty(),
    false,  // variable is mutable
    llvm::GlobalValue::InternalLinkage,
    Builder.getInt32(0), // Zero Initializing All Scalars
    Name
    );
  }
  else if(VarType=="StringType"){ // i1
    llvm::Value *GlobalStr = Builder.CreateGlobalString("", Name);

  }
  else{
    throw runtime_error("This type is not recognised by FieldDeclAST::Codegen() - Scalar");
  }*/
Value* IdList::Codegen() {
	// cout << "The size of Vars is: " << Vars.size() << endl; // For bug testing, ensuring that FieldDeclListAST has the right number of declarations
	while(!Vars.empty()){
		Vars.front()->Codegen();
		Vars.pop_front();
	}
}

Value* PackageAST::Codegen() {}

Value* ExternFunctionAST::Codegen() {
	if(VariableName == "print_int"){
		llvm::Function* temp = gen_print_int_def();
		FunctionProtos["print_int"] = temp;
	}
	else if(VariableName == "print_string"){
		llvm::Function* temp = gen_print_string_def();
		FunctionProtos["print_string"] = temp;
	}
	else if(VariableName == "read_int"){
		llvm::Function* temp = gen_read_int_def();
		FunctionProtos["read_int"] = temp;
	}
	else{
		throw runtime_error("ExternAST::Codegen() doesn't recognise this function call.");
	}
}

Value* ProgramAST::Codegen() {}

int main() {
	// initialize LLVM
  LLVMContext &Context = getGlobalContext();
  // Make the module, which holds all the code.
  TheModule = new Module("module for very simple expressions", Context);
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
	// Print out all of the generated code to stderr
  TheModule->dump();
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}
