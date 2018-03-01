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
#include "decafexpr-defs.h"

int yylex(void);
int yyerror(char *);

using namespace std;
using namespace llvm;

// print AST?
bool printAST = false;
// print to StdErr SymTbl Errors?
bool SymTblErr = false;
bool Maxx = false;

#include "decafexpr.cc"

symboltable* symtable = new symboltable();
static std::map<std::string, llvm::Function*> FunctionProtos;
list<Value*> NeededForFunctionArgs;
vector<string> FunctionArgsName;
Function* CurrentFunction;
// this global variable contains all the generated code
static llvm::Module *TheModule;

// this is the method used to construct the LLVM intermediate code (IR)
static llvm::IRBuilder<> Builder(llvm::getGlobalContext());
// the calls to getGlobalContext() in the init above and in the
// following code ensures that we are incrementally generating
// instructions in the right order

//sets the args of a function
void setFuncArgs(llvm::Function *f, vector<string> funargs){
  llvm::Function::arg_iterator I,J;
  unsigned Idx = 0;
  for (I = f->arg_begin(), J = f->arg_end(); I != J; ++I, ++Idx){
    I->setName(funargs[Idx]);
  }
}

llvm::FunctionType* get_function_type(decafType dt, vector<Type*> args){
  string s = TyString(dt);
  FunctionType *ft;
  if (s == "VoidType"){
    ft =  llvm::FunctionType::get(Type::getVoidTy(getGlobalContext()), args, false);
    return ft;
  }
  else if (s == "IntType") {
    ft =  llvm::FunctionType::get(Type::getInt32Ty(getGlobalContext()), args, false);
return ft;
  }
  else if (s == "BoolType") {
    ft =  llvm::FunctionType::get(Type::getInt1Ty(getGlobalContext()), args, false);
return ft;
  }
  else if (s == "StringType") {
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
  llvm::FunctionType *FT = llvm::FunctionType::get(llvm::IntegerType::get(llvm::getGlobalContext(), 32), false);
  llvm::Function *TheFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", TheModule);
  if (TheFunction == 0) {
    throw runtime_error("empty function block");
  }
  // Create a new basic block which contains a sequence of LLVM instructions
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", TheFunction);
  // All subsequent calls to IRBuilder will place instructions in this location
  Builder.SetInsertPoint(BB);
	Builder.CreateRet(ConstantInt::get(getGlobalContext(), APInt(32, 0)));
	Builder.SetInsertPoint(BB, TheFunction->getEntryBlock().begin());
  return TheFunction;
}

llvm::Function *gen_function_def(list<Value*> &FunctionArgs, string FunctionName, /*string FunctionType,*/ vector<string> &FunctionArgsName, decafType dt){
  llvm::FunctionType *FT;
	vector<Type*> FunctionArgTypes;
	if(FunctionArgs.empty()){
		FT = llvm::FunctionType::get(Type::getVoidTy(getGlobalContext()), false);
	}
	else{
    int i = 0;
		while( !FunctionArgs.empty() ){
			FunctionArgTypes.push_back(FunctionArgs.front()->getType());
      AllocaInst* Alloca;
      descriptor *temp = new descriptor;
      if (FunctionArgs.front()->getType() == Type::getInt1Ty(getGlobalContext())){
        Alloca = Builder.CreateAlloca(Type::getInt1Ty(getGlobalContext()), nullptr, FunctionArgsName[i]);
        if(Maxx) cout << "Created Alloca bool in gen_func_decl" << endl;
    //    temp->Val = Builder.CreateIntCast(Alloca, Type::getInt1Ty(getGlobalContext()), false, FunctionArgsName[i]);
        if (Maxx) cout << "Create int cast for bool ok" << endl;
      }
      else if (FunctionArgs.front()->getType() == Type::getInt32Ty(getGlobalContext())) {
        Alloca = Builder.CreateAlloca(Type::getInt32Ty(getGlobalContext()), nullptr, FunctionArgsName[i]);
        if (Maxx) cout << "Created Alloca int in gen_func_def" << endl;
      //  temp->Val = Builder.CreateIntCast(Alloca, Type::getInt32Ty(getGlobalContext()), false, FunctionArgsName[i]); // GET LOCAL CONTEXT?!
        if (Maxx) cout << "Builder.CreateInt cast for INT OK" << endl;
      }
      else{
        throw runtime_error("ERROR: Type not recognised inside of gen_function_def().");
      }
      temp->Val = NULL;
      temp->lineno = lineno;
      temp->mem_loca = Alloca;
      symtable->enter_symtbl(FunctionArgsName[i], temp);
      FunctionArgs.pop_front();
      i++;
		}
    FT = get_function_type(dt, FunctionArgTypes);
//		FT = llvm::FunctionType::get(Type::getVoidTy(getGlobalContext()), FunctionArgTypes, false);
	}
/*FUNCTION DECLARATION*/
  llvm::Function *TheFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, FunctionName, TheModule);
  if (TheFunction == 0) {
    throw runtime_error("empty function block");
  }
  setFuncArgs(TheFunction, FunctionArgsName);
  // Create a new basic block which contains a sequence of LLVM instructions
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", TheFunction);
  // All subsequent calls to IRBuilder will place instructions in this location
  Builder.SetInsertPoint(BB);
	Builder.CreateRet(nullptr);
	Builder.SetInsertPoint(BB, TheFunction->getEntryBlock().begin());
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
%token T_BOOLTYPE
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
%token T_INTTYPE
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
%token T_STRINGTYPE
%token <number> T_TRUE
%token T_VAR
%token T_VOID
%token T_WHILE
%token T_WHITESPACE

%type <decaftype> type method_type extern_type
%type <ast> rvalue expr constant bool_constant method_call method_arg method_arg_list assign assign_comma_list
%type <ast> block method_block statement statement_list var_decl_list var_decl var_list param_list param_comma_list
%type <ast> method_decl method_decl_list field_decl_list field_decl field_list extern_type_list extern_defn
%type <ast> extern_list decafpackage

%left T_OR
%left T_AND
%left T_EQ T_NEQ T_LT T_LEQ T_GEQ T_GT
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%left T_NOT
%right UMINUS

%%

start: program

  /* Program = Externs package identifier "{" FieldDecls MethodDecls "}" . */
program: extern_list decafpackage
    {
        ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2);
		if (printAST) {
			cout << getString(prog) << endl;
		}
        delete prog;
    }

  /* Externs    = { ExternDefn } . */
extern_list: extern_list extern_defn
    { decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | /* extern_list can be empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

  /* ExternDefn = extern func identifier "(" [ { ExternType }+, ] ")" MethodType ";" . */
extern_defn: T_EXTERN T_FUNC T_ID T_LPAREN extern_type_list T_RPAREN method_type T_SEMICOLON
    {
		$$ = new ExternAST((decafType)$7, *$3, (TypedSymbolListAST *)$5);
		$$->Codegen();
		delete $3; }
    | T_EXTERN T_FUNC T_ID T_LPAREN T_RPAREN method_type T_SEMICOLON
    { $$ = new ExternAST((decafType)$6, *$3, NULL); $$->Codegen(); delete $3; }
    ;

extern_type_list: extern_type
    { $$ = new TypedSymbolListAST(string(""), (decafType)$1); }
    | extern_type T_COMMA extern_type_list
    {
        TypedSymbolListAST *tlist = (TypedSymbolListAST *)$3;
        tlist->push_front(string(""), (decafType)$1);
        $$ = tlist;
    }
    ;

  /* ExternType = ( string | MethodType ) . */
extern_type: T_STRINGTYPE
    { $$ = stringTy; }
    | type
    { $$ = $1; }
    ;

  /* Program = Externs package identifier "{" FieldDecls MethodDecls "}" . */
decafpackage: T_PACKAGE T_ID begin_block field_decl_list method_decl_list end_block
    { $$ = new PackageAST(*$2, (FieldDeclListAST *)$4, (decafStmtList *)$5); delete $2; }
    | T_PACKAGE T_ID begin_block field_decl_list end_block
    { $$ = new PackageAST(*$2, (FieldDeclListAST *)$4, new decafStmtList()); delete $2; }
    ;

  /* FieldDecls = { FieldDecl } . */
field_decl_list: field_decl_list field_decl
    { decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | /* empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

  /*
  FieldDecl  = var { identifier }+, Type ";" .
  FieldDecl  = var { identifier }+, ArrayType ";" .
  FieldDecl  = var identifier Type "=" Constant ";" .
  */
field_decl: T_VAR field_list T_SEMICOLON
    { $$ = $2;
		}
    | T_VAR T_ID type T_ASSIGN constant T_SEMICOLON
	{ 	$$ = new AssignGlobalVarAST((decafType)$3, *$2, $5); delete $2; }
    ;

field_list: T_ID T_COMMA field_list
    { FieldDeclListAST *flist = (FieldDeclListAST *)$3; flist->new_sym(*$1); $$ = flist; delete $1; }
    | T_ID type
    { $$ = new FieldDeclListAST(*$1, (decafType)$2, -1); delete $1; }
    | T_ID T_LSB T_INTCONSTANT T_RSB type
    { $$ = new FieldDeclListAST(*$1, (decafType)$5, $3); delete $1; }
    ;

  /* MethodDecls = { MethodDecl } . */
method_decl_list: method_decl_list method_decl
    { decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | method_decl {
			// $$->Codegen();
      decafStmtList *slist = new decafStmtList(); slist->push_back($1); $$ = slist;
    }
    ;

/* MethodDecl  = func identifier "(" [ { identifier Type }+, ] ")" MethodType Block . */
method_decl: T_FUNC T_ID T_LPAREN param_list T_RPAREN method_type method_block {
      $$ = new MethodDeclAST((decafType)$6, *$2, (TypedSymbolListAST *)$4, (MethodBlockAST *)$7);
			$$->Codegen();
			delete $2;
		}
;

  /* MethodType = ( void | Type ) . */
method_type: T_VOID
    { $$ = voidTy; }
    | type
    { $$ = $1; }
    ;

param_list: param_comma_list
    { $$ = $1;
		}
    | /* empty */
    { $$ = new TypedSymbolListAST(); }
    ;

param_comma_list: T_ID type T_COMMA param_comma_list
		{		TypedSymbolListAST *tlist = (TypedSymbolListAST *)$4;
				tlist->push_front(*$1, (decafType)$2);
				$$ = tlist;
				delete $1;
    }
    | T_ID type
		{ 	$$ = new TypedSymbolListAST(*$1, (decafType)$2); delete $1; }
    ;

  /* Type = ( int | bool ) . */
type: T_INTTYPE
    { $$ = intTy; }
    | T_BOOLTYPE
    { $$ = boolTy; }
    ;

  /* Block = "{" VarDecls Statements "}" . */
block: T_LCB var_decl_list statement_list T_RCB
    { $$ = new BlockAST((decafStmtList *)$2, (decafStmtList *)$3); }

method_block: T_LCB var_decl_list statement_list T_RCB
    { $$ = new MethodBlockAST((decafStmtList *)$2, (decafStmtList *)$3); }

begin_block: T_LCB
		{ symtable->new_symtbl(); }
		;

end_block: T_RCB
		{ symtable->remove_symtbl(); }
		;

  /* VarDecls = { VarDecl } .  */
var_decl_list: var_decl var_decl_list
    { decafStmtList *slist = (decafStmtList *)$2; slist->push_front($1); $$ = slist; }
    | /* empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

  /* VarDecl  = var { identifier }+, Type ";" . */
var_decl: T_VAR var_list T_SEMICOLON
    { $$ = $2; }

var_list: T_ID T_COMMA var_list
    {
        TypedSymbolListAST *tlist = (TypedSymbolListAST *)$3;
        tlist->new_sym(*$1);
        $$ = tlist;
        delete $1;
    }
    | T_ID type
    {
			$$ = new TypedSymbolListAST(*$1, (decafType)$2);
			delete $1;
		}
    ;

  /* Statements = { Statement } . */
statement_list: statement statement_list
    { decafStmtList *slist = (decafStmtList *)$2; slist->push_front($1); $$ = slist; }
    | /* empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

  /*
  Statement = Block .
  Statement  = MethodCall ";" .
  Statement = Assign ";" .
  Statement = if "(" Expr ")" Block [ else Block ] .
  Statement =  while "(" Expr ")" Block .
  Statement = for "(" { Assign }+, ";" Expr ";" { Assign }+, ")" Block .
  Statement = return [ "(" [ Expr ] ")" ] ";" .
  Statement = break ";" .
  Statement = continue ";" .
  */
statement: assign T_SEMICOLON
    { $$ = $1; }
    | method_call T_SEMICOLON
    { $$ = $1; }
    | T_IF T_LPAREN expr T_RPAREN block T_ELSE block
    { $$ = new IfStmtAST($3, (BlockAST *)$5, (BlockAST *)$7); }
    | T_IF T_LPAREN expr T_RPAREN block
    { $$ = new IfStmtAST($3, (BlockAST *)$5, NULL); }
    | T_WHILE T_LPAREN expr T_RPAREN block
    { $$ = new WhileStmtAST($3, (BlockAST *)$5); }
    | T_FOR T_LPAREN assign_comma_list T_SEMICOLON expr T_SEMICOLON assign_comma_list T_RPAREN block
    { $$ = new ForStmtAST((decafStmtList *)$3, $5, (decafStmtList *)$7, (BlockAST *)$9); }
    | T_RETURN T_LPAREN expr T_RPAREN T_SEMICOLON
    { $$ = new ReturnStmtAST($3); }
    | T_RETURN T_LPAREN T_RPAREN T_SEMICOLON
    { $$ = new ReturnStmtAST(NULL); }
    | T_RETURN T_SEMICOLON
    { $$ = new ReturnStmtAST(NULL); }
    | T_BREAK T_SEMICOLON
    { $$ = new BreakStmtAST(); }
    | T_CONTINUE T_SEMICOLON
    { $$ = new ContinueStmtAST(); }
    | block
    { $$ = $1; }
    ;


assign: T_ID T_ASSIGN expr
    { //Value *V = $3->Codegen();
      if (Maxx) cout << "AssignVar AST" << endl;
      $$ = new AssignVarAST(*$1, $3 );
		delete $1; }
    | T_ID T_LSB expr T_RSB T_ASSIGN expr
    { $$ = new AssignArrayLocAST(*$1, $3, $6); delete $1; }
    ;

  /* MethodCall = identifier "(" [ { MethodArg }+, ] ")" . */
method_call: T_ID T_LPAREN method_arg_list T_RPAREN
    { $$ = new MethodCallAST(*$1, (decafStmtList *)$3);
		delete $1; }
    | T_ID T_LPAREN T_RPAREN
    { $$ = new MethodCallAST(*$1, (decafStmtList *)NULL);
		delete $1; }
    ;

method_arg_list: method_arg
    { decafStmtList *slist = new decafStmtList(); slist->push_front($1); $$ = slist; }
    | method_arg T_COMMA method_arg_list
    { decafStmtList *slist = (decafStmtList *)$3; slist->push_front($1); $$ = slist; }
    ;

  /* MethodArg  = Expr | string_lit . */
method_arg: expr
    { $$ = $1; }
    | T_STRINGCONSTANT
    { $$ = new StringConstAST(*$1); delete $1; }
    ;

assign_comma_list: assign
    { decafStmtList *slist = new decafStmtList(); slist->push_front($1); $$ = slist; }
    | assign T_COMMA assign_comma_list
    { decafStmtList *slist = (decafStmtList *)$3; slist->push_front($1); $$ = slist; }
    ;

  /*
  Expr = identifier .
  Expr = MethodCall .
  Expr = Constant .
  Expr = Expr BinaryOperator Expr .
  Expr = UnaryOperator Expr .
  Expr = "(" Expr ")" .
  Expr = identifier "[" Expr "]" .
  */
rvalue: T_ID
    { if (Maxx) cout << "rvalue created " << endl;
      $$ = new VariableExprAST(*$1); delete $1; }
    | T_ID T_LSB expr T_RSB
    { $$ = new ArrayLocExprAST(*$1, $3); delete $1; }
    ;

expr: rvalue
    { $$ = $1; }
    | method_call
    { $$ = $1; }
    | constant
    { $$ = $1; }
    | expr T_PLUS expr
    { $$ = new BinaryExprAST(T_PLUS, $1, $3); }
    | expr T_MINUS expr
    { $$ = new BinaryExprAST(T_MINUS, $1, $3); }
    | expr T_MULT expr
    { $$ = new BinaryExprAST(T_MULT, $1, $3); }
    | expr T_DIV expr
    { $$ = new BinaryExprAST(T_DIV, $1, $3); }
    | expr T_LEFTSHIFT expr
    { $$ = new BinaryExprAST(T_LEFTSHIFT, $1, $3); }
    | expr T_RIGHTSHIFT expr
    { $$ = new BinaryExprAST(T_RIGHTSHIFT, $1, $3); }
    | expr T_MOD expr
    { $$ = new BinaryExprAST(T_MOD, $1, $3); }
    | expr T_LT expr
    { $$ = new BinaryExprAST(T_LT, $1, $3); }
    | expr T_GT expr
    { $$ = new BinaryExprAST(T_GT, $1, $3); }
    | expr T_LEQ expr
    { $$ = new BinaryExprAST(T_LEQ, $1, $3); }
    | expr T_GEQ expr
    { $$ = new BinaryExprAST(T_GEQ, $1, $3); }
    | expr T_EQ expr
    { $$ = new BinaryExprAST(T_EQ, $1, $3); }
    | expr T_NEQ expr
    { $$ = new BinaryExprAST(T_NEQ, $1, $3); }
    | expr T_AND expr
    { $$ = new BinaryExprAST(T_AND, $1, $3); }
    | expr T_OR expr
    { $$ = new BinaryExprAST(T_OR, $1, $3); }
    | T_MINUS expr %prec UMINUS
    { $$ = new UnaryExprAST(T_MINUS, $2); }
    | T_NOT expr
    { $$ = new UnaryExprAST(T_NOT, $2); }
    | T_LPAREN expr T_RPAREN
    { $$ = $2; }
    ;

  /* Constant = ( int_lit | char_lit | BoolConstant ) . */
constant: T_INTCONSTANT
    { $$ = new NumberExprAST($1); }
    | T_CHARCONSTANT
    { $$ = new NumberExprAST($1); }
    | bool_constant
    { $$ = $1; }
    ;

  /* BoolConstant = ( true | false ) . */
bool_constant: T_TRUE
    { $$ = new BoolExprAST(true); }
    | T_FALSE
    { $$ = new BoolExprAST(false); }
    ;

%%


/***********/
/**CODEGEN**/
/***********/

Value* TypedSymbolListAST::Codegen() {
	AllocaInst *Alloca;
	while(arglist.size() != 0){
		if(retrieve_front_type()=="IntType"){
			Alloca = Builder.CreateAlloca(Type::getInt32Ty(getGlobalContext()), nullptr, retrieve_front());
			descriptor* AlreadyDeclared = symtable->access_symtbl(retrieve_front());
			if(AlreadyDeclared != NULL && AlreadyDeclared->Val != NULL){
				Builder.CreateStore(symtable->access_symtbl(retrieve_front())->Val, Alloca);
			}
		}
		else if(retrieve_front_type()=="BoolType"){
			Alloca = Builder.CreateAlloca(Type::getInt1Ty(getGlobalContext()), nullptr, retrieve_front());
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
		arglist.pop_front();
	}
}

Value* NumberExprAST::Codegen() {
	return ConstantInt::get(getGlobalContext(), APInt(32, Val));
}

Value* StringConstAST::Codegen() {
	llvm::GlobalVariable *GS = Builder.CreateGlobalString(StringConst, "globalstring");
	return Builder.CreateConstGEP2_32(GS->getValueType(), GS, 0, 0, "cast");
}

Value* BoolExprAST::Codegen() {
	return ConstantInt::get(getGlobalContext(), APInt(1, Val));
}

Value* VariableExprAST::Codegen() {
  descriptor* access = symtable->access_symtbl(Name);
	if( access != NULL ){
		Value *V = access->mem_loca;
		return Builder.CreateLoad(V,Name);
	}
	else{
		throw runtime_error("VariableExprAST: This variable has not been defined inside the Symbol Table");
	}
}

  Value* MethodCallAST::Codegen() {
Function *CallFunc = FunctionProtos[Name];
  if (CallFunc == 0){
    throw runtime_error("could not find the function requested.");
  }

  vector<Value*> FuncArgs;
  if(Args != NULL){ // An empty DecafStmtList is registered as NULL
		if(Args->size() != 0){
			FuncArgs = Args->VectorOfArgs();
		}
	}
  else{
    if (Maxx) cout << "Args is NULL" << endl;
  }
//check if the function has a return type void
Value *V;
if (Args != NULL){
  V = Args->Codegen();
}
if(Name == "print_int" && V->getType()->isIntegerTy(1)){

  V = Builder.CreateZExt(V, Builder.getInt32Ty(), "zexttmp");
  return Builder.CreateCall(CallFunc, V, "calltmp");
}
  bool isVoid = CallFunc->getReturnType()->isVoidTy();
  if (Maxx) cout << "Builder createCall " << Name << endl;
	return Builder.CreateCall(FunctionProtos[Name], FuncArgs, isVoid ? "" : "calltmp");

	}

Value* BinaryExprAST::Codegen() {
	Value *L = LHS->Codegen();
  Value *R = RHS->Codegen();
  if (Maxx) cout << "Binary Expr AST. Op = " << Op << endl;
  if (L == 0 || R == 0) return 0;

  switch (Op) {
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

Value* UnaryExprAST::Codegen() {
  if (Maxx) cout << "Binary Expr AST. Op = " << Op << endl;
  Value* itemp = Expr->Codegen();
  switch (Op){
    case 286: return Builder.CreateNeg(itemp, "uminustmp");
		case 290: return Builder.CreateNot(itemp, "notstmp");
    default: throw runtime_error("what operator is this? never heard of it.");
  }
  //return ConstantInt::get(getGlobalContext(), APInt(32, Val));
}

Value* AssignVarAST::Codegen() {
	descriptor* temp = NULL;
  if (Maxx) cout << "In AssignVarAST Codegen()" << endl;
  Value *V;
  if (Expr != NULL){
    V = Expr->Codegen();
  }
  if (Maxx) cout << "Expr->Codegen() OK!" << endl;
  setValue(V);
  if (Maxx) cout << "SetValue(V) OK!" << endl;
	temp = symtable->access_symtbl(Name);
	if(temp == NULL){throw runtime_error("This variable has not been declared.");}
	llvm::AllocaInst* Alloca = temp->mem_loca;
  if (Maxx) cout << "assign var check OK" << endl;
	Builder.CreateStore(Val, Alloca);
  if (Maxx) cout << "AssignVar Builder.CreateStore OK" << endl;
}

Value* AssignArrayLocAST::Codegen() {}

Value* ArrayLocExprAST::Codegen() {}

Value* BlockAST::Codegen() {
	  symtable->new_symtbl();
		Vars->Codegen();
		Statements->Codegen();
		symtable->remove_symtbl();
}

Value* MethodBlockAST::Codegen() {
		// The new symbol table is created and destroyed inside of MethodDecl
    Vars->Codegen();
    if (Maxx) cout << "In MethodBlock Vars->Codegen() OK" << endl;
    Statements->Codegen();
    if (Maxx) cout << "In MethodBlock Statements->Codegen() OK" << endl;
  }

Value* IfStmtAST::Codegen() {}

Value* WhileStmtAST::Codegen() {}

Value* ForStmtAST::Codegen() {}

Value* ReturnStmtAST::Codegen() {}

Value* BreakStmtAST::Codegen() {}

Value* ContinueStmtAST::Codegen() {}

Value* MethodDeclAST::Codegen() {
	symtable->new_symtbl();
  if (Maxx) cout << "In MethodDecl codegen() OK" << endl;
  if (Name == "main"){
    gen_main_def();
  }
  else{
	//	string FunctionType = TyString(ReturnType);
		FunctionArgs->FillOutArgs(NeededForFunctionArgs, FunctionArgsName);
		FunctionProtos[Name] = gen_function_def(NeededForFunctionArgs, Name,/* FunctionType, */FunctionArgsName, ReturnType); // NeededForFunctionArgs & FunctionArgsName is passed by reference, so we don't need to empty it
    FunctionArgs->Codegen();
  }
  Block->Codegen();
	symtable->remove_symtbl();
	Builder.ClearInsertionPoint();
}

Value* AssignGlobalVarAST::Codegen() {}

Value* FieldDecl::Codegen() {}

Value* FieldDeclListAST::Codegen() {}

Value* PackageAST::Codegen() {}

Value* ExternAST::Codegen() {
	if(Name == "print_int"){
		llvm::Function* temp = gen_print_int_def();
		FunctionProtos["print_int"] = temp;
	}
	else if(Name == "print_string"){
		llvm::Function* temp = gen_print_string_def();
		FunctionProtos["print_string"] = temp;
	}
	else if(Name == "read_int"){
		Function* temp = gen_read_int_def();
		FunctionProtos["read_int"] = temp;
	}
	else{
		throw runtime_error("Extern doesn't recognise this function call.");
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
