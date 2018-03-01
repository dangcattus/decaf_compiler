%{
#include "decafcomp-defs.h"
#include "decafcomp.tab.h"
#include <cstring>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;

int lineno = 1;
int tokenpos = 1;

int char_int(char* d){ int sum = atoi(d); return(int) sum; };

/*used his code*/
string remove_newlines (string s) {
  string newstring;
  for (string::iterator i = s.begin(); i != s.end(); i++) {
    switch(*i) {
    case '\n':
      lineno += 1; tokenpos = 0;
      newstring.push_back('\\');
      newstring.push_back('n');
      break;
    case '(':
      newstring.push_back('\\');
      newstring.push_back('(');
      break;
    case ')':
      newstring.push_back('\\');
      newstring.push_back(')');
      break;
    default:
      newstring.push_back(*i);
    }
  }
  return newstring;
}

void process_ws() {
  tokenpos += yyleng;
  string lexeme(yytext);
  lexeme = remove_newlines(lexeme);
}


string *esc_char(string os){
	string* s = new string(os);
	s->erase(0, 1);
	s->pop_back();
	int i = s->find("\\n");
	if(i != string::npos){
		const char* replace = "\n";
		s->replace(s->find("\\n"), 2, replace);
	}
	i = s->find("\\t");
	if(i != string::npos){
		const char* replace = "\t";
		s->replace(s->find("\\t"), 2, replace);
	}
	i = s->find("\\v");
	if(i != string::npos){
		const char* replace = "\v";
		s->replace(s->find("\\v"), 2, replace);
	}
	i = s->find("\\r");
	if(i != string::npos){
		const char* replace = "\r";
		s->replace(s->find("\\r"), 2, replace);
	}
	i = s->find("\\a");
	if(i != string::npos){
		const char* replace = "\a";
		s->replace(s->find("\\a"), 2, replace);
	}
	i = s->find("\\f");
	if(i != string::npos){
		const char* replace = "\f";
		s->replace(s->find("\\f"), 2, replace);
	}
	i = s->find("\\b");
	if(i != string::npos){
		const char* replace = "\b";
		s->replace(s->find("\\b"), 2, replace);
	}
	i = s->rfind("\\\"");
	if(i != string::npos){
		const char* replace = "\"";
		s->replace(s->find("\\\""), 2, replace);
	}
	i = s->find("\\\\");
	if(i != string::npos){
		const char* replace = "\\";
		s->replace(s->find("\\\\"), 2, replace);
	}
	return s;
	/*
	const char* c = s->c_str();
	return c;
	*/
}

int conv_intconstant(const char *s) {
  if ((s[0] == '0') && (s[1] == 'x')) {
    int x;
    sscanf(s, "%x", &x);
    return x;
  } else {
    return atoi(s);
  }
}

%}

/*	Name defintions for tokens	*/
DIGIT                      (0[xX][0-9a-fA-F]+)|([0-9]+)
CHAR_NO_NL_SINGLEQUOTE     [^\\n']
ESCAPE_CHAR                [\\][nrtvfab\\\'\"]

%%	/*	Pattern definitions for all tokens	*/
\/\/[^\n]*\n               { process_ws(); } /* ignore comments */
extern                     { return T_EXTERN; }
package                    { return T_PACKAGE; }
void                       { yylval.sval = new string("VoidType"); return T_VOID;}
bool                       { yylval.sval = new string("BoolType"); return T_BOOLTYPE;}
if                         { return T_IF; }
else                       { return T_ELSE; }
for                        { return T_FOR; }
while                      { return T_WHILE;}
return                     { return T_RETURN;}
var												 { return T_VAR; }
false                      { yylval.number = 0; return T_FALSE; }
true                       { yylval.number = 1; return T_TRUE; }
int												 { yylval.sval = new string("IntType"); return T_INTTYPE; }
string                     { yylval.sval = new string("StringType"); return T_STRINGTYPE; }
func											 { return T_FUNC; }
{DIGIT}+									 { yylval.number = conv_intconstant(yytext); return T_INTCONSTANT; }
[']({CHAR_NO_NL_SINGLEQUOTE}|{ESCAPE_CHAR})['] { char escape_check = yytext[1]; char newvar = yytext[2]; if(escape_check == '\\'){if(newvar == 'r'){yylval.number = 13;}else if(newvar == 't'){yylval.number = 9;}else if(newvar == 'f'){yylval.number = 12;}else if(newvar == 'v'){yylval.number = 11;}else if(newvar == 'n'){yylval.number = 10;}else if(newvar == '\''){yylval.number = 39;}else if(newvar == 'b'){yylval.number = 8;}else if(newvar == 'a'){yylval.number = 7;}else if(newvar == '\\'){yylval.number = 92;}else if(newvar == '"'){yylval.number = 34;}else{yylval.number = atoi(yytext);}}else{char non_escape_val = yytext[1]; yylval.number = non_escape_val;} return T_CHARCONSTANT;}
;													 { return T_SEMICOLON; }
\=\=                       { yylval.sval = new string("Eq"); return T_EQ; }
\!\=											 { yylval.sval = new string("Neq"); return T_NEQ; }
\>\=                       { yylval.sval = new string("Geq"); return T_GEQ; }
>                          { yylval.sval = new string("Gt"); return T_GT; }
\<                         { yylval.sval = new string("Lt"); return T_LT; }
\<\=                       { yylval.sval = new string("Leq"); return T_LEQ;}
\=                         { return T_ASSIGN;}
\{                         { return T_LCB; }
\}                         { return T_RCB; }
\(                         { return T_LPAREN; }
\)                         { return T_RPAREN; }
\[                          { return T_LSB;}
\]                          { return T_RSB;}
func                       { return T_FUNC; }
,                          { return T_COMMA; }
continue									 { yylval.sval = new string("ContinueStmt"); return T_CONTINUE; }
break                      { yylval.sval = new string("BreakStmt"); return T_BREAK; }
% 										     { yylval.sval = new string("Mod"); return T_MOD;}
\|\|                       { yylval.sval = new string("Or"); return T_OR; }
&&                         { yylval.sval = new string("And"); return T_AND;}
\"([^\\"\n]|\\[nrtvfab\\'"])*\"		     { yylval.sval = esc_char(yytext); return T_STRINGCONSTANT; }
\/                         { yylval.sval = new string("Div"); return T_DIV; }
\*                         { yylval.sval = new string("Mult"); return T_MULT;}
\+                         { yylval.sval = new string("Plus"); return T_PLUS; }
\-                         { yylval.sval = new string("Minus"); return T_MINUS;}
!                          { yylval.sval = new string("Not"); return T_NOT;}
\<\<                       { yylval.sval = new string("Leftshift"); return T_LEFTSHIFT; }
\>\>											 { yylval.sval = new string("Rightshift"); return T_RIGHTSHIFT; }
[a-zA-Z\_][a-zA-Z\_0-9]*   { yylval.sval = new string(yytext); return T_ID; } /* note that identifier pattern must be after all keywords */
[\t\r\n\a\v\b ]+           { process_ws(); } /* ignore whitespace */
.                          { cerr << "Error: unexpected character in input" << endl; return -1; }

%%

int yyerror(const char *s) {
  cerr << lineno << ": " << s << " at char " << tokenpos << endl;
  return 1;
}
