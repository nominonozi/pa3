%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

extern char *curr_filename;

/* Locations */
#define YYLTYPE int              
#define cool_yylloc curr_lineno  
extern int node_lineno;          

#define YYLLOC_DEFAULT(Current, Rhs, N)         \
Current = Rhs[1];                             \
node_lineno = Current;

#define SET_NODELOC(Current)  \
node_lineno = Current;

void yyerror(char *s);        
extern int yylex();           

Program ast_root;	      
Classes parse_results;        
int omerrs = 0;               
%}

%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;          /* 单个特征：方法/属性 */
  Features features;        /* 特征列表 */
  Formal formal;            /* 单个形式参数 */
  Formals formals;          /* 形式参数列表 */
  Case case_;               /* 单个case分支 */
  Cases cases;              /* case分支列表 */
  Expression expression;    /* 单个表达式 */
  Expressions expressions;  /* 表达式列表 */
  char *error_msg;
}

/* Token 声明（保持不变） */
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/* 修复非终结符类型声明：明确「单个」与「列表」的区别 */
%type <program> program
%type <classes> class_list
%type <class_> class
%type <feature> feature       /* 单个特征：返回 Feature */
%type <features> feature_list /* 特征列表：返回 Features */
%type <formal> formal         /* 单个参数：返回 Formal */
%type <formals> formal_list   /* 参数列表：返回 Formals */
%type <expression> expr let_expr /* 单个表达式：返回 Expression */
%type <expressions> expr_list expr_block_list /* 表达式列表：返回 Expressions */
%type <case_> case_branch     /* 单个case分支：返回 Case */
%type <cases> case_list       /* case分支列表：返回 Cases */

/* 运算符优先级与结合性（保持不变） */
%nonassoc IN
%right ASSIGN
%right NOT
%nonassoc LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%left '@'
%left '.'

%%
/* 程序入口规则（保持不变） */
program	: class_list	{ @$ = @1; SET_NODELOC(@1); ast_root = program($1); }
        | error ';'     { @$ = @1; SET_NODELOC(@1); ast_root = program(nil_Classes()); }
;

/* 类列表规则（保持不变） */
class_list
: class			{ $$ = single_Classes($1); parse_results = $$; }
| class_list class	{ $$ = append_Classes($1, single_Classes($2)); parse_results = $$; }
| class_list error ';'  { @$ = @1; SET_NODELOC(@1); $$ = $1; }
;

/* 类定义规则（保持不变） */
class	: CLASS TYPEID '{' feature_list '}' ';'
        { @$ = @1; SET_NODELOC(@1);
          $$ = class_($2, idtable.add_string("Object"), $4, stringtable.add_string(curr_filename)); 
        }
        | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
        { @$ = @1; SET_NODELOC(@1);
          $$ = class_($2, $4, $6, stringtable.add_string(curr_filename)); 
        }
        | CLASS error '{' feature_list '}' ';'
        { @$ = @1; SET_NODELOC(@1);
          $$ = class_(idtable.add_string("ErrorClass"), idtable.add_string("Object"), 
                      $4, stringtable.add_string(curr_filename)); 
        }
;

/* 特征列表规则：接收「单个特征」构建列表（修复参数类型） */
feature_list
: /* 空列表 */
  { $$ = nil_Features(); }
| feature_list feature ';'  /* $2 是单个特征（Feature），符合 single_Features 参数要求 */
  { $$ = append_Features($1, single_Features($2)); }
| feature_list error ';'    
  { @$ = @1; SET_NODELOC(@1); $$ = $1; }
;

/* 单个特征规则：返回 Feature（修复返回值类型） */
feature
: /* 方法定义：返回单个方法（Feature） */
  OBJECTID '(' formal_list ')' ':' TYPEID '{' expr '}'
  { @$ = @1; SET_NODELOC(@1);
    $$ = method($1, $3, $6, $8); /* method() 返回 Feature，匹配 %type <feature> */
  }
| /* 属性定义：返回单个属性（Feature） */
  OBJECTID ':' TYPEID
  { @$ = @1; SET_NODELOC(@1);
    $$ = attr($1, $3, no_expr()); /* attr() 返回 Feature，匹配 %type <feature> */
  }
| OBJECTID ':' TYPEID ASSIGN expr
  { @$ = @1; SET_NODELOC(@1);
    $$ = attr($1, $3, $5); /* attr() 返回 Feature，匹配 %type <feature> */
  }
;

/* 形式参数列表（保持不变） */
formal_list
: /* 空列表 */
  { $$ = nil_Formals(); }
| formal
  { $$ = single_Formals($1); }
| formal_list ',' formal
  { $$ = append_Formals($1, single_Formals($3)); }
;

/* 单个形式参数（保持不变） */
formal
: OBJECTID ':' TYPEID
  { @$ = @1; SET_NODELOC(@1);
    $$ = formal($1, $3); 
  }
;

/* 表达式规则（保持不变，依赖正确的类型声明） */
expr
: INT_CONST
  { @$ = @1; SET_NODELOC(@1); $$ = int_const($1); }
| STR_CONST
  { @$ = @1; SET_NODELOC(@1); $$ = string_const($1); }
| BOOL_CONST
  { @$ = @1; SET_NODELOC(@1); $$ = bool_const($1); }
| OBJECTID
  { @$ = @1; SET_NODELOC(@1); $$ = object($1); }
| '(' expr ')'
  { @$ = @1; SET_NODELOC(@1); $$ = $2; }
| '~' expr
  { @$ = @1; SET_NODELOC(@1); $$ = neg($2); }
| NOT expr
  { @$ = @1; SET_NODELOC(@1); $$ = comp($2); }
| ISVOID expr
  { @$ = @1; SET_NODELOC(@1); $$ = isvoid($2); }
| expr '+' expr
  { @$ = @2; SET_NODELOC(@2); $$ = plus($1, $3); }
| expr '-' expr
  { @$ = @2; SET_NODELOC(@2); $$ = sub($1, $3); }
| expr '*' expr
  { @$ = @2; SET_NODELOC(@2); $$ = mul($1, $3); }
| expr '/' expr
  { @$ = @2; SET_NODELOC(@2); $$ = divide($1, $3); }
| expr '<' expr
  { @$ = @2; SET_NODELOC(@2); $$ = lt($1, $3); }
| expr LE expr
  { @$ = @2; SET_NODELOC(@2); $$ = leq($1, $3); }
| expr '=' expr
  { @$ = @2; SET_NODELOC(@2); $$ = eq($1, $3); }
| OBJECTID ASSIGN expr
  { @$ = @2; SET_NODELOC(@2); $$ = assign($1, $3); }
| IF expr THEN expr ELSE expr FI
  { @$ = @1; SET_NODELOC(@1); $$ = cond($2, $4, $6); }
| WHILE expr LOOP expr POOL
  { @$ = @1; SET_NODELOC(@1); $$ = loop($2, $4); }
/* 修复代码块：expr_block_list 是 Expressions，符合 block() 参数要求 */
| '{' expr_block_list '}'
  { @$ = @1; SET_NODELOC(@1); $$ = block($2); }
| LET let_expr
  { @$ = @1; SET_NODELOC(@1); $$ = $2; }
| expr '.' OBJECTID '(' expr_list ')'
  { @$ = @2; SET_NODELOC(@2); $$ = dispatch($1, $3, $5); }
| OBJECTID '(' expr_list ')'
  { @$ = @1; SET_NODELOC(@1); 
    $$ = dispatch(object(idtable.add_string("self")), $1, $3); 
  }
| expr '@' TYPEID '.' OBJECTID '(' expr_list ')'
  { @$ = @3; SET_NODELOC(@3); $$ = static_dispatch($1, $3, $5, $7); }
| NEW TYPEID
  { @$ = @1; SET_NODELOC(@1); $$ = new_($2); }
| CASE expr OF case_list ESAC
  { @$ = @1; SET_NODELOC(@1); $$ = typcase($2, $4); }
| error ';'
  { @$ = @1; SET_NODELOC(@1); $$ = no_expr(); }
| error ')'
  { @$ = @1; SET_NODELOC(@1); $$ = no_expr(); }
;

/* Let 表达式（保持不变，依赖正确的类型声明） */
let_expr
: OBJECTID ':' TYPEID ASSIGN expr ',' let_expr
  { @$ = @1; SET_NODELOC(@1);
    $$ = let($1, $3, $5, $7); 
  }
| OBJECTID ':' TYPEID ',' let_expr
  { @$ = @1; SET_NODELOC(@1);
    $$ = let($1, $3, no_expr(), $5); 
  }
| OBJECTID ':' TYPEID ASSIGN expr IN expr
  { @$ = @1; SET_NODELOC(@1);
    $$ = let($1, $3, $5, $7); 
  }
| OBJECTID ':' TYPEID IN expr
  { @$ = @1; SET_NODELOC(@1);
    $$ = let($1, $3, no_expr(), $5); 
  }
| error IN expr
  { @$ = @3; SET_NODELOC(@3); $$ = $3; }
;

/* 修复代码块表达式列表：返回 Expressions（匹配 block() 参数） */
expr_block_list
: expr ';'  /* $1 是单个表达式（Expression），符合 single_Expressions 参数要求 */
  { $$ = single_Expressions($1); }
| expr_block_list expr ';'  /* $1 是列表（Expressions），$2 是单个表达式→转列表，符合 append 要求 */
  { $$ = append_Expressions($1, single_Expressions($2)); }
| expr_block_list error ';'  
  { @$ = @1; SET_NODELOC(@1); $$ = $1; }
;

/* 表达式列表（方法参数，保持不变） */
expr_list
: /* 空列表 */
  { $$ = nil_Expressions(); }
| expr  /* 单个表达式→转列表 */
  { $$ = single_Expressions($1); }
| expr_list ',' expr  /* 列表 + 单个表达式→转列表→拼接 */
  { $$ = append_Expressions($1, single_Expressions($3)); }
;

/* Case 分支列表（保持不变） */
case_list
: case_branch
  { $$ = single_Cases($1); }
| case_list case_branch
  { $$ = append_Cases($1, single_Cases($2)); }
;

/* 单个 Case 分支（保持不变） */
case_branch
: OBJECTID ':' TYPEID DARROW expr ';'
  { @$ = @1; SET_NODELOC(@1);
    $$ = branch($1, $3, $5); 
  }
| error ';'  
  { @$ = @1; SET_NODELOC(@1); 
    $$ = branch(idtable.add_string("ErrorVar"), idtable.add_string("Object"), no_expr()); 
  }
;

%%

/* 错误处理函数（保持不变） */
void yyerror(char *s)
{
  extern int curr_lineno;
  
  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
  << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;
  
  if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}
