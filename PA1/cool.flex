/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

inline void add_char_to_buf(char c) {
  if (string_buf_ptr == &string_buf[MAX_STR_CONST-1]) {
    cool_yylval.error_msg = "String constant too long";
  } else if (c == '\0'){
    cool_yylval.error_msg = "String contains null character";
  } else {
    *string_buf_ptr++ = c;
  }
}

%}

/*
 * Define names for regular expressions here.
 */

/*
 *key works
 */
DARROW          =>
ASSIGN          <-
LE              <=
ISVOID          isvoid
NOT             not
CLASS           class
ELSE            else
FI              fi
IF              if
IN              in
INHERITS        inherits
LET             let
LOOP            loop
POOL            pool
THEN            then
WHILE           while
CASE            case
ESAC            esac
NEW             new
OF              of

/*
 *digits, identifier, const, and operators
 */
DIGIT          [0-9]
OBJECTID       [a-z][a-zA-Z0-9_]*
TYPEID         [A-Z][a-zA-Z0-9_]*

FALSE          f[aA][lL][sS][eE]
TRUE           t[rR][uU][eE]

/*
 * whitespace
 */
WS       [ \t\r\v\f]+

%x COMMENTS
%x STRING_CONST
%option stack
%%

 /*
  *  Nested comments
  */
\-\-.*
\-\-.*[\n]  curr_lineno++;        
\(\*        yy_push_state(COMMENTS);
\*\)        {cool_yylval.error_msg = "Unmatched *)";return (ERROR);}
<COMMENTS>{
  \(\*      yy_push_state(COMMENTS);
  \*\)      yy_pop_state();
  <<EOF>>   {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "EOF in comment";
    return (ERROR);  
  }
  .
}

 /*
  *  The multiple-character operators.
  */
\.              {return yytext[0];}
@               {return yytext[0];}
~               {return yytext[0];}
(?i:{ISVOID})	{return (ISVOID);}
\*|\/           {return yytext[0];}
\+|\-           {return yytext[0];}
{LE}            {return (LE);}           
\<|\=           {return yytext[0];}
(?i:{NOT})	{return (NOT);}
{ASSIGN}	{return (ASSIGN);}
\(|\)|\{|\}|\,|\:|\;              {return yytext[0];}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{DARROW}		{return (DARROW);}
(?i:{CLASS})		{return (CLASS);}
(?i:{ELSE})		{return (ELSE);}
(?i:{FI})		{return (FI);}
(?i:{IF})		{return (IF);}
(?i:{IN})		{return (IN);}
(?i:{INHERITS})		{return (INHERITS);}
(?i:{LET})		{return (LET);}
(?i:{LOOP})		{return (LOOP);}
(?i:{POOL})		{return (POOL);}
(?i:{THEN})		{return (THEN);}
(?i:{WHILE})		{return (WHILE);}
(?i:{CASE})		{return (CASE);}
(?i:{ESAC})		{return (ESAC);}
(?i:{NEW})		{return (NEW);}
(?i:{OF})		{return (OF);}



 /*
  *  int, bool and  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
{FALSE}         {cool_yylval.boolean=0;return (BOOL_CONST);}
{TRUE}          {cool_yylval.boolean=1;return (BOOL_CONST);}

{DIGIT}+        {
  cool_yylval.symbol=inttable.add_string(yytext);
  return (INT_CONST);
}


\"      string_buf_ptr = string_buf; cool_yylval.error_msg = "";BEGIN(STRING_CONST);

<STRING_CONST>{\"        {BEGIN(INITIAL);
    if (cool_yylval.error_msg != "") {
      return (ERROR);
    }
    *string_buf_ptr = '\0';
    cool_yylval.symbol=stringtable.add_string(string_buf);
    return (STR_CONST);
  }
  
  \0   {cool_yylval.error_msg = "String contains null character";}

  \n        {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "Unterminated string constant";
    return (ERROR);
  }

  <<EOF>>   {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "‘EOF in string constant";
    return (ERROR);  
  }
  
  \\n  {add_char_to_buf('\n');}
  \\t  {add_char_to_buf('\t');}
  \\b  {add_char_to_buf('\b');}
  \\f  {add_char_to_buf('\f');}
  
  \\(.|\n)  {curr_lineno++;add_char_to_buf(yytext[1]);}
  
  [^\\\n\"\0]+        {
    char *yptr = yytext;
    
    while ( *yptr ) {
      add_char_to_buf(*yptr++);
    }
  }
}
 /*
  * identifier
  */

{OBJECTID}		{cool_yylval.symbol=idtable.add_string(yytext);return (OBJECTID);}
{TYPEID}		{cool_yylval.symbol=idtable.add_string(yytext);return (TYPEID);}


<*>\n              {curr_lineno++;}
<INITIAL><<EOF>>   {curr_lineno=1;return 0;}
<*>{WS}
 /*
  * defualt: fix me
  */
.                  {cool_yylval.error_msg = yytext;return (ERROR);}
%%
