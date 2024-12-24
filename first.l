%{
#include<stdio.h>    
%}

%%
if|else|printf {printf("%s is a keyword\n",yytext);}
[0-9]+ {printf("%s is a number\n",yytext);}
[a-z,A-Z]+ {printf("%s is a word\n",yytext);}
[.] {printf("%s is unknow character\n",yytext);}
%%

int main()
{
    printf("\nEnter string:\n");
    yylex();
    return 0;
}

int yywrap()
{
    return 1;
}