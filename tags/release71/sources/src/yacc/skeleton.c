#include "defs.h"

char *header[] =
{
  "#open \"obj\";;",
  "#open \"parsing\";;",
  0
};

char *define_tables[] =
{
  "let yytables =",
  "  { actions=yyact;",
  "    transl=yytransl;",
  "    lhs=yylhs;",
  "    len=yylen;",
  "    defred=yydefred;",
  "    dgoto=yydgoto;",
  "    sindex=yysindex;",
  "    rindex=yyrindex;",
  "    gindex=yygindex;",
  "    tablesize=YYTABLESIZE;",
  "    table=yytable;",
  "    check=yycheck };;",
  0
};

write_section(section)
char *section[];
{
    register int i;
    register FILE *fp;

    fp = code_file;
    for (i = 0; section[i]; ++i)
    {
	++outline;
	fprintf(fp, "%s\n", section[i]);
    }
}
