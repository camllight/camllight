#include <stdio.h>
#include "driver.h"

char * stdlib;
stringlist opt = NULL;

main(argc, argv)
     int argc;
     char ** argv;
{
  int i;
  char * a;
  extern char * getenv();
  char * cmd;
  int status;
  char * lang;

  stdlib = getenv("CAMLLIB");
  if (stdlib == NULL) {
    fprintf(stderr, "Variable CAMLLIB is undefined.\n");
    exit(2);
  }

  lang = getenv("LANG");
  if (lang != NULL) {
    opt = add2(opt, "-lang", lang);
  }

  for (i = 1; i < argc; i++) {
    a = argv[i];
    if (eq(a, "-g") || eq(a, "-debug")) {
      opt = add(opt, "-g");
    } else
    if (eq(a, "-I") || eq(a, "-include") || eq(a, "-O") || eq(a, "-open")) {
      opt = add2(opt, a, argv[++i]);
    } else
    if (eq(a, "-stdlib")) {
      stdlib = argv[++i];
    } else
    if (prefix(a, "-")) {
      fprintf(stderr, "Unknown option \"%s\", ignored\n", a);
    } else {
      execute(TERMINATE, a, STR, "-stdlib", STR, stdlib, LST, opt, END);
    }
  }
  execute(TERMINATE, "camlrun.exe",
  		  STR, strconc(stdlib, "\\camltop"),
		  STR, "-stdlib",
		  STR, stdlib,
		  LST, opt,
		  END);
  return 0;
}
