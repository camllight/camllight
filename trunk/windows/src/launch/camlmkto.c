#include <stdio.h>
#include "../version.h"
#include "driver.h"

char * camlrun = "camlrun.exe";
char * stdlib;
int linkalso = 1;
stringlist includes = NULL;
stringlist linkopt = NULL;
stringlist linkfiles = NULL;
char * output = "camltop.exe";
int custom = 0;
stringlist ccfiles = NULL;
stringlist ccopt = NULL;
stringlist perv = NULL;

#ifdef USE_GCC
char * cc_comp = "gcc";
char * ext_obj = ".o";
char * ext_lib = ".a";
#else
char * cc_comp = "cl";
char * ext_obj = ".obj";
char * ext_lib = ".lib";
#endif

char * syst_perv[] = {
   "baltree","bool","char","eq","exc","fchar","filename","float","format","fstring","fvect",
   "gc","genlex","hashtbl","int","io","iparsing","lexing","list","map","obj","pair","parsing",
   "printexc","printf","queue","random","ref","set","sort","stack","stream","string",
   "toplevel","vect","big_int","nat","num","ratio","arstatus",
   NULL };

main(argc, argv)
     int argc;
     char ** argv;
{
  int i;
  char * a;
  extern char * getenv();
  int status;
  char * lang;
  
  stdlib = getenv("CAMLLIB");
  if (stdlib == NULL) {
    fprintf(stderr, "Variable CAMLLIB is undefined.\n");
    exit(2);
  }

  lang = getenv("LANG");
  if (lang != NULL) {
    linkopt = add2(linkopt, "-lang", lang);
  }

  for (i = 0; syst_perv[i] != NULL; i++) perv = add(perv, syst_perv[i]);

  for (i = 1; i < argc; i++) {
    a = argv[i];
    if (suffix(a, ".zo")) {
      linkfiles = add(linkfiles, a);
	  perv = add(perv, modulename(a));
    } else
    if (suffix(a, ".zi")) {
	  perv = add(perv, modulename(a));
    } else
    if (eq(a, "-I") || eq(a, "-include")) {
      includes = add2(includes, "-I", argv[++i]);
    } else
    if (eq(a, "-o") || eq(a, "-exec")) {
      output = argv[++i];
    } else
    if (eq(a, "-stdlib")) {
      stdlib = argv[++i];
    } else
    if (eq(a, "-custom")) {
      custom = 1;
    } else
    if (suffix(a, ".c")) {
	  status = execute(NOTERM, cc_comp,
	  		  		   STR, "-c",
			  		   STR, strconc("-I", stdlib),
			  		   LST, ccopt,
			  		   STR, a,
			  		   END);
      if (status != 0) exit(status);
	  ccfiles = add(ccfiles, change_suffix(a, ext_obj));
    } else
    if (suffix(a, ext_obj) || suffix(a, ext_lib)
#ifdef USE_GCC
		|| prefix(a, "-l")
#endif
       ) {    
	  ccfiles = add(ccfiles, a);
    } else
    if (eq(a, "-ccopt")) {
      ccopt = add(ccopt, argv[++i]);
    } else
    if (prefix(a, "-")) {
      fprintf(stderr, "Unknown option \"%s\", ignored\n", a);
    } else {
      fprintf(stderr, "I don't know what to do with file \"%s\", ignored\n",
              a);
    }
  }
  {
  static char require[] = "camlXXXX.req";
  static char temp_out[] = "camlXXXX.byt";
  tempname(require);
  tempname(temp_out);
  redirect_stdout(require);
  status = execute(NOTERM, camlrun,
  				   STR, strconc(stdlib, "\\provide"),
				   STR, "-stdlib",
				   STR, stdlib,
				   LST, includes,
				   LST, perv,
				   END);
  end_redirect_stdout();
  if (status != 0) exit(status);

  if (!custom) {
    status = execute(NOTERM, camlrun,
					 STR, strconc(stdlib, "\\camllink"),
					 STR, "-stdlib", STR, stdlib,
					 STR, "-require", STR, require,
					 STR, "-exec", STR, temp_out,
					 STR, "-g",
					 LST, linkopt,
					 LST, includes,
					 STR, "stdlib.zo",
					 LST, linkfiles,
					 STR, "toplib.zo",
					 END);
    unlink(require);
    if (status != 0) exit(status);
	status = execute(NOTERM, camlrun,
					 STR, strconc(stdlib, "\\expunge"),
					 STR, temp_out,
					 STR, output,
					 LST, perv,
					 END);
    unlink(temp_out);
    exit(status);

  } else {

    static char expunged_out[] = "expnXXXX.byt";
    static char prim_file[] = "primXXXX.c";
	static char prim_file_obj[] = "primXXXX.obj";
    tempname(expunged_out);
    tempname(prim_file);
	strncpy(prim_file_obj, prim_file, 8);
    status = execute(NOTERM, camlrun,
					 STR, strconc(stdlib, "\\camllink"),
					 STR, "-stdlib", STR, stdlib,
					 STR, "-custom", STR, prim_file,
					 STR, "-require", STR, require,
					 STR, "-exec", STR, temp_out,
					 STR, "-g",
					 LST, linkopt,
					 LST, includes,
					 STR, "stdlib.zo",
					 LST, linkfiles,
					 STR, "toplib.zo",
					 END);
    if (status != 0) goto end;
	status = execute(NOTERM, camlrun,
					 STR, strconc(stdlib, "\\expunge"),
					 STR, temp_out,
					 STR, expunged_out,
					 LST, perv,
					 END);
    if (status != 0) goto end;
	status = execute(NOTERM, cc_comp,
					 STR, strconc("-I", stdlib),
					 STR, "-o", STR, output,
					 LST, ccopt,
					 STR, prim_file,
					 LST, ccfiles,
					 STR, strconc(stdlib, strconc("\\libcaml", ext_lib)),
					 END);
    if (status != 0) goto end;
    status = append(expunged_out, output);
  end:
    unlink(require);
    unlink(temp_out);
    unlink(expunged_out);
    unlink(prim_file);
#ifndef USE_GCC
	unlink(prim_file_obj);
#endif
    if (status != 0) unlink(output);
    exit(status);
  }
  }
  return 0;
}
