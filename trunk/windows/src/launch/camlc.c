#include <stdio.h>
#include "../version.h"
#include "driver.h"

char * camlrun = "camlrun.exe";
char * stdlib;
int linkalso = 1;
stringlist includes = NULL;
stringlist compopt = NULL;
stringlist compfiles = NULL;
stringlist linkopt = NULL;
stringlist linkfiles = NULL;
char * linkout = "camlout.exe";
int custom = 0;
stringlist ccfiles = NULL;
stringlist ccopt = NULL;

#ifdef USE_GCC
char * cc_comp = "gcc";
char * ext_obj = ".o";
char * ext_lib = ".a";
#else
char * cc_comp = "cl";
char * ext_obj = ".obj";
char * ext_lib = ".lib";
#endif

extern int expand_diversions(int *ref_argc, char ***ref_argv);
     
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
  
  if (expand_diversions(&argc, &argv) == -1) return 2; 

  stdlib = getenv("CAMLLIB");
  if (stdlib == NULL) {
    fprintf(stderr, "Variable CAMLLIB is undefined.\n");
    exit(2);
  }

  lang = getenv("LANG");
  if (lang != NULL) {
    compopt = add2(compopt, "-lang", lang);
    linkopt = add2(linkopt, "-lang", lang);
  }

  for (i = 1; i < argc; i++) {
    a = argv[i];
    if (suffix(a, ".ml")) {
      compfiles = add(compfiles, a);
      linkfiles = add(linkfiles, a);
    } else
    if (suffix(a, ".mli")) {
      compfiles = add(compfiles, a);
    } else
    if (suffix(a, ".zo")) {
      linkfiles = add(linkfiles, a);
    } else
    if (eq(a, "-c")) {
      linkalso = 0;
    } else
    if (eq(a, "-I") || eq(a, "-include")) {
      includes = add2(includes, "-I", argv[++i]);
    } else
    if (eq(a, "-O") || eq(a, "-open")) {
      compopt = add2(compopt, "-O", argv[++i]);
    } else
    if (eq(a, "-i")) {
      compopt = add(compopt, "-i");
    } else
    if (eq(a, "-g") || eq(a, "-debug")) {
      compopt = add(compopt, "-g");
      linkopt = add(linkopt, "-g");
    } else
    if (eq(a, "-o") || eq(a, "-exec")) {
      linkout = argv[++i];
    } else
    if (eq(a, "-W")) {
      compopt = add(compopt, "-W");
    } else
    if (eq(a, "-stdlib")) {
      stdlib = argv[++i];
    } else
    if (eq(a, "-v") || eq(a, "-version")) {
      printf("The Caml Light system for MSDOS/Windows, version %s\n",
             VERSION);
      printf("  (standard library from %s)\n", stdlib);
      execute(NOTERM, camlrun, STR, "-V", END);
	  execute(NOTERM, camlrun,
	  	      STR, strconc(stdlib, "\\camlcomp"),
	  	      STR, "-version",
	  	      END);
	  execute(NOTERM, camlrun,
	  	      STR, strconc(stdlib, "\\camllink"),
	  	      STR, "-version",
	  	      END);
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
  if (compfiles != NULL) {
    status = execute(NOTERM, camlrun,
					 STR, strconc(stdlib, "\\camlcomp"),
					 STR, "-stdlib",
					 STR, stdlib,
					 LST, includes,
					 LST, compopt,
					 LST, compfiles,
					 END);
    if (status != 0) exit(status);
  }
  if (linkalso && linkfiles != NULL) {
    if (!custom) {
	  execute(TERMINATE, camlrun,
	  		  STR, strconc(stdlib, "\\camllink"),
			  STR, "-stdlib",
			  STR, stdlib,
			  LST, includes,
			  LST, linkopt,
			  STR, "-exec",
			  STR, linkout,
			  STR, strconc(stdlib, "\\stdlib.zo"),
			  LST, linkfiles,
			  END);
    } else {
      static char temp_out[] = "camlXXXX.byt";
      static char prim_file[] = "primXXXX.c";
	  static char prim_file_obj[] = "primXXXX.obj";
      tempname(temp_out);
      tempname(prim_file);
	  strncpy(prim_file_obj, prim_file, 8);

	  status = execute(NOTERM, camlrun,
			  		   STR, strconc(stdlib, "\\camllink"),
					   STR, "-stdlib",
					   STR, stdlib,
					   STR, "-custom",
					   STR, prim_file,
					   LST, includes,
					   LST, linkopt,
					   STR, "-exec",
					   STR, temp_out,
					   STR, strconc(stdlib, "\\stdlib.zo"),
					   LST, linkfiles,
					   END);
      if (status != 0) goto end;
	  status = execute(NOTERM, cc_comp,
	  				   STR, strconc("-I", stdlib),
					   STR, "-o",
					   STR, linkout,
					   LST, ccopt,
					   STR, prim_file,
					   LST, ccfiles,
					   STR, strconc(stdlib, strconc("\\libcaml", ext_lib)),
					   END);
      if (status != 0) goto end;
      status = append(temp_out, linkout);
    end:
      unlink(temp_out);
      unlink(prim_file);
#ifndef USE_GCC
	  unlink(prim_file_obj);
#endif
      if (status != 0) unlink(linkout);
      exit(status);
    }
  }
  return 0;
}
