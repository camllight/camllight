/* There is a clash on "Atom" macro (X11/mlvalues) */
/* tk.h must be included first */
#include <stdlib.h>
#include <unistd.h>

#include <tk.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>

/* Code part of the callback dispatcher */
static code_t handler_code = NULL;

/* Initialization: has to be called with the GLOBAL callback dispatcher */
value camltk_install_callback_handler(handler) /* ML */
  value handler;
{
  handler_code = Code_val(handler);
  return Atom(0);
}

/* Hack for calling Caml global function from C */
value camltk_dispatch_callback(arg)
     value arg;
{
  value clos;
  clos = alloc(2,Closure_tag);
  Code_val(clos) = handler_code;
  Env_val(clos) = Atom(0);
  callback(clos, arg);
}

/* Copy a list of strings from C to Caml */
value copy_string_list(argc, argv)
     int argc;
     char ** argv;
{
  value res;
  int i;
  Push_roots(r, 2);
#define oldres r[0]
#define str r[1]
  res = Atom(0);
  for (i = argc-1; i >= 0; i--) {
    oldres = res;
    str = copy_string(argv[i]);
    res = alloc(2, 1);
    Field(res, 0) = str;
    Field(res, 1) = oldres;
  }
  Pop_roots();
#undef oldres
#undef str
  return res;
}

char *string_to_c(s)
     value s;
{
#ifdef ANSI
  extern mlsize_t string_length(value);
#else
  extern mlsize_t string_length();
#endif
  int l = string_length(s);
  char *res = (char *)malloc(string_length(s) + 1);
  if (NULL == res) 
    raise_out_of_memory();
  else {
    bcopy(String_val(s),res,l);
    res[l] = '\0';
  }
  return res;
}



/* The Tcl command for evaluating callback in Caml */
int CamlCBCmd(clientdata, interp, argc, argv)
     ClientData clientdata;
     Tcl_Interp *interp;
     int argc;
     char *argv[];
{
  camltk_dispatch_callback(copy_string_list(argc,argv));
  /* Assume no result */
  /* Never fails (Caml would have raised an exception) */
  interp->result = "";
  return TCL_OK;
}

/* Note: raise_with_string WILL copy the error message */
void tk_error(errmsg)
     char *errmsg;
{
  raise_with_string(TCL_ERROR_EXN, errmsg);
}

/* 
   Dealing with signals: when a signal handler is defined in Caml,
   the actual execution of the signal handler upon reception of the
   signal is delayed until we are sure we are out of the GC.
   If a signal occurs during the MainLoop, we would have to wait
   the next event for the handler to be invoked.
*/

/* The following function will invoke a pending signal handler if any */
void invoke_pending_caml_signals (clientdata) 
     ClientData clientdata;
{
  enter_blocking_section();
  /* Rearm timer */
  Tk_CreateTimerHandler(100, invoke_pending_caml_signals, NULL);
  leave_blocking_section();
}


Tcl_Interp *cltclinterp = NULL;
static Tk_Window mainWindow;

#define RCNAME ".camltkrc"

/* Initialisation */
value camltk_opentk(display, name) /* ML */
     value display,name;
{
  /* Create an interpreter, dies if error */
  cltclinterp = Tcl_CreateInterp();
  /* Create the camlcallback command */
  Tcl_CreateCommand(cltclinterp,
		    "camlcb", CamlCBCmd, 
		    (ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
  /* Open main window */
  mainWindow = Tk_CreateMainWindow(cltclinterp,
				   String_val (display), /* screenname */
				   String_val(name), /* basename */
				   "Tk"  /* classname */
				   );
  if (NULL == mainWindow)
    tk_error(cltclinterp->result);
  
  Tk_GeometryRequest(mainWindow,200,200);
  if (Tcl_Init(cltclinterp) != TCL_OK)
    tk_error(cltclinterp->result);
  if (Tk_Init(cltclinterp) != TCL_OK)
    tk_error(cltclinterp->result);

  /* This is required by "unknown" and thus autoload */
  Tcl_SetVar(cltclinterp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
	     
  /* Load the traditional rc file */
  {
    char *home = getenv("HOME");
    if (home != NULL) {
      char *f = stat_alloc(strlen(home)+strlen(RCNAME)+2);
      f[0]='\0';
      strcat(f, home);
      strcat(f, "/");
      strcat(f, RCNAME);
      if (0 == access(f,R_OK)) 
	if (TCL_OK != Tcl_EvalFile(cltclinterp,f)) {
	  stat_free(f);
	  tk_error(cltclinterp->result);
	};
      stat_free(f);
    }
  }

  /* Initialise signal handling */
  Tk_CreateTimerHandler(100, invoke_pending_caml_signals, NULL);
  
  return Atom(0);
}

/*
 * Calling Tcl from Caml
 *   this version works on an arbitrary Tcl command
 */
value camltk_tcl_eval(str) /* ML */
value str; 
{
  int code;

  if (!cltclinterp) tk_error("Tcl/Tk not initialised");

  code = Tcl_Eval(cltclinterp,String_val(str));
  switch (code) {
  case TCL_OK:
    return copy_string (cltclinterp->result);
  case TCL_ERROR:
    tk_error(cltclinterp->result);
  default:  /* TCL_BREAK, TCL_CONTINUE, TCL_RETURN */
    tk_error("bad tcl result");
  }
}


/* 
 * Calling Tcl from Caml
 *   direct call, argument is TkArgs vect
  type TkArgs =
      TkToken of string
    | TkTokenList of TkArgs list		(* to be expanded *)
    | TkQuote of TkArgs 	                (* mapped to Tcl list *)
  ;;
 */

/* 
 * Compute the size of the argument (of type TkArgs). 
 * TkTokenList must be expanded,
 * TkQuote count for one.
 */
int argv_size(v)
value v;
{
  switch (Tag_val(v)) {
  case 0:			/* TkToken */
    return 1;
  case 1:			/* TkTokenList */
    { int n;
      value l;
      for (l=Field(v,0),n=0;Tag_val(l)==1;l=Field(l,1))
	n+=argv_size(Field(l,0));
      return n;
    }
  case 2:			/* TkQuote */
    return 1;
  }
}

/* 
 * Memory of allocated Tcl lists.
 */
static char *tcllists[64];
static int startfree = 0;
/* If size is lower, do not allocate */
static char *quotedargv[16];

/* Fill a preallocated vector arguments, doing expansion and all */
int fill_args (argv, where, v) 
char ** argv;
int where;
value v;
{
  switch (Tag_val(v)) {
  case 0:
    argv[where] = String_val(Field(v,0));
    return (where + 1);
  case 1:
    { value l;
      for (l=Field(v,0);Tag_val(l)==1;l=Field(l,1))
	where = fill_args(argv,where,Field(l,0));
      return where;
    }
  case 2:
    { char **tmpargv;
      int size = argv_size(Field(v,0));
      if (size < 16)
	tmpargv = &quotedargv[0];
      else
	tmpargv = (char **)stat_alloc((size + 1) * sizeof(char *));
      fill_args(tmpargv,0,Field(v,0));
      tmpargv[size] = NULL;
      argv[where] = Tcl_Merge(size,tmpargv);
      tcllists[startfree++] = argv[where]; /* so we can free it later */
      if (size >= 16) 
	stat_free((char *)tmpargv);
      return (where + 1);
    }
  }
}


value camltk_tcl_direct_eval(v) /* ML */
value v; 
{
  int i;
  int size;			/* size of argv */
  char **argv;
  int result;
  Tcl_CmdInfo info;
  int wherewasi,whereami;       /* positions in tcllists array */

  if (!cltclinterp) tk_error("Tcl/Tk not initialised");

  /* walk the array to compute final size for Tcl */
  for(i=0,size=0;i<Wosize_val(v);i++)
    size += argv_size(Field(v,i));

  /* one slot for NULL, one slot for "unknown" if command not found */
  argv = (char **)stat_alloc((size + 2) * sizeof(char *));

  wherewasi = startfree;

  /* Copy */
  {
    int where;			/*  */
    for(i=0, where=0;i<Wosize_val(v);i++)
      where = fill_args(argv,where,Field(v,i));
    argv[size] = NULL;
  }

  whereami = startfree;

  /* Eval */
  Tcl_ResetResult(cltclinterp);
  if (Tcl_GetCommandInfo(cltclinterp,argv[0],&info)) { /* command found */
    result = (*info.proc)(info.clientData,cltclinterp,size,argv);
  } else /* implement the autoload stuff */
    if (Tcl_GetCommandInfo(cltclinterp,"unknown",&info)) { /* unknown found */
      for (i = size; i >= 0; i--)
	argv[i+1] = argv[i];
      argv[0] = "unknown";
      result = (*info.proc)(info.clientData,cltclinterp,size+1,argv);
    } else { /* ah, it isn't there at all */
      result = TCL_ERROR;
      Tcl_AppendResult(cltclinterp, "Unknown command \"", argv[0], "\"", NULL);
    };
  /* Free the various things we allocated */
  stat_free((char *)argv);
  for (i=wherewasi; i<whereami; i++)
    free(tcllists[i]);
  startfree = wherewasi;
  
  switch (result) {
  case TCL_OK:
    return copy_string (cltclinterp->result);
  case TCL_ERROR:
    tk_error(cltclinterp->result);
  default:  /* TCL_BREAK, TCL_CONTINUE, TCL_RETURN */
    tk_error("bad tcl result");
  }
}

/* Parsing results */
value camltk_splitlist (v) /* ML */
     value v;
{
  int argc;
  char **argv;
  int result;

  if (!cltclinterp) tk_error("Tcl/Tk not initialised");
  
  result = Tcl_SplitList(cltclinterp,String_val(v),&argc,&argv);
  switch(result) {
  case TCL_OK:
   { value res = copy_string_list(argc,argv);
     free((char *)argv);	/* only one large block was allocated */
     return res;
   }
  case TCL_ERROR:
  default:
    tk_error(cltclinterp->result);
  }
}

/*
 * File descriptor callbacks
 * It's not possible to call non-global ML code, so use general 
 *  callback stuff for fd. 
 */

void FileProc(clientdata, mask)
     ClientData clientdata;
     int mask;
{
  static char *arg[3] = {"camlcb", NULL, NULL};

  arg[1] = (char *)clientdata;
  camltk_dispatch_callback(copy_string_list(2,arg));
}



value camltk_add_file_input(fd, cbid)    /* ML */
     value fd;
     value cbid;
{
  /* Make a copy of the cbid and put it in the CliendData */
  char *cbid_save = string_to_c(cbid);

  Tk_CreateFileHandler(Int_val(fd), TK_READABLE, 
		       FileProc, (ClientData) cbid_save);
  return Atom(0);
}

value camltk_rem_file_input(fd) /* ML */
     value fd;
{
  Tk_DeleteFileHandler(Int_val(fd));
  return Atom(0);
}

value camltk_tk_mainloop() /* ML */
{
  Tk_MainLoop();
  return Atom(0);
}

