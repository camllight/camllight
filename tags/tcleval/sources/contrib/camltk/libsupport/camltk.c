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


static Tcl_Interp *tclinterp;
static Tk_Window mainWindow;

#define RCNAME ".camltkrc"

/* Initialisation */
value camltk_opentk(display, name) /* ML */
     value display,name;
{
  /* Create an interpreter */
  tclinterp = Tcl_CreateInterp();  /* dies if error */
  /* Create the camlcallback command */
  Tcl_CreateCommand(tclinterp,
		    "camlcb", CamlCBCmd, 
		    (ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
  /* Open main window */
  mainWindow = Tk_CreateMainWindow(tclinterp,
				   String_val (display), /* screenname */
				   String_val(name), /* basename */
				   "Tk"  /* classname */
				   );
  if (NULL == mainWindow)
    tk_error(tclinterp->result);
  
  Tk_GeometryRequest(mainWindow,200,200);
  if (Tcl_Init(tclinterp) != TCL_OK)
    tk_error(tclinterp->result);
  if (Tk_Init(tclinterp) != TCL_OK)
    tk_error(tclinterp->result);

  /* This is required by "unknown" */
  Tcl_SetVar(tclinterp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
	     
  {
    char *home = getenv("HOME");
    if (home != NULL) {
      char *f = (char *)malloc(strlen(home)+strlen(RCNAME)+2);
      if (f == NULL) goto finish;
      f[0]='\0';
      strcat(f, home);
      strcat(f, "/");
      strcat(f, RCNAME);
      if (0 == access(f,R_OK)) 
	if (TCL_OK != Tcl_EvalFile(tclinterp,f))
	  tk_error(tclinterp->result);
    }
  }

 Tk_CreateTimerHandler(100, invoke_pending_caml_signals, NULL);
  
 finish:
  return Atom(0);
}

/* calling tcl from Caml */
/* type: string -> string */
value camltk_tcl_eval(str) /* ML */
value str; 
{
  int code;

  code = Tcl_Eval(tclinterp,String_val(str));
  switch (code) {
  case TCL_OK:
    return copy_string (tclinterp->result);
  case TCL_ERROR:
    tk_error(tclinterp->result);
  default:  /* TCL_BREAK, TCL_CONTINUE, TCL_RETURN */
    tk_error("bad tcl result");
  }
}

/*********** File descriptor callbacks **************/
/* It's not possible to call non-global ML code, so use general 
   callback stuff for fd. 
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