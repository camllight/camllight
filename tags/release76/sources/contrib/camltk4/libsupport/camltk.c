/* There is a clash on "Atom" macro (X11/mlvalues) */
/* tk.h must be included first */
#include <stdlib.h>
#include <unistd.h>
#include <sys/param.h>

#include <tk.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>

/* The Tcl interpretor */
Tcl_Interp *cltclinterp = NULL;

/* Code part of the callback dispatcher */
static code_t handler_code = NULL;

/* Initialization: has to be called (once) with the global Caml function
 * implementing the callback dispatcher 
 */
value camltk_install_callback_handler(handler) /* ML */
  value handler;
{
  handler_code = Code_val(handler);
  return Atom(0);
}

/* Hack for calling Caml global function from C */
/* arg has been allocated by us, and it might move
   when we allocate the pair and the closure
   */
value camltk_dispatch_callback(id,arg)
     int id;
     value arg;
{
  value clos;
  value res = Atom(0);
  Push_roots(r,2);
#define garg r[0]
#define pair r[1]
  garg = arg;			/* save it */
  pair = alloc_tuple(2);	/* alloc a pair */
  Field(pair, 0) = Val_int(id);
  Field(pair, 1) = garg;
  clos = alloc(2,Closure_tag);
  Code_val(clos) = handler_code;
  Env_val(clos) = Atom(0);
  res = callback(clos, pair);
  Pop_roots();
  return res;
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
  char *res = stat_alloc(l + 1);
  bcopy(String_val(s),res,l);
  res[l] = '\0';
  return res;
}



/* The Tcl command for evaluating callback in Caml */
int CamlCBCmd(clientdata, interp, argc, argv)
     ClientData clientdata;
     Tcl_Interp *interp;
     int argc;
     char *argv[];
{
  /* Assumes no result */
  Tcl_SetResult(interp, NULL, NULL);
  if (argc >= 2) {
    int id;
    if (Tcl_GetInt(interp, argv[1], &id) != TCL_OK)
      return TCL_ERROR;
    camltk_dispatch_callback(id, copy_string_list(argc-2,&argv[2]));
    /* Never fails (Caml would have raised an exception) */
    /* but result may have been set by callback */
    return TCL_OK;
  }
  else
    return TCL_ERROR;
}

/* Callbacks are always of type _ -> unit, to simplify storage
 * But a callback can nevertheless return something (to Tcl) by
 * using the following. TCL_VOLATILE ensures that Tcl will make
 * a copy of the string
 */
value camltk_return (v) /* ML */
     value v;
{
  Tcl_SetResult(cltclinterp, String_val(v), TCL_VOLATILE);
  return Val_unit;
}

/* Note: raise_with_string WILL copy the error message */
void tk_error(errmsg)
     char *errmsg;
{
  raise_with_string(TCL_ERROR_EXN, errmsg);
}

/* 
 * Dealing with signals: when a signal handler is defined in Caml,
 * the actual execution of the signal handler upon reception of the
 * signal is delayed until we are sure we are out of the GC.
 * If a signal occurs during the MainLoop, we would have to wait
 *  the next event for the handler to be invoked.
 * The following function will invoke a pending signal handler if any,
 * and we put in on a regular timer.
 */

#define SIGNAL_INTERVAL 300

int signal_events = 0; /* do we have a pending timer */

void invoke_pending_caml_signals (clientdata) 
     ClientData clientdata;
{
  signal_events = 0;
  enter_blocking_section(); /* triggers signal handling */
  /* Rearm timer */
  Tk_CreateTimerHandler(SIGNAL_INTERVAL, invoke_pending_caml_signals, NULL);
  signal_events = 1;
  leave_blocking_section();
}

/* Now the real Tk stuff */

static Tk_Window cltk_mainWindow;

#define RCNAME ".camltkrc"
#define CAMLCB "camlcb"

/* In slave mode, the interpreter *already* exists */
int cltk_slave_mode = 0;

/* Initialisation, based on tkMain.c */
value camltk_opentk(display, name) /* ML */
     value display,name;
{

  if (!cltk_slave_mode) {
    /* Create an interpreter, dies if error */
    cltclinterp = Tcl_CreateInterp();

    if (Tcl_Init(cltclinterp) != TCL_OK)
      tk_error(cltclinterp->result);
    Tcl_SetVar(cltclinterp, "argv0", String_val (name), TCL_GLOBAL_ONLY);
    { /* Sets display if needed */
      char *args;
      char *tkargv[2];
      if (string_length(display) > 0) {
	Tcl_SetVar(cltclinterp, "argc", "2", TCL_GLOBAL_ONLY);
	tkargv[0] = "-display";
	tkargv[1] = String_val(display);
	args = Tcl_Merge(2, tkargv);
	Tcl_SetVar(cltclinterp, "argv", args, TCL_GLOBAL_ONLY);
	free(args);
      }
    }
    if (Tk_Init(cltclinterp) != TCL_OK)
      tk_error(cltclinterp->result);

    /* Retrieve the main window */
    cltk_mainWindow = Tk_MainWindow(cltclinterp);

    if (NULL == cltk_mainWindow)
      tk_error(cltclinterp->result);
  
    Tk_GeometryRequest(cltk_mainWindow,200,200);
  }

  /* Create the camlcallback command */
  Tcl_CreateCommand(cltclinterp,
		    CAMLCB, CamlCBCmd, 
		    (ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

  /* This is required by "unknown" and thus autoload */
  Tcl_SetVar(cltclinterp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
  /* Our hack for implementing break in callbacks */
  Tcl_SetVar(cltclinterp, "BreakBindingsSequence", "0", TCL_GLOBAL_ONLY);

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
    return copy_string(cltclinterp->result);
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
      for (l=Field(v,0), n=0; Tag_val(l)==1; l=Field(l,1))
	n+=argv_size(Field(l,0));
      return n;
    }
  case 2:			/* TkQuote */
    return 1;
  }
}

/* 
 * Memory of allocated Tcl lists.
 * We should not need more than MAX_LIST
 */
#define MAX_LIST 256
static char *tcllists[MAX_LIST];

static int startfree = 0;
/* If size is lower, do not allocate */
static char *quotedargv[16];

/* Fill a preallocated vector arguments, doing expansion and all.
 * Assumes Tcl will 
 *  not tamper with our strings
 *  make copies if strings are "persistent"
 */
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
      for (l=Field(v,0); Tag_val(l)==1; l=Field(l,1))
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

  /* +4: one slot for NULL
         one slot for "unknown" if command not found
	 two slots for chaining local roots */
  argv = (char **)stat_alloc((size + 4) * sizeof(char *));

  wherewasi = startfree;

  /* Copy */
  {
    int where;
    for(i=0, where=0;i<Wosize_val(v);i++)
      where = fill_args(argv,where,Field(v,i));
    argv[size] = NULL;
    argv[size + 1] = NULL;
  }

  /* Register argv as local roots for the GC (cf. Push_roots in memory.h) */
  argv[size + 2] = (char *)(size + 2);
  argv[size + 3] = (char *) c_roots_head;
  c_roots_head = (value *)&(argv[size + 2]);

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

  /* Remove argv from the local roots */
  Pop_roots();

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
  /* argv is allocated by Tcl, to be freed by us */
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
 */

void FileProc(clientdata, mask)
     ClientData clientdata;
     int mask;
{
  camltk_dispatch_callback((int)clientdata,Atom(0));
}

value camltk_add_file_input(fd, cbid)    /* ML */
     value fd;
     value cbid;
{
  Tk_CreateFileHandler(Int_val(fd), TK_READABLE, 
		       FileProc, (ClientData)(Int_val(cbid)));
  return Atom(0);
}

value camltk_rem_file_input(fd) /* ML */
     value fd;
{
  Tk_DeleteFileHandler(Int_val(fd));
  return Atom(0);
}

value camltk_add_file_output(fd, cbid)    /* ML */
     value fd;
     value cbid;
{
  Tk_CreateFileHandler(Int_val(fd), TK_WRITABLE, 
		       FileProc, (ClientData) (Int_val(cbid)));
  return Val_unit;
}

value camltk_rem_file_output(fd) /* ML */
     value fd;
{
  Tk_DeleteFileHandler(Int_val(fd));
  return Val_unit;
}

value camltk_tk_mainloop() /* ML */
{
  if (!signal_events) {
    /* Initialise signal handling */
    signal_events = 1;
    Tk_CreateTimerHandler(100, invoke_pending_caml_signals, NULL);
  };
  Tk_MainLoop();
  return Atom(0);
}

/* Note: this HAS to be reported "as-is" in ML source */
static int event_flag_table[] = {
  TK_DONT_WAIT, TK_X_EVENTS, TK_FILE_EVENTS, TK_TIMER_EVENTS, TK_IDLE_EVENTS,
  TK_ALL_EVENTS
};

value camltk_dooneevent(flags) /* ML */
     value flags;
{
  int ret;
  ret = Tk_DoOneEvent(convert_flag_list(flags, event_flag_table));
  return Val_int(ret);
}

/* Basically the same thing as FileProc */
void TimerProc (clientdata)
     ClientData clientdata;
{
  camltk_dispatch_callback((int)clientdata,Atom(0));
}

value camltk_add_timer(milli, cbid) /* ML */
     value milli;
     value cbid;
{
  /* look at tkEvent.c , Tk_Token is an int */ 
  return (Val_int(Tk_CreateTimerHandler(Int_val(milli), TimerProc, 
				       (ClientData) (Int_val(cbid)))));
}

value camltk_rem_timer(token) /* ML */
     value token;
{
  Tk_DeleteTimerHandler((Tk_TimerToken) Int_val(token));
  return Atom(0);
}

/* The following are replacements for 
    tkwait variable
    tkwait visibility
    tkwait window
   in the case where we use threads (staying in Tk_MainLoop() prevents
   thread scheduling from taking place).
   We could get away we using a regular timer than invokes some nop function.
*/

/* Forward declaration to keep the compiler happy */
static char *		WaitVariableProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, char *name1, char *name2,
			    int flags));
static void		WaitVisibilityProc _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));
static void		WaitWindowProc _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));

static char * WaitVariableProc(clientdata, interp, name1, name2, flags)
     ClientData clientdata;
     Tcl_Interp *interp;	/* Interpreter containing variable. */
     char *name1;		/* Name of variable. */
     char *name2;		/* Second part of variable name. */
     int flags;			/* Information about what happened. */
{
  char *fullvar;

  /* Rebuild the full variable name */
  if (NULL == name2) {
    fullvar = stat_alloc (strlen(name1) + 1);
    strcpy(fullvar,name1);
  } else { 
    fullvar= stat_alloc (strlen(name1) + strlen(name2) + 3);
    strcpy(fullvar,name1);
    strcat(fullvar,"(");
    strcat(fullvar,name2);
    strcat(fullvar,")");
  }
  Tcl_UntraceVar(interp, fullvar,
		TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		WaitVariableProc, clientdata);
  stat_free(fullvar);
  camltk_dispatch_callback((int)clientdata,Atom(0));
  return (char *)NULL;
}

/* Sets up a callback upon modification of a variable */
value camltk_trace_var(var,cbid) /* ML */
     value var;
     value cbid;
{
  /* Make a copy of var, since Tcl will modify it in place, and we
   * don't trust that much what it will do here
   */
  char *cvar = string_to_c(var);

  if (Tcl_TraceVar(cltclinterp, cvar,
		   TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		   WaitVariableProc,
		   (ClientData) (Int_val(cbid)))
		   != TCL_OK) {
    stat_free(cvar);
    tk_error(cltclinterp->result);
  };
  stat_free(cvar);
  return Atom(0);
}

/* For the other handlers, we need a bit more data */
struct WinCBData {
  int cbid;
  Tk_Window win;
};

static void WaitVisibilityProc(clientData, eventPtr)
    ClientData clientData;	
    XEvent *eventPtr;		/* Information about event (not used). */
{
  struct WinCBData *vis = clientData;
  int cbid = vis->cbid;

  Tk_DeleteEventHandler(vis->win, VisibilityChangeMask,
	    WaitVisibilityProc, clientData);

  stat_free((char *)vis);
  camltk_dispatch_callback(cbid,Val_int(0));
}

/* Sets up a callback upon Visibility of a window */
value camltk_wait_vis(win,cbid) /* ML */
     value win;
     value cbid;
{
  struct WinCBData *vis =
    (struct WinCBData *)stat_alloc(sizeof(struct WinCBData));
  vis->win = Tk_NameToWindow(cltclinterp, String_val(win), cltk_mainWindow);
  if (vis -> win == NULL) {
    stat_free((char *)vis);
    tk_error(cltclinterp->result);
  };
  vis->cbid = Int_val(cbid);
  Tk_CreateEventHandler(vis->win, VisibilityChangeMask,
			WaitVisibilityProc, (ClientData) vis);
  return Atom(0);
}

static void WaitWindowProc(clientData, eventPtr)
    ClientData clientData;	
    XEvent *eventPtr;		
{
  if (eventPtr->type == DestroyNotify) {
    struct WinCBData *vis = clientData;
    int cbid = vis->cbid;
    stat_free((char *)clientData);
    /* The handler is destroyed by Tk itself */
    camltk_dispatch_callback(cbid,Atom(0));
  }
}

/* Sets up a callback upon window destruction */
value camltk_wait_des(win,cbid) /* ML */
     value win;
     value cbid;
{
  struct WinCBData *vis =
    (struct WinCBData *)stat_alloc(sizeof(struct WinCBData));
  vis->win = Tk_NameToWindow(cltclinterp, String_val(win), cltk_mainWindow);
  if (vis -> win == NULL) {
    stat_free((char *)vis);
    tk_error(cltclinterp->result);
  };
  vis->cbid = Int_val(cbid);
  Tk_CreateEventHandler(vis->win, StructureNotifyMask,
			WaitWindowProc, (ClientData) vis);
  return Atom(0);
}
