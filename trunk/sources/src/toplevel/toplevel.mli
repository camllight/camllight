(* System functions for interactive use *)

value quit : unit -> unit
        (* Exit the toplevel loop and terminate the [camllight] command. *)
  and include : string -> unit
        (* Read, compile and execute source phrases from the given file.
           The [.ml] extension is automatically added to the file name,
           if not present.
           This is textual inclusion: phrases are processed just as if
           they were typed on standard input. In particular, global
           identifiers defined by these phrases are entered in the
           module named [top], not in a new module. *)
  and load : string -> unit
        (* Load in memory the source code for a module
           implementation. Read, compile and execute source phrases
           from the given file. The [.ml] extension is automatically
           added if not present.  The [load] function behaves much
           like [include], except that a new module is created, with
           name the base name of the source file name. Global
           identifiers defined in the source file are entered in this
           module, instead of the [top] module as in the case of
           [include]. For instance, assuming file [foo.ml] contains
           the single phrase
           [
           let bar = 1;;
           ] 
           executing [load "foo"] defines the identifier [foo__bar]
           with value [1].  Caveat: the loaded module is not
           automatically opened: the identifier [bar] does not
           automatically complete to [foo__bar]. To achieve this, you
           must execute the directive [#open "foo"] afterwards. *)
  and compile : string -> unit
        (* Compile the source code for a module implementation or
           interface ([.ml] or [.mli] file). Compilation proceeds as
           with the batch compiler, and produces the same results as
           [camlc -c].  If the toplevel system is in debugging mode
           (option [-g] or function [debug_mode] below), the
           compilation is also performed in debugging mode, as when
           giving the [-g] option to the batch compiler. The result of
           the compilation is left in files ([.zo], [.zi],
           [.zix]). The compiled code is not loaded in memory. Use
           [load_object] to load a [.zo] file produced by [compile]. *)
  and load_object : string -> unit
        (* Load in memory the compiled bytecode contained in the given
           file.  The [.zo] extension is automatically added to the
           file name, if not present. The bytecode file has been
           produced either by the standalone compiler [camlc] or by
           the [compile] function. Global identifiers defined in the
           file being loaded are entered in their own module, not in
           the [top] module, just as with the [load] function. *)
  and trace : string -> unit
        (* After the execution of [trace "foo"], all calls to the
           global function named [foo] will be ``traced''. That is,
           the argument and the result are displayed for each call, as
           well as the exceptions escaping out of [foo], either raised
           by [foo] itself, or raised by one of the functions called
           from [foo]. If [foo] is a curried function, each argument
           is printed as it is passed to the function. 
           Only functions implemented in ML can be traced; system primitives
           such as [string_length] or user-supplied C functions cannot. *)
  and untrace : string -> unit
        (* Executing [untrace "foo"] stops all tracing over the global
           function named [foo]. *)
  and verbose_mode: bool -> unit
        (* [verbose_mode true] causes the [compile] function to print
           the inferred types and other information. [verbose_mode false]
           reverts to the default silent behavior. *)
  and install_printer : string -> unit
        (* [install_printer "printername"] registers the function named
           [printername] as a printer for objects whose types match
           its argument type. That is, the toplevel loop will call
           [printername] when it has such an object to print.
           The printing function [printername] must use the [format] library
           module to produce its output, otherwise the output of
           [printername] will not be correctly located in the values
           printed by the toplevel loop. *)
  and remove_printer : string -> unit
        (* [remove_printer "printername"] removes the function named
           [printername] from the table of toplevel printers. *)
  and set_print_depth : int -> unit
        (* [set_print_depth n] limits the printing of values to a maximal
           depth of [n]. The parts of values whose depth exceeds [n]
           are printed as [...] (ellipsis). *)
  and set_print_length : int -> unit
        (* [set_print_length n] limits the number of value nodes
           printed to at most [n]. Remaining parts of values
           are printed as [...] (ellipsis). *)
  and debug_mode: bool -> unit
        (* Set whether extended module interfaces must be used
           [debug_mode true] or not [debug_mode false]. Extended
           module interfaces are [.zix] files that describe the actual
           implementation of a module, including private types and
           variables. They are generated when compiling with [camlc -g],
           or with the [compile] function above when [debug_mode] is
           [true]. When [debug_mode] is [true], toplevel phrases can refer
           to private types and variables of modules, and private functions
           can be traced with [trace]. Setting [debug_mode true] is equivalent
           to starting the toplevel with the [-g] option. *)
  and cd : string -> unit
        (* Change the current working directory. *)
  and directory : string -> unit
        (* Add the given directory to the search path for files.
           Same behavior as the [-I] option or the [#directory] directive. *)
;;
