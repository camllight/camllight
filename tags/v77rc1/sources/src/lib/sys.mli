(* System interface. *)

(* This module provides a simple interface to the operating system. *)

exception Sys_error of string;;
        (* Raised by some functions in the [sys] and [io] modules,
           when the underlying system calls fail. The argument to
           [Sys_error] is a string describing the error. The texts
           of the error messages are implementation-dependent, and should
           not be relied upon to catch specific system errors. *)

value command_line : string vect;;
        (* The command line arguments given to the process.
           The first element is the command name used to invoke the program. *)

value interactive: bool;;
        (* True if we're running under the toplevel system. False if
           we're running as a standalone program. *)

type file_perm == int;;

value s_irusr : file_perm
  and s_iwusr : file_perm
  and s_ixusr : file_perm
  and s_irgrp : file_perm
  and s_iwgrp : file_perm
  and s_ixgrp : file_perm
  and s_iroth : file_perm
  and s_iwoth : file_perm
  and s_ixoth : file_perm
  and s_isuid : file_perm
  and s_isgid : file_perm
  and s_irall : file_perm
  and s_iwall : file_perm
  and s_ixall : file_perm
;;
        (* Access permissions for files. [r] is reading permission,
           [w] is writing permission, [x] is execution permission.
           [usr] means permissions for the user owning the file,
           [grp] for the group owning the file, [oth] for others.
           [isuid] and [isgid] are for set-user-id and set-group-id files,
           respectively. The remaining are combinations of the permissions
           above. *)

type open_flag =
    O_RDONLY                       (* open read-only *)
  | O_WRONLY                       (* open write-only *)
  | O_RDWR                         (* open for reading and writing *)
  | O_APPEND                       (* open for appending *)
  | O_CREAT                        (* create the file if nonexistent *)
  | O_TRUNC                        (* truncate the file to 0 if it exists *)
  | O_EXCL                         (* fails if the file exists *)
  | O_BINARY                       (* open in binary mode *)
  | O_TEXT                         (* open in text mode *)
;;
        (* The commands for [open]. *)

value exit : int -> 'a = 1 "sys_exit"
        (* Terminate the program and return the given status code to
	   the operating system.
           In contrast with the function [exit] from module [io], this
           [exit] function does not flush the standard
           output and standard error channels. *)
  and open : string -> open_flag list -> file_perm -> int = 3 "sys_open"
        (* Open a file. The second argument is the opening mode.
           The third argument is the permissions to use if the file
           must be created. The result is a file descriptor opened on the
           file. *)
  and close : int -> unit = 1 "sys_close"
        (* Close a file descriptor. *)
  and remove : string -> unit = 1 "sys_remove"
        (* Remove the given file name from the file system. *)
  and rename : string -> string -> unit = 2 "sys_rename"
        (* Rename a file. The first argument is the old name and the
           second is the new name. *)
  and getenv : string -> string = 1 "sys_getenv"
        (* Return the value associated to a variable in the process
           environment. Raise [Not_found] if the variable is unbound. *)
  and chdir : string -> unit = 1 "sys_chdir"
        (* Change the current working directory of the process.
	   Note that there is no easy way of getting the current
	   working directory from the operating system. *)
  and system_command : string -> int = 1 "sys_system_command"
        (* Execute the given shell command and return its exit code. *)
;;

value time : unit -> float = 1 "sys_time"
        (* Return the processor time, in seconds, used by the program
           since the beginning of execution. *)
;;

exception Break
        (* Exception [Break] is raised on user interrupt if [catch_break]
           is on. *)
;;
value catch_break : bool -> unit = 1 "sys_catch_break"
        (* [catch_break] governs whether user interrupt terminates the program
           or raises the [Break] exception. Call [catch_break true] to enable
	   raising [Break], and [catch_break false] to let the system
	   terminate the program on user interrupt. *)
;;

(*--*)

value max_vect_length : int
  and max_string_length : int
        (* Max length for arrays and strings, as imposed by the
           runtime system. *)
;;
value word_size : int
        (* Size of a machine word (in bits). *)
;;
