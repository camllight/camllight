(* Interface to the Unix system *)

(*** Error report *)

type error =
    ENOERR
  | EPERM               (* Not owner *)
  | ENOENT              (* No such file or directory *)
  | ESRCH               (* No such process *)
  | EINTR               (* Interrupted system call *)
  | EIO                 (* I/O error *)
  | ENXIO               (* No such device or address *)
  | E2BIG               (* Arg list too long *)
  | ENOEXEC             (* Exec format error *)
  | EBADF               (* Bad file number *)
  | ECHILD              (* No children *)
  | EAGAIN              (* No more processes *)
  | ENOMEM              (* Not enough core *)
  | EACCES              (* Permission denied *)
  | EFAULT              (* Bad address *)
  | ENOTBLK             (* Block device required *)
  | EBUSY               (* Mount device busy *)
  | EEXIST              (* File exists *)
  | EXDEV               (* Cross-device link *)
  | ENODEV              (* No such device *)
  | ENOTDIR             (* Not a directory*)
  | EISDIR              (* Is a directory *)
  | EINVAL              (* Invalid argument *)
  | ENFILE              (* File table overflow *)
  | EMFILE              (* Too many open files *)
  | ENOTTY              (* Not a typewriter *)
  | ETXTBSY             (* Text file busy *)
  | EFBIG               (* File too large *)
  | ENOSPC              (* No space left on device *)
  | ESPIPE              (* Illegal seek *)
  | EROFS               (* Read-only file system *)
  | EMLINK              (* Too many links *)
  | EPIPE               (* Broken pipe *)
  | EDOM                (* Argument too large *)
  | ERANGE              (* Result too large *)
  | EWOULDBLOCK         (* Operation would block *)
  | EINPROGRESS         (* Operation now in progress *)
  | EALREADY            (* Operation already in progress *)
  | ENOTSOCK            (* Socket operation on non-socket *)
  | EDESTADDRREQ        (* Destination address required *)
  | EMSGSIZE            (* Message too long *)
  | EPROTOTYPE          (* Protocol wrong type for socket *)
  | ENOPROTOOPT         (* Protocol not available *)
  | EPROTONOSUPPORT     (* Protocol not supported *)
  | ESOCKTNOSUPPORT     (* Socket type not supported *)
  | EOPNOTSUPP          (* Operation not supported on socket *)
  | EPFNOSUPPORT        (* Protocol family not supported *)
  | EAFNOSUPPORT        (* Address family not supported by protocol family *)
  | EADDRINUSE          (* Address already in use *)
  | EADDRNOTAVAIL       (* Can't assign requested address *)
  | ENETDOWN            (* Network is down *)
  | ENETUNREACH         (* Network is unreachable *)
  | ENETRESET           (* Network dropped connection on reset *)
  | ECONNABORTED        (* Software caused connection abort *)
  | ECONNRESET          (* Connection reset by peer *)
  | ENOBUFS             (* No buffer space available *)
  | EISCONN             (* Socket is already connected *)
  | ENOTCONN            (* Socket is not connected *)
  | ESHUTDOWN           (* Can't send after socket shutdown *)
  | ETOOMANYREFS        (* Too many references: can't splice *)
  | ETIMEDOUT           (* Connection timed out *)
  | ECONNREFUSED        (* Connection refused *)
  | ELOOP               (* Too many levels of symbolic links *)
  | ENAMETOOLONG        (* File name too long *)
  | EHOSTDOWN           (* Host is down *)
  | EHOSTUNREACH        (* No route to host *)
  | ENOTEMPTY           (* Directory not empty *)
  | EPROCLIM            (* Too many processes *)
  | EUSERS              (* Too many users *)
  | EDQUOT              (* Disc quota exceeded *)
  | ESTALE              (* Stale NFS file handle *)
  | EREMOTE             (* Too many levels of remote in path *)
  | EIDRM               (* Identifier removed *)
  | EDEADLK             (* Deadlock condition. *)
  | ENOLCK              (* No record locks available. *)
  | ENOSYS              (* Function not implemented *)
  | EUNKNOWNERR
;;
        (* The type of error codes. *)

exception Unix_error of error * string * string;;
        (* Raised by the system calls below when an error is encountered.
           The first component is the error code; the second component
           is the function name; the third component is the string parameter
           to the function, if it has one, or the empty string otherwise. *)

value error_message : error -> string = 1 "unix_error_message";;
        (* Return a string describing the given error code. *)

value handle_unix_error : ('a -> 'b) -> 'a -> 'b
        (* [handle_unix_error f x] applies [f] to [x] and returns the result.
           If the exception [Unix_error] is raised, it prints a message
           describing the error and exits with code 2. *)
;;

(*** Interface with the parent process *)

value environment : unit -> string vect = 1 "unix_environment";;
        (* Return the process environment, as an array of strings
           with the format ``variable=value''. See also [sys__getenv]. *)

(*** Process handling *)

type process_status =
    WEXITED of int
  | WSIGNALED of int * bool
  | WSTOPPED of int
;;
        (* The termination status of a process. [WEXITED] means that the
           process terminated normally by [exit]; the argument is the return
           code. [WSIGNALED] means that the process was killed by a signal;
           the first argument is the signal number, the second argument
           indicates whether a ``core dump'' was performed. [WSTOPPED] means
           that the process was stopped by a signal; the argument is the
           signal number. *)

type wait_flag =
    WNOHANG
  | WUNTRACED
;;
        (* Flags for [waitopt] and [waitpid].
           [WNOHANG] means do not block if no child has
           died yet, but immediately return with a pid equal to 0.
           [WUNTRACED] means report also the children that receive stop
           signals. *)

value execv : string -> string vect -> unit = 2 "unix_execv"
        (* [execv prog args] execute the program in file [prog], with
           the arguments [args], and the current process environment. *)
  and execve : string -> string vect -> string vect -> unit = 3 "unix_execve"
        (* Same as [execv], except that the third argument provides the
           environment to the program executed. *)
  and execvp : string -> string vect -> unit = 2 "unix_execvp"
        (* Same as [execv], except that the program is searched in the path. *)
  and fork : unit -> int = 1 "unix_fork"
        (* Fork a new process. The returned integer is 0 for the child
           process, the pid of the child process for the parent process. *)
  and wait : unit -> int * process_status = 1 "unix_wait"
        (* Wait until one of the children processes die, and return its pid
           and termination status. *)
  and waitopt : wait_flag list -> int * process_status = 1 "unix_waitopt"
        (* Same as [wait], but takes a list of options to avoid blocking,
           or also report stopped children. The pid returned is 0 if no
           child has changed status. *)
  and waitpid : wait_flag list -> int -> int * process_status
           = 2 "unix_waitpid"
        (* Same as [waitopt], but waits for the process whose pid is given.
           Negative pid arguments represent process groups. *)
  and system : string -> process_status
        (* Execute the given command, wait until it terminates, and return
           its termination status. The string is interpreted by the shell
           [/bin/sh] and therefore can contain redirections, quotes, variables,
           etc. The result [WEXITED 127] indicates that the shell couldn't
           be executed. *)
  and getpid : unit -> int = 1 "unix_getpid"
        (* Return the pid of the process. *)
  and getppid : unit -> int = 1 "unix_getppid"
        (* Return the pid of the parent process. *)
  and nice : int -> int = 1 "unix_nice"
        (* Change the process priority. The integer argument is added to the
           ``nice'' value. (Higher values of the ``nice'' value mean
           lower priorities.) Return the new nice value. *)
;;

(*** Basic file input/output *)

type file_descr;;
        (* The abstract type of file descriptors. *)

value stdin : file_descr
  and stdout : file_descr
  and stderr : file_descr
        (* File descriptors for standard input, standard output and
           standard error. *)
;;

type open_flag =
    O_RDONLY                            (* Open for reading *)
  | O_WRONLY                            (* Open for writing *)
  | O_RDWR                              (* Open for reading and writing *)
  | O_NDELAY                            (* Open in non-blocking mode *)
  | O_APPEND                            (* Open for append *)
  | O_CREAT                             (* Create if nonexistent *)
  | O_TRUNC                             (* Truncate to 0 length if existing *)
  | O_EXCL                              (* Fail if existing *)
;;
        (* The flags to [open]. *)

type file_perm == int;;
        (* The type of file access rights. *)

value open : string -> open_flag list -> file_perm -> file_descr
           = 3 "unix_open"
        (* Open the named file with the given flags. Third argument is
           the permissions to give to the file if it is created. Return
           a file descriptor on the named file. *)
  and close : file_descr -> unit = 1 "unix_close"
        (* Close a file descriptor. *)
  and read : file_descr -> string -> int -> int -> int = 4 "unix_read"
        (* [read fd buff start len] reads [len] characters from descriptor
           [fd], storing them in string [buff], starting at position [ofs]
           in string [buff]. Return the number of characters actually read. *)
  and write : file_descr -> string -> int -> int -> int = 4 "unix_write"
        (* [write fd buff start len] writes [len] characters to descriptor
           [fd], taking them from string [buff], starting at position [ofs]
           in string [buff]. Return the number of characters actually
           written. *)
;;

(*** Interfacing with the standard input/output library (module io). *)

value in_channel_of_descr : file_descr -> in_channel = 1 "open_descriptor"
        (* Create an input channel reading from the given descriptor. *)
  and out_channel_of_descr : file_descr -> out_channel = 1 "open_descriptor"
        (* Create an output channel writing on the given descriptor. *)
  and descr_of_in_channel : in_channel -> file_descr = 1 "channel_descriptor"
        (* Return the descriptor corresponding to an input channel. *)
  and descr_of_out_channel : out_channel -> file_descr = 1 "channel_descriptor"
        (* Return the descriptor corresponding to an output channel. *)
;;

(*** Seeking and truncating *)

type seek_command =
    SEEK_SET
  | SEEK_CUR
  | SEEK_END
;;
        (* Positioning modes for [lseek]. [SEEK_SET] indicates positions
           relative to the beginning of the file, [SEEK_CUR] relative to
           the current position, [SEEK_END] relative to the end of the
           file. *)

value lseek : file_descr -> int -> seek_command -> int = 3 "unix_lseek"
        (* Set the current position for a file descriptor *)
  and truncate : string -> int -> unit = 2 "unix_truncate"
        (* Truncates the named file to the given size. *)
  and ftruncate : file_descr -> int -> unit = 2 "unix_ftruncate"
        (* Truncates the file corresponding to the given descriptor
           to the given size. *)
;;

(*** File statistics *)

type file_kind =
    S_REG                               (* Regular file *)
  | S_DIR                               (* Directory *)
  | S_CHR                               (* Character device *)
  | S_BLK                               (* Block device *)
  | S_LNK                               (* Symbolic link *)
  | S_FIFO                              (* Named pipe *)
  | S_SOCK                              (* Socket *)
;;
type stats =
  { st_dev : int;                       (* Device number *)
    st_ino : int;                       (* Inode number *)
    st_kind : file_kind;                (* Kind of the file *)
    st_perm : file_perm;                (* Access rights *)
    st_nlink : int;                     (* Number of links *)
    st_uid : int;                       (* User id of the owner *)
    st_gid : int;                       (* Group id of the owner *)
    st_rdev : int;                      (* Device minor number *)
    st_size : int;                      (* Size in bytes *)
    st_atime : int;                     (* Last access time *)
    st_mtime : int;                     (* Last modification time *)
    st_ctime : int }                    (* Last status change time *)
;;
        (* The informations returned by the [stat] calls. *)

value stat : string -> stats = 1 "unix_stat"
        (* Return the information for the named file. *)
  and lstat : string -> stats = 1 "unix_lstat"
        (* Same as [stat], but in case the file is a symbolic link,
           return the information for the link itself. *)
  and fstat : file_descr -> stats = 1 "unix_fstat"
        (* Return the information for the file associated with the given
           descriptor. *)
;;

(*** Operations on file names *)

value unlink : string -> unit = 1 "unix_unlink"
        (* Removes the named file *)
  and rename : string -> string -> unit = 2 "unix_rename"
        (* [rename old new] changes the name of a file from [old] to [new]. *)
  and link : string -> string -> unit = 2 "unix_link"
        (* [link source dest] creates a hard link named [dest] to the file
           named [new]. *)
;;

(*** File permissions and ownership *)

type access_permission =
    R_OK                                (* Read permission *)
  | W_OK                                (* Write permission *)
  | X_OK                                (* Execution permission *)
  | F_OK                                (* File exists *)
;;
        (* Flags for the [access] call. *)

value chmod : string -> file_perm -> unit = 2 "unix_chmod"
        (* Change the permissions of the named file. *) 
  and fchmod : file_descr -> file_perm -> unit = 2 "unix_fchmod"
        (* Change the permissions of an opened file. *) 
  and chown : string -> int -> int -> unit = 3 "unix_chown"
        (* Change the owner uid and owner gid of the named file. *) 
  and fchown : file_descr -> int -> int -> unit = 3 "unix_fchown"
        (* Change the owner uid and owner gid of an opened file. *) 
  and umask : int -> int = 1 "unix_umask"
        (* Set the process creation mask, and return the previous mask. *)
  and access : string -> access_permission list -> unit = 2 "unix_access"
        (* Check that the process has the given permissions over the named
           file. Raise [Unix_error] otherwise. *)
;;

(*** File descriptor hacking *)

value fcntl_int : file_descr -> int -> int -> int = 3 "unix_fcntl_int"
        (* Interface to [fcntl] in the case where the argument is an
           integer. The first integer argument is the command code;
           the second is the integer parameter. *)
  and fcntl_ptr : file_descr -> int -> string -> int = 3 "unix_fcntl_ptr"
        (* Interface to [fcntl] in the case where the argument is a pointer.
           The integer argument is the command code. A pointer to the string
           argument is passed as argument to the command. *)
;;

(*** Directories *)

value mkdir : string -> file_perm -> unit = 2 "unix_mkdir"
        (* Create a directory with the given permissions. *)
  and rmdir : string -> unit = 1 "unix_rmdir"
        (* Remove an empty directory. *)
  and chdir : string -> unit = 1 "unix_chdir"
        (* Change the process working directory. *)
  and getcwd : unit -> string = 1 "unix_getcwd"
        (* Return the name of the current working directory. *)
;;

type dir_handle
;;
        (* The type of descriptors over opened directories. *)

value opendir : string -> dir_handle = 1 "unix_opendir"
        (* Open a descriptor on a directory *)
  and readdir : dir_handle -> string = 1 "unix_readdir"
        (* Return the next entry in a directory.
           Raise [End_of_file] when the end of the directory has been 
           reached. *)
  and rewinddir : dir_handle -> unit = 1 "unix_rewinddir"
        (* Reposition the descriptor to the beginning of the directory *)
  and closedir : dir_handle -> unit = 1 "unix_closedir"
        (* Close a directory descriptor. *)
;;

(*** Pipes and redirections *)

value pipe : unit -> file_descr * file_descr = 1 "unix_pipe"
        (* Create a pipe. The first component of the result is opened
           for reading, that's the exit to the pipe. The second component is
           opened for writing, that's the entrace to the pipe. *) 
  and dup : file_descr -> file_descr = 1 "unix_dup"
        (* Duplicate a descriptor. *)
  and dup2 : file_descr -> file_descr -> unit = 2 "unix_dup2"
        (* [dup2 fd1 fd2] duplicates [fd1] to [fd2], closing [fd2] if already
           opened. *)
;;

value open_process_in: string -> in_channel
  and open_process_out: string -> out_channel
  and open_process: string -> in_channel * out_channel
        (* High-level pipe and process management. These functions
           run the given command in parallel with the program,
           and return channels connected to the standard input and/or
           the standard output of the command. The command is interpreted
           by the shell [/bin/sh] (cf. [system]). Warning: writes on channels
           are buffered, hence be careful to call [flush] at the right times
           to ensure correct synchronization. *)
  and close_process_in: in_channel -> process_status
  and close_process_out: out_channel -> process_status
  and close_process: in_channel * out_channel -> process_status
        (* Close channels opened by [open_process_in], [open_process_out]
           and [open_process], respectively, wait for the associated
           command to terminate, and return its termination status. *)
;;

(*** Symbolic links *)

value symlink : string -> string -> unit = 2 "unix_symlink"
        (* [symlink source dest] creates the file [dest] as a symbolic link
           to the file [source]. *)
  and readlink : string -> string = 1 "unix_readlink"
        (* Read the contents of a link. *)
;;

(*** Named pipes *)

value mkfifo : string -> file_perm -> unit = 2 "unix_mkfifo"
        (* Create a named pipe with the given permissions. *)
;;

(*** Special files *)

value ioctl_int : file_descr -> int -> int -> int = 3 "unix_ioctl_int"
        (* Interface to [ioctl] in the case where the argument is an
           integer. The first integer argument is the command code;
           the second is the integer parameter. *)
  and ioctl_ptr : file_descr -> int -> string -> int = 3 "unix_ioctl_ptr"
        (* Interface to [ioctl] in the case where the argument is a pointer.
           The integer argument is the command code. A pointer to the string
           argument is passed as argument to the command. *)
;;

(*** Polling *)

value select :
  file_descr list -> file_descr list -> file_descr list -> float ->
        file_descr list * file_descr list * file_descr list = 4 "unix_select"
;;
        (* Wait until some input/output operations become possible on
           some channels. The three list arguments are, respectively, a set
           of descriptors to check for reading (first argument), for writing
           (second argument), or for exceptional conditions (third argument).
           The fourth argument is the maximal timeout, in seconds; a
           negative fourth argument means no timeout (unbounded wait).
           The result is composed of three sets of descriptors: those ready
           for reading (first component), ready for writing (second component),
           and over which an exceptional condition is pending (third
           component). *)

(*** Locking *)

type lock_command =
    F_ULOCK               (* Unlock a region *)
  | F_LOCK                (* Lock a region, and block if already locked *)
  | F_TLOCK               (* Lock a region, or fail if already locked *)
  | F_TEST                (* Test a region for other process' locks *)
;;
        (* Commands for [lockf]. *)

value lockf : file_descr -> lock_command -> int -> unit = 3 "unix_lockf"
;;
        (* [lockf fd cmd size] puts a lock on a region of the file opened
           as [fd]. The region starts at the current read/write position for
           [fd] (as set by [lseek]), and extends [size] bytes forward if
           [size] is positive, [size] bytes backwards if [size] is negative,
           or to the end of the file if [size] is zero. *)

(*** Signals *)

type signal =
    SIGHUP              (* hangup *)
  | SIGINT              (* interrupt *)
  | SIGQUIT             (* quit *)
  | SIGILL              (* illegal instruction (not reset when caught) *)
  | SIGTRAP             (* trace trap (not reset when caught) *)
  | SIGABRT             (* used by abort *)
  | SIGEMT              (* EMT instruction *)
  | SIGFPE              (* floating point exception *)
  | SIGKILL             (* kill (cannot be caught or ignored) *)
  | SIGBUS              (* bus error *)
  | SIGSEGV             (* segmentation violation *)
  | SIGSYS              (* bad argument to system call *)
  | SIGPIPE             (* write on a pipe with no one to read it *)
  | SIGALRM             (* alarm clock *)
  | SIGTERM             (* software termination signal from kill *)
  | SIGURG              (* urgent condition on IO channel *)
  | SIGSTOP             (* sendable stop signal not from tty *)
  | SIGTSTP             (* stop signal from tty *)
  | SIGCONT             (* continue a stopped process *)
  | SIGCHLD             (* to parent on child stop or exit *)
  | SIGIO               (* input/output possible signal *)
  | SIGXCPU             (* exceeded CPU time limit *)
  | SIGXFSZ             (* exceeded file size limit *)
  | SIGVTALRM           (* virtual time alarm *)
  | SIGPROF             (* profiling time alarm *)
  | SIGWINCH            (* window changed *)
  | SIGLOST             (* resource lost (eg, record-lock lost) *)
  | SIGUSR1             (* user defined signal 1 *)
  | SIGUSR2             (* user defined signal 2 *)
;;
        (* The type of signals. *)

type signal_handler =
    Signal_default                      (* Default behavior for the signal *)
  | Signal_ignore                       (* Ignore the signal *)
  | Signal_handle of (unit -> unit)     (* Call the given function
                                           when the signal occurs. *)
;;
        (* The behavior on receipt of a signal *)

value kill : int -> signal -> unit = 2 "unix_kill"
        (* Send a signal to the process with the given process id. *)
  and signal : signal -> signal_handler -> unit
        (* Set the behavior to be taken on receipt of the given signal. *)
  and pause : unit -> unit = 1 "unix_pause"
        (* Wait until a non-ignored signal is delivered. *)
;;

(*** Time functions *)

type process_times =
  { tms_utime : float;          (* User time for the process *)
    tms_stime : float;          (* System time for the process *)
    tms_cutime : float;         (* User time for the children processes *)
    tms_cstime : float }        (* System time for the children processes *)
;;
        (* The execution times (CPU times) of a process. *)

type tm =
  { tm_sec : int;                       (* Seconds 0..59 *)
    tm_min : int;                       (* Minutes 0..59 *)
    tm_hour : int;                      (* Hours 0..23 *)
    tm_mday : int;                      (* Day of month 1..31 *)
    tm_mon : int;                       (* Month of year 0..11 *)
    tm_year : int;                      (* Year - 1900 *)
    tm_wday : int;                      (* Day of week (Sunday is 0) *)
    tm_yday : int;                      (* Day of year 0..365 *)
    tm_isdst : bool }                   (* Daylight time savings in effect *)
;;
        (* The type representing wallclock time and calendar date. *)

value time : unit -> int = 1 "unix_time"
        (* Return the current time since 00:00:00 GMT, Jan. 1, 1970,
           in seconds. *)
  and gettimeofday : unit -> float = 1 "unix_gettimeofday"
        (* Same as [time], but with resolution better than 1 second. *)
  and gmtime : int -> tm = 1 "unix_gmtime"
        (* Convert a time in seconds, as returned by [time], into a date and
           a time. Assumes Greenwich meridian time zone. *)
  and localtime : int -> tm = 1 "unix_localtime"
        (* Convert a time in seconds, as returned by [time], into a date and
           a time. Assumes the local time zone. *)
  and alarm : int -> int = 1 "unix_alarm"
        (* Schedule a [SIGALRM] signals after the given number of seconds. *)
  and sleep : int -> unit = 1 "unix_sleep"
        (* Stop execution for the given number of seconds. *)
  and times : unit -> process_times = 1 "unix_times"
        (* Return the execution times of the process. *)
  and utimes : string -> int -> int -> unit = 3 "unix_utimes"
        (* Set the last access time (second arg) and last modification time
           (third arg) for a file. Times are expressed in seconds from
           00:00:00 GMT, Jan. 1, 1970. *)
;;

(*** User id, group id *)

value getuid : unit -> int = 1 "unix_getuid"
        (* Return the user id of the user executing the process. *)
  and geteuid : unit -> int = 1 "unix_geteuid"
        (* Return the effective user id under which the process runs. *)
  and setuid : int -> unit = 1 "unix_setuid"
        (* Set the real user id and effective user id for the process. *)
  and getgid : unit -> int = 1 "unix_getgid"
        (* Return the group id of the user executing the process. *)
  and getegid : unit -> int = 1 "unix_getegid"
        (* Return the effective group id under which the process runs. *)
  and setgid : int -> unit = 1 "unix_setgid"
        (* Set the real group id and effective group id for the process. *)
  and getgroups : unit -> int vect = 1 "unix_getgroups"
        (* Return the list of groups to which the user executing the process
           belongs. *)
;;

type passwd_entry =
  { pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string };;
        (* Structure of entries in the [passwd] database. *)

type group_entry =
  { gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string vect };;
        (* Structure of entries in the [groups] database. *)

value getlogin : unit -> string = 1 "unix_getlogin"
        (* Return the login name of the user executing the process. *)
  and getpwnam : string -> passwd_entry = 1 "unix_getpwnam"
        (* Find an entry in [passwd] with the given name, or raise
           [Not_found]. *)
  and getgrnam : string -> group_entry = 1 "unix_getgrnam"
        (* Find an entry in [group] with the given name, or raise
           [Not_found]. *)
  and getpwuid : int -> passwd_entry = 1 "unix_getpwuid"
        (* Find an entry in [passwd] with the given user id, or raise
           [Not_found]. *)
  and getgrgid : int -> group_entry = 1 "unix_getgrgid"
        (* Find an entry in [group] with the given group id, or raise
           [Not_found]. *)
;;

(*** Internet addresses *)

type inet_addr;;
        (* The abstract type of Internet addresses. *)

value inet_addr_of_string : string -> inet_addr
                                    = 1 "unix_inet_addr_of_string"
  and string_of_inet_addr : inet_addr -> string
                                    = 1 "unix_string_of_inet_addr"
        (* Conversions between string with the format [XXX.YYY.ZZZ.TTT]
           and Internet addresses. [inet_addr_of_string] raises [Failure]
           when given a string that does not match this format. *)
;;

(*** Sockets *)

type socket_domain =
    PF_UNIX                             (* Unix domain *)
  | PF_INET                             (* Internet domain *)
;;
        (* The type of socket domains. *)

type socket_type =
    SOCK_STREAM                         (* Stream socket *)
  | SOCK_DGRAM                          (* Datagram socket *)
  | SOCK_RAW                            (* Raw socket *)
  | SOCK_SEQPACKET                      (* Sequenced packets socket *)
;;
        (* The type of socket kinds, specifying the semantics of
           communications. *)

type sockaddr =
    ADDR_UNIX of string
  | ADDR_INET of inet_addr * int
;;
        (* The type of socket addresses. [ADDR_UNIX name] is a socket
           address in the Unix domain; [name] is a file name in the file
           system. [ADDR_INET(addr,port)] is a socket address in the Internet
           domain; [addr] is the Internet address of the machine, and
           [port] is the port number. *)

type shutdown_command =
    SHUTDOWN_RECEIVE                    (* Close for receiving *)
  | SHUTDOWN_SEND                       (* Close for sending *)
  | SHUTDOWN_ALL                        (* Close both *)
;;
        (* The type of commands for [shutdown]. *)

type msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK
;;
        (* The flags for [recv], [recvfrom], [send] and [sendto]. *)

value socket : socket_domain -> socket_type -> int -> file_descr
                                  = 3 "unix_socket"
        (* Create a new socket in the given domain, and with the
           given kind. The third argument is the protocol type; 0 selects
           the default protocol for that kind of sockets. *)
  and socketpair :
        socket_domain -> socket_type -> int -> file_descr * file_descr
                                  = 3 "unix_socketpair"
        (* Create a pair of unnamed sockets, connected together. *)
  and accept : file_descr -> file_descr * sockaddr = 1 "unix_accept"
        (* Accept connections on the given socket. The returned descriptor
           is a socket connected to the client; the returned address is
           the address of the connecting client. *)
  and bind : file_descr -> sockaddr -> unit = 2 "unix_bind"
        (* Bind a socket to an address. *)
  and connect : file_descr -> sockaddr -> unit = 2 "unix_connect"
        (* Connect a socket to an address. *)
  and listen : file_descr -> int -> unit = 2 "unix_listen"
        (* Set up a socket for receiving connection requests. The integer
           argument is the maximal number of pending requests. *)
  and shutdown : file_descr -> shutdown_command -> unit = 2 "unix_shutdown" 
        (* Shutdown a socket connection. [SHUTDOWN_SEND] as second argument
           causes reads on the other end of the connection to return
           an end-of-file condition.
           [SHUTDOWN_RECEIVE] causes writes on the other end of the connection
           to return a closed pipe condition ([SIGPIPE] signal). *)
  and getsockname : file_descr -> sockaddr = 1 "unix_getsockname"
        (* Return the address of the given socket. *)
  and getpeername : file_descr -> sockaddr = 1 "unix_getpeername"
        (* Return the address of the host connected to the given socket. *)
  and recv : file_descr -> string -> int -> int -> msg_flag list -> int
                                  = 5 "unix_recv"
  and recvfrom :
        file_descr -> string -> int -> int -> msg_flag list -> int * sockaddr
                                  = 5 "unix_recvfrom"
        (* Receive data from an unconnected socket. *)
  and send : file_descr -> string -> int -> int -> msg_flag list -> int
                                  = 5 "unix_send"
  and sendto :
        file_descr -> string -> int -> int -> msg_flag list -> sockaddr -> int
                                  = 6 "unix_sendto"
        (* Send data over an unconnected socket. *)
;;

(*** High-level network connection functions *)

value open_connection : sockaddr -> in_channel * out_channel
        (* Connect to a server at the given address.
           Return a pair of buffered channels connected to the server.
           Remember to call [flush] on the output channel at the right times
           to ensure correct synchronization. *)
  and shutdown_connection : in_channel -> unit
        (* ``Shut down'' a connection established with [open_connection];
           that is, transmit an end-of-file condition to the server reading
           on the other side of the connection. *)
  and establish_server : (in_channel -> out_channel -> unit) -> sockaddr -> unit
        (* Establish a server on the given address.
           The function given as first argument is called for each connection
           with two buffered channels connected to the client. A new process
           is created for each connection. The function [establish_server]
           never returns normally. *)
;;

(*** Host and protocol databases *)

type host_entry =
  { h_name : string;
    h_aliases : string vect;
    h_addrtype : socket_domain;
    h_addr_list : inet_addr vect };;
          (* Structure of entries in the [hosts] database. *)

type protocol_entry =
  { p_name : string;
    p_aliases : string vect;
    p_proto : int };;
          (* Structure of entries in the [protocols] database. *)

type service_entry =
  { s_name : string;
    s_aliases : string vect;
    s_port : int;
    s_proto : string };;
          (* Structure of entries in the [services] database. *)

value gethostname : unit -> string = 1 "unix_gethostname"
        (* Return the name of the local host. *)
  and gethostbyname : string -> host_entry = 1 "unix_gethostbyname"
        (* Find an entry in [hosts] with the given name, or raise
           [Not_found]. *)
  and gethostbyaddr : inet_addr -> host_entry = 1 "unix_gethostbyaddr"
        (* Find an entry in [hosts] with the given address, or raise
           [Not_found]. *)
  and getprotobyname : string -> protocol_entry
                                         = 1 "unix_getprotobyname"
        (* Find an entry in [protocols] with the given name, or raise
           [Not_found]. *)
  and getprotobynumber : int -> protocol_entry
                                         = 1 "unix_getprotobynumber"
        (* Find an entry in [protocols] with the given protocol number,
           or raise [Not_found]. *)
  and getservbyname : string -> string -> service_entry
                                         = 2 "unix_getservbyname"
        (* Find an entry in [services] with the given name, or raise
           [Not_found]. *)
  and getservbyport : int -> string -> service_entry
                                         = 2 "unix_getservbyport"
        (* Find an entry in [services] with the given service number,
           or raise [Not_found]. *)
;;

(*** Terminal interface *)

(* The following functions implement the POSIX standard terminal
   interface. They provide control over asynchronous communication ports
   and pseudo-terminals. Refer to the [termios] man page for a
   complete description. *)

type terminal_io = {
  (* Input modes: *)
    mutable c_ignbrk: bool;  (* Ignore the break condition. *)
    mutable c_brkint: bool;  (* Signal interrupt on break condition. *)
    mutable c_ignpar: bool;  (* Ignore characters with parity errors. *)
    mutable c_parmrk: bool;  (* Mark parity errors. *)
    mutable c_inpck: bool;   (* Enable parity check on input. *)
    mutable c_istrip: bool;  (* Strip 8th bit on input characters. *)
    mutable c_inlcr: bool;   (* Map NL to CR on input. *)
    mutable c_igncr: bool;   (* Ignore CR on input. *)
    mutable c_icrnl: bool;   (* Map CR to NL on input. *)
    mutable c_ixon: bool;    (* Recognize XON/XOFF characters on input. *)
    mutable c_ixoff: bool;   (* Emit XON/XOFF chars to control input flow. *)
  (* Output modes: *)
    mutable c_opost: bool;   (* Enable output processing. *)
  (* Control modes: *)
    mutable c_obaud: int;    (* Output baud rate (0 means close connection).*)
    mutable c_ibaud: int;    (* Input baud rate. *)
    mutable c_csize: int;    (* Number of bits per character (5-8). *)
    mutable c_cstopb: int;   (* Number of stop bits (1-2). *)
    mutable c_cread: bool;   (* Reception is enabled. *)
    mutable c_parenb: bool;  (* Enable parity generation and detection. *)
    mutable c_parodd: bool;  (* Specify odd parity instead of even. *)
    mutable c_hupcl: bool;   (* Hang up on last close. *)
    mutable c_clocal: bool;  (* Ignore modem status lines. *)
  (* Local modes: *)
    mutable c_isig: bool;    (* Generate signal on INTR, QUIT, SUSP. *)
    mutable c_icanon: bool;  (* Enable canonical processing
                                (line buffering and editing) *)
    mutable c_noflsh: bool;  (* Disable flush after INTR, QUIT, SUSP. *)
    mutable c_echo: bool;    (* Echo input characters. *)
    mutable c_echoe: bool;   (* Echo ERASE (to erase previous character). *)
    mutable c_echok: bool;   (* Echo KILL (to erase the current line). *)
    mutable c_echonl: bool;  (* Echo NL even if c_echo is not set. *)
  (* Control characters: *)
    mutable c_vintr: char;   (* Interrupt character (usually ctrl-C). *)
    mutable c_vquit: char;   (* Quit character (usually ctrl-\). *)
    mutable c_verase: char;  (* Erase character (usually DEL or ctrl-H). *)
    mutable c_vkill: char;   (* Kill line character (usually ctrl-U). *)
    mutable c_veof: char;    (* End-of-file character (usually ctrl-D). *)
    mutable c_veol: char;    (* Alternate end-of-line char. (usually none). *)
    mutable c_vmin: int;     (* Minimum number of characters to read
                                before the read request is satisfied. *)
    mutable c_vtime: int;    (* Maximum read wait (in 0.1s units). *)
    mutable c_vstart: char;  (* Start character (usually ctrl-Q). *)
    mutable c_vstop: char    (* Stop character (usually ctrl-S). *)
  };;

value tcgetattr: file_descr -> terminal_io = 1 "unix_tcgetattr";;
        (* Return the status of the terminal referred to by the given
           file descriptor. *)

type setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH;;

value tcsetattr: file_descr -> setattr_when -> terminal_io -> unit
               = 3 "unix_tcsetattr";;
        (* Set the status of the terminal referred to by the given
           file descriptor. The second argument indicates when the
           status change takes place: immediately ([TCSANOW]),
           when all pending output has been transmitted ([TCSADRAIN]),
           or after flushing all input that has been received but not
           read ([TCSAFLUSH]). [TCSADRAIN] is recommended when changing
           the output parameters; [TCSAFLUSH], when changing the input
           parameters. *)

value tcsendbreak: file_descr -> int -> unit = 2 "unix_tcsendbreak";;
        (* Send a break condition on the given file descriptor.
           The second argument is the duration of the break, in 0.1s units;
           0 means standard duration (0.25s). *)

value tcdrain: file_descr -> unit = 1 "unix_tcdrain";;
        (* Waits until all output written on the given file descriptor
           has been transmitted. *)

type flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH;;

value tcflush: file_descr -> flush_queue -> unit = 2 "unix_tcflush";;
        (* Discard data written on the given file descriptor but not yet
           transmitted, or data received but not yet read, depending on the
           second argument: [TCIFLUSH] flushes data received but not read,
           [TCOFLUSH] flushes data written but not transmitted, and
           [TCIOFLUSH] flushes both. *)

type flow_action = TCOOFF | TCOON | TCIOFF | TCION;;

value tcflow: file_descr -> flow_action -> unit = 2 "unix_tcflow";;
        (* Suspend or restart reception or transmission of data on
           the given file descriptor, depending on the second argument:
           [TCOOFF] suspends output, [TCOON] restarts output,
           [TCIOFF] transmits a STOP character to suspend input,
           and [TCION] transmits a START character to restart input. *)
