(* Operations on file names *)

value current_dir_name : string
        (* The conventional name for the current directory
           (e.g. [.] in Unix). *)
  and concat : string -> string -> string
        (* [concat dir file] returns a file name that designates file
           [file] in directory [dir]. *)
  and is_absolute : string -> bool
        (* Return [true] if the file name is absolute or starts with an
           explicit reference to the current directory ([./] or [../] in
           Unix), and [false] if it is relative to the current directory. *)
  and check_suffix : string -> string -> bool
        (* [check_suffix name suff] returns [true] if the filename [name]
           ends with the suffix [suff]. *)
  and chop_suffix : string -> string -> string
        (* [chop_suffix name suff] removes the suffix [suff] from 
           the filename [name]. The behavior is undefined if [name] does not
           end with the suffix [suff]. *)
  and basename : string -> string
  and dirname : string -> string
        (* Split a file name into directory name / base file name.
           [concat (dirname name) (basename name)] returns a file name
           which is equivalent to [name]. Moreover, after setting the
           current directory to [dirname name] (with [sys__chdir]),
           references to [basename name] (which is a relative file name)
	   designate the same file as [name] before the call to [chdir]. *)
;;
