
(*************************************************************************)
(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(*                          CamlLight                                    *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            Inria                                      *)
(*                      Domaine de Voluceau                              *)
(*                      78150  Rocquencourt                              *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* SearchIsos.ml Main program for the standalone library search          *)
(*		Roberto Di Cosmo                			 *)

#open "sys";;
#open "arg";;
#open "unix";;
#open "module_iterator";;
#open "Equal";;
#open "myTypes";;
#open "TypeOfString";;
#open "builtins";;
#open "const";;
#open "globals";;
#open "common";;

(* Usage *)
let usage () =
  prerr_endline "Usage: search_isos type [.zi files or directories]";
  exit 1;;

(* parse the command line *)

(* No arguments: teach user proper usage *)
if (vect_length command_line) < 2
  then usage();;

(* Get user options using Xavier's nice arg.ml interface *)

let modules = ref ([]: string list);; (* argument pointer *)

let speclist =
 [
  ("-debug", arg__Unit((fun x -> debug := true))); (* turn on debugging *)
  ("-nounit", arg__Unit((fun x -> TypeRewrite__full_iso := false))) (* do not collapse types *)
  ]  
and anonfun m_name = modules:=m_name::!modules
in parse speclist anonfun;;

(* Hey, the first one is really the type we look for! *)
(* give an error if missing                           *)

let search_type =
  match rev !modules with
    [] -> usage()
  | ty::names -> modules := names; ty;;

(* A test for a proper module interface filename *)
let proper_interf_fn fn =
  let l = (string_length fn)
  in (l > 3 & (sub_string fn (l-3) 3) = ".zi");;

(* Look up all .zi files in a directory *)

let interf_of_dir dirname =
  let result = ref [] 
  and d = opendir dirname in
  begin try
    while true do
      let fn = (readdir d) in
      if proper_interf_fn fn
      then result := filename__concat dirname fn :: !result
    done
  with End_of_file -> ()
  end;
  closedir d;
  !result
;;

(* Rewrite "modules" to expand directories and add 
   the .zi extension if necessary. No arguments is taken to mean "." *)

match !modules with
    [] ->
      modules := interf_of_dir "."
  | _ ->
      modules :=
        flat_map
          (fun name ->
            let fullname =
              if proper_interf_fn name then name else name ^ ".zi" in
            try
              unix__stat fullname; [fullname]
            with Unix_error _ ->
              try
                match (unix__stat name).st_kind with
                  S_DIR -> interf_of_dir name
                | _ -> raise (Unix_error(ENOENT, "", ""))
              with Unix_error _ ->
                prerr_endline ("Warning: " ^ fullname ^ " not found, ignored.");
                [])
          !modules
;;

(* Now we can start the search *)

let main() =
  let theType = (* check for syntax errors *)
    begin try
      (TypeOfString__type_of_string search_type)
    with _ ->
      prerr_endline "# Syntax error: type garbled!"; exit 1
    end in
  let typ_repr = (myTypes__squeeze_typ theType) in
  module_iterator__iterate_on_module_interface
    (filter_iso_to typ_repr) !modules;
  exit 0
;;

printexc__f main ();;
