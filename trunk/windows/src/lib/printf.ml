#open "exc";;
#open "eq";;
#open "bool";;
#open "float";;
#open "int";;
#open "fchar";;
#open "fstring";;
#open "io";;
#open "obj";;

let printf_formats = 
  (hashtbl__new 13 : (char, out_channel -> obj -> unit) hashtbl__t);;

let fprintf outchan format =
  let rec doprn i =
    if i >= string_length format then
      magic(fun x -> invalid_arg "fprintf: too many arguments")
    else
      match nth_char format i with
        `%` ->
          let j = skip_args (succ i) in
          begin match nth_char format j with
            `%` ->
              output_char outchan `%`;
              doprn (succ j)
          | `s` ->
              magic(fun s ->
                if (not is_block (repr s)) or obj_tag (repr s) != 253 then
                  invalid_arg "fprintf: string argument expected"
                else if j <= i+1 then
                  output_string outchan s
                else begin
                  let p =
                    try
                      int_of_string (sub_string format (i+1) (j-i-1))
                    with _ ->
                      invalid_arg "fprintf: bad %s format" in
                  if p > 0 & string_length s < p then begin
                    output_string outchan
                                  (make_string (p - string_length s) ` `);
                    output_string outchan s
                  end else if p < 0 & string_length s < -p then begin
                    output_string outchan s;
                    output_string outchan
                                  (make_string (-p - string_length s) ` `)
                  end else
                    output_string outchan s
                end;
                doprn (succ j))
          | `c` ->
              magic(fun c ->
                if is_block (repr c) then
                  invalid_arg "fprintf: char argument expected"
                else begin
                  output_char outchan c;
                  doprn (succ j)
                end)
          | `d` | `o` | `x` | `X` | `u` ->
              magic(doint i j)
          | `f` | `e` | `E` | `g` | `G` ->
              magic(dofloat i j)
          | `b` ->
              magic(fun b ->
                if is_block (repr b) then
                  output_string outchan (if b then "true" else "false")
                else
                  invalid_arg "fprintf: boolean argument expected";
                doprn (succ j))
          | c ->
              begin try
                let f = hashtbl__find printf_formats c in
                magic(fun arg -> f outchan arg; doprn (succ j))
              with Not_found ->
                invalid_arg ("fprintf: unknown format " ^ char_for_read c)
              end
          end
      |  c  -> output_char outchan c; doprn (succ i)

  and skip_args j =
    match nth_char format j with
      `0` | `1` | `2` | `3` | `4` | `5` | `6` | `7` | `8` | `9` |
      ` ` | `.` | `-` ->
        skip_args (succ j)
    | c ->
        j
    
  and doint i j n =
    if is_block (repr n) then
      invalid_arg "fprintf: int argument expected"
    else begin
      let len = j-i in
      let fmt = create_string (len+2) in
      blit_string format i fmt 0 len;
      set_nth_char fmt len `l`;
      set_nth_char fmt (len+1) (nth_char format j);
      output_string outchan (format_int fmt n);
      doprn (succ j)
    end

  and dofloat i j f =
    if (not is_block (repr f)) or obj_tag (repr f) != 254 then
      invalid_arg "fprintf: float argument expected"
    else begin
      output_string outchan (format_float (sub_string format i (j-i+1)) f);
      doprn (succ j)
    end

  in doprn 0
;;

let add_format letter (action : out_channel -> 'a -> unit) =
  hashtbl__add printf_formats letter (magic action)
;;

let add_integer_format letter (action : out_channel -> int -> unit) =
  add_format letter 
    (fun oc n ->
      if is_block n then
        invalid_arg "fprintf: integer argument expected"
      else
        action oc (magic n))
;;

let add_string_format letter (action : out_channel -> string -> unit) =
  add_format letter 
    (fun oc s ->
      if is_block s & obj_tag s == 253 then
        action oc (magic s)
      else
        invalid_arg "fprintf: string argument expected")
;;

let printf fmt = fprintf std_out fmt    (* Don't eta-reduce: this confuses *)
and eprintf fmt = fprintf std_err fmt   (* the intelligent linker *)
;;

let fprint = output_string
and print = print_string
and eprint = prerr_string
;;
