#open "exc";;
#open "eq";;
#open "bool";;
#open "float";;
#open "int";;
#open "fstring";;
#open "io";;
#open "list";;
#open "obj";;
#open "ref";;

type ('a, 'b, 'c) format == string;;

let fprintf outchan format =
  let rec doprn i =
    if i >= string_length format then
      magic ()
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
                if j <= i+1 then
                  output_string outchan s
                else begin
                  let p =
                    try
                      int_of_string (sub_string format (i+1) (j-i-1))
                    with _ ->
                      invalid_arg "fprintf: bad %s format" in
                  if p > 0 && string_length s < p then begin
                    output_string outchan
                                  (make_string (p - string_length s) ` `);
                    output_string outchan s
                  end else if p < 0 && string_length s < -p then begin
                    output_string outchan s;
                    output_string outchan
                                  (make_string (-p - string_length s) ` `)
                  end else
                    output_string outchan s
                end;
                doprn (succ j))
          | `c` ->
              magic(fun c ->
                output_char outchan c;
                doprn (succ j))
          | `d` | `o` | `x` | `X` | `u` ->
              magic(doint i j)
          | `f` | `e` | `E` | `g` | `G` ->
              magic(dofloat i j)
          | `b` ->
              magic(fun b ->
                output_string outchan (string_of_bool b);
                doprn (succ j))
          | `a` ->
              magic(fun printer arg ->
                printer outchan arg;
                doprn(succ j))
          | `t` ->
              magic(fun printer ->
                printer outchan;
                doprn(succ j))
          | c ->
              invalid_arg ("fprintf: unknown format")
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
    let len = j-i in
    let fmt = create_string (len+2) in
    blit_string format i fmt 0 len;
    set_nth_char fmt len `l`;
    set_nth_char fmt (len+1) (nth_char format j);
    output_string outchan (format_int fmt n);
    doprn (succ j)

  and dofloat i j f =
    output_string outchan (format_float (sub_string format i (j-i+1)) f);
    doprn (succ j)

  in doprn 0
;;

let printf fmt = fprintf std_out fmt    (* Don't eta-reduce: this confuses *)
and eprintf fmt = fprintf std_err fmt   (* the intelligent linker *)
;;

let fprint chan str = output_string chan str
and print str = print_string str
and eprint str = prerr_string str
;;

let sprintf format =
  let format = (magic format : string) in
  let res = ref [] in

  let rec doprn start i =
    if i >= string_length format then begin
      if i > start then res := sub_string format start (i-start) :: !res;
      magic(concat(rev !res))
    end else
      if nth_char format i != `%` then
        doprn start (i+1)
      else begin
        if i > start then res := sub_string format start (i-start) :: !res;
        let j = skip_args (succ i) in
        match nth_char format j with
          `%` ->
            doprn j (succ j)
        | `s` ->
            magic(dostring i j)
        | `c` ->
            magic(fun c ->
                res := make_string 1 c :: !res;
                doprn (succ j) (succ j))
        | `d` | `i` | `o` | `x` | `X` | `u` ->
            magic(doint i j)
        | `f` | `e` | `E` | `g` | `G` ->
            magic(dofloat i j)
        | `b` ->
            magic(fun b ->
                res := string_of_bool b :: !res;
                doprn (succ j) (succ j))
        | `a` ->
            magic(fun printer arg ->
              res := printer () arg :: !res;
              doprn (succ j) (succ j))
        | `t` ->
            magic(fun printer ->
              res := printer () :: !res;
              doprn (succ j) (succ j))
        | c ->
            invalid_arg ("sprintf: unknown format")
      end

  and skip_args j =
    match nth_char format j with
      `0` | `1` | `2` | `3` | `4` | `5` | `6` | `7` | `8` | `9` |
      ` ` | `.` | `-` ->
        skip_args (succ j)
    | c ->
        j
    
  and dostring i j s =
    if j <= i+1 then
      res := s :: !res
    else begin
      let p =
        try
          int_of_string (sub_string format (i+1) (j-i-1))
        with _ ->
          invalid_arg "sprintf: bad %s format" in
      if p > 0 && string_length s < p then begin
        res := make_string (p - string_length s) ` ` :: !res;
        res := s :: !res
      end else if p < 0 && string_length s < -p then begin
        res := s :: !res;
        res := make_string (-p - string_length s) ` ` :: !res
      end else
        res := s :: !res
    end;
    doprn (succ j) (succ j)

  and doint i j n =
    let len = j-i in
    let fmt = create_string (len+2) in
    blit_string format i fmt 0 len;
    set_nth_char fmt len `l`;
    set_nth_char fmt (len+1) (nth_char format j);
    res := format_int fmt n :: !res;
    doprn (succ j) (succ j)

  and dofloat i j f =
    res := format_float (sub_string format i (j-i+1)) f :: !res;
    doprn (succ j) (succ j)

  in doprn 0 0
;;
