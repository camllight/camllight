#open "prop";;
#open "asynt";;

let examine cha�ne =
    let proposition = analyse_proposition cha�ne in
    let variables = variables_libres proposition in
    try
      v�rifie_tautologie proposition variables;
      begin match variables with
      | [] ->
          print_string "Th�or�me: "
      | [var] ->
          print_string ("Th�or�me: pour toute proposition "^var^", ")
      | _ ->
          print_string "Th�or�me: pour toutes propositions ";
          do_list (function var -> print_string (var^", ")) variables
      end;
      print_string cha�ne;
      print_newline()
    with R�futation liaisons ->
      print_string (cha�ne ^ " n'est pas un th�or�me,\n");
      print_string "car la proposition est fausse quand\n";
      do_list
       (function (var, b) ->
         print_string (var ^ " est ");
         print_string (if b then "vraie" else "fausse");
         print_newline ())
       liaisons;;
let boucle () =
  try
    while true do
      print_string ">>> "; examine(read_line())
    done
  with End_of_file -> ();;

if sys__interactive then () else begin boucle(); exit 0 end;;
