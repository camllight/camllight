#open "expr";;
#open "auto";;
#open "determ";;

let ligne_trouvée = ref false;;

let grep_sur_canal auto nom_fich canal =
  try
    while true do
      let ligne = input_line canal in
      if fastrec__reconnaît auto ligne then begin
        ligne_trouvée := true;
        print_string nom_fich;
        print_string": ";
        print_endline ligne
      end
    done
  with End_of_file -> ();;

let grep_sur_fichier auto nom_fich =
  try
    let canal = open_in nom_fich in
    try grep_sur_canal auto nom_fich canal; close_in canal
    with exc -> close_in canal; raise exc
  with sys__Sys_error message ->
    prerr_string "Erreur sur le fichier ";
    prerr_string nom_fich;
    prerr_string ": ";
    prerr_endline message;;

let construire_auto expr =
  déterminise(expr_vers_automate(lire(stream_of_string expr)));;

let grep expr fichier =
  grep_sur_fichier (construire_auto expr) fichier;;

if sys__interactive then () else
  if vect_length sys__command_line < 2 then begin
    prerr_endline "Utilisation: grep <motif> <fichiers>";
    exit 2
  end else begin
    let auto =
      try construire_auto sys__command_line.(1)
      with Parse_error | Parse_failure ->
        prerr_endline "Erreur de syntaxe dans l'expression";
        exit 2 in
    if vect_length sys__command_line >= 3 then
      for i = 2 to vect_length sys__command_line - 1 do
        grep_sur_fichier auto sys__command_line.(i)
      done
    else
      grep_sur_canal auto "(entrée standard)" std_in;
    exit (if !ligne_trouvée then 0 else 1)
  end;;
