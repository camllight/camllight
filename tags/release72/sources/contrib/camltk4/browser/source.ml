#open "misc";;
#open "lexer";;
#open "lexing";;

(* Get an image of the file in a string *)
let load_file filename = 
  let real_name = find_in_path filename in
  let ic = open_in_bin real_name in
  let size = in_channel_length ic in
  let s = create_string size in
    really_input ic s 0 size;
    close_in ic;
    s
;;    

(* Compute decorations *)
let compute_source_tags s =
  let l = create_lexer_string s
  and keywords = ref []
  and comments = ref [] 
  and modules = ref [] in
  let rec next_token () =
    match main l with
      Keyword(_,i,j) -> keywords := (i,j) :: !keywords;
                        next_token()
   |  Commentaire(i,j) -> comments := (i,j) :: !comments;
                          next_token()
   |  Open (i,j) -> modules := (i,j) :: !modules; next_token()
   |  Dontcare -> next_token()
   | EOF -> () in
  next_token();
  ["keyword", !keywords; "comment", !comments; "modules", !modules]
;;


