#open "tk";;

let main () =
  let top = OpenTk ()  in
  (* The widgets. They all have "top" as parent widget. *)
  let en1 = entry__create top [TextWidth 6; Relief Sunken] in
  let lab1 = label__create top [Text "plus"] in
  let en2 = entry__create top [TextWidth 6 ; Relief Sunken] in
  let lab2 = label__create top [Text "="] in
  let result_display = label__create top [] in
  (* References holding values of entry widgets *)
  let n1 = ref 0
  and n2 = ref 0  in
  (* Refresh result *)
  let refresh () =
    label__configure result_display [Text (string_of_int (!n1 + !n2))]  in
  (* Electric *)
  let get_and_refresh (w,r) =
    fun _ _ _ _ ->
      try
       r := int_of_string (entry__get w);
       refresh ()
      with
      	Failure "int_of_string" ->
          label__configure result_display [Text "error"]
  in
  (* Set the callbacks *)
  entry__configure en1 [ScrollCommand (get_and_refresh (en1,n1)) ];
  entry__configure en2 [ScrollCommand (get_and_refresh (en2,n2)) ];
  (* Map the widgets *)
  pack [en1;lab1;en2;lab2;result_display] [];
  (* Make the window resizable *)
  wm__minsize_set top 1 1;
  (* Start interaction (event-driven program) *)
  MainLoop ()
;;

printexc__f main () ;;
