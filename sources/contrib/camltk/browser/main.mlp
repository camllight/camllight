#open "unix";;
#open "tk";;
#open "config";;
#open "misc";;
#open "modules";;
#open "hyper_printers";;
#open "visual";;
#open "source";;


let bye () = 
  CloseTk();
  exit 0
;;

(* as directive #open *)
let internal_open_module name =
  used_modules := find_module name :: !used_modules; ()
;;
let internal_close_module name =
      used_modules := exceptq (find_module name) !used_modules; ()
;;


(* Open generic requester *)
let new_open_top = 
  let cnter = ref 0 in
  function () ->
   incr cnter; "open" ^ string_of_int !cnter
;;


let open title action =
  let t = toplevelw__create (support__new_toplevel_widget "open") [] in
  focus__set t;
  grab__set_local t;
  let tit = label__create t [Text title] in
  let e = entry__create t [Relief Sunken] in
    tk__bind e [[], XKey "Return"]
      	(BindSet ([], fun _ -> action (entry__get e); destroy t));

  let f = frame__create t [] in
  let bok = button__create f
      	    [Text "Ok"; 
      	     Command (fun () -> action (entry__get e); destroy t)] in
  let bcancel = button__create f
      	    [Text "Cancel"; 
      	     Command (fun () -> destroy t)] in

    tk__bind t [[], XKey "Return"]
      	 (BindSet ([], (fun _ -> button__invoke bok)));
    tk__bind t [[], XKey "Escape"]
      	 (BindSet ([], (fun _ -> button__invoke bcancel)));
    tk__bind e [[], XKey "Escape"]
      	 (BindSet ([], (fun _ -> button__invoke bcancel)));

    pack [bok; bcancel] [Side Side_Left; Fill Fill_X; Expand true];
    pack [tit;e] [Fill Fill_X];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    util__resizeable t;
    focus__set e
;;

let internal_add_directory dirname =
 load_path := dirname :: !load_path
;;

let add_directory () =
  open "Add a directory to the load path" internal_add_directory
;;

let main () = 
  let top = OpenTkClass "CamlBrowser" in
     signal SIGINT (Signal_handle bye);
     signal SIGTERM (Signal_handle bye);
  (* Resources *)
  resource__add "*library_path" "/usr/local/lib/caml-light"
      WidgetDefault;
  resource__add "*module_set" "cautious"
      WidgetDefault;
  let libpath = resource__get top "library_path" "library_path" in
  let module_set = resource__get top "module_set" "module_set" in
  (* Initialisation of the module machinery *)
  default_used_modules := assoc module_set default_used_interfaces;
  path_library := libpath;
  load_path := [!path_library];
  (* highlighting of anchors *)
  begin match colormodel__get top with
      Monochrome -> anchor_attrib := [Underline true]
    | _ -> ()
  end;

   let t = label__create top [Text "Caml Browser"; Relief Raised] in

   let g = frame__create top [] in (* hgroup *)

   let f = frame__create g [] in (* vgroup *)
   let mods = label__create f [Text "Opened Modules"] in

   let f1 = frame__create f [Relief Sunken] in (* hgroup *)
   let lb = listbox__create f1 [] in
   let sb = scrollbar__create f1 [] in
     util__scroll_listbox_link sb lb;
     pack [lb] [Side Side_Left; Fill Fill_Both; Expand true];
     pack [sb] [Side Side_Right; Fill Fill_Y];

    tk__bind lb [[Double], WhatButton 1] 
           (BindSet ([], fun _ ->
                          do_list (fun s ->
			             visual_module false (listbox__get lb s))
                                     (listbox__curselection lb)));
   let open_module n =
     try 
       internal_open_module n ;
       listbox__insert lb End [n]
     with
       Toplevel -> begin
      	  dialog (support__new_toplevel_widget "error")
	      "Caml Browser Error"
	      ( "Cannot open module :" ^ n)
	      ""
	      0
	      ["Ok"];
	  ()
         end

   and close_module () =
     do_list (fun s -> internal_close_module (listbox__get lb s);
                       listbox__delete lb s s)
             (listbox__curselection lb) in

    do_list (fun n -> internal_open_module n ;
                      listbox__insert lb End [n]) !default_used_modules;
 

   let o2 = button__create f
      [Text "Open source file"; Relief Raised;
       Command (fun () -> open "Open a source file" display_file)] in
   let c = button__create f
      [Text "Close Module"; Relief Raised;
       Command close_module] in
   let o = button__create f
      [Text "Open Module"; Relief Raised; 
       Command (fun () -> open "Open a module" open_module)] in
   let a = button__create f
      [Text "Add Directory"; Relief Raised; 
       Command add_directory] in

   pack [mods] [Fill Fill_X];
   pack [f1] [Fill Fill_Both; Expand true];
   pack [o2;c;o;a] [Side Side_Bottom; Anchor Center; Fill Fill_X];

   let f2 = frame__create g [] in
   let m = label__create f2 [Text "Enter a global symbol:"] in
   let e = entry__create f2 [Relief Sunken] in
      tk__bind e [[], XKey "Return"]
                 (BindSet ([], (fun _ -> visual_search_any 
      	       	       	       	       	  (entry__get e))));
   pack [m;e] [Fill Fill_X];


   pack [f] [Side Side_Left; Fill Fill_Both; Expand true];
   pack [f2] [Side Side_Right; Anchor Center];

   let b = button__create top [Text "Quit"; Relief Raised; Command bye] in
   pack [t] [Fill Fill_X];
   pack [g] [Expand true; Fill Fill_Both];
   pack [b] [Side Side_Bottom; Fill Fill_X];
   (* Make e "autofocus" *)
   tk__bind e [[Any], Enter]
       	 (BindSet ([], (fun _ -> focus__set e)));
   tk__bind e [[Any], Leave]
       	 (BindSet ([], (fun _ -> focus__none ())));
   util__resizeable top;
   focus__set e;
   try
     tk__MainLoop()
   with
     ex -> begin CloseTk(); raise ex end
;;

(* protocol__debug := true;; *)


printexc__f (handle_unix_error main) ()
;;

