#open "tk";;

let parse_color line =
  let sep = function
    ` ` | `\t` -> true
   | _ -> false in
  let l = support__split_str sep line in
  let r::rest = l in
  let g::rest = rest in
  let b::rest = rest in
  let name = support__catenate_sep " " rest in
    r,g,b,name
;;


(* Same bug as in the book (when color name contains spaces)
   e.g. selection get will return "{ghost white}" 
*)



let top = OpenTk () in
  let lb = listbox__create top [] in
   pack [lb][];
  let f = open_in "/usr/lib/X11/rgb.txt" in
    begin try
      while true do
	let _,_,_,name = parse_color (input_line f) in
      	 listbox__insert lb End [name]
      done
     with End_of_file -> close_in f
    end;

    bind lb [[Double], WhatButton 1]
        (BindSet([], function _ ->
	               listbox__configure lb
      	       	       	 [Background (NamedColor (selection__get()))]));
    MainLoop()
;;


