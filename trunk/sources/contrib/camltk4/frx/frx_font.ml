#open "tk";;
#open "support";;

let version = "$Id$"
;;

(* 
 * Finding fonts. Inspired by code in Ical by Sanjay Ghemawat.
 * Possibly bogus because some families use "i" for italic where others
 * use "o".
 * wght: bold, medium
 * slant: i, o, r
 * pxlsz: 8, 10, ...
*)

let available_fonts = ref (set__empty compare_strings)
;;

let get_canvas = 
  frx_misc__autodef (fun () -> canvas__create default_toplevel_widget [])
;;

let find fmly wght slant pxlsz =
  let fontspec =
     "-*-"^fmly^"-"^wght^"-"^slant^"-normal-*-"^string_of_int pxlsz^"-*-*-*-*-*-iso8859-1" in
    if set__mem fontspec !available_fonts then fontspec
    else
      let c = get_canvas() in
      try
	let tag = canvas__create_text c (Pixels 0) (Pixels 0) 
                                [Text "foo"; Font fontspec] in
	   canvas__delete c [tag];
	   available_fonts := set__add fontspec !available_fonts;
	   fontspec
      with
        _ -> raise (Invalid_argument fontspec)
;;
