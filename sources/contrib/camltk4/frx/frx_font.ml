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
let find fmly wght slant pxlsz =
  let fontspec =
     "-*-"^fmly^"-"^wght^"-"^slant^"-normal-*-"^string_of_int pxlsz^"-*-*-*-*-*-iso8859-1" in
  let c = canvas__create default_toplevel_widget [] in
  try
    canvas__createtext c (Pixels 0) (Pixels 0) [Text "foo"; Font fontspec];
    destroy c;
    fontspec
  with
    _ -> destroy c; raise (Invalid_argument fontspec)
;;
