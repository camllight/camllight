#open "tk";;

let top = OpenTk() in
  let lb = label__create top 
      	    [Bitmap (BitmapFile "/usr/local/lib/tk/scripts/demos/bitmaps/flagdown")]
  and ll = label__create top
      	    [Text "No new mail"] in
   pack [lb; ll] [];
   MainLoop()
;;
