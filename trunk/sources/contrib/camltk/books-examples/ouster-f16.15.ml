#open "tk";;

let top = OpenTk() in
 let mbar = 
    frame__create top [Relief Raised; Borderwidth (Pixels 2)] 
 and dummy = 
    frame__create top [Width (Centimeters 10.); Height (Centimeters 5.)] in
    pack [mbar; dummy] [Side Side_Top; Fill Fill_X];
 let file = menubutton__create mbar [Text "File"; UnderlinedChar 0]
 and edit = menubutton__create mbar [Text "Edit"; UnderlinedChar 0]
 and graphics = menubutton__create mbar [Text "Graphics"; UnderlinedChar 0]
 and text = menubutton__create mbar [Text "Text"; UnderlinedChar 0]
 and view = menubutton__create mbar [Text "View"; UnderlinedChar 0]
 and help = menubutton__create mbar [Text "Help"; UnderlinedChar 0] in
   pack [file;edit;graphics;text;view] [Side Side_Left];
   pack [help] [Side Side_Right];
   (* same code as chap16-14 *)
  let m = menu__create text [] in
   let bold = textvariable__new() 
   and italic = textvariable__new() 
   and underline = textvariable__new() in
   menu__add_checkbutton m [Label "Bold"; Variable bold];
   menu__add_checkbutton m [Label "Italic"; Variable italic];
   menu__add_checkbutton m [Label "Underline"; Variable underline];
   menu__add_separator m;
   let font = textvariable__new() in
   menu__add_radiobutton m [Label "Times"; Variable font; Value "times"];
   menu__add_radiobutton m [Label "Helvetica"; Variable font; Value "helvetica"];
   menu__add_radiobutton m [Label "Courier"; Variable font; Value "courier"];
   menu__add_separator m;
   menu__add_command m [Label "Insert Bullet";
      	       	        Command (function () -> 
      	       	       	       	  print_string "Insert Bullet\n"; 
      	       	       	       	  flush stdout)];   
   menu__add_command m [Label "Margins and Tags...";
      	       	        Command (function () -> 
      	       	       	       	  print_string "margins\n"; 
      	       	       	       	  flush stdout)]; 
   menubutton__configure text [Menu m];

    menubar__set mbar [file;edit;graphics;text;view;help];
    focus__set mbar;
    MainLoop()
;;
