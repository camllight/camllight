#open"tk";;

let top = OpenTk () in
 let m = menu__create top [] in
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
 
   menu__post m 40 40;				  
   MainLoop()
;;
