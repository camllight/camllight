let bind widget tag eventsequence action =
  tkDo [| cCAMLtoTKwidget widget_canvas_table widget;
          TkToken "bind";
      	  cCAMLtoTKtagOrId tag;
       	  cCAMLtoTKeventSequence eventsequence;
  begin match action with
  | BindRemove -> TkToken ""
  | BindSet (what, f) ->
     let cbId = register_callback widget (wrapeventInfo f what) in
     TkToken ("camlcb " ^ cbId ^ (writeeventField what))
  | BindSetBreakable (what, f) ->
     let cbId = register_callback widget (wrapeventInfo f what) in
     TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
              " ; if { $BreakBindingsSequence == 1 } then \
                     { break ;} ; set BreakBindingsSequence 0"
                )
  |  BindExtend (what, f) ->
      let cbId = register_callback widget (wrapeventInfo f what) in
      TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
  end |]
;;
