(* TODO: restrict event fields *)
let canvas_bind widget tag eventsequence action =
  TkEval [| CAMLtoTKWidget Widget_canvas_table widget;
      	    TkToken "bind";
      	    CAMLtoTKTagOrId tag;
       	    CAMLtoTKEventSequence eventsequence;
  begin match action with
     BindRemove -> TkToken ""
  |  BindSet (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("camlcb " ^ CbId ^ (WriteEventField what))
  |  BindSetBreakable (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("camlcb " ^ CbId ^ (WriteEventField what)^
                " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  BindExtend (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("+camlcb " ^ CbId ^ (WriteEventField what))

  end |];
  ()
;;

