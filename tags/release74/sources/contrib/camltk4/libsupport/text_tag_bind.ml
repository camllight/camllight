let tag_bind widget tag eventsequence action =
  check_widget_class widget widget_text_table;
  tkDo [| cCAMLtoTKwidget widget_text_table widget;
          TkToken "tag";
          TkToken "bind";
          cCAMLtoTKtextTag tag;
          cCAMLtoTKeventSequence eventsequence;
  begin match action with
     BindRemove -> TkToken ""
  |  BindSet (what, f) ->
      let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what))
  |  BindSetBreakable (what, f) ->
      let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  BindExtend (what, f) ->
      let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
  end
  |]
;;
