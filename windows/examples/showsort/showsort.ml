(* Animation of sorting algorithms. *)

#open "graphics";;

(* The status of a sorting process *)

type status =
    Finished
  | Pause of unit -> status
;;

(* Information on a given sorting process *)

type graphic_context =
  { array: int vect;                    (* Data to sort *)
    x0: int;                            (* X coordinate, lower left corner *)
    y0: int;                            (* Y coordinate, lower left corner *)
    width: int;                         (* Width in pixels *)
    height: int;                        (* Height in pixels *)
    nelts: int;                         (* Number of elements in the array *)
    maxval: int;                        (* Max value in the array + 1 *)
    rad: int;                           (* Dimension of the rectangles *)
    mutable action: status              (* What to do next *)
  }
;;

(* Array assignment and exchange with screen update *)

let draw gc i v =
  fill_rect (gc.x0 + (gc.width * i) / gc.nelts)
            (gc.y0 + (gc.height * v) / gc.maxval)
            gc.rad gc.rad
;;

let assign gc i v =
  set_color background; draw gc i gc.array.(i);
  set_color foreground; draw gc i v;
  gc.array.(i) <- v
;;

let exchange gc i j =
  let val_i = gc.array.(i) in
  assign gc i gc.array.(j);
  assign gc j val_i
;;

(* Construction of a graphic context *)

let initialize name funct array maxval x y w h =
  let (_, label_height) = text_size name in
  let rad = (w - 2) / (vect_length array) - 1 in
  let gc =
    { array = copy_vect array;
      x0 = x + 1;                       (* Leave one pixel left for Y axis *)
      y0 = y + 1;                       (* Leave one pixel below for X axis *)
      width = w - 2;                    (* 1 pixel left, 1 pixel right *)
      height = h - 1 - label_height - rad;
      nelts = vect_length array;
      maxval = maxval;
      rad = rad;
      action = Finished } in
  moveto (gc.x0 - 1) (gc.y0 + gc.height);
  lineto (gc.x0 - 1) (gc.y0 - 1);
  lineto (gc.x0 + gc.width) (gc.y0 - 1);
  moveto (gc.x0 - 1) (gc.y0 + gc.height);
  draw_string name;
  for i = 0 to vect_length array - 1 do
    draw gc i array.(i)
  done;
  gc.action <- funct gc;
  gc
;;

(* Main animation function *)

let skip_key () = let _ = read_key () in ();;

let display functs nelts maxval =
  let a = make_vect nelts 0 in
  for i = 0 to nelts - 1 do
    a.(i) <- random__int maxval
  done;
  let q = queue__new () in
  for i = 0 to vect_length functs - 1 do
    let (name, funct, x, y, w, h) = functs.(i) in
    queue__add (initialize name funct a maxval x y w h) q
  done;
  let delay = ref 0 in
  try
    while true do
      let gc = queue__take q in
        begin match gc.action with
        | Finished -> ()
        | Pause f ->
            gc.action <- f ();
            for i = 0 to !delay do () done;
            queue__add gc q
        end;
      if key_pressed() then begin
        match read_key() with
        | `q`|`Q` ->
            raise Exit
        | `0`..`9` as c ->
            delay := (int_of_char c - 48) * 500
        | _ ->
            ()
      end
    done
  with Exit -> ()
     | queue__Empty -> skip_key ()
;;

(* The sorting functions.
   These functions are written in some kind of continuation-passing style
   so that whenever they are about to perform a comparison, they stop
   and return a function that does the remainder of the sort. *)

(* Bubble sort *)

let bubble_sort gc =
  let ordered = ref true in
  let rec sweep i =
    if i+1 >= vect_length gc.array then
      if !ordered then
        Finished
      else begin
        ordered := true;
        sweep 0
      end
    else
      Pause(fun () ->
        if gc.array.(i+1) < gc.array.(i) then begin
          exchange gc i (i+1);
          ordered := false
        end;
        sweep(i+1))
  in sweep 0
;;

(* Insertion sort *)

let insertion_sort gc =
  let rec loop1 i =
    if i >= vect_length gc.array then Finished else
    let val_i = gc.array.(i) in
    let rec loop2 j =
      if j < 1 then begin
        assign gc j val_i;
        loop1 (i+1)
      end else
        Pause(fun () ->
          if val_i >= gc.array.(j-1) then begin
            assign gc j val_i;
            loop1 (i+1)
          end else begin
            assign gc j gc.array.(j-1);
            loop2 (j-1)
          end)
    in loop2 i
  in loop1 1
;;

(* Selection sort *)

let selection_sort gc =
  let rec loop1 i =
    if i+1 >= vect_length gc.array then Finished else
    let min = ref i in
    let rec loop2 j =
      if j >= vect_length gc.array then begin
        exchange gc i !min;
        loop1 (i+1)
      end else
        Pause(fun () ->
          if gc.array.(j) < gc.array.(!min) then min := j;
          loop2 (j+1))
    in loop2 (i+1)
  in loop1 0
;;

(* Quick sort *)

let quick_sort gc =
  let rec quick lo hi cont =
    if lo >= hi then cont () else begin
      let p = gc.array.(hi) in
      let rec loop1 i j =
        if i >= j then begin
          exchange gc hi i;
          quick lo (i-1) (fun () -> quick (i+1) hi cont)
        end else begin
          let rec loop3 i j =
            if j <= lo then begin
              if i < j then exchange gc i j;
              loop1 i j
            end else
              Pause(fun () ->
                if p <= gc.array.(j) then
                  loop3 i (j-1)
                else begin
                  if i < j then exchange gc i j;
                  loop1 i j
                end) in
          let rec loop2 i j =
            if i >= hi then loop3 i j else
              Pause(fun () ->
                if gc.array.(i) <= p then
                  loop2 (i+1) j
                else
                  loop3 i j)
          in loop2 i j
        end
      in loop1 lo hi
    end
  in quick 0 (vect_length gc.array - 1) (fun () -> Finished)
;;

(* Heap sort *)

let heap_sort gc =

  let father n = (n-1) / 2
  and left_son n = n + n + 1
  and right_son n = n + n + 2 in

  let rec from_bottom i last cont =
    if i == 0 then cont() else begin
      let j = father i in
      let ls = left_son j and rs = right_son j in
      if rs > last then
        Pause(fun () ->
          if gc.array.(j) < gc.array.(ls) then exchange gc j ls;
          from_bottom j last cont)
      else
        Pause(fun () ->
          if gc.array.(j) < gc.array.(ls) then
            Pause(fun () ->
              if gc.array.(rs) <= gc.array.(ls) then
                exchange gc j ls
              else
                exchange gc j rs;
              from_bottom j last cont)
          else
            Pause(fun () ->
              if gc.array.(j) < gc.array.(rs) then
                Pause(fun () ->
                  if gc.array.(rs) <= gc.array.(ls) then
                    exchange gc j ls
                  else
                    exchange gc j rs;
                  from_bottom j last cont)
              else
                from_bottom j last cont))
    end in

  let rec from_top i last cont =
    let ls = left_son i and rs = right_son i in
    if ls > last then
      cont ()
    else if rs > last then
      Pause(fun () ->
        if gc.array.(ls) <= gc.array.(i) then
          cont ()
        else begin
          exchange gc i ls;
          from_top ls last cont
        end)
    else
      Pause(fun () ->
        if gc.array.(ls) <= gc.array.(i) then
          Pause(fun () ->
            if gc.array.(rs) <= gc.array.(i) then
              cont ()
            else
              Pause(fun () ->
                if gc.array.(rs) <= gc.array.(ls) then begin
                  exchange gc i ls; from_top ls last cont
                end else begin
                  exchange gc i rs; from_top rs last cont
                end))
        else
          Pause(fun () ->
            if gc.array.(rs) <= gc.array.(ls) then begin
              exchange gc i ls; from_top ls last cont
            end else begin
              exchange gc i rs; from_top rs last cont
            end)) in

  let rec loop1 last cont =
    if last >= vect_length gc.array
    then cont()
    else from_bottom last last (fun () -> loop1 (last+1) cont) in

  let rec loop2 last cont =
    if last < 0 then cont() else begin
      exchange gc 0 (last+1);
      from_top 0 last (fun () -> loop2 (last-1) cont)
    end in

  loop1 1 (fun () -> loop2 (vect_length gc.array - 2) (fun () -> Finished))
;;

(* Merge sort *)

let merge_sort gc =
  let rec merge i l1 l2 cont =
    match (l1, l2) with
    | ([], []) ->
        cont()
    | ([], v2::r2) ->
        assign gc i v2; merge (i+1) l1 r2 cont
    | (v1::r1, []) ->
        assign gc i v1; merge (i+1) r1 l2 cont
    | (v1::r1, v2::r2) ->
        Pause(fun () ->
          if v1 < v2
          then begin assign gc i v1; merge (i+1) r1 l2 cont end
          else begin assign gc i v2; merge (i+1) l1 r2 cont end) in
  let rec msort start len cont =
    if len < 2 then cont() else begin
      let m = len / 2 in
      msort start m (fun () ->
        msort (start+m) (len-m) (fun () ->
          merge start
                (list_of_vect (sub_vect gc.array start m))
                (list_of_vect (sub_vect gc.array (start+m) (len-m)))
                cont))
    end in
  msort 0 (vect_length gc.array) (fun () -> Finished)
;;

(* Main program *)

let animate() =
  open_graph "";
  moveto 0 0; draw_string "Press a key to start...";
  let seed = ref 0 in
  while not (key_pressed()) do incr seed done;
  skip_key ();
  random__init !seed;
  clear_graph();
  let prompt = "0: fastest ... 9: slowest, press 'q' to quit" in
  moveto 0 0; draw_string prompt;
  let (_, h) = text_size prompt in
  let sx = size_x() / 2 and sy = (size_y() - h) / 3 in
  display [| "Bubble", bubble_sort, 0, h, sx, sy;
             "Insertion", insertion_sort, 0, h+sy, sx, sy;
             "Selection", selection_sort, 0, h+2*sy, sx, sy;
             "Quicksort", quick_sort, sx, h, sx, sy;
             "Heapsort", heap_sort, sx, h+sy, sx, sy;
             "Mergesort", merge_sort, sx, h+2*sy, sx, sy |]
          100 1000;
  close_graph()
;;

if sys__interactive then () else begin animate(); exit 0 end;;
