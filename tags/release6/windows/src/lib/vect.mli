(* Operations on vectors *)

value vect_length : 'a vect -> int = 1 "vect_length"
        (* Return the length (number of elements) of the given vector. *)
;;
value vect_item : 'a vect -> int -> 'a
        (* [vect_item v n] returns the element number [n] of vector [v].
           The first element has number 0.
           The last element has number [vect_length v - 1].
           Raise [Invalid_argument "vect_item"]  if [n] is outside the range
           0 -- [(vect_length v - 1)].
           You can also write [v.(n)] instead of [vect_item v n]. *)
  and vect_assign : 'a vect -> int -> 'a -> unit
        (* [vect_assign v n x] modifies vector [v] in place, replacing
           element number [n] with [x].
           Raise [Invalid_argument "vect_assign"] if [n] is outside the range
           0 -- [vect_length v - 1].
           You can also write [v.(n) <- x] instead of [vect_assign v n x]. *)
;;
value make_vect : int -> 'a -> 'a vect
        (* [make_vect n x] returns a fresh vector of length [n], initialized with [x].
	   All the elements of this new vector are initially [eq] to [x]. *)
  and make_matrix : int -> int -> 'a -> 'a vect vect
        (* [make_matrix dimx dimy e] returns a two-dimensional array
           (a vector of vectors) with first dimension [dimx] and
           second dimension [dimy]. All the elements of this new matrix
	   are initially [eq] to [e]. The element ([x,y]) of a matrix [m] is accessed
           with the notation [m.(x).(y)]. *)  
;;
value concat_vect : 'a vect -> 'a vect -> 'a vect
        (* [concat_vect v1 v2] returns a fresh vector containing the
           concatenation of vectors [v1] and [v2]. *)
  and sub_vect : 'a vect -> int -> int -> 'a vect
        (* [sub_vect v start len] returns a fresh vector of length [len],
           containing the elements number [start] to [start + len - 1]
           of vector [v].
           Raise [Invalid_argument "sub_vect"] if [start] and [len] do not
           designate a valid subvector of [v]; that is, if
           [start < 0], or [len < 0], or [start + len > vect_length v]. *)
  and copy_vect : 'a vect -> 'a vect
        (* [copy_vect v] returns a copy of [v], that is, a fresh vector
           containing the same elements as [v]. *)
;;
value fill_vect : 'a vect -> int -> int -> 'a -> unit
        (* [fill_vect v ofs len x] modifies vector [v] in place,
           storing [x] in elements number [ofs] to [ofs + len - 1].
           Raise [Invalid_argument "fill_vect"] if [ofs] and [len] do not
           designate a valid subvector of [v]. *)
  and blit_vect : 'a vect -> int -> 'a vect -> int -> int -> unit
        (* [blit_vect v1 o1 v2 o2 len] copies [len] elements
           from vector [v1], starting at element number [o1], to vector [v2],
           starting at element number [o2]. It works correctly even if
           [v1] and [v2] are the same vector, and the source and
           destination chunks overlap.
           Raise [Invalid_argument "blit_vect"] if [o1] and [len] do not
           designate a valid subvector of [v1], or if [o2] and [len] do not
           designate a valid subvector of [v2]. *)
;;
value list_of_vect : 'a vect -> 'a list
        (* [list_of_vect v] returns the list of all the elements of [v], that is:
           [[v.(0); v.(1); ...; v.(vect_length v - 1)]]. *)
  and vect_of_list : 'a list -> 'a vect
        (* [vect_of_list l] returns a fresh vector containing the elements
           of [l]. *)
;;
value map_vect : ('a -> 'b) -> 'a vect -> 'b vect
        (* [map_vect f v] applies function [f] to all the elements of [v],
           and builds a vector with the results returned by [f]:
           [[| f v.(0); f v.(1); ...; f v.(vect_length v - 1) |]]. *)
  and map_vect_list : ('a -> 'b) -> 'a vect -> 'b list
        (* [map_vect_list f v] applies function [f] to all the elements of [v],
           and builds a list with the results returned by [f]:
           [[ f v.(0); f v.(1); ...; f v.(vect_length v - 1) ]]. *)
  and do_vect : ('a -> 'b) -> 'a vect -> unit
        (* [do_vect f v] applies function [f] in turn to all the elements of [v],
	   discarding all the results:
           [f v.(0); f v.(1); ...; f v.(vect_length v - 1); ()]. *)
;;
