(* Memory management control and statistics. *)

type stat = {
  minor_words : int;
  promoted_words : int;
  major_words : int;
  minor_collections : int;
  major_collections : int;
  heap_size : int;
  heap_chunks : int;
  live_words : int;
  live_blocks : int;
  free_words : int;
  free_blocks : int;
  largest_free : int;
  fragments : int
};;

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : bool
};;

value stat : unit -> stat = 1 "gc_stat";;
value get : unit -> control = 1 "gc_get";;
value set : control -> unit = 1 "gc_set";;
value minor : unit -> unit = 1 "gc_minor";;
value major : unit -> unit = 1 "gc_major";;
value full_major : unit -> unit = 1 "gc_full_major";;
