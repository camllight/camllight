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
        (* Memory allocation and garbage collection statistics,
           as returned by the [stat] function. *)

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : bool
};;
	(* User-settable GC parameters. *)

value stat : unit -> stat = 1 "gc_stat";;
        (* Return the current memory allocation and garbage collection 
           statistics. *)
value get : unit -> control = 1 "gc_get";;
        (* Return the current values of the GC parameters. *)
value set : control -> unit = 1 "gc_set";;
        (* Set the values of the GC parameters. *)
value minor : unit -> unit = 1 "gc_minor";;
        (* Trigger a minor collection. *)
value major : unit -> unit = 1 "gc_major";;
        (* Trigger a major collection cycle. *)
value full_major : unit -> unit = 1 "gc_full_major";;
        (* Trigger a complete major collection. *)
