(* Hash tables and hash functions *)

(* Hash tables are hashed association tables, with in-place modification. *)

type ('a, 'b) t;;
        (* The type of hash tables from type ['a] to type ['b]. *)

value new : int -> ('a,'b) t
        (* [new n] creates a new, empty hash table, with initial size [n].
           The table grows as needed, so [n] is just an initial guess.
           Better results are said to be achieved when [n] is a prime
           number. Raise [Invalid_argument "hashtbl__new"] if [n] is
           less than 1. *)

  and clear : ('a, 'b) t -> unit
        (* Empty a hash table. *)

  and add : ('a, 'b) t -> 'a -> 'b -> unit
        (* [add tbl x y] adds a binding of [x] to [y] in table [tbl].
           Previous bindings for [x] are not removed, but simply
           hidden. That is, after performing [remove tbl x], the previous
           binding for [x], if any, is restored.
           (This is the semantics of association lists.) *)

  and find : ('a, 'b) t -> 'a -> 'b
        (* [find tbl x] returns the current binding of [x] in [tbl],
           or raises [Not_found] if no such binding exists. *)

  and find_all : ('a, 'b) t -> 'a -> 'b list
        (* [find_all tbl x] returns the list of all data associated with [x]
           in [tbl]. The current binding is returned first, then the previous
           bindings, in reverse order of introduction in the table. *)

  and remove : ('a, 'b) t -> 'a -> unit
        (* [remove tbl x] removes the current binding of [x] in [tbl],
           restoring the previous binding if it exists.
           It does nothing if [x] is not bound in [tbl]. *)

  and do_table : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
        (* [do_table f tbl] applies [f] to all bindings in table [tbl],
	   discarding all the results.
           [f] receives the key as first argument, and the associated value
           as second argument.
           Each binding is presented exactly once to [f].
           The order in which the bindings are passed to
           [f] is unpredictable, except that successive bindings for the same
           key are presented in reverse chronological order 
           (most recent first). *)

  and do_table_rev : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
        (* Same as [do_table], except that successive bindings for the same
           key are presented in chronological order (oldest first). *)
;;

(*** The polymorphic hash primitive *)

value hash : 'a -> int
        (* [hash x] associates a positive integer to any value of
           any type. It is guaranteed that
                if [x = y], then [hash x = hash y]. 
           Moreover, [hash] always terminates, even on cyclic
           structures. *)
;;
value hash_param : int -> int -> 'a -> int = 3 "hash_univ_param"
        (* [hash_param n m x] computes a hash value for [x], with the
           same properties as for [hash]. The two extra parameters [n] and
           [m] give more precise control over hashing. Hashing performs a
           depth-first, right-to-left traversal of the structure [x], stopping
           after [n] meaningful nodes were encountered, or [m] nodes,
           meaningful or not, were encountered. Meaningful nodes are: integers;
           floating-point numbers; strings; characters; booleans; and constant
           constructors. Larger values of [m] and [n] means that more
           nodes are taken into account to compute the final hash
           value, and therefore collisions are less likely to happen.
           However, hashing takes longer. The parameters [m] and [n]
           govern the tradeoff between accuracy and speed. *)
;;
