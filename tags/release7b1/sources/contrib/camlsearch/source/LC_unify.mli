type 'a option =
  None
| Some of 'a;;
type Head_type == int * myTypes__skel_typ;; (* record the length of premisses lists *)
type Flat_type = Fl of Head_type * Flat_type list;;
value unify_Left_Commutative : Flat_type -> Flat_type -> (bool * (int *int) list);;
value flatten : myTypes__skel_typ -> Flat_type;;



