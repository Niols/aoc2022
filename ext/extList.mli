include module type of List

val union_sorted : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Returns the union of two sorted lists seen as sets. [union_sorted cmp l1 l2]
   is equivalent to [sort_uniq cmp (l1 @ l2)] except faster. *)

val inter_sorted : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Returns the intersection of two sorted lists seen as sets. [inter_sorted cmp
    l1 l2] is equivalent to [filter (fun x1 -> exists (fun x2 -> cmp x1 x2 = 0)
    l2) l1] except much faster. *)

val diff_sorted : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Returns the difference of two sorted lists seen as sets. [diff_sorted cmp l1
   l2] is equivalent to [filter (fun x1 -> not (exists (fun x2 -> cmp x1 x2 = 0)
   l2)) l1] except much faster. *)

val symdiff_sorted : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Returns the symmetric difference of two sorted lists seen as sets.
   [symdiff_sorted cmp l1 l2] is equivalent to [union_sorted (diff_sorted l1 l2)
   (diff_sorted l2 l1)] except faster. *)

val hdn : int -> 'a list -> 'a list
(** [hdn n l] returns the [n] first elements of [l] in a list. If [n] is bigger
   than the size of [l], returns [l]. *)

val bd : 'a list -> 'a list

val ft : 'a list -> 'a

val init_until : (int -> 'a option) -> 'a list
(** [init_until f] initialises a list by calling [f] until it returns [None]. *)

val sub : 'a list -> int -> int -> 'a list
(** [sub l pos len] is a list of length [len], containing the sublist of [l]
    that starts at position [pos] and has length [len].

    @raise Invalid_argument if [pos] and [len] do not designate a valid sublist
      of [l]. *)
