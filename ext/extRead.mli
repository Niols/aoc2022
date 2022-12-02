(** {1 Helpers to Read Simple Space-based Files} *)

(** {2 Casts} *)

type 'a cast

(** {3 Simple} *)

val int : int cast
val bit : bool cast
val float : float cast
val char : char cast
val string : string cast

val no_space_string : string cast
(** Cast that checks that its input does not contain a space and returns it
   right away. In [seq], [list] and [array] below, it is no different to
   [string]. In the [tupleng] functions below, it is only different to [string]
   in last position. *)

val list : 'a cast -> 'a list cast
val array : 'a cast -> 'a array cast

val non_empty_list : 'a cast -> 'a list cast
val non_empty_array : 'a cast -> 'a array cast

(** {3 Tuples}

   For each tuple size n (up to 5), we provide a function tuplen and a function
   tupleng, the former taking one cast and the latter taking n casts ("g" stands
   for "generic"). *)

val tuple2g : 'a cast -> 'b cast -> ('a * 'b) cast
(** [tuple2g c1 c2] is a cast that cuts its input at the first space character
   and applies [c1] on the first part and [c2] on the second part, returning the
   2-tuple (pair) of the results. *)

val tuple2 : 'a cast -> ('a * 'a) cast
(** [tuple2 c s = tuple2g c c s]. *)

val pairg : 'a cast -> 'b cast -> ('a * 'b) cast
(** Alias for [tuple2g]. *)

val pair : 'a cast -> ('a * 'a) cast
(** Alias for [tuple2]. *)

val tuple3g : 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast
(** [tuple3g c1 c2 c3] is a cast that cuts its input at the two first space
   characters and applies [c1] on the first part, [c2] on the second part and
   [c3] on the third part, returning the 3-tuple of the results. *)

val tuple3 : 'a cast -> ('a * 'a * 'a) cast
(** [tuple3 c s = tuple3g c c c s]. *)

val tripleg : 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast
(** Alias for [tuple3g]. *)

val triple : 'a cast -> ('a * 'a * 'a) cast
(** Alias for [tuple3]. *)

val tuple4g : 'a cast -> 'b cast -> 'c cast -> 'd cast -> ('a * 'b * 'c * 'd) cast
(** [tuple4g c1 c2 c3 c4] is a cast that cuts its input at the three first space
   characters and applies [c1] on the first part, [c2] on the second part, [c3]
   on the third part and [c4] on the fourth part, returning the 4-tuple of the
   results. *)

val tuple4 : 'a cast -> ('a * 'a * 'a * 'a) cast
(** [tuple4 c s = tuple4g c c c c s]. *)

val tuple5g : 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast
  -> ('a * 'b * 'c * 'd * 'e) cast
(** [tuple5g c1 c2 c3 c4 c5] is a cast that cuts its input at the first space
   character and applies [c1] on the first part, [c2] on the second part, [c3]
   on the third part, [c4] on the fourth part and [c5] on the fifth part,
   returning the 5-tuple of the results. *)

val tuple5 : 'a cast -> ('a * 'a * 'a * 'a * 'a) cast
(** [tuple5 c s = tuple5g c c c c c s]. *)

(** {3 Custom} *)

val cast : (string -> 'a) -> 'a cast
(** Create a cast from a [string -> 'a] function. *)

(** {2 Reader} *)

val of_string : 'a cast -> string -> 'a
(** [string c s] reads [s] and casts it using [c]. *)

val line : 'a cast -> 'a
(** [line c] reads one line from standard input and casts it using [c]. *)

val line_of_chan : in_channel -> 'a cast -> 'a
(** [line_from_chan ichan c] reads one line from [ichan] and casts it using [c]. *)

val lines_until_empty : 'a cast -> 'a list
(** [lines_until_empty c] reads lines from standard input and casts them using
    [c]. It stops at the first empty line it meets. *)

val lines_of_chan_until_empty : in_channel -> 'a cast -> 'a list
(** [lines_of_chan_until_empty ichan c] reads lines from [ichan] and casts them
    using [c]. It stops at the first empty line it meets. *)
