open Core

type 'a t
val empty : 'a t
val of_list : 'a list -> 'a t
val concat : 'a t -> 'a t -> 'a t
val map : ('a t) -> f:('a -> 'b) -> 'b t
val iter : ('a t) -> f:('a -> unit) -> unit
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val dedup : 'a t -> compare:('a -> 'a -> int) -> 'a t
val cons : 'a -> 'a t -> 'a t
val to_list : 'a t -> 'a list
val is_empty : 'a t -> bool
val cartesian_product : ('a t) list -> ('a list) t
val length : 'a t -> int
