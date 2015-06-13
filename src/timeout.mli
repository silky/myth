type t
exception Timeout_exception

val unlimited : t
val create : float -> t
val check_timeout : t -> unit
