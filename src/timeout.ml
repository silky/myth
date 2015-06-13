open Unix

type t = { base : float; timeout : float }
exception Timeout_exception

let unlimited : t = { base = 0.0; timeout = -1.0 }
let create (period:float) : t = { base = gettimeofday (); timeout = period }
let check_timeout (tmo:t) =
  if tmo.timeout > 0.0 && gettimeofday () -. tmo.base > tmo.timeout then
    raise Timeout_exception
