#use "nat.decls"
#use "compare.decls"

let nat_max : nat -> nat -> nat |>
{
  0 => ( 0 => 0
       | 1 => 1
       | 2 => 2 )
| 1 => ( 0 => 1
       | 1 => 1
       | 2 => 2 )
| 2 => ( 0 => 2
       | 1 => 2
       | 2 => 2 )
} = ?
