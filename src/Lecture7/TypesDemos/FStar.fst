module Fib

val fibonacci : int -> Tot (x: int{x > 0})
let rec fibonacci n =
  if n <= 1 then 1 else fibonacci (n - 1) + fibonacci (n - 2)

val fibonacci2 : (x: int{x >= 0}) -> Tot (y: int{y > 0 /\ y >= x})
let rec fibonacci2 n =
  if (n = 0) || (n = 1) 
  then 1 
  else fibonacci2 (n - 1) + fibonacci2 (n - 2)

