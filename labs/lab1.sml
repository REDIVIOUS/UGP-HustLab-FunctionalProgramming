(* exercise 1.2 *)
(* mult : int list -> int *)
(* REQUIRES: true *)
(* ENSURES: mult(L) evaluates to the product of the integers in L. *)
fun mult ([ ] : int list): int = 1
    | mult (x ::L) = x * (mult L)

(* exercise 1.3 *)
(* mult : int list list -> int *)
(* REQUIRES: true *)
(* ENSURES: mult(R) evaluates to the product of all the integers in the lists of R. *)
fun Mult ([ ] : int list list): int = 1
    | Mult (r :: R) = mult(r) * Mult(R)

(* exercise 1.4 *)
(* mult’ : int list * int -> int *)
(* REQUIRES: true *)
(* ENSURES: mult’(L, a) … (* FILL IN *) *)
(* 函数功能：一个元组前面是一个整型表，后面是一个整型a，结果是将前面表中所有元素和a相乘的乘积（尾递归）*)
fun mult' ([ ] : int list, a : int): int = a
	  | mult' (x :: L, a) = mult' (L, x * a)
(* 计算a与列表R中所有整数的乘积 *)
(* Mult’ : int list list * int -> int *)
fun Mult' ([ ] : int list list, a : int): int = a 	
    | Mult' (r::R, a) = Mult' (R, mult'(r, 1) * a)

(* exercise 1.5 *)
(* double : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: double n evaluates to 2 * n.*)
fun double (0 : int) : int = 0
    | double n = 2 + double (n - 1)
(* 计算n的平方 *)
(* square int -> int *)
fun square (0 : int): int =0
    | square n = double(n) + square(n - 1) - 1

(* exercise 1.6 *)
(* divisibleByThree : int -> bool *)
(* REQUIRES: true *)
(* ENSURES: divisibleByThree n evaluates to true if n is a multiple of 3 and to false otherwise *)
fun divisibleByThree (0 : int) : bool = true
    | divisibleByThree (1 : int) : bool = false 
    | divisibleByThree (2 : int) : bool = false
    | divisibleByThree n = if n > 0 then divisibleByThree (n - 3) else divisibleByThree (n + 3)

(* exercise 1.7 *)
(* oddP : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: evenP n evaluates to true iff n is odd. *)
fun oddP (0 : int) : bool = false
  	    | oddP 1 = true
  	    | oddP n = oddP (n - 2)







