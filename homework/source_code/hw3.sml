(* question 2 *)
(* toInt: int -> int list -> int. *)
(* 给出基数b以及各次幂的系数list，求整数n *)
fun toInt (b :int) (L :int list) :int = 
        case L of [ ] => 0
             | x :: L => x + b * toInt b L

(* toBase: int -> int -> int list *)
(* 给出基数b和整数n，求各次幂的系数list *)
fun toBase (b :int) (n :int) :int list = 
        case n of 0 => [ ]
                | _ => (n mod b) :: toBase b (n div b)

(* convert: int * int -> int list -> int list *)
(* 给出基数b1和b2，以及基数b1的各次幂系数，求对应数在b2下的各次幂系数 *)
fun convert (b1 :int, b2 :int) (L :int list) :int list = toBase b2 (toInt b1 L)

(* test *)
val result1 = toInt 3              (* fn : int list -> int *)
val result2 = result1 [1,1]        (* 4 *)
val result3 = toInt 3 [1,1]        (* 4 *)

val result4 = toBase 5             (* fn : int -> int list *)
val result5 = result4 28           (* [3,0,1] *)
val result6 = toBase 5 28          (* [3,0,1] *)

val result7 = convert (10,2)       (* fn : int list -> int list *)
val result8 = result7 [1,1]        (* [1,1,0,1] *)
val result9 = convert (10,2) [1,1] (* [1,1,0,1] *)










