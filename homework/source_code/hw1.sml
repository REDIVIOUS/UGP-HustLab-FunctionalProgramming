(* question 4-1 *)
(* zip: string list * int list -> (string * int) list *)
(* 分别提取string list和int list中的i号元素，组成二元组，如果长度不同按照长度小的算 *)
fun zip ([ ] : string list, L2 : int list) : (string * int) list = [ ]
    | zip (L1, [ ]) = [ ]
    | zip (x :: L1, y :: L2) = (x, y) :: zip (L1, L2)

(* question 4-2 *)
(* unzip: (string * int) list -> string list * int list *)
(* 执行zip的反向操作，将二元组list中的元素还原为string list和int list *)
fun unzip ([ ] : (string * int) list) : string list * int list = ([ ], [ ])
    | unzip ((x , y) :: L) = let val (X, Y) = unzip L in (x :: X, y :: Y) end

(* test *)
val result1 = zip(["book","pen","pencil","computer"],[15,3,1,4000])
(* [("book",15),("pen",3),("pencil",1),("computer",4000)]: (string * int) list *)
val result2 = zip(["apple","banana","strawberry","watermelon"],[4,3,5])
(* [("apple",4),("banana",3),("strawberry",5)]: (string * int) list *)

val result3 = unzip result1
(* (["book","pen","pencil","computer"],[15,3,1,4000]): string list * int list *)
val result4 = unzip result2
(* (["apple","banana","strawberry"],[4,3,5]): string list * int list *)