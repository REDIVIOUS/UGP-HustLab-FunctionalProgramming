(* exercise 3.1 *)
(* thenAddOne: ((int->int) * int)->int *)
(* 将一个整数通过函数变换(如翻倍、平方、阶乘后)后加一 *)
fun thenAddOne (f: int->int, v) = f (v) + 1

(* exercise 3.2 *)
(* mapList: ((‘a -> ‘b) * ‘a list) -> ‘b list *)
(* 实现整数集的数学变换（翻倍、平方、阶乘）*)
fun mapList (f, [ ]) = [ ]
    | mapList (f, x :: L) = f (x) :: mapList (f, L)

(* exercise 3.3 *)
(* mapList': (‘a -> ‘b) -> (‘a list -> ‘b list) *)
(* 实现整数集的数学变换（翻倍、平方、阶乘）*)
fun mapList' f = map f;

(* exercise 3.4 *)
(* findOdd: int list -> int option *)
(* 如果x为L中第一个奇数返回 SOME x，否则返回 NONE *)
fun findOdd [ ] = NONE
    | findOdd (x :: L) = if (x mod 2 <> 0) then SOME x else findOdd L

(* exercise 3.5 *)
(* subsetSumOption: int list * int -> int list option *)
(* 如果L中存在子集L’使得其中所有元素之和为s，则结果为SOME L‘，否则为NONE *)
fun subsetSumOption (L, 0) = SOME [ ]
    | subsetSumOption ([ ], _) = NONE 
    | subsetSumOption (x :: L, s) = 
        case subsetSumOption (L, s - x) of
            NONE => subsetSumOption (L, s)
            | SOME A => SOME (x :: A)

(* exercise 3.6 *)
(* exists: (‘a -> bool) -> ‘a list -> bool *)
(* 如果L中存在x作为函数p的参数使得函数p结果为真，就返回true，否则返回false *)
fun exists f [ ] = false
    | exists f (x :: L) = f (x) orelse exists f L

(* forall: (‘a -> bool) -> ‘a list -> bool *)
(* 如果L任意的x作为函数p的参数都能使得函数p结果为真，就返回true，否则返回false *)
fun forall f [ ] = true
    | forall f (x :: L) = f (x) andalso forall f L

(* exercise 3.7 *)
(* treeFilter: (‘a -> bool) -> ‘a tree -> ‘a option tree *)
(* 将树树中满足条件p的节点封装成option类型保留，否则替换成NONE*)
datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
fun treeFilter f Empty = Empty
    | treeFilter f (Node (t1, x, t2)) = 
        if f x then Node (treeFilter f t1, SOME x, treeFilter f t2)
               else Node (treeFilter f t1, NONE, treeFilter f t2)

(* 翻倍、平方、阶乘测试函数 *)
fun double x = 2 * x
fun square x = x * x
fun factorial 0 = 1
    | factorial x = x * factorial (x - 1)

(* 测试奇偶性函数 *)
fun ifeven x = x mod 2 = 0
fun ifodd x = x mod 2 <> 0

(* 测试样例 *)
val result1 = thenAddOne (double, 3) (* 7 *)
val result2 = thenAddOne (double, 0) (* 1 *)
val result3 = thenAddOne (square, 5) (* 26 *)
val result4 = thenAddOne (square, 0) (* 1 *)
val result5 = thenAddOne (factorial, 5) (* 121 *)
val result6 = thenAddOne (factorial, 0) (* 2 *)

val result7 = mapList (double, [0]) (* [0] *)
val result8 = mapList (double, [1,2,3]) (* [2,4,6] *)
val result9 = mapList (square, [0]) (* [0] *)
val result10 = mapList (square, [1,2,3]) (* [1,4,9] *)
val result11 = mapList (factorial, [0]) (* [1] *)
val result12 = mapList (factorial, [1,2,3]) (* [1,2,6] *)

val result13 = mapList' double [0] (* [0] *)
val result14 = mapList' double [1,2,3] (* [2,4,6] *)
val result15 = mapList' square [0] (* [0] *)
val result16 = mapList' square [1,2,3] (* [1,4,9] *)
val result17 = mapList' factorial [0] (* [1] *)
val result18 = mapList' factorial [1,2,3] (* [1,2,6] *)

val result19 = findOdd [2,3,4,5] (* SOME 3 *)
val result20 = findOdd [2,4,6,8] (* NONE *)

val result21 = subsetSumOption ([1,2,3],4) (* SOME [1,3] *)
val result22 = subsetSumOption ([1,2,3],10) (* NONE *)

val result23 = exists ifeven [1,3] (* false *)
val result24 = exists ifeven [1,2,3,4] (* true *)
val result25 = exists ifodd [2,4] (* false *)
val result26 = exists ifodd [1,2,3,4] (* true *)

val result27 = forall ifeven [1,2,3,4] (* false *)
val result28 = forall ifeven [2,4] (* true *)
val result29 = forall ifodd [1,2,3,4] (* false *)
val result30 = forall ifodd [1,3] (* true *)

val result31 = treeFilter ifeven (Node(Node(Empty,1,Empty),2,Node(Empty,3,Empty)))
(* (Node(Empty,NONE,Empty),SOME 2,Node (Empty,NONE,Empty)) *)
val result32 = treeFilter ifodd (Node(Node(Empty,1,Empty),2,Node(Empty,3,Empty)))
(* Node(Node(Empty,SOME 1,Empty),NONE,Node (Empty,SOME 3,Empty)) *)
(* 由于sml的显示原因，在控制台中显示SOME 1 和 SOME 3 都是SOME # *)




