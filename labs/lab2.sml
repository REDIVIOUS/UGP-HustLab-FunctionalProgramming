(* exercise 2.1 *)
(* reverse: int list -> list *)
(* 实现输出表的逆序输出，不能借助帮助函数 *)
fun reverse [ ]: int list = [ ]
    | reverse (x :: L) = reverse (L) @ [x]

(* helpreverse: int list * int list -> int list *)
(* reverse' int list -> int list *)
(* 借助帮助函数实现reverse‘，时间复杂度为O(n) *)
fun helpreverse ([ ], rev): int list = rev
    | helpreverse (x :: L, rev) = helpreverse(L, x :: rev)
fun reverse' L = helpreverse(L, [ ])

(* exercise 2.2 *)
(* interleave int list * int list -> int list *)
(* 两个int list中的元素在结果list中交替出现 *)
fun interleave (L, [ ]): int list = L
    | interleave ([ ], R) = R 
    | interleave (l :: L, r :: R) = l :: r :: interleave(L, R)

(* exercise 2.3 *)
(* listToTree: int list -> tree *)
(* 表转分割乘两段,两端元素个数最多差1 *)
fun split (_, [ ]): int list * int * int list = raise Fail "empty list"
    | split (L1, x :: L2) = if abs(length L1 - length L2 ) <= 1 then (L1, x, L2)
        else split (L1 @ [x], L2)

(* listToTree: int list -> tree *)
(* 表转换成一棵平衡树 *)
datatype  tree = Empty | Node of tree * int * tree
fun listToTree [ ] = Empty
    | listToTree [x] = Node(Empty, x, Empty)
    | listToTree L =
        let val (L, x ,R) = split ([ ], L)
        in Node(listToTree L, x, listToTree R)
        end

(* exercise 2.4 *)
(* revT: tree -> tree *)
(* 对中序遍历的树进行反转 *)
fun revT Empty = Empty
    | revT (Node (t1, x, t2)) = Node(revT t2, x, revT t1)


(* exercise 2.5 *)
(* binarySearch: tree * int -> bool *)
(* 给定一棵树，判断树中是否有参数y *)
fun binarySearch(Empty, y) = false
    | binarySearch(Node (t1, x, t2), y) = 
        case Int.compare(y, x) of
            LESS => binarySearch (t1, y)
            | EQUAL => true
            | GREATER => binarySearch (t2, y)


(* test value *)
val test_list1 : int list = [1,2,3]
val test_list2 : int list = [3,2,1]
val test_list3 : int list = [4,5,6,7,8,9]
val test_tree1 : tree = Node(Node(Empty,7,Empty),8,Node(Empty,9,Empty))
val test_tree2 : tree = Node(Node(Empty,0,Empty),1,Node(Empty,2,Empty))

(* test example *)
val result1 = reverse test_list1
val result2 = reverse test_list2

val result3 = reverse' test_list1
val result4 = reverse' test_list2

val result5 = interleave (test_list1,test_list2)
val result6 = interleave (test_list1,test_list3)

val result7 = listToTree test_list1
val result8 = listToTree test_list2

val result9 = revT test_tree1
val result10 = revT test_tree2
val result11 = revT result7

val result12 = binarySearch (test_tree2, 0)
val result13 = binarySearch (test_tree2, 7)
val result14 = binarySearch (result7, 3)