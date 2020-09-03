(* question 4 *)
(* ListAddx: int list * int -> int list *)
(* PrefixSum: int list -> int list *)
(* 要求：WPrefixSum(n) = O(n2)。(n为输入int list的长度) *)
fun ListAddx ([ ], x) = [ ]
    | ListAddx (y :: L, x) = (x + y) :: ListAddx (L, x)
fun PrefixSum [ ] = [ ]
    | PrefixSum (x :: L) = x :: ListAddx (PrefixSum L, x)
    

(* PrefixSumHelp: int list * int -> int list *)
(* fastPrefixSum: int list -> int list *)
(* 要求：WfastPrefixSum(n) =O(n).  *)
fun PrefixSumHelp ([ ], x) = [ ]
    | PrefixSumHelp (y :: L, x) = (x + y) :: PrefixSumHelp (L, x + y)

fun fastPrefixSum L = PrefixSumHelp (L, 0)

(* test_question 4 *)
val result1 = PrefixSum [1,2,3]      (* [1,3,6] *)
val result2 = PrefixSum [6,3,1]      (* [6,9,10] *)

val result3 = fastPrefixSum [1,2,3]  (* [1,3,6] *)
val result4 = fastPrefixSum [6,3,1]  (* [6,9,10] *)


(* question 5 *)
(* defination of tree*)
datatype tree = Empty | Node of tree * int * tree
(* treecompare : tree * tree -> order *)
(* when given two trees, returns a value of type order, 
based on which tree has a larger value at the root node *)
fun treecompare (Empty, Empty) = EQUAL
    | treecompare (Empty, _ ) = LESS
    | treecompare ( _ , Empty) = GREATER
    | treecompare (Node( _ , i , _ ), Node( _ , j , _ )) = Int.compare(i, j)

(* SwapDown : tree -> tree *)
(* REQUIRES the subtrees of t are both minheaps *)
(* ENSURES swapDown(t) = if t is Empty or all of t’s immediate children are empty then
* just return t, otherwise returns a minheap which contains exactly the elements in t. *) 
fun cmproot (x, Empty) = (false, x ,Empty)
    | cmproot(x, Node (l, y, r)) = 
        case Int.compare(x, y) of
            GREATER => (true, y, Node (l, x, r))
            | _ => (false, x, Node (l, y, r))

fun SwapDown Empty = Empty
    | SwapDown (Node (Empty, x, Empty)) = Node (Empty, x, Empty)
    | SwapDown (Node (l, x, r)) = 
        let
          val (cmpleft, root1, left) = cmproot (x, l)
          val (cmpright, root2, right) = cmproot (x, r)
        in
          if cmpleft then SwapDown (Node (SwapDown left, root1, r)) else 
            if cmpright then SwapDown (Node (l, root2, SwapDown right)) else Node (l, x, r)
        end

(* heapify : tree -> tree *)
(* given an arbitrary tree t, evaluates to a minheap with exactly the elements of t. *)
fun heapify Empty = Empty
    | heapify (Node (l, x, r)) = SwapDown (Node (heapify l, x, heapify r))


(* test_question 5 *)
val tree1 = Empty
val tree2 = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))
val tree3 = Node(Node(Empty, 7, Node(Empty, 4, Empty)), 6, Node(Empty, 5, Empty))

val result5 = treecompare(tree1, Empty)                   (* EQUAL *)
val result6 = treecompare(tree2, Empty)                   (* GREATER *)
val result7 = treecompare(tree2, Node(Empty, 2, Empty))   (* EQUAL *)

val result8 = SwapDown tree1 (* Empty *)  
val result9 = SwapDown tree2 (* Node(Node(Empty,2,Empty),1,Node(Empty,3,Empty)) *)

val result10 = heapify tree1 (* Empty *)
val result11 = heapify tree2 (* Node(Node(Empty,2,Empty),1,Node(Empty,3,Empty)) *)
val result12 = heapify tree3 (* Node(Node(Empty,6,Node(Empty,7,Empty)),5,Node(Empty,6,Empty)) *)
