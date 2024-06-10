(* Define the treeNode datatype *)
datatype treeNode = 
    null |
    node of int * treeNode * treeNode;

(* Function to return the node value of bst *)
fun entry(null) = raise Fail "Tree is empty"
  | entry(node (v, _, _)) = v;

(* Function to return the left subtree of bst *)
fun left(null) = null
  | left(node (_, l, _)) = l;

(* Function to return the right subtree of bst *)
fun right(null) = null
  | right(node (_, _, r)) = r;

(* Function to create a new tree *)
fun makeBst(elt, l, r) = node (elt, l, r);

(* Function to print each node in bst in preorder *)
fun preorder(null) = ()
  | preorder(node (v, l, r)) = (
      print (Int.toString v ^ " ");
      preorder(l);
      preorder(r)
    );

(* Function to insert a value into the BST *)
fun insert(v, null) = node (v, null, null)
  | insert(v, node (value, l, r)) =
    if v < value then node (value, insert(v, l), r)
    else if v > value then node (value, l, insert(v, r))
    else node (value, l, r); (* duplicate, return the same node *)

(* Sample usage *)
val tree = makeBst(5, null, null);
val tree = insert(3, tree);
val tree = insert(4, tree);
val tree = insert(7, tree);
val _ = preorder(tree);
