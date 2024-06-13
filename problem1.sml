
datatype treeNode = 
    null |
    node of int * treeNode * treeNode;


fun entry(null) = raise Fail "Tree is empty"
  | entry(node (v, _, _)) = v;


fun left(null) = null
  | left(node (_, l, _)) = l;


fun right(null) = null
  | right(node (_, _, r)) = r;


fun makeBst(elt, l, r) = node (elt, l, r);


fun preorder(null) = ()
  | preorder(node (v, l, r)) = (
      print (Int.toString v ^ " ");
      preorder(l);
      preorder(r)
    );


fun insert(v, null) = node (v, null, null)
  | insert(v, node (value, l, r)) =
    if v < value then node (value, insert(v, l), r)
    else if v > value then node (value, l, insert(v, r))
    else node (value, l, r); 


val tree = makeBst(5, null, null);
val tree = insert(3, tree);
val tree = insert(4, tree);
val tree = insert(7, tree);
val _ = preorder(tree);
