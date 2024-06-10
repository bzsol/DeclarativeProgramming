(* Helper function to check if an element is in a list *)
fun member(_, []) = false
  | member(x, y::ys) = (x = y) orelse member(x, ys);

(* Helper function to check if all elements of one list are in another *)
fun subset([], _) = true
  | subset(x::xs, ys) = member(x, ys) andalso subset(xs, ys);

(* Function to test if two sets are equal *)
fun setEquals(s1, s2) = subset(s1, s2) andalso subset(s2, s1);

(* Function to perform union of two sets *)
fun union([], s2) = s2
  | union(x::xs, s2) = if member(x, s2) then union(xs, s2) else x :: union(xs, s2);

(* Function to perform intersection of two sets *)
fun intersect([], _) = []
  | intersect(x::xs, s2) = if member(x, s2) then x :: intersect(xs, s2) else intersect(xs, s2);

(* Test cases *)
val testSetEquals1 = setEquals([1, 2, 3], [3, 2, 1]); (* Expected: true *)
val testSetEquals2 = setEquals([1, 2], [3, 2, 1]); (* Expected: false *)
val testSetEquals3 = setEquals(["clyde","blinky","sue"], ["sue","clyde","blinky"]); (* Expected: true *)

val testUnion1 = union([1, 2, 3], [3, 2, 1]); (* Expected: [1, 2, 3] *)
val testUnion2 = union([1, 2, 3], [3, 4, 5]); (* Expected: [1, 2, 3, 4, 5] *)
val testUnion3 = union(["a", "b", "c"], ["d", "e", "f"]); (* Expected: ["a", "b", "c", "d", "e", "f"] *)

val testIntersect1 = intersect([1, 2, 3], [3, 2, 1]); (* Expected: [1, 2, 3] *)
val testIntersect2 = intersect([1, 2, 3], [4, 5, 6]); (* Expected: [] *)
val testIntersect3 = intersect([1, 2, 3], [2, 3, 4, 5, 6]); (* Expected: [2, 3] *)

(* Output the results for verification *)
val _ = print ("setEquals([1, 2, 3], [3, 2, 1]) ---> " ^ Bool.toString(testSetEquals1) ^ "\n");
val _ = print ("setEquals([1, 2], [3, 2, 1]) ---> " ^ Bool.toString(testSetEquals2) ^ "\n");
val _ = print ("setEquals([\"clyde\",\"blinky\",\"sue\"], [\"sue\",\"clyde\",\"blinky\"]) ---> " ^ Bool.toString(testSetEquals3) ^ "\n");

val _ = print ("union([1, 2, 3], [3, 2, 1]) ---> [" ^ String.concatWith ", " (List.map Int.toString testUnion1) ^ "]\n");
val _ = print ("union([1, 2, 3], [3, 4, 5]) ---> [" ^ String.concatWith ", " (List.map Int.toString testUnion2) ^ "]\n");
val _ = print ("union([\"a\", \"b\", \"c\"], [\"d\", \"e\", \"f\"]) ---> [" ^ String.concatWith ", " testUnion3 ^ "]\n");

val _ = print ("intersect([1, 2, 3], [3, 2, 1]) ---> [" ^ String.concatWith ", " (List.map Int.toString testIntersect1) ^ "]\n");
val _ = print ("intersect([1, 2, 3], [4, 5, 6]) ---> [" ^ String.concatWith ", " (List.map Int.toString testIntersect2) ^ "]\n");
val _ = print ("intersect([1, 2, 3], [2, 3, 4, 5, 6]) ---> [" ^ String.concatWith ", " (List.map Int.toString testIntersect3) ^ "]\n");
