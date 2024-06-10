datatype 'a nestedList = List of 'a nestedList list | Elem of 'a

(* Function to check if two nestedList elements are equal *)
fun nestedListEqual (Elem x, Elem y) = x = y
  | nestedListEqual (List xs, List ys) = setEquals (List xs, List ys)
  | nestedListEqual (_, _) = false

(* Function to check if a nestedList element exists in a list of nestedList elements *)
and elemInNestedList (e, []) = false
  | elemInNestedList (e, x::xs) = nestedListEqual (e, x) orelse elemInNestedList (e, xs)

(* Function to check if two nestedLists are equal *)
and setEquals (List s1, List s2) =
    let
        fun lengthAndElemsMatch ([], []) = true
          | lengthAndElemsMatch (x::xs, ys) = 
              elemInNestedList (x, ys) andalso lengthAndElemsMatch (xs, List.filter (fn y => not (nestedListEqual (x, y))) ys)
          | lengthAndElemsMatch (_, _) = false
    in
        lengthAndElemsMatch (s1, s2)
    end
  | setEquals (_, _) = false

(* Function to perform union of two nestedLists *)
fun union (List s1, List s2) =
    let
        fun addUnique ([], acc) = acc
          | addUnique (x::xs, acc) = 
              if elemInNestedList (x, acc) 
              then addUnique (xs, acc) 
              else addUnique (xs, x::acc)
    in
        List (addUnique (s1 @ s2, []))
    end
  | union (_, _) = raise Fail "Invalid input"

(* Function to perform intersection of two nestedLists *)
fun intersect (List s1, List s2) =
    let
        fun filterCommon ([], acc) = acc
          | filterCommon (x::xs, acc) = 
              if elemInNestedList (x, s2) 
              then filterCommon (xs, x::acc) 
              else filterCommon (xs, acc)
    in
        List (filterCommon (s1, []))
    end
  | intersect (_, _) = raise Fail "Invalid input"

(* Test cases for setEquals *)
val _ = print ("setEquals(List [Elem 1,Elem 2,Elem 3],List [Elem 3,Elem 2,Elem 1]) ---> " ^ Bool.toString (setEquals(List [Elem 1,Elem 2,Elem 3],List [Elem 3,Elem 2,Elem 1])) ^ "\n")
val _ = print ("setEquals(List [Elem 1,List [Elem 2,Elem 3]],List [List [Elem 3,Elem 2],Elem 1]) ---> " ^ Bool.toString (setEquals(List [Elem 1,List [Elem 2,Elem 3]],List [List [Elem 3,Elem 2],Elem 1])) ^ "\n")
val _ = print ("setEquals(List [Elem 1,Elem 2,Elem 3],List [List [Elem 3,Elem 2],Elem 1]) ---> " ^ Bool.toString (setEquals(List [Elem 1,Elem 2,Elem 3],List [List [Elem 3,Elem 2],Elem 1])) ^ "\n")
val _ = print ("setEquals(List [Elem 1,Elem 2,Elem 3],List [List [Elem 1,Elem 2,Elem 3]]) ---> " ^ Bool.toString (setEquals(List [Elem 1,Elem 2,Elem 3],List [List [Elem 1,Elem 2,Elem 3]])) ^ "\n")
val _ = print ("setEquals(List [List [Elem 1,Elem 4], List [Elem 2, Elem 3]], List [List [Elem 3, Elem 2], List [Elem 4, Elem 1]]) ---> " ^ Bool.toString (setEquals(List [List [Elem 1,Elem 4], List [Elem 2, Elem 3]], List [List [Elem 3, Elem 2], List [Elem 4, Elem 1]])) ^ "\n")

(* Test cases for union *)
val _ = print ("union(List [List [Elem 1, Elem 2, Elem 3]], List [List [Elem 3, Elem 4, Elem 5]]) ---> " ^ Bool.toString (setEquals(union(List [List [Elem 1, Elem 2, Elem 3]], List [List [Elem 3, Elem 4, Elem 5]]), List [List [Elem 1,Elem 2,Elem 3],List [Elem 3,Elem 4,Elem 5]])) ^ "\n")
val _ = print ("union(List [List[Elem 1,Elem 2,Elem 3]], List[List[Elem 3,Elem 2,Elem 1]]) ---> " ^ Bool.toString (setEquals(union(List [List[Elem 1,Elem 2,Elem 3]], List[List[Elem 3,Elem 2,Elem 1]]), List [List [Elem 1,Elem 2,Elem 3]])) ^ "\n")

(* Test cases for intersect *)
val _ = print ("intersect(List [List [Elem 1,Elem 2,Elem 3]],List [List [Elem 3,Elem 2,Elem 1]]) ---> " ^ Bool.toString (setEquals(intersect(List [List [Elem 1,Elem 2,Elem 3]],List [List [Elem 3,Elem 2,Elem 1]]), List [List [Elem 1,Elem 2,Elem 3]])) ^ "\n")
val _ = print ("intersect(List [List [Elem 1,Elem 2,Elem 3]],List [List [Elem 4,Elem 5,Elem 6]]) ---> " ^ Bool.toString (setEquals(intersect(List [List [Elem 1,Elem 2,Elem 3]],List [List [Elem 4,Elem 5,Elem 6]]), List [])) ^ "\n")
val _ = print ("intersect(List [List [Elem 1],List [Elem 2],List [Elem 3]],List [List [Elem 2],List [Elem 3],List [Elem 4]]) ---> " ^ Bool.toString (setEquals(intersect(List [List [Elem 1],List [Elem 2],List [Elem 3]],List [List [Elem 2],List [Elem 3],List [Elem 4]]), List [List [Elem 2], List [Elem 3]])) ^ "\n")
