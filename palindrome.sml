
fun listReverse l =
  let
    fun revHelper([], acc) = acc
      | revHelper(x::xs, acc) = revHelper(xs, x::acc)
  in
    revHelper(l, [])
  end;


fun palindrome w =
  let
    fun explode s = List.tabulate (String.size s, fn i => String.sub (s, i))
    val charList = explode w
    val reversedList = listReverse charList
  in
    charList = reversedList
  end;


val _ = print ("listReverse [1, 2, 3, 4] ---> " ^ String.concatWith ", " (List.map Int.toString (listReverse [1, 2, 3, 4])) ^ "\n");
val _ = print ("listReverse [\"a\", \"b\", \"c\", \"d\"] ---> " ^ String.concatWith ", " (listReverse ["a", "b", "c", "d"]) ^ "\n");


val _ = print ("palindrome \"malayalam\" ---> " ^ Bool.toString (palindrome "malayalam") ^ "\n");
val _ = print ("palindrome \"myxomatosis\" ---> " ^ Bool.toString (palindrome "myxomatosis") ^ "\n");
