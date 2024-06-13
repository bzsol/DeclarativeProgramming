
fun genList(start, stop) =
    if start > stop then []
    else start :: genList(start + 1, stop);


fun pairSum([], _) = false
  | pairSum([_], _) = false
  | pairSum(x::y::rest, num) = (x + y = num) orelse pairSum(y::rest, num);


val _ = print ("genList(1, 5) ---> " ^ String.concatWith ", " (List.map Int.toString (genList(1, 5))) ^ "\n");


val _ = print ("pairSum([1, 2, 3], 3) ---> " ^ Bool.toString (pairSum([1, 2, 3], 3)) ^ "\n");
val _ = print ("pairSum(genList(1, 100), 1000) ---> " ^ Bool.toString (pairSum(genList(1, 100), 1000)) ^ "\n");

datatype lazyListEntry =
  entry of int * (unit -> lazyListEntry) |
  null;


fun intSeq(start:int, stop:int) : lazyListEntry =
  if start > stop then
     null
  else
     entry(start, fn() => intSeq(start+1, stop));


fun pairSumLazy(null, _) = false
  | pairSumLazy(entry(x, xf), num) =
    case xf() of
        null => false
      | entry(y, yf) => (x + y = num) orelse pairSumLazy(entry(y, yf), num);


fun printLazyList(null) = ()
  | printLazyList(entry(x, xf)) =
    (print (Int.toString x ^ " ");
    printLazyList (xf()));


val testLazyList = intSeq(1, 5);
val _ = (print ("intSeq(1, 5) ---> "); printLazyList testLazyList; print "\n");

val _ = print ("pairSumLazy(intSeq(1, 5), 3) ---> " ^ Bool.toString (pairSumLazy(intSeq(1, 5), 3)) ^ "\n");
val _ = print ("pairSumLazy(intSeq(1, 100), 1000) ---> " ^ Bool.toString (pairSumLazy(intSeq(1, 100), 1000)) ^ "\n");
