(* Function to generate a list of consecutive integers from start to stop *)
fun genList(start, stop) =
    if start > stop then []
    else start :: genList(start + 1, stop);

(* Function to check if any two adjacent values in numList sum to num *)
fun pairSum([], _) = false
  | pairSum([_], _) = false
  | pairSum(x::y::rest, num) = (x + y = num) orelse pairSum(y::rest, num);

(* Test cases for genList *)
val _ = print ("genList(1, 5) ---> " ^ String.concatWith ", " (List.map Int.toString (genList(1, 5))) ^ "\n");

(* Test cases for pairSum *)
val _ = print ("pairSum([1, 2, 3], 3) ---> " ^ Bool.toString (pairSum([1, 2, 3], 3)) ^ "\n");
val _ = print ("pairSum(genList(1, 100), 1000) ---> " ^ Bool.toString (pairSum(genList(1, 100), 1000)) ^ "\n");

datatype lazyListEntry =
  entry of int * (unit -> lazyListEntry) |
  null;

(* Function to generate a lazy list of integers from start to stop *)
fun intSeq(start:int, stop:int) : lazyListEntry =
  if start > stop then
     null
  else
     entry(start, fn() => intSeq(start+1, stop));

(* Function to check if any two adjacent values in a lazy list sum to num *)
fun pairSumLazy(null, _) = false
  | pairSumLazy(entry(x, xf), num) =
    case xf() of
        null => false
      | entry(y, yf) => (x + y = num) orelse pairSumLazy(entry(y, yf), num);

(* Helper function to print lazy list for testing *)
fun printLazyList(null) = ()
  | printLazyList(entry(x, xf)) =
    (print (Int.toString x ^ " ");
    printLazyList (xf()));

(* Test case for intSeq and pairSumLazy *)
val testLazyList = intSeq(1, 5);
val _ = (print ("intSeq(1, 5) ---> "); printLazyList testLazyList; print "\n");

val _ = print ("pairSumLazy(intSeq(1, 5), 3) ---> " ^ Bool.toString (pairSumLazy(intSeq(1, 5), 3)) ^ "\n");
val _ = print ("pairSumLazy(intSeq(1, 100), 1000) ---> " ^ Bool.toString (pairSumLazy(intSeq(1, 100), 1000)) ^ "\n");
