datatype 'a lazyList = 
    nullList 
  | cons of 'a * (unit -> 'a lazyList);

(* Function to generate a lazy list of integers from first to last *)
fun seq(first, last) =
  if first > last then
    nullList
  else
    cons (first, fn () => seq(first + 1, last));

(* Function to generate an infinite lazy list of integers starting from first *)
fun infSeq(first) = 
  cons (first, fn () => infSeq(first + 1));

(* Function to get the first n elements of a lazy list as a standard list *)
fun firstN(_, 0) = []
  | firstN(nullList, _) = []
  | firstN(cons(x, xf), n) = x :: firstN(xf(), n - 1);

(* Function to get the nth element of a lazy list *)
fun Nth(_, 0) = NONE
  | Nth(nullList, _) = NONE
  | Nth(cons(x, xf), 1) = SOME x
  | Nth(cons(_, xf), n) = Nth(xf(), n - 1);

(* Function to filter out multiples of n from a lazy list *)
fun filterMultiples(nullList, _) = nullList
  | filterMultiples(cons(x, xf), n) =
    if x mod n = 0 then
      filterMultiples(xf(), n)
    else
      cons(x, fn () => filterMultiples(xf(), n));

(* Helper function to print lazy list for testing *)
fun printLazyList(nullList) = ()
  | printLazyList(cons(x, xf)) = 
    (print (Int.toString x ^ " ");
     printLazyList(xf()));

(* Test seq *)
val testSeq = seq(1, 5);
val _ = (print "seq(1, 5) ---> "; printLazyList testSeq; print "\n");

(* Test infSeq (print first 10 elements) *)
val testInfSeq = infSeq(1);
val _ = (print "infSeq(1) first 10 elements ---> ";
         print (String.concatWith ", " (List.map Int.toString (firstN(testInfSeq, 10))));
         print "\n");

(* Test firstN *)
val testFirstN = firstN(testSeq, 3);
val _ = (print "firstN(seq(1, 5), 3) ---> ";
         print (String.concatWith ", " (List.map Int.toString testFirstN));
         print "\n");

(* Test Nth *)
val testNth = Nth(testSeq, 3);
val _ = print ("Nth(seq(1, 5), 3) ---> " ^
               (case testNth of
                  NONE => "NONE"
                | SOME x => Int.toString x) ^ "\n");

(* Test filterMultiples *)
val testFilterMultiples = filterMultiples(seq(1, 10), 2);
val _ = (print "filterMultiples(seq(1, 10), 2) ---> ";
         printLazyList(testFilterMultiples);
         print "\n");

(* Additional test for filterMultiples with another value *)
val testFilterMultiples2 = filterMultiples(seq(1, 10), 3);
val _ = (print "filterMultiples(seq(1, 10), 3) ---> ";
         printLazyList(testFilterMultiples2);
         print "\n");
