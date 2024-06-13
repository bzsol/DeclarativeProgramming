
fun reverse(xs) =
    let
        fun revHelper([], acc) = acc
          | revHelper(x::xs, acc) = revHelper(xs, x::acc)
    in
        revHelper(xs, [])
    end;


fun mergeWithDuplicateCheck([], ys) = (ys, false)
  | mergeWithDuplicateCheck(xs, []) = (xs, false)
  | mergeWithDuplicateCheck(x::xs, y::ys) =
        if x = y then
            ([], true)
        else if x < y then
            let
                val (merged, dup) = mergeWithDuplicateCheck(xs, y::ys)
            in
                (x::merged, dup)
            end
        else
            let
                val (merged, dup) = mergeWithDuplicateCheck(x::xs, ys)
            in
                (y::merged, dup)
            end;


fun mergePairsWithDuplicateCheck([], acc) = (reverse(acc), false)
  | mergePairsWithDuplicateCheck([x], acc) = (reverse(x::acc), false)
  | mergePairsWithDuplicateCheck(x::y::xs, acc) =
        let
            val (merged, dup) = mergeWithDuplicateCheck(x, y)
        in
            if dup then
                ([], true)
            else
                mergePairsWithDuplicateCheck(xs, merged::acc)
        end;


fun mergeSortIterWithDuplicateCheck([]) = ([], false)
  | mergeSortIterWithDuplicateCheck([x]) = (x, false)
  | mergeSortIterWithDuplicateCheck(xs) =
        let
            val (merged, dup) = mergePairsWithDuplicateCheck(xs, [])
        in
            if dup then
                ([], true)
            else
                mergeSortIterWithDuplicateCheck(merged)
        end;


fun listifyWithDuplicateCheck(L) =
    let
        fun aux([], curr, res) = (reverse(curr :: res), false)
          | aux([x], curr, res) = (reverse((x :: curr) :: res), false)
          | aux(x::y::xs, curr, res) =
                if x = y then
                    ([], true)
                else if x < y then
                    aux(y::xs, x::curr, res)
                else
                    let
                        val (sublists, dup) = aux(y::xs, [], reverse((x :: curr) :: res))
                    in
                        if dup then
                            ([], true)
                        else
                            (sublists, false)
                    end
    in
        aux(L, [], [])
    end;


fun duplicates2(L) =
    let
        val (sublists, dup) = listifyWithDuplicateCheck(L)
    in
        if dup then
            true
        else
            let
                val (_, dupFinal) = mergeSortIterWithDuplicateCheck(sublists)
            in
                dupFinal
            end
    end;


val test1 = duplicates2([3,5,1,1,9,2,1,0]); (* true *)
val test2 = duplicates2([1,2,3,4,5,6]);     (* false *)
val test3 = duplicates2([5,4,3,2,1,1]);     (* true *)
val test4 = duplicates2([10,9,8,7,6,5,4,3,2,1]); (* false *)
val test5 = duplicates2([1,1,1,1,1]);       (* true *)

(* Print test cases *)
val _ = print("Test 1: " ^ Bool.toString(test1) ^ "\n");
val _ = print("Test 2: " ^ Bool.toString(test2) ^ "\n");
val _ = print("Test 3: " ^ Bool.toString(test3) ^ "\n");
val _ = print("Test 4: " ^ Bool.toString(test4) ^ "\n");
val _ = print("Test 5: " ^ Bool.toString(test5) ^ "\n");
