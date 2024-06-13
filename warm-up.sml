
fun clone a n =
  let
    fun aux(0, acc) = acc
      | aux(m, acc) = aux(m - 1, a :: acc)
  in
    if n < 0 then []
    else aux(n, [])
  end;


fun sepConcat (sep, []) = ""
  | sepConcat (sep, [s]) = s
  | sepConcat (sep, s::ss) =
      let
        fun foldHelper(x, acc) = acc ^ sep ^ x
      in
        List.foldl foldHelper s ss
      end;


fun listNth (lst, i) =
  let
    fun nthHelper ([], _) = NONE
      | nthHelper (x::xs, 0) = SOME x
      | nthHelper (x::xs, n) = nthHelper (xs, n - 1)
  in
    if i < 0 then NONE else nthHelper (lst, i)
  end;


fun listAssoc (key, []) = NONE
  | listAssoc (key, (k, v)::rest) =
      if key = k then SOME v
      else listAssoc (key, rest);

(* Test cases for clone *)
val _ = print ("clone 0 5 ---> " ^ String.concatWith ", " (List.map Int.toString (clone 0 5)) ^ "\n");
val _ = print ("clone \"mmmm\" 3 ---> " ^ String.concatWith ", " (clone "mmmm" 3) ^ "\n");

(* Test cases for sepConcat *)
val _ = print ("sepConcat (\",\", [\"good\", \"better\", \"best\"]) ---> " ^ sepConcat (",", ["good", "better", "best"]) ^ "\n");
val _ = print ("sepConcat (\",\", []) ---> " ^ sepConcat (",", []) ^ "\n");

(* Test cases for listNth *)
val _ = print ("listNth ([\"good\", \"better\", \"best\"], 1) ---> " ^ (case listNth (["good", "better", "best"], 1) of NONE => "NONE" | SOME x => x) ^ "\n");
val _ = print ("listNth ([9, 12, 4, 18], 10) ---> " ^ (case listNth ([9, 12, 4, 18], 10) of NONE => "NONE" | SOME x => Int.toString x) ^ "\n");

(* Test cases for listAssoc *)
val l = [("william", 10), ("ranjit", 4), ("mooneer", 92), ("kin", 68), ("ranjit", 0)];
val _ = print ("listAssoc (\"ranjit\", l) ---> " ^ (case listAssoc ("ranjit", l) of NONE => "NONE" | SOME x => Int.toString x) ^ "\n");
val _ = print ("listAssoc (\"genghiz\", l) ---> " ^ (case listAssoc ("genghiz", l) of NONE => "NONE" | SOME x => Int.toString x) ^ "\n");
