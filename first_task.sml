(* Define the first expression: (λx. x) (λy. x) (xy) *)
val expr1 = 
  let
    (* Lambda x. x *)
    fun identity x = x

    (* Lambda y. x *)
    fun const_x y = "x"

    (* Apply identity to const_x, then apply the result to "xy" *)
  in
    (identity const_x) "xy"
  end

(* Define the second expression: λy. (λx. x) (xy) *)
fun expr2 y =
  let
    (* Lambda x. x *)
    fun identity x = x

    (* Apply identity to "xy" *)
  in
    identity ("xy")
  end

(* Compare the two expressions *)
val result1 = expr1
val result2 = expr2 "ignored"

val are_equal = (result1 = result2)

(* Print the results *)
val _ = print ("Expression 1 evaluates to: " ^ result1 ^ "\n")
val _ = print ("Expression 2 evaluates to: " ^ result2 ^ "\n")
val _ = print ("Are the expressions equal? " ^ Bool.toString are_equal ^ "\n")
