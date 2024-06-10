(* Part (a) *)
fun map_first_two f lst =
    let
        (* Helper function to process the list in pairs *)
        fun helper (a::b::rest) = (f (a, b)) :: helper (b::rest)
          | helper _ = []  (* Base case: return an empty list if fewer than two elements *)
    in
        helper lst
    end

(* Part (b) *)
fun curry3 f x y z = f (x, y, z)


(* Example usage for map_first_two *)
val result1 = map_first_two (op +) [2, 3, 4, 5, 7];
(* result1 should be [5, 7, 9, 12] *)

(* Example usage for curry3 *)
fun add3 (x, y, z) = x + y + z;
val curriedAdd3 = curry3 add3;
val result2 = curriedAdd3 1 2 3;
(* result2 should be 6 *)

(* Printing the results *)
val _ = (
    print ("result1 = [");
    List.app (fn x => print (Int.toString x ^ " ")) result1;
    print ("]\n");
    print ("result2 = " ^ Int.toString result2 ^ "\n")
);
