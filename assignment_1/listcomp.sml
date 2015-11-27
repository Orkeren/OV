(* A small language of list expressions. *)

datatype Value = IntVal of int
               | ListVal of Value list

datatype Exp = Const of Value (* A literal value. *)
             | Var of string (* The value of some variable in scope. *)
             | List of Exp list (* A list of expressions. *)
             | Compr of Exp * string * Exp * Exp option
             (* List comprehension of the form

                  [ Exp for X in Exp if Exp ]

                The predicate ('if Exp') is optional, as indicated by
                the option type.  The predicate is considered true if
                it evalutes to a non-zero integer, false if ite
                valuates to zero. *)
             | Range of Exp * Exp
             | Plus of Exp * Exp (* Adding two integers. *)
             | Minus of Exp * Exp (* Subtracting two integers. *)
             | Times of Exp * Exp (* Multiplying two integers. *)
             | Modulo of Exp * Exp (* Modulo of two integers. *)
             | Equal of Exp * Exp
             (* Compare two integers for equality.  1 if true, otherwise 0. *)
             | Less of Exp * Exp
             (* Compare two integers for less-than.  1 if true, otherwise 0. *)

(* A list mapping variable names to their values. *)
type SymTab = (string * Value) list

(* Insert a variable into the variable table. *)
fun insert k v vtable = (k,v) :: vtable

fun range x y zs = if (x > y) then zs else range x (y - 1) (y::zs)

(* Lookup a the value of a variable. *)
fun lookup k0 ((k1,v) :: vtable) = if k0 = k1 then SOME v else lookup k0 vtable
  | lookup _ _ = NONE

(* The evaluation function. *)
fun eval vtable e =
  case e of
      Const v => v
    | Var k => 
      (case lookup k vtable of
          NONE => raise Fail "Unknown variable"
        | SOME x => x)
    | List l => ListVal (map (eval vtable) l)
    | Compr (e, k, a, p) => 
        (case eval vtable a of
          ListVal x =>
            (case p of 
                NONE => 
                  let
                    fun fun1 an = eval [(k, an)] e
                    val a1 = eval vtable a
                  in
                    map fun1 a1
                  end
              | SOME p1 => raise Fail "Not handled.")
      | _ => raise Fail "Only functions that produce lists please!")
    | Range (x,y) =>
        let
          val IntVal x1 = eval vtable x
          val IntVal y1 = eval vtable y
        in
          ListVal (map IntVal (range x1 y1 []))
        end
    | Plus (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b =>
            let
              val IntVal x1 = eval vtable x
              val IntVal y1 = eval vtable y
            in
              IntVal (x1+y1)
            end
        | ListVal bb => raise Fail "No lists please!")
      | ListVal aa => raise Fail "No lists please!")
    | Minus (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b =>
            let
              val IntVal x1 = eval vtable x
              val IntVal y1 = eval vtable y
            in
              IntVal (x1-y1)
            end
        | ListVal bb => raise Fail "No lists please!")
      | ListVal aa => raise Fail "No lists please!")
    | Times (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b =>
            let
              val IntVal x1 = eval vtable x
              val IntVal y1 = eval vtable y
            in
              IntVal (x1*y1)
            end
        | ListVal bb => raise Fail "No lists please!")
      | ListVal aa => raise Fail "No lists please!")
    | Modulo (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b =>
            let
              val IntVal x1 = eval vtable x
              val IntVal y1 = eval vtable y
            in
              IntVal (x1 mod y1)
            end
        | ListVal bb => raise Fail "No lists please!")
      | ListVal aa => raise Fail "No lists please!")
    | Equal (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b =>
            let
              val IntVal x1 = eval vtable x
              val IntVal y1 = eval vtable y
            in
              if x1 = y1 then IntVal 1 else IntVal 0
            end
        | ListVal bb => raise Fail "No lists please!")
      | ListVal aa => raise Fail "No lists please!")
    | Less (x,y) =>
(case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b =>
            let
              val IntVal x1 = eval vtable x
              val IntVal y1 = eval vtable y
            in
              if x1 < y1 then IntVal 1 else IntVal 0 
            end
        | ListVal bb => raise Fail "No lists please!")
      | ListVal aa => raise Fail "No lists please!")

fun intConst x = (Const (IntVal x))
(*

Tests commented out as they would currently raise exceptions.

(* Python: xs = range(0,9) *)
val xs = Range (intConst 0, intConst 9)

(* List the numbers in a range: *)
(* Python: [ x for x in xs ] *)
val test0 = eval [] (Compr (Var "x",
                            "x",
                            xs,
                            NONE))

(* Square the numbers in the range: *)
(* Python [ x * x for x in xs ] *)
val test1 = eval [] (Compr (Times (Var "x", Var "x"),
                            "x",
                            xs,
                            NONE))

(* Filter a range: *)
(* Python: [ x for x in xs if x % 2 == 0 ] *)
val test3 = eval [] (Compr (Var "x",
                            "x",
                            xs,
                            SOME (Equal (Modulo (Var "x", intConst 2), intConst 0))))

(* Repeat a value some times: *)
(* Python: [ 5 for x in xs ] *)
val test4 = eval [] (Compr (intConst 5, "x",  xs, NONE))

(* Repeat ^that^ some times: *)
(* Python: [ [ 5 for x in xs ] for y in xs ] *)
val test5 = eval [] (Compr (Compr (intConst 5,
                                   "x",
                                   xs,
                                   NONE),
                            "y",
                            xs,
                            NONE))

(* Use variables from two, nested contexts: *)
(* Python: [ [ x * y for x in xs ] for y in xs ] *)
val test6 = eval [] (Compr (Compr (Times (Var "x", Var "y"),
                                   "x",
                                   xs,
                                   NONE),
                            "y",
                            xs,
                            NONE))
*)
