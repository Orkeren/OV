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
            ListVal vs =>
              (case p of 
                  NONE => ListVal (map (fn v => eval (insert k v vtable) e) vs)
                | SOME p1 => 
                    ListVal (foldr (fn (v, vv) => 
                      if (eval (insert k v vtable) p1 = IntVal 1) 
                        then ([(eval (insert k v vtable) e)] @ vv) 
                        else [] @ vv) [] vs))
          | _ => raise Fail "Expression in list position of comprehension does not evaluate to a list.") 
    | Range (x,y) =>
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b => ListVal (map IntVal (range a b []))
        | _ => raise Fail "Only IntVals please!")
      | _ => raise Fail "Only IntVals please")
    | Plus (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b => IntVal (a+b)
        | _ => raise Fail "Only IntVals please!")
      | _ => raise Fail "Only IntVals please")
    | Minus (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b => IntVal (a-b)
        | _ => raise Fail "Only IntVals please!")
      | _ => raise Fail "Only IntVals please")
    | Times (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b => IntVal (a*b)
        | _ => raise Fail "Only IntVals please!")
      | _ => raise Fail "Only IntVals please")
    | Modulo (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b => IntVal (a mod b)
        | _ => raise Fail "Only IntVals please!")
      | _ => raise Fail "Only IntVals please")
    | Equal (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b =>
            if a = b then IntVal 1 else IntVal 0
        | _ => raise Fail "Only IntVals please!")
      | _ => raise Fail "Only IntVals please")
    | Less (x,y) => 
        (case eval vtable x of
        IntVal a =>
        (case eval vtable y of
          IntVal b =>
            if a < b then IntVal 1 else IntVal 0
        | _ => raise Fail "Only IntVals please!")
      | _ => raise Fail "Only IntVals please")

fun intConst x = (Const (IntVal x))


(* Tests commented out as they would currently raise exceptions. *)

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
