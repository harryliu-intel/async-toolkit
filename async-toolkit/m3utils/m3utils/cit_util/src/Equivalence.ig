GENERIC INTERFACE Equivalence(Elem);

(*
A "T" represents an equivalence relation on the set of all "Elem.T"s.
A newly created "Default" has each "Elem.T" in its own equivalence
class.

Interface "Elem" is expected to have the following declaration:

| PROCEDURE Equal(e1, e2: Elem.T): BOOLEAN;

which defines the a priori equality of two elements.

The "Default" implementation also expects an "ElemElemTbl".
*)

TYPE
  T = OBJECT METHODS
    equal(e1, e2: Elem.T): BOOLEAN;
(* are "e1" and "e2" members of the same equivalence class? *)

    identify(e1, e2: Elem.T): BOOLEAN;
(* join the two equivalence classes represented by "e1" and "e2".
   return "TRUE" iff they are already equal. *)

    canon(e: Elem.T): Elem.T;
(* return the canonical representative of the class containing "e". *)

    iterate(): Iterator;
(* For each element which is not its own canonical representative,
   obtain that element as "alias", and
   its canonical representative as "canon". *)
  END;
  Iterator = OBJECT METHODS
    next(VAR alias, canon: Elem.T): BOOLEAN;
  END;

  Default <: T OBJECT METHODS
    init(sizeHint: CARDINAL := 0): Default;
  END;


END Equivalence.
