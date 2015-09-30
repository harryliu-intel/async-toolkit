(* $Id: ToRefany.i3,v 1.2 2000/12/16 01:38:49 mika Exp $ *)

INTERFACE ToRefany;

IMPORT Word;
IMPORT ToRefanyClass;

TYPE T = REFANY;

(* for a type T, the set of supertypes is super(T) *)
(* for two types T, U, the least common supertype is lcm(T,U) *)

(* hash a 
   reference using the Hash of the lowest registered type in super(type(a)) *) 
PROCEDURE Hash(a : T) : Word.T;

(* check two references for equality using the Equal of the lowest 
   registered type in super(lcm(type(a),type(b))).  The result is FALSE 
   if the references have no least common supertype. *)
PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE AddType(type : ToRefanyClass.T);

END ToRefany.
