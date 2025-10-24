INTERFACE MyAtom;
(* same interface as Atom + CHARS ops.  for speed and memory *)
(* Not threadsafe. *)

IMPORT Word;

TYPE T <: REFANY;
     
CONST Brand = "MyAtom";

PROCEDURE FromChars(READONLY c : ARRAY OF CHAR) : T;

PROCEDURE FromText(t : TEXT) : T;

PROCEDURE ToText(a : T) : TEXT;

PROCEDURE Equal(a1, a2 : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

CONST Compare : PROCEDURE(a1, a2 : T) : [-1..1] = NIL;

END MyAtom.

