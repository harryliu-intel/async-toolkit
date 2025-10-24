INTERFACE Name;
IMPORT Word;

TYPE T <: REFANY;

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "Name";

PROCEDURE Append(a, b : T) : T;              (* not threadsafe *)
  (* a.b *)

PROCEDURE ParseChars(READONLY chars : ARRAY OF CHAR) : T;

PROCEDURE ParseCharsRef(c : REF ARRAY OF CHAR) : T;

PROCEDURE ParseText(txt : TEXT) : T; (* less efficient than ParseChars *)

PROCEDURE Format(t : T) : TEXT;

PROCEDURE Parent(t : T) : T;

PROCEDURE Tail(t : T) : T;

PROCEDURE Empty() : T; (* return representation of empty string *)

CONST Sep = '.';

END Name.
