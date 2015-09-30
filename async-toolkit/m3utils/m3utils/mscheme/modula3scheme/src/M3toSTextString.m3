(* $Id: M3toSTextString.m3,v 1.1 2008/10/22 05:08:22 mika Exp $ *)

MODULE M3toSTextString;
IMPORT Text, Scheme;

PROCEDURE FromScheme(s : S) : T RAISES { Scheme.E } =
  BEGIN IF s = NIL THEN RAISE Scheme.E("NIL string") ELSE RETURN Text.FromChars(s^) END END FromScheme;

BEGIN END M3toSTextString.
