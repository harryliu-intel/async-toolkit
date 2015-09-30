(* $Id: SchemeAutoTbl.ig,v 1.1 2008/10/22 05:08:22 mika Exp $ *)

GENERIC INTERFACE SchemeAutoTbl(Tbl, Key, Value);

TYPE (* there are just here to make the compiler shut up for now *)
  Table = Tbl.T;
  K = Key.T;
  V = Value.T;

PROCEDURE Register();

END SchemeAutoTbl.
