INTERFACE RegModula3Naming;
IMPORT RegReg, RegRegfile, RegAddrmap, RegField;
IMPORT RegGenState;
FROM RegModula3 IMPORT RW;

(* procedures to map RDL names to Modula-3 *)

CONST Brand = "RegModula3Naming";

PROCEDURE RegTypename(r : RegReg.T; state : RegGenState.T) : TEXT;

PROCEDURE RegfileTypename(r : RegRegfile.T; state : RegGenState.T) : TEXT;

PROCEDURE MapIntfName(a : RegAddrmap.T; state : RegGenState.T) : TEXT;

PROCEDURE MapTypename(a : RegAddrmap.T; state : RegGenState.T) : TEXT;

PROCEDURE FieldName(f : RegField.T; debug : BOOLEAN) : TEXT;

  (**********************************************************************)

PROCEDURE IdiomName(txt : TEXT; debug := TRUE) : TEXT;

CONST PathSep = "_p_";

END RegModula3Naming.
