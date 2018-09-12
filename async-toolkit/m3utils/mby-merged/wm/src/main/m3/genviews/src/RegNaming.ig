GENERIC INTERFACE RegNaming(Constants);
IMPORT RegReg, RegRegfile, RegAddrmap, RegField;
IMPORT RegGenState;

(* procedures to map RDL names to target language names *)

CONST Brand = "Naming(" & Constants.Brand & ")";

PROCEDURE RegTypename(r : RegReg.T; state : RegGenState.T) : TEXT;

PROCEDURE RegfileTypename(r : RegRegfile.T; state : RegGenState.T) : TEXT;

PROCEDURE MapIntfName(a : RegAddrmap.T; state : RegGenState.T) : TEXT;

PROCEDURE MapTypename(a : RegAddrmap.T; state : RegGenState.T) : TEXT;

PROCEDURE FieldName(f : RegField.T; debug : BOOLEAN) : TEXT;

END RegNaming.
