INTERFACE SpiceCircuit;
IMPORT SpiceObjectSeq;
IMPORT Refany;
IMPORT TextSeq;
IMPORT TextTextTbl;

TYPE
  T = OBJECT
    name     : TEXT; (* if any *)

    params   : TextSeq.T;
    (* names of parameters in order *)
    
    elements : SpiceObjectSeq.T;
    (* elements within object *)

    bindings : TextTextTbl.T := NIL;
    (* this is for CDL-style postfix bindings, e.g., 

       .SUBCKT vendor.intel.g1i.xor004aa.2d02x5 a b c d out0 vcc vssx LG=1.4E-8
+WG=3.0E-8

       If no such bindings exist, the table will be NIL 

       keys of table are binding names, e.g., LG, WG
       values of table are binding defaults, e.g., 1.4E-8, 3.0E-8

       values are unparsed TEXT
       equals signs are stripped

    *)
  END;

CONST Brand = "SpiceCircuit";

CONST Equal = Refany.Equal;

END SpiceCircuit.
