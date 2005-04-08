INTERFACE FmtScan;
IMPORT Fmt;

(* Fmt/Scan pairs for use in CommandLoop generics.
   Generic "T" should be declared as follows:

   FmtScanT = OBJECT
     fmt(i: Elem.T): TEXT;
     scan(t: TEXT): Elem.T RAISES {FmtScan.Error};
   END;

*)

EXCEPTION
  Error;

PROCEDURE Int(base: INTEGER): IntT;
PROCEDURE Bool(): BoolT;
PROCEDURE LongReal(digits: CARDINAL; style: Fmt.Style): LongRealT;

TYPE
  IntT = OBJECT METHODS
    fmt(i: INTEGER): TEXT;
    scan(t: TEXT): INTEGER RAISES {Error};
  END;
  BoolT = OBJECT METHODS
    fmt(i: BOOLEAN): TEXT;
    scan(t: TEXT): BOOLEAN RAISES {Error};
  END;
  LongRealT = OBJECT METHODS
    fmt(i: LONGREAL): TEXT;
    scan(t: TEXT): LONGREAL RAISES {Error};
  END;


END FmtScan.
