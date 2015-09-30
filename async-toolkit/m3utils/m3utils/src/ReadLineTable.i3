(* $Id: ReadLineTable.i3,v 1.1 2009/04/24 07:09:13 mika Exp $ *)

INTERFACE ReadLineTable;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(cols : CARDINAL;
         fmt : TEXT := NIL; (* in the format of Fmt.FN *)
         READONLY headings : ARRAY OF TEXT := ARRAY OF TEXT {}) : T;
    addRow(READONLY cols : ARRAY OF TEXT);
    addHline(of := '-');
  END;

CONST Brand = "ReadLineTable";

END ReadLineTable.
