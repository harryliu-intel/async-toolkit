(* $Id$ *)

INTERFACE HTMLFormatting;
IMPORT HTML;
IMPORT DBerr;

TYPE 
  T <: Public;

  Public = OBJECT 
    next : T := NIL;
  METHODS
    format(t : HTML.Stuff) : HTML.Stuff RAISES { DBerr.Error };
    colAttrs() : TEXT;
    matchesByName(colName : TEXT) : BOOLEAN;
    tdAttrs() : TEXT;
  END;

  RowFormat = T OBJECT METHODS
    (* set the tags for a row *)
    bgcolor() : TEXT;
    align() : TEXT;
    valign() : TEXT;
  END;

  EveryRowFormat <: RowFormat;
  (* just returns NIL for each tag *)

  NamedColumnFormat <: PubNamedColumnFormat;
 
  PubNamedColumnFormat = T OBJECT
    column : TEXT;
  END;

  Alignment <: PubAlignment;

  AlignmentType = { Left, Center, Right, Justify, Char };

  PubAlignment = NamedColumnFormat OBJECT
    type := AlignmentType.Right
  END;

  Dynamic = NamedColumnFormat OBJECT
  END;
  (* calls format() with the contents of the cell *)

  DynamicTD = NamedColumnFormat BRANDED Brand & " DynamicTD" OBJECT
  END;
  (* as above, but the user has to print <td> and </td> (they aren't added
     by the conversion code *)
  

CONST Brand = "HTMLFormatting";

PROCEDURE MakeRowFormat(bgcolor, align, valign := NIL) : RowFormat;

END HTMLFormatting.
