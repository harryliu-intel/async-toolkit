(* $Id$ *)

INTERFACE HTMLTable;
IMPORT HTML;
IMPORT HTMLFormatting;

TYPE
  Whence = { Bottom, Top }; 

TYPE
  T <: Public;
  AMatrix = REFANY;  (* must be a 2D matrix of approved types *)
  AVector = REFANY; (* should be a vector of allowed types *)

  Public = HTML.T OBJECT
  METHODS
    init(stuff         : AMatrix; 
         useBorders    : BOOLEAN := TRUE; 
         borderWidth   : CARDINAL := 0;
         colNames      : REF ARRAY OF TEXT := NIL;
         formatting    : HTMLFormatting.T := NIL;
         rowFormatting : HTMLFormatting.RowFormat := NIL;
         firstIsHead := FALSE) : T;

    (* this thing defaults to adding row at the end *)
    addRow(stuff : AVector; 
           row : CARDINAL := 0; 
           whence := Whence.Bottom);
  END;

PROCEDURE Horiz(c0, c1, c2, c3, c4, c5, c6 : HTML.Stuff := NIL;
                formatting : HTMLFormatting.T := NIL) : T;  
  (* quick horizontal arrangement, col names are the integers starting from
     zero, no borders *)

PROCEDURE Vert(c0, c1, c2, c3, c4, c5, c6 : HTML.Stuff := NIL;
               formatting : HTMLFormatting.T := NIL) : T;  
  (* quick vertical arrangement, row names are the integers starting from
     zero, no borders *)

END HTMLTable.
