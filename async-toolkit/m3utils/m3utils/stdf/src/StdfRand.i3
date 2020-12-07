INTERFACE StdfRand;

(* the procedures in this interface return the length by incrementing
   the len field *)

CONST EmptySet = SET OF CHAR {} ;
      
PROCEDURE Chars(VAR     x : ARRAY OF CHAR;
                fromSet   := EmptySet;
                utf8      := FALSE);
  (* fill the array x with characters *)

PROCEDURE Char(fromSet := EmptySet) : CHAR;

TYPE  Weighting        = [-1..LAST(CARDINAL)];
CONST WeightingUniform =  0; (* uniformly weighted random nos. *)
      WeightingDefault = -1; (* default weighting requested *)

PROCEDURE U1(weighting : Weighting := WeightingDefault) : [ 0..255 ];

PROCEDURE U2(weighting : Weighting := WeightingDefault) : [ 0..65535 ];

PROCEDURE ConfigureCharacterSet(READONLY set : SET OF CHAR; utf8 : BOOLEAN);

PROCEDURE ConfigureWeighting(weighting : CARDINAL);
  
CONST Brand = "StdfRand";

CONST Alpha     = SET OF CHAR { 'a' .. 'z', 'A' .. 'z' };
      Printable = SET OF CHAR { VAL(32, CHAR) .. VAL(126, CHAR) };

      (* could do UTF-8 as well .... might be harder ? *)
      
END StdfRand.
