INTERFACE StdfRand;
IMPORT Random;

(* the procedures in this interface return the length by incrementing
   the len field *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rand : Random.T) : T;
    
    chars(VAR x : ARRAY OF CHAR; fromSet := EmptySet; utf8 := FALSE);
    char(fromSet := EmptySet) : CHAR;
    u1(weighting : Weighting := WeightingDefault) : [0..255];
    u2(weighting : Weighting := WeightingDefault) : [0..65535];

    configureCharacterSet(READONLY set : SET OF CHAR; utf8 : BOOLEAN);
    configureWeighting(weighting : CARDINAL);
  END;
      
TYPE  Weighting        = [-1..LAST(CARDINAL)];
CONST WeightingUniform =  0; (* uniformly weighted random nos. *)
      WeightingDefault = -1; (* default weighting requested *)

CONST Brand = "StdfRand";

CONST Alpha     = SET OF CHAR { 'a' .. 'z', 'A' .. 'z' };
      Printable = SET OF CHAR { VAL(32, CHAR) .. VAL(126, CHAR) };

      (* could do UTF-8 as well .... might be harder ? *)

CONST EmptySet = SET OF CHAR {} ;
      
END StdfRand.
