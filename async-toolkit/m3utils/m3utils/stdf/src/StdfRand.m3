(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE StdfRand;
IMPORT Random;

REVEAL
  T = Public BRANDED Brand OBJECT
    rand : Random.T;
    defWeighting : CARDINAL := WeightingUniform;
    defCharSet : SET OF CHAR := Printable;
    defUtf8 := FALSE;
  OVERRIDES
    init := Init;
    
    chars := Chars;
    char := Char;
    u1 := U1;
    u2 := U2;

    configureCharacterSet := ConfigureCharacterSet;
    configureWeighting := ConfigureWeighting;
  END;

PROCEDURE Init(t : T; rand : Random.T) : T =
  BEGIN
    t.rand := rand;
    RETURN t
  END Init;
  
PROCEDURE Chars(t : T;
                VAR     x : ARRAY OF CHAR;
                fromSet   := EmptySet;
                utf8      := FALSE) =
  (* fill the array x with characters *)
  (* utf8 not yet supported *)
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      x[i] := Char(t, fromSet)
    END
  END Chars;

PROCEDURE Char(t : T; fromSet := EmptySet) : CHAR
  RAISES {} =
  BEGIN
    IF fromSet = EmptySet THEN
      fromSet := t.defCharSet
    END;

    LOOP
      WITH ansatz = VAL(t.rand.integer(ORD(FIRST(CHAR)),
                                     ORD(LAST((CHAR)))),
                        CHAR) DO
        IF ansatz IN fromSet THEN
          RETURN ansatz
        END
      END
    END;
  END Char;

TYPE U1_T = [ 0 .. 255 ];
     
PROCEDURE U1(t : T; weighting : Weighting) : U1_T =
  VAR
    x := 1.0d0;
  BEGIN
    IF weighting = WeightingDefault THEN
      weighting := t.defWeighting
    END;
    FOR i := 0 TO weighting DO
      x := x * t.rand.longreal(0.0d0, 1.0d0)
    END;
    RETURN TRUNC(FLOAT(NUMBER(U1_T),LONGREAL) / x)
  END U1;           

TYPE U2_T = [ 0 .. 65535 ];
     
PROCEDURE U2(t : T; weighting : Weighting) : U2_T =
  VAR
    x := 1.0d0;
  BEGIN
    IF weighting = WeightingDefault THEN
      weighting := t.defWeighting
    END;
    FOR i := 0 TO weighting*2 DO
      x := x * t.rand.longreal(0.0d0, 1.0d0)
    END;
    RETURN TRUNC(FLOAT(NUMBER(U2_T),LONGREAL) / x)
  END U2;

PROCEDURE ConfigureCharacterSet(t : T; READONLY set : SET OF CHAR; utf8 : BOOLEAN) =
  BEGIN
    <*ASSERT set # EmptySet*>
    t.defCharSet := set;
    t.defUtf8    := utf8
  END ConfigureCharacterSet;

PROCEDURE ConfigureWeighting(t : T; weighting : CARDINAL) =
  BEGIN
    t.defWeighting := weighting
  END ConfigureWeighting;

BEGIN END StdfRand.
