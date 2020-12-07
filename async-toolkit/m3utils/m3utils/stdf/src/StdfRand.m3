MODULE StdfRand;
IMPORT Random;

PROCEDURE Chars(VAR     x : ARRAY OF CHAR;
                fromSet   := EmptySet;
                utf8      := FALSE) =
  (* fill the array x with characters *)
  (* utf8 not yet supported *)
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      x[i] := Char(fromSet)
    END
  END Chars;

VAR rand := NEW(Random.Default).init(); (* hrm shouldnt be here *)
    
PROCEDURE Char(fromSet := EmptySet) : CHAR
  RAISES {} =
  BEGIN
    IF fromSet = EmptySet THEN
      fromSet := defCharSet
    END;

    LOOP
      WITH ansatz = VAL(rand.integer(ORD(FIRST(CHAR)),
                                     ORD(LAST((CHAR)))),
                        CHAR) DO
        IF ansatz IN fromSet THEN
          RETURN ansatz
        END
      END
    END;
  END Char;

TYPE U1_T = [ 0 .. 255 ];
     
PROCEDURE U1(weighting : Weighting) : U1_T =
  VAR
    x := 1.0d0;
  BEGIN
    IF weighting = WeightingDefault THEN
      weighting := defWeighting
    END;
    FOR i := 0 TO weighting DO
      x := x * rand.longreal(0.0d0, 1.0d0)
    END;
    RETURN TRUNC(FLOAT(NUMBER(U1_T),LONGREAL) / x)
  END U1;           

TYPE U2_T = [ 0 .. 65535 ];
     
PROCEDURE U2(weighting : Weighting) : U2_T =
  VAR
    x := 1.0d0;
  BEGIN
    IF weighting = WeightingDefault THEN
      weighting := defWeighting
    END;
    FOR i := 0 TO weighting*2 DO
      x := x * rand.longreal(0.0d0, 1.0d0)
    END;
    RETURN TRUNC(FLOAT(NUMBER(U2_T),LONGREAL) / x)
  END U2;

VAR defCharSet := Printable;
    defUtf8    := FALSE;
    
PROCEDURE ConfigureCharacterSet(READONLY set : SET OF CHAR; utf8 : BOOLEAN) =
  BEGIN
    defCharSet := set;
    defUtf8    := utf8
  END ConfigureCharacterSet;

VAR defWeighting : CARDINAL := WeightingUniform;
    
PROCEDURE ConfigureWeighting(weighting : CARDINAL) =
  BEGIN
    defWeighting := weighting
  END ConfigureWeighting;

BEGIN END StdfRand.
