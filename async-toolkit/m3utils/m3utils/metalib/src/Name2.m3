MODULE Name2 EXPORTS Name;
IMPORT CardSeq;

(* alternative implementation of Name interface *)

(* unfinished *)

TYPE
  L = RECORD
    offset : CARDINAL;
    hashV  : Word.T;
  END;

PROCEDURE Hash(a : T) : Word.T      = BEGIN RETURN a.hashV END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

VAR data := NEW(REF ARRAY OF CHAR, 0); (* data *)
    nextd:= 0;
    map  := NEW(REF ARRAY OF L, 0);    (* metadata *)
    nextm:= 0;
    aux  := NEW(REF ARRAY OF CardSeq.T, 1024);

PROCEDURE Grow() =
  VAR 
    new := NEW(REF ARRAY OF CHAR, ((NUMBER(data) + 1000) * 3) DIV 2);
  BEGIN
    SUBARRAY(new^,0,NUMBER(data^)) := data^;
    data := new
  END Grow;

PROCEDURE RepHash(READONLY chars : ARRAY OF CHAR) : Word.T =
  (* not the worlds best hash function *)
  VAR res : Word.T := 0;
  BEGIN
    FOR i := 0 TO NUMBER(chars) DO res := Word.Plus(ORD(chars[i]),res) END;
    RETURN res
  END RepHash;

PROCEDURE ParseChars(READONLY chars : ARRAY OF CHAR) : T =
  VAR 
    h := RepHash(chars);
    bucket := aux[h MOD NUMBER(aux^)];
  BEGIN
    FOR i := 0 TO bucket.size() - 1 DO
      WITH try = bucket.get(i) DO
        IF RepEqual(chars, SUBARRAY(data^,try+1,ORD(data[try]))) THEN
          RETURN try
        END
      END
    END;
    VAR
      res := next;
    BEGIN
      IF next + NUMBER(chars) + 1 > NUMBER(data^) THEN Grow() END;
      data[next] := VAL(NUMBER(chars,CHAR));
      SUBARRAY(data^,next+1,NUMBER(chars)) := chars;
      next := next + NUMBER(chars) + 1;
      RETURN res
    END
  END ParseChars;

BEGIN
  FOR i := FIRST(aux) TO LAST(aux) DO
    aux[i] := NEW(CardSeq.T).init();
  END
END Name2.
