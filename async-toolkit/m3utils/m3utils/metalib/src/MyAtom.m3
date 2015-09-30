MODULE MyAtom;

IMPORT Text, MyAtomMyAtomTbl, Word;

REVEAL
  T = BRANDED Brand REF RECORD
        ptr  : CARDINAL;
        hash : Word.T;
      END;

TYPE
  NewMyAtomTbl = MyAtomMyAtomTbl.Default OBJECT OVERRIDES
    keyEqual := TblEqual;
  END;

VAR
  table := NEW(NewMyAtomTbl).init();
  next  : T := NEW(T);  (* the next atom to be returned *)
  data := NEW(REF ARRAY OF CHAR, 0);
  ndata := 0;

PROCEDURE Grow(n : CARDINAL) =
  (* assure there is enough space in data to write n more bytes *)
  BEGIN
    IF ndata + n > NUMBER(data^) THEN
      WITH ns = (NUMBER(data^)*3 + 1000) DIV 2 DO
        VAR new := NEW(REF ARRAY OF CHAR, ns);
        BEGIN
          SUBARRAY(new^, 0, NUMBER(data^)) := data^;
          data := new
        END
      END
    END
  END Grow;

PROCEDURE FromChars(READONLY t: ARRAY OF CHAR): T =
  VAR a: T;
      len := NUMBER(t);
  BEGIN
    Grow(len+1);
    next.ptr := ndata;
    data[ndata] := VAL(len+1,CHAR);
    SUBARRAY(data^,ndata+1,len)  := t;

    next.hash := HashChars(t);

    IF NOT table.get(next, a) THEN
      a := next;
      next := NEW(T);
      INC(ndata, len+1);
      EVAL table.put(a, a)
    END;
    RETURN a
  END FromChars;

PROCEDURE HashChars(READONLY t : ARRAY OF CHAR) : Word.T = 
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO 
      res := Word.Xor (Word.LeftRotate (res, 13), ORD (t[i]))
    END;
    RETURN res
  END HashChars;

PROCEDURE FromText(t : TEXT) : T =
  VAR
    len := Text.Length(t);
    chars : ARRAY [0..ORD(LAST(CHAR)) ] OF CHAR;
  BEGIN
    Text.SetChars(SUBARRAY(chars,0,len),t);
    RETURN FromChars(SUBARRAY(chars,0,len))
  END FromText;

PROCEDURE ToText(a: T): TEXT =
  BEGIN
    RETURN Text.FromChars(SUBARRAY(data^,a.ptr+1,ORD(data[a.ptr])-1)) 
  END ToText;

PROCEDURE Hash(a: T): INTEGER =
  BEGIN
    RETURN a.hash
  END Hash;

PROCEDURE TblEqual(<*UNUSED*>self: NewMyAtomTbl;  READONLY a1, a2: T): BOOLEAN =
  (* This procedure is only used as the "keyEqual" method
     of the internal atom table.  It cannot rely on REF
     equality.  *)
  BEGIN
    RETURN (a1 = a2)
        OR ((a1.hash = a2.hash) AND 
             (SUBARRAY(data^,a1.ptr,ORD(data[a1.ptr])) =
              SUBARRAY(data^,a2.ptr,ORD(data[a2.ptr]))) )
  END TblEqual;

PROCEDURE Equal(a1, a2: T): BOOLEAN =
  BEGIN
    RETURN a1 = a2
  END Equal;

BEGIN
END MyAtom.
