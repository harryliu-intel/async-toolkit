MODULE Name;
IMPORT Word, MyAtom AS Atom, NameNameTbl, MyAtomList AS AtomList, Text;
IMPORT IO;

(* INVARIANT:

   every Name.T exists in tbl exactly once 
*)

REVEAL T = AtomList.T BRANDED OBJECT END; (* can only create here *)

PROCEDURE Hash(a : T) : Word.T =
  VAR
    res := 0;
  BEGIN
    WHILE a # NIL DO
      res := Word.Plus(res, Atom.Hash(a.head)); a := a.tail
    END;
    RETURN res
  END Hash;

PROCEDURE TblEqual(<*UNUSED*>self : NameNameTbl.T; 
                   READONLY a, b : T) : BOOLEAN =
  BEGIN 
    RETURN (a = b) OR 
           (a.head = b.head AND a.tail = b.tail)
  END TblEqual;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Extend(root : T; by : Atom.T) : T =
  VAR res := tbl.t;
  BEGIN
    res.head := by;
    res.tail := root;
    IF NOT tbl.get(res,res) THEN
      (* lazy alloc *)
      EVAL tbl.put(res,res); tbl.t := NEW(T)
    END;
    RETURN res
  END Extend;

PROCEDURE Append(a, b : T) : T =
  VAR
    r := AtomList.Reverse(b);
    new := a;
  BEGIN
    WHILE r # NIL DO
      new := Extend(new, r.head); r := r.tail
    END;
    RETURN new
  END Append;

VAR tbl : NameNameTbl.Default OBJECT t : T END := 
  NEW(NameNameTbl.Default OBJECT t : T END, t := NEW(T), keyEqual := TblEqual).init();

PROCEDURE ParseCharsRef(chars : REF ARRAY OF CHAR) : T =
  BEGIN RETURN ParseChars(chars^) END ParseCharsRef;

PROCEDURE ParseChars(READONLY chars : ARRAY OF CHAR) : T =
  VAR
    p := 0; (* start of current segment *)
    res : T := NIL;

  PROCEDURE Make(beg, end : CARDINAL) =
    BEGIN
      WITH (*seg = Text.FromChars(SUBARRAY(chars,beg,end-beg+1)),
           atm = Atom.FromText(seg) *)
            atm = Atom.FromChars(SUBARRAY(chars,beg,end-beg+1))  DO
(*
      IO.Put("seg \"" & seg   & "\" atom \"" & Atom.ToText(atm) & "\"\n"); 
*)
        res := Extend(res, atm)
      END
    END Make;

  BEGIN
    IF NUMBER(chars) = 0 THEN RETURN NIL END;
    FOR i := FIRST(chars) TO LAST(chars) DO
      IF chars[i] = Sep THEN Make(p,i-1); p := i + 1 END
    END;
    Make(p,LAST(chars));
    RETURN res
  END ParseChars;

PROCEDURE ParseText(txt : TEXT) : T =
  VAR
    chars := NEW(REF ARRAY OF CHAR, Text.Length(txt));
  BEGIN
    Text.SetChars(chars^,txt);
    RETURN ParseChars(chars^)
  END ParseText;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
    IF t = NIL THEN RETURN "" END;
    
    VAR res := Atom.ToText(t.head);
    BEGIN
      t := t.tail;
      WHILE t # NIL DO
        res := Atom.ToText(t.head) & "." & res;
        t := t.tail
      END;
      RETURN res
    END
  END Format;

PROCEDURE Parent(t : T) : T = BEGIN RETURN t.tail END Parent;

PROCEDURE Tail(t : T) : T = BEGIN RETURN ParseText(Atom.ToText(t.head)) END Tail;

PROCEDURE Empty() : T = BEGIN RETURN NIL END Empty;

BEGIN END Name.

