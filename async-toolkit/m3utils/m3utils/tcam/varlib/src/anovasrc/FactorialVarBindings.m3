(* $Id: FactorialVarBindings.m3,v 1.1 2006/02/27 23:43:47 mika Exp $ *)

MODULE FactorialVarBindings;
IMPORT RefList;
IMPORT TextReader, Text, Process;
IMPORT FloatMode, Lex, Scan;

PROCEDURE AddTextBinding(arg : TEXT; VAR bindings : RefList.T) 
  RAISES { TextReader.NoMore } =
  VAR
    reader := NEW(TextReader.T).init(arg);
    var := reader.nextE("=");
    val := reader.nextE("");
    p := bindings;
  BEGIN
    (* search for existing binding *)
    WHILE p # NIL DO
      IF Text.Equal(NARROW(p.head,T).varName,var) THEN
        IF NOT ISTYPE(p.head,TextBinding) THEN
          Process.Crash("Type mismatch in binding for \"" & var & "\"!")
        END;
        WITH ob = NARROW(p.head,TextBinding) DO
          VAR
            new := NEW(REF ARRAY OF TEXT, NUMBER(ob.val^) + 1);
          BEGIN
            SUBARRAY(new^,0,NUMBER(ob.val^)) := ob.val^;
            new[LAST(new^)] := val;
            ob.val := new
          END;
          RETURN
        END
      END;
      p := p.tail
    END;

    (* failed, add fresh binding *)
    VAR
      nb := NEW(TextBinding, varName := var, val := NEW(REF ARRAY OF TEXT,1));
    BEGIN
      nb.val[0] := val;
      bindings := RefList.Cons(nb,bindings)
    END
  END AddTextBinding;





PROCEDURE AddIntBinding(arg : TEXT; VAR bindings : RefList.T) RAISES { TextReader.NoMore, Lex.Error, FloatMode.Trap } =
  VAR
    reader := NEW(TextReader.T).init(arg);
    var := reader.nextE("=");
    val := Scan.Int(reader.nextE(""));
    p := bindings;
  BEGIN
    (* search for existing binding *)
    WHILE p # NIL DO
      IF Text.Equal(NARROW(p.head,T).varName,var) THEN
        IF NOT ISTYPE(p.head,IntBinding) THEN
          Process.Crash("Type mismatch in binding for \"" & var & "\"!")
        END;
        WITH ob = NARROW(p.head,IntBinding) DO
          VAR
            new := NEW(REF ARRAY OF INTEGER, NUMBER(ob.val^) + 1);
          BEGIN
            SUBARRAY(new^,0,NUMBER(ob.val^)) := ob.val^;
            new[LAST(new^)] := val;
            ob.val := new
          END;
          RETURN
        END
      END;
      p := p.tail
    END;

    (* failed, add fresh binding *)
    VAR
      nb := NEW(IntBinding, varName := var, val := NEW(REF ARRAY OF INTEGER,1));
    BEGIN
      nb.val[0] := val;
      bindings := RefList.Cons(nb,bindings)
    END
  END AddIntBinding;

PROCEDURE AddRealBinding(arg : TEXT; VAR bindings : RefList.T) RAISES { TextReader.NoMore, Lex.Error, FloatMode.Trap } =
  VAR
    reader := NEW(TextReader.T).init(arg);
    var := reader.nextE("=");
    val := Scan.LongReal(reader.nextE(""));
    p := bindings;
  BEGIN
    (* search for existing binding *)
    WHILE p # NIL DO
      IF Text.Equal(NARROW(p.head,T).varName,var) THEN
        IF NOT ISTYPE(p.head,LRBinding) THEN
          Process.Crash("Type mismatch in binding for \"" & var & "\"!")
        END;
        WITH ob = NARROW(p.head,LRBinding) DO
          VAR
            new := NEW(REF ARRAY OF LONGREAL, NUMBER(ob.val^) + 1);
          BEGIN
            SUBARRAY(new^,0,NUMBER(ob.val^)) := ob.val^;
            new[LAST(new^)] := val;
            ob.val := new
          END;
          RETURN
        END
      END;
      p := p.tail
    END;

    (* failed, add fresh binding *)
    VAR
      nb := NEW(LRBinding, varName := var, val := NEW(REF ARRAY OF LONGREAL,1));
    BEGIN
      nb.val[0] := val;
      bindings := RefList.Cons(nb,bindings)
    END
  END AddRealBinding;

PROCEDURE AddBoolBinding(arg : TEXT; VAR bindings : RefList.T) RAISES { TextReader.NoMore, Lex.Error } =
  VAR
    reader := NEW(TextReader.T).init(arg);
    var := reader.nextE("=");
    val := Scan.Bool(reader.nextE(""));
    p := bindings;
  BEGIN
    (* search for existing binding *)
    WHILE p # NIL DO
      IF Text.Equal(NARROW(p.head,T).varName,var) THEN
        IF NOT ISTYPE(p.head,BoolBinding) THEN
          Process.Crash("Type mismatch in binding for \"" & var & "\"!")
        END;
        WITH ob = NARROW(p.head,BoolBinding) DO
          VAR
            new := NEW(REF ARRAY OF BOOLEAN, NUMBER(ob.val^) + 1);
          BEGIN
            SUBARRAY(new^,0,NUMBER(ob.val^)) := ob.val^;
            new[LAST(new^)] := val;
            ob.val := new
          END;
          RETURN
        END
      END;
      p := p.tail
    END;

    (* failed, add fresh binding *)
    VAR
      nb := NEW(BoolBinding, varName := var, val := NEW(REF ARRAY OF BOOLEAN,1));
    BEGIN
      nb.val[0] := val;
      bindings := RefList.Cons(nb,bindings)
    END
  END AddBoolBinding;


BEGIN END FactorialVarBindings.
