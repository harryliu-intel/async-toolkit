(* $Id$ *)

MODULE SchemeUtils;
IMPORT Scheme, SchemeInputPort, SchemeClass;
IMPORT Wr, Fmt, Wx, Stdio;
FROM Scheme IMPORT Object, E, Symbol, Vector, String, Pair, Character, 
                   LongReal, Boolean;
FROM SchemeChar IMPORT Char, Chr;

(* the return Str(Error...) is really quite bizarre; also in the Sym
   and Vec routines below... *)

PROCEDURE Str(x : Object) : String =
  BEGIN
    IF x # NIL AND ISTYPE(x,String) THEN RETURN x 
    ELSE RETURN Str(Error("expected a string, got: " & DebugFormat(x)))
    END
  END Str;

PROCEDURE Sym(x : Object) : Symbol =
  BEGIN
    IF x # NIL AND ISTYPE(x,Symbol) THEN RETURN x 
    ELSE RETURN Sym(Error("expected a symbol, got: " & DebugFormat(x)))
    END
  END Sym;

PROCEDURE Vec(x : Object) : Vector =
  BEGIN
    IF x # NIL AND ISTYPE(x,Vector) THEN RETURN x 
    ELSE RETURN Vec(Error("expected a vector, got: " & DebugFormat(x)))
    END
  END Vec;

PROCEDURE InPort(x : Object; interp : Scheme.T) : SchemeInputPort.T =
  BEGIN
    IF x = NIL THEN RETURN interp.input END;
    IF ISTYPE(x,SchemeInputPort.T) THEN RETURN x 
    ELSE 
      RETURN InPort(Error("expected a schemeInputPort, got: " & 
                          DebugFormat(x)), interp)
    END
  END InPort;

PROCEDURE OutPort(x : Object; interp : Scheme.T) : Wr.T =
  BEGIN
    IF x = NIL THEN RETURN interp.output END;
    IF ISTYPE(x,Wr.T) THEN RETURN x 
    ELSE 
      RETURN OutPort(Error("expected an output port, got: " & 
                           DebugFormat(x)), interp)
    END
  END OutPort;

PROCEDURE Error(message : TEXT) : Object RAISES { E } =
  BEGIN
    Wr.PutText(Stdio.stderr, "**** ERROR: " & message);
    RAISE E(message)
  END Error;

PROCEDURE Warn(message : TEXT) : Object =
  BEGIN
    Wr.PutText(Stdio.stderr, "**** WARNING: " & message);
    RETURN "<warn>"
  END Warn;

PROCEDURE First(x : Object) : Object =
  BEGIN
    IF x = NIL THEN RETURN NIL END;

    TYPECASE x OF Pair(p) => RETURN p.first ELSE RETURN NIL END
  END First;

PROCEDURE Rest(x : Object) : Object =
  BEGIN
    IF x = NIL THEN RETURN NIL END;

    TYPECASE x OF Pair(p) => RETURN p.rest ELSE RETURN NIL END
  END Rest;

PROCEDURE Second(x : Object) : Object =
  BEGIN RETURN First(Rest(x)) END Second;

PROCEDURE Third(x : Object) : Object = 
  BEGIN RETURN First(Rest(Rest(x))) END Third;

PROCEDURE PedanticFirst(x : Object) : Object =
  BEGIN
    IF x # NIL AND ISTYPE(x, Pair) THEN 
      RETURN NARROW(x,Pair).first
    ELSE 
      RETURN Error("Attempt to car of a non-Pair:" & Stringify(x)) 
    END
  END PedanticFirst;

PROCEDURE PedanticRest(x : Object) : Object =
  BEGIN
    IF x # NIL AND ISTYPE(x, Pair) THEN 
      RETURN NARROW(x,Pair).rest
    ELSE 
      RETURN Error("Attempt to cdr of a non-Pair:" & Stringify(x)) 
    END
  END PedanticRest;

PROCEDURE SetFirst(x, y : Object) : Object =
  BEGIN
    TYPECASE x OF 
      Pair(p) => p.first := y; RETURN y
    ELSE 
      RETURN Error("Attempt to set-car of a non-Pair:" & Stringify(x)) 
    END
  END SetFirst;

PROCEDURE SetRest(x, y : Object) : Object =
  BEGIN
    TYPECASE x OF 
      Pair(p) => p.rest := y; RETURN y
    ELSE 
      RETURN Error("Attempt to set-cdr of a non-Pair:" & Stringify(x)) 
    END
  END SetRest;

PROCEDURE List1(x : Object) : Pair =
  BEGIN RETURN NEW(Pair, first := x, rest := NIL) END List1;

PROCEDURE List2(x, y : Object) : Pair =
  BEGIN 
    RETURN NEW(Pair, first := x, rest := NEW(Pair,first := y, rest := NIL))
  END List2;

PROCEDURE ListStar(x : Object) : Object =
  BEGIN
    IF Rest(x) = NIL THEN RETURN First(x) 
    ELSE RETURN Cons(First(x), ListStar(Rest(x)))
    END
  END ListStar;
  
PROCEDURE Cons(a, b : Object) : Pair = 
  BEGIN RETURN NEW(Pair, first := a, rest := b) END Cons;

PROCEDURE Reverse(x : Object) : Object =
  VAR result : Object := NIL;
  BEGIN
    WHILE x # NIL AND ISTYPE(x,Pair) DO
      result := Cons(First(x), result); 
      x := Rest(x)
    END;
    RETURN result
  END Reverse;

PROCEDURE Equal(x, y : Object) : BOOLEAN =
  BEGIN
    IF x = NIL OR y = NIL THEN 
      RETURN x = y
    ELSE
      TYPECASE x OF
        String(sx) =>
        TYPECASE y OF 
          String(sy) =>
          IF NUMBER(sx^) # NUMBER(sy^) THEN RETURN FALSE END;
          FOR i := FIRST(sx^) TO LAST(sx^) DO
            IF sx[i] # sy[i] THEN RETURN FALSE END
          END;
          RETURN TRUE
        ELSE (* y not string *)
          RETURN FALSE
        END
      |
        Vector(vx) =>
        TYPECASE y OF 
          Vector(vy) =>
          IF NUMBER(vx^) # NUMBER(vy^) THEN RETURN FALSE END;
          FOR i := FIRST(vx^) TO LAST(vx^) DO
            IF vx[i] # vy[i] THEN RETURN FALSE END
          END;
          RETURN TRUE
        ELSE (* y not string *)
          RETURN FALSE
        END
      ELSE
        RETURN x = y (* right? *)
      END
    END
  END Equal;

PROCEDURE Eqv(x, y : Object) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      LongReal(lx) => 
      TYPECASE y OF LongReal(ly) => RETURN lx^ = ly^ ELSE RETURN FALSE END
      (* chars are shared in our system, no need to check values here *)
    ELSE
      RETURN x = y
    END
  END Eqv;

PROCEDURE Length(x : Object) : CARDINAL =
  VAR len := 0;
  BEGIN
    WHILE x # NIL AND ISTYPE(x,Pair) DO INC(len); x := Rest(x) END;
    RETURN len
  END Length;

PROCEDURE ListToString(chars: Object) : String =
  VAR str := NEW(String, Length(chars));
      i := 0;
  BEGIN
    WHILE chars # NIL AND ISTYPE(chars,Pair) DO
      str[i] := Char(First(chars));
      chars := Rest(chars);
      INC(i)
    END;
    RETURN str
  END ListToString;

PROCEDURE ListToVector(objs : Object) : Vector =
  VAR vec := NEW(Vector, Length(objs));
      i := 0;
  BEGIN
    WHILE objs # NIL AND ISTYPE(objs,Pair) DO
      vec[i] := Chr(First(objs));
      objs := Rest(objs);
      INC(i)
    END;
    RETURN vec
  END ListToVector;

PROCEDURE Write(x : Object; port : Wr.T; quoted : BOOLEAN) : Object =
  BEGIN
    Wr.PutText(port, StringifyQ(x, quoted));
    Wr.Flush(port);
    RETURN x
  END Write;

PROCEDURE VectorToList(x : Object) : Pair =
  BEGIN
    TYPECASE x OF
      Vector(vec) =>
      VAR result : Pair := NIL; BEGIN
        FOR i := LAST(vec^) TO FIRST(vec^) BY -1 DO
          result := Cons(vec[i],result)
        END;
        RETURN result
      END
    ELSE
      EVAL Error("expected a vector, got: " & DebugFormat(x));
      RETURN NIL
    END
  END VectorToList;

PROCEDURE P(msg : TEXT; x : Object) : Object =
  (* for debugging *)
  BEGIN
    Wr.PutText(Stdio.stdout, msg & ": " & Stringify(x));
    RETURN x
  END P;

PROCEDURE Stringify(x : Object) : TEXT =
  BEGIN RETURN StringifyQ(x,TRUE) END Stringify;

PROCEDURE StringifyQ(x : Object; quoted : BOOLEAN) : TEXT =
  BEGIN
    WITH buf = Wx.New() DO
      StringifyB(x, quoted, buf);
      RETURN Wx.ToText(buf)
    END
  END StringifyQ;

PROCEDURE StringifyB(x : Object; quoted : BOOLEAN; buf : Wx.T) =

  PROCEDURE Put(txt : TEXT) = BEGIN Wx.PutText(buf,txt) END Put;
  PROCEDURE PutC(c : CHAR) = BEGIN Wx.PutChar(buf,c) END PutC;

  BEGIN
    IF x = NIL THEN 
      Put("()")
    ELSE
      <* ASSERT NOT ISTYPE(x,TEXT) *>
      TYPECASE x OF
        LongReal(lr) =>
        IF FLOAT(ROUND(lr^),LONGREAL) = lr^ THEN
          Put(Fmt.Int(ROUND(lr^)))
        ELSE
          Put(Fmt.LongReal(lr^))
        END
      |
        Character(c) =>
        IF quoted THEN Put("#" & BS) END;
        PutC(Char(c))
      |
        Pair(p) => p.stringifyPair(quoted,buf)
      |
        String(s) =>
        IF quoted THEN PutC(DQC) END;
        FOR i := FIRST(s^) TO LAST(s^) DO
          IF quoted AND s[i] = DQC THEN PutC(BSC) END;
          PutC(s[i])
        END;
        IF quoted THEN PutC(DQC) END
      |
        Vector(v) =>
        Put("#(");
        FOR i := FIRST(v^) TO LAST(v^) DO
          StringifyB(v[i], quoted, buf);
          IF i # LAST(v^) THEN PutC(' ') END
        END;
        PutC(')')
      |
        Boolean(b) =>
        CASE b^ OF
          TRUE => Put("#t")
        |
          FALSE => Put("#f")
        END
      ELSE
        Put(DebugFormat(x)) (* quoting? *)
      END
    END
  END StringifyB;

(* special characters below, they mess up auto-indentation in emacs... *)
CONST BS = "\\"; DQC = '"'; BSC= '\\';

BEGIN END SchemeUtils.
