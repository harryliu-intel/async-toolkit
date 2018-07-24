MODULE CompPath;
FROM Fmt IMPORT F, Int;
IMPORT Debug AS M3Debug;
IMPORT CompAddr, CompRange;
IMPORT Env;
IMPORT TextList;

VAR silent := Env.Get("WM_SILENT") # NIL;

REVEAL T = TextList.T BRANDED OBJECT END;
    
PROCEDURE Cat(a : T; b : TEXT) : T =
  BEGIN
    IF silent THEN RETURN NIL END;
    IF a = NIL OR b = NIL THEN
      (* this case is the probing an array case *)
      RETURN NIL
    END;
    RETURN TextList.Cons(b,a)
  END Cat;

PROCEDURE Empty() : T =
  BEGIN
    RETURN TextList.List1("")
  END Empty;
  
PROCEDURE CatArray(a : T; b : TEXT; i : CARDINAL) : T =
  BEGIN
    IF silent THEN RETURN NIL END;
    IF a = NIL OR b = NIL THEN
      (* this case is the probing an array case *)
      RETURN NIL
    END;
    RETURN Cat(a,b & F("[%s]",Int(i)))
  END CatArray;

PROCEDURE Debug(reg : T; at : CompRange.T) =
  BEGIN
    IF reg # NIL THEN
      M3Debug.Out(F("%s @ 16_%s = %s", ToText(reg), Int(
                                          CompAddr.DeltaBytes(at.pos,
                                                              CompAddr.Zero),
                                          base := 16),
                  CompRange.Format(at)))
                  
    END
  END Debug;

PROCEDURE ToText(t : T) : TEXT =
  VAR
    p := t;
    txt := "";
  BEGIN
    WHILE p # NIL DO
      txt := p.head & txt;
      p := p.tail
    END;
    RETURN txt
  END ToText;

PROCEDURE One(txt : TEXT) : T = BEGIN RETURN TextList.List1(txt) END One;
  
BEGIN END CompPath.
