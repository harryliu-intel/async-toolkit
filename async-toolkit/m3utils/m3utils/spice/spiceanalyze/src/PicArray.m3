MODULE PicArray;
IMPORT PicSeq;
IMPORT PicSeqSeq;
IMPORT Pic;
IMPORT PicExtent;
IMPORT PicPoint;
IMPORT Canvas;

REVEAL
  T = Public BRANDED Brand OBJECT
    nx, ny : CARDINAL;
    rep : PicSeqSeq.T;
  METHODS
    get(x, y : CARDINAL) : Pic.T := Get;
  OVERRIDES
    init := Init;
    put  := Put;
    minExtent := MinExtent;
    render := Render;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.nx := 0;
    t.ny := 0;
    t.rep := NEW(PicSeqSeq.T).init();
    RETURN t
  END Init;

PROCEDURE Get(t : T; x, y : CARDINAL) : Pic.T =
  BEGIN
    IF y > t.rep.size() - 1 THEN
      RETURN NIL
    ELSE
      WITH row = t.rep.get(y) DO
        IF x > row.size() - 1 THEN
          RETURN NIL
        ELSE
          RETURN row.get(x)
        END
      END
    END
  END Get;

PROCEDURE Put(t : T; x, y : CARDINAL; pic : Pic.T) =
  BEGIN
    t.nx := MAX(t.nx, x + 1);
    t.ny := MAX(t.ny, y + 1);
    WHILE y >= t.rep.size() DO
      t.rep.addhi(NEW(PicSeq.T).init())
    END;
    WITH row = t.rep.get(y) DO
      WHILE x >= row.size() DO
        row.addhi(NIL)
      END
    END;
    t.rep.get(y).put(x, pic)
  END Put;

PROCEDURE MinExtent(t : T) : PicExtent.T =
  VAR
    xtent := NEW(REF ARRAY OF LONGREAL, t.nx);
    ytent := NEW(REF ARRAY OF LONGREAL, t.ny);
  BEGIN
    FOR i := FIRST(xtent^) TO LAST(xtent^) DO
      xtent[i] := 0.0d0
    END;
    FOR i := FIRST(ytent^) TO LAST(ytent^) DO
      ytent[i] := 0.0d0
    END;
    
    FOR x := 0 TO t.nx - 1 DO
      FOR y := 0 TO t.ny - 1 DO
        WITH pic = t.get(x, y) DO
          IF pic # NIL THEN
            WITH e = pic.minExtent() DO
              xtent[x] := MAX(xtent[x], e.ur.x - e.ll.x);
              ytent[y] := MAX(ytent[y], e.ur.y - e.ll.y)
            END
          END
        END
      END
    END;

    VAR
      res := PicExtent.Zero;
    BEGIN
      FOR i := FIRST(xtent^) TO LAST(xtent^) DO
        res.ll.x := res.ll.x + xtent[i] 
      END;
      FOR i := FIRST(ytent^) TO LAST(ytent^) DO
        res.ll.y := res.ll.y + ytent[i]
      END;
      RETURN res
    END
  END MinExtent;

PROCEDURE Render(t : T; READONLY at : PicPoint.T; canvas : Canvas.T) =
  BEGIN
    (* unfinished *)
  END Render;
  
BEGIN END PicArray.
