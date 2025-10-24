MODULE PicArray;
IMPORT PicSeq;
IMPORT PicSeqSeq;
IMPORT Pic;
IMPORT PicExtent;
IMPORT PicPoint;
IMPORT Canvas;
IMPORT Debug;

CONST doDebug = TRUE;

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
    RETURN ComputeMinExtent(t, xtent^, ytent^)
  END MinExtent;
  
PROCEDURE ComputeMinExtent(t : T;
                           VAR xtent, ytent : ARRAY OF LONGREAL) : PicExtent.T =
  BEGIN
    FOR i := FIRST(xtent) TO LAST(xtent) DO
      xtent[i] := 0.0d0
    END;
    FOR i := FIRST(ytent) TO LAST(ytent) DO
      ytent[i] := 0.0d0
    END;
    
    FOR x := 0 TO t.nx - 1 DO
      FOR y := 0 TO t.ny - 1 DO
        WITH pic = t.get(x, y) DO
          IF pic # NIL THEN
            WITH e = pic.minExtent() DO
              IF doDebug THEN
                Debug.Out("PicArray.ComputeMinExtent: e=" & PicExtent.Format(e))
              END;
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
      FOR i := FIRST(xtent) TO LAST(xtent) DO
        res.ur.x := res.ur.x + xtent[i] 
      END;
      FOR i := FIRST(ytent) TO LAST(ytent) DO
        res.ur.y := res.ur.y + ytent[i]
      END;
      RETURN res
    END
  END ComputeMinExtent;

PROCEDURE Render(t : T; READONLY at : PicPoint.T; canvas : Canvas.T) =
  VAR
    xtent     := NEW(REF ARRAY OF LONGREAL, t.nx);
    ytent     := NEW(REF ARRAY OF LONGREAL, t.ny);
    minExtent := ComputeMinExtent(t, xtent^, ytent^);
    curExtent := t.curExtent();
    xtrExtent := PicPoint.Minus(minExtent.ur, curExtent.ur);
    nxf       := FLOAT(t.nx, LONGREAL);
    nyf       := FLOAT(t.ny, LONGREAL);
    offset    := PicPoint.Times(0.5d0, PicPoint.T { xtrExtent.x / nxf,
                                                    xtrExtent.y / nyf });
    ll        := at;
  BEGIN
    FOR i := FIRST(xtent^) TO LAST(xtent^) DO
      ll.x := ll.x + offset.x;
      ll.y := at.y;
      FOR j := FIRST(ytent^) TO LAST(ytent^) DO
        ll.y := ll.y + offset.y;
        WITH cell = t.get(i, j) DO
          IF cell # NIL THEN
            cell.render(ll, canvas)
          END
        END;
        ll.y := ll.y + offset.y;
        ll.y := ll.y + ytent[j]
      END;
      ll.x := ll.x + offset.x;
      ll.x := ll.x + xtent[i]
    END
  END Render;
  
BEGIN END PicArray.
