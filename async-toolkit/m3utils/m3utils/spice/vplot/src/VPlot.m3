MODULE VPlot EXPORTS Main;
IMPORT TextSeq;
IMPORT Pathname;
IMPORT Params;
IMPORT Rd;
IMPORT Text;
IMPORT FileRd;
IMPORT Voronoi;
IMPORT FS;
IMPORT TextReader;
IMPORT Debug;
FROM Fmt IMPORT Int, F, Real;
IMPORT RefList;
IMPORT VSeq, VSeqRefTbl;
IMPORT Scan;
IMPORT Triangle;
IMPORT TriangleSet, TriangleSetDef;
IMPORT TriangleSeq;
IMPORT Wr, FileWr;

(* read schmoozer results and make boundary plots *)

CONST TE = Text.Equal;

VAR
  params := NEW(TextSeq.T).init();

CONST 
  PFNames = ARRAY PassFail OF TEXT { "PASS", "FAIL" };
  PFN     = ARRAY PassFail OF TEXT { "P"   , "F"    };

TYPE
  PassFail = { Pass, Fail };
  
  Rec = OBJECT
    vals   : TextSeq.T;
    result : PassFail;
  END;

  Qrec = OBJECT
    x, y   : REAL;
    result : PassFail;
  END;

PROCEDURE Translate(rec : Rec) : Qrec =
  BEGIN
    RETURN NEW(Qrec, 
               x := Scan.Real(rec.vals.get(0)),
               y := Scan.Real(rec.vals.get(1)),
               result := rec.result)
  END Translate;

VAR 
  recs : RefList.T := NIL;
  tbl := NEW(VSeqRefTbl.Default).init();

PROCEDURE PutAt(s : TextSeq.T; at : CARDINAL; x : TEXT) =
  BEGIN
    WHILE at > s.size()-1 DO s.addhi(NIL) END;
    s.put(at, x)
  END PutAt;

VAR ignored := 0;

PROCEDURE ParseFile(pn : Pathname.T) =

  PROCEDURE StoreVal(key, val : TEXT) =
    BEGIN
      FOR i := 0 TO params.size()-1 DO
        IF TE(params.get(i), key) THEN
          PutAt(rec.vals, i, val); RETURN
        END
      END;
      params.addhi(key);
      PutAt(rec.vals, params.size()-1, val)
    END StoreVal;

  VAR 
    rd : Rd.T;
    rec := NEW(Rec, vals := NEW(TextSeq.T).init());
    w : TEXT;
    success := FALSE;
  BEGIN
    rd := FileRd.Open(pn);
    TRY
      LOOP
        WITH line   = Rd.GetLine(rd),
             reader = NEW(TextReader.T).init(line) DO
          w := reader.nextE(" ");
          IF TE(w,"PassFail") THEN
            VAR v := reader.nextE(" ");
            BEGIN
              FOR i := FIRST(PFNames) TO LAST(PFNames) DO
                IF TE(v, PFNames[i]) THEN
                  rec.result := i; success := TRUE; EXIT
                END
              END
            END
          ELSE
            REPEAT
              WITH v = reader.nextE(" ") DO
                StoreVal(w, v)
              END
            UNTIL NOT reader.next(" ", w)
          END
        END
      END
    EXCEPT
      TextReader.NoMore => Debug.Error("TextReader.NoMore in " & pn)
    |
      Rd.EndOfFile => (* skip *)
    END;
    Rd.Close(rd);

    IF success THEN
      Debug.Out(F("Parsed %s : %s", 
                  FmtVSeq(rec.vals, " "), 
                  PFNames[rec.result]));
      recs := RefList.Cons(rec, recs)
    ELSE
      INC(ignored)
    END
  END ParseFile;

PROCEDURE Partition() =
  VAR 
    p := recs;
    q : REFANY;
  BEGIN
    WHILE p # NIL DO
      WITH rec = NARROW(p.head, Rec),
           sub = VSeq.Sub(rec.vals, 2) DO
        q := NIL;
        EVAL tbl.get(sub, q);
        q := RefList.Cons(Translate(rec), q);
        EVAL tbl.put(sub, q)
      END;
      p := p.tail
    END
  END Partition;

PROCEDURE ProcessPartitions() =
  VAR
    iter := tbl.iterate();
    s : VSeq.T;
    r : REFANY;
  BEGIN
    WHILE iter.next(s, r) DO ProcessPartition(s,r) END
  END ProcessPartitions;


PROCEDURE FmtVSeq(s : VSeq.T; delim : TEXT) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := 0 TO s.size()-1 DO
      res := res & s.get(i);
      IF i # s.size()-1 THEN res := res & delim END
    END;
    RETURN res
  END FmtVSeq;

PROCEDURE ProcessPartition(s : VSeq.T; r : RefList.T) =
  VAR 
    arr := NEW(REF ARRAY OF Qrec, RefList.Length(r));
    i : CARDINAL := 0;
    tris := NEW(TriangleSetDef.T).init();
    tc, mc := 0;
    cptr : Voronoi.CTriple;
    doutWr := FileWr.Open(FmtVSeq(s, "_") & ".ddat");
    sumx, sumy := 0.0;
  BEGIN
    Debug.Out("Processing partition " & FmtVSeq(s, " "));

    WHILE r # NIL DO
      arr[i] := r.head;
      r := r.tail;
      INC(i)
    END;

    FOR i := FIRST(arr^) TO LAST(arr^) DO
      sumx := sumx + arr[i].x;
      sumy := sumy + arr[i].y
    END;

    LOCK Voronoi.mu DO
      Voronoi.Init();

      FOR i := FIRST(arr^) TO LAST(arr^) DO
        Voronoi.AddSite2(FLOAT(arr[i].x/sumx,LONGREAL), FLOAT(arr[i].y/sumy,LONGREAL))
      END;

      Debug.Out(F("ProcessPartition: calling Voronoi with %s points",
                  Int(NUMBER(arr^))));

      Voronoi.Setup();
      
      Voronoi.Delaunay();

      LOOP 
        cptr := Voronoi.Next();
        IF Voronoi.IsNull(cptr) THEN EXIT END;

        VAR 
          tri := NEW(Triangle.T);
        BEGIN
          FOR i := FIRST(tri.s) TO LAST(tri.s) DO
            tri.s[i] := Voronoi.GetIdx(cptr,i)
          END;
          INC(tc);
          DumpTri(tri, arr^, doutWr);
          IF CheckTri(tris, arr^, tri) THEN INC(mc) END
        END
      END;

      Wr.Close(doutWr);

      Voronoi.Finish();

      Debug.Out(F("Voronoi done, %s triangles, %s match pattern",
                  Int(tc), Int(mc)));

      VAR
        tlookuptbl := NEW(REF ARRAY OF RefList.T, NUMBER(arr^));
      BEGIN
        FOR i := FIRST(tlookuptbl^) TO LAST(tlookuptbl^) DO
          tlookuptbl[i] := NIL
        END;
        Tabulate(tris,tlookuptbl);
        MarkNeighbors(tris,tlookuptbl,arr^);
      END
    END;

    DumpCentroids(tris, arr^);

    WITH wfn = FmtVSeq(s, "_") & ".dat",
         wr  = FileWr.Open(wfn) DO
      DumpMiddles(tris, arr^, wr);
      Wr.Close(wr)
    END;

    WITH wfn = FmtVSeq(s, "_") & ".tdat",
         wr  = FileWr.Open(wfn) DO
      DumpTris(tris, arr^, wr);
      Wr.Close(wr)
    END;

  END ProcessPartition;

PROCEDURE DumpMiddles(tris         : TriangleSet.T; 
                      READONLY arr : ARRAY OF Qrec;
                      wr           : Wr.T) =
  VAR 
    tr : Triangle.T;
    iter := tris.iterate();
  BEGIN
    WHILE iter.next(tr) DO
      FOR i := 0 TO 2 DO
        WITH j    = (i+1) MOD 3,
             diff =  arr[tr.s[i]].result # arr[tr.s[j]].result,
             midx =  (arr[tr.s[i]].x + arr[tr.s[j]].x)/2.0,
             midy =  (arr[tr.s[i]].y + arr[tr.s[j]].y)/2.0 DO
          IF diff THEN
            Wr.PutText(wr, F("%s %s\n", Real(midx), Real(midy)))
          END
        END
      END
    END
  END DumpMiddles;

PROCEDURE DumpTri(tr : Triangle.T; 
                  READONLY arr : ARRAY OF Qrec;
                  wr           : Wr.T) =
  PROCEDURE P(i : CARDINAL) =
    BEGIN
      Wr.PutText(wr, F("%s %s\n", Real(arr[tr.s[i]].x), Real(arr[tr.s[i]].y)))
    END P;

  BEGIN
    P(0); P(1); P(2); P(0);
    Wr.PutChar(wr, '\n')
  END DumpTri;

PROCEDURE DumpTris   (tris         : TriangleSet.T; 
                      READONLY arr : ARRAY OF Qrec;
                      wr           : Wr.T) =


  VAR 
    tr : Triangle.T;
    iter := tris.iterate();
  BEGIN
    WHILE iter.next(tr) DO
      DumpTri(tr, arr, wr)
    END
  END DumpTris;

PROCEDURE DumpCentroids(tris         : TriangleSet.T; 
                        READONLY arr : ARRAY OF Qrec) =
  VAR 
    tr : Triangle.T;
  BEGIN
    IF tris.size() = 0 THEN RETURN END;

    WITH iter = tris.iterate() DO EVAL iter.next(tr) END;

    tr := FindStart(tr);

    DrawFrom(tr, arr)
  END DumpCentroids;

PROCEDURE DrawFrom(tr : Triangle.T; READONLY arr : ARRAY OF Qrec) =

  PROCEDURE Centroid(tr : Triangle.T) : ARRAY [0..1] OF REAL =
    VAR
      cx, cy := 0.0;
    BEGIN
      FOR i := FIRST(tr.s) TO LAST(tr.s) DO
        WITH p = arr[tr.s[i]] DO
          cx := cx + p.x;
          cy := cy + p.y
        END
      END;
      RETURN ARRAY [0..1] OF REAL { cx/3.0 , cy/3.0 }
    END Centroid;

  VAR
    visited := NEW(TriangleSetDef.T).init();
    found : BOOLEAN;
  BEGIN
    REPEAT
      found := FALSE;
      FOR i := 0 TO 2 DO 
        WITH n = tr.neighbors[i] DO
          IF n # NIL AND NOT visited.member(n) THEN
            tr := n; EVAL visited.insert(n); found := TRUE;

            WITH centroid = Centroid(tr) DO
              Debug.Out(F("%s %s", Real(centroid[0]), Real(centroid[1])))
            END
          END
        END
      END
    UNTIL NOT found
  END DrawFrom;

PROCEDURE FindStart(tr : Triangle.T) : Triangle.T =
  VAR
    visited := NEW(TriangleSetDef.T).init();
    tovisit := NEW(TriangleSeq.T).init();
    
  PROCEDURE Visit(tr : Triangle.T) : BOOLEAN =
    VAR 
      found := FALSE;
    BEGIN
      EVAL visited.insert(tr);
      FOR i := 0 TO 2 DO 
        WITH n = tr.neighbors[i] DO
          IF n # NIL AND NOT visited.member(n) THEN
            tovisit.addhi(n); found := TRUE
          END
        END
      END;
      RETURN found
    END Visit;
    
  VAR 
    p := tr;
  BEGIN
    tovisit.addhi(tr);
    LOOP
      p := tovisit.remlo();
      IF NOT Visit(p) THEN RETURN p END
    END
  END FindStart;

PROCEDURE CheckTri(tris       : TriangleSet.T;
                   READONLY a : ARRAY OF Qrec; 
                   tri        : Triangle.T) : BOOLEAN =
  BEGIN
(*
    Debug.Out(F("vals %s %s %s", 
                PFNames[a[tri.s[0]].result],
                PFNames[a[tri.s[1]].result],
                PFNames[a[tri.s[2]].result]));
*)

    IF a[tri.s[0]].result # a[tri.s[1]].result OR 
       a[tri.s[1]].result # a[tri.s[2]].result OR
       a[tri.s[2]].result # a[tri.s[0]].result  THEN
      (* hot triangle *)
      Debug.Out(F("CheckTri %s", FmtTri(tri, a)));

      EVAL tris.insert(tri); 
      RETURN TRUE
    END;
    RETURN FALSE
  END CheckTri;

PROCEDURE Tabulate(tris : TriangleSet.T; VAR tbl : REF ARRAY OF RefList.T) =

  PROCEDURE AddTo(VAR q : RefList.T) = BEGIN q := RefList.Cons(tr, q) END AddTo;

  VAR
    tr : Triangle.T;
  BEGIN
    WITH iter = tris.iterate() DO
      WHILE iter.next(tr) DO
        AddTo(tbl[tr.s[0]]); AddTo(tbl[tr.s[1]]); AddTo(tbl[tr.s[2]])
      END
    END
  END Tabulate;

PROCEDURE FP(id : CARDINAL; READONLY arr : ARRAY OF Qrec) : TEXT =
  BEGIN RETURN F("%s(%s)", Int(id), PFN[arr[id].result]) END FP;

PROCEDURE FmtTri(tri : Triangle.T; READONLY arr : ARRAY OF Qrec) : TEXT =
  BEGIN
    RETURN F("{ %s , %s , %s }", 
                  FP(tri.s[0], arr), 
                  FP(tri.s[1], arr), 
                  FP(tri.s[2], arr))
  END FmtTri;

PROCEDURE MarkNeighbors(tris         : TriangleSet.T; 
                        READONLY tbl : REF ARRAY OF RefList.T; 
                        READONLY arr : ARRAY OF Qrec) =

  PROCEDURE DoOne(tr : Triangle.T; start : CARDINAL) =
    VAR
      nxt := (start + 1) MOD 3;
      p := tbl[tr.s[start]];
    BEGIN
      WHILE p # NIL DO
        IF p.head # tr THEN
          (* see if it is also for the nxt *)
          IF RefList.Member(tbl[tr.s[nxt]], p.head) THEN
            Debug.Out(F("MarkNeighbors %s (%s) %s", 
                      FmtTri(tr,arr), Int(start), FmtTri(p.head,arr)));
            tr.neighbors[start] := p.head
          END
        END;
        p := p.tail
      END
    END DoOne;

  VAR
    tr : Triangle.T;
  BEGIN
    WITH iter = tris.iterate() DO
      WHILE iter.next(tr) DO
        FOR i := 0 TO 2 DO
          DoOne(tr, i)
        END
      END
    END
  END MarkNeighbors;

VAR
  dirNm : Pathname.T := Params.Get(1);
  fn : Pathname.T;
BEGIN
  params.addhi(Params.Get(2));
  params.addhi(Params.Get(3));
  
  WITH iter = FS.Iterate(dirNm) DO
    WHILE iter.next(fn) DO
      ParseFile(dirNm & "/" & fn)
    END
  END;

  Debug.Out(Int(RefList.Length(recs)) & " records, " & Int(ignored)&" ignored");
  Debug.Out(Int(params.size()) & " parameters:");
  FOR i := 0 TO params.size()-1 DO
    Debug.Out(params.get(i))
  END;

  Partition();
  Debug.Out(Int(tbl.size()) & " partitions");

  ProcessPartitions();

END VPlot.
