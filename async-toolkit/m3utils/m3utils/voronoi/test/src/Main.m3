(* $Id$ *)

MODULE Main;
IMPORT Voronoi;
IMPORT Debug, Fmt, Word, Tick;
IMPORT SyphPoint, SyphPointSeq, SyphPointMST;
IMPORT CardPair, CardPairSet, CardPairSetDef;
IMPORT CITRandom;
IMPORT Wr, FileWr;
IMPORT Stdio;

CONST
  MaxPts = 100;
  min = 0;
  max = 1000;
  Iters = 100;
VAR
  rand := NEW(CITRandom.T).init(FALSE);

PROCEDURE PickSeq(pts : CARDINAL; elimDups : BOOLEAN) : SyphPointSeq.T =
  VAR
    seq := NEW(SyphPointSeq.T).init();
    added : CardPairSet.T;
    p : SyphPoint.T;
  BEGIN
    IF elimDups THEN
      added := NEW(CardPairSetDef.T).init();
    END;
    FOR i := 0 TO pts-1 DO
      p := SyphPoint.T { rand.integer(min,max),
                         rand.integer(min,max),
                         NIL };
      IF NOT elimDups OR NOT added.member(CardPair.T { p.x, p.y }) THEN 
         seq.addhi(p);
         IF elimDups THEN EVAL added.insert(CardPair.T { p.x, p.y }) END
      END
    END;
    RETURN seq
  END PickSeq;

PROCEDURE RunIters(pts : CARDINAL; iters : CARDINAL; testDelaunay : BOOLEAN) =
  VAR
    sum : REAL;
  BEGIN
    FOR i := 1 TO iters DO
      sum := 0.0;
      IF testDelaunay = FALSE THEN
        VAR
          netseq := PickSeq(pts, elimDups := FALSE);
          mst := NEW(SyphPointMST.T).init(netseq);
          len: LONGREAL;
          from, to : SyphPoint.T;
          tot := 0.0d0;
        BEGIN
          FOR i := 0 TO mst.size() - 1 DO
            mst.getLink(i,from,to,len);
            tot := tot + len
          END;
          sum := sum + FLOAT(tot)
        END
      ELSE
        
        (* Delaunay triangulation first, then MST *)
        
        VAR
          netseq := PickSeq(pts, elimDups := FALSE);
          mst : SyphPointMST.T;
          len: LONGREAL; 
          from, to : SyphPoint.T; 
          tot := 0.0d0; 
          links := NEW(CardPairSetDef.T).init();
          t : Voronoi.Triple;
        BEGIN 
          Voronoi.Init();
          FOR i := 0 TO netseq.size() - 1 DO
            WITH p = netseq.get(i) DO
              Voronoi.AddSite(Voronoi.Point{FLOAT(p.x), FLOAT(p.y)})
            END
          END;
          Voronoi.Setup();
          
          Voronoi.Delaunay();
          WHILE Voronoi.NextTriple(t) DO
            
            EVAL links.insert(CardPair.T { MIN(t.s1, t.s2), 
                                           MAX(t.s1, t.s2) });
            EVAL links.insert(CardPair.T { MIN(t.s2, t.s3), 
                                           MAX(t.s2, t.s3) });
            EVAL links.insert(CardPair.T { MIN(t.s3, t.s1), 
                                           MAX(t.s3, t.s1) })
          END;
          Voronoi.Finish();
          
          mst := NEW(SyphPointMST.T).init(netseq, links);
          FOR i := 0 TO mst.size() - 1 DO 
            mst.getLink(i,from,to,len); 
            tot := tot + len
          END; 
          sum := sum + FLOAT(tot)
        END
      END
    END
  END RunIters;


VAR
  now, time : Word.T;
  wrM := FileWr.Open("mst_delay.dat");
  wrD := FileWr.Open("delaunay_delay.dat");

BEGIN
(*
  LOOP
    RunIters(100, Iters, TRUE)
  END;
*)

  FOR pts := 4 TO MaxPts BY 2 DO
    Wr.PutText(Stdio.stdout, "\n" & Fmt.Int(pts) & " points...\n");

    now := Tick.Now();
    RunIters(pts, Iters, TRUE);
    time := Tick.Now()-now;
    Wr.PutText(wrD, Fmt.Int(pts) & " " & Fmt.LongReal(Tick.ToSeconds(time)) & "\n");
    Wr.Flush(wrD);
    Wr.PutText(Stdio.stdout, "Delaunay : " & Fmt.LongReal(Tick.ToSeconds(time)) & " seconds\n");
    Wr.Flush(Stdio.stdout);

    now := Tick.Now();
    RunIters(pts, Iters, FALSE);
    time := Tick.Now()-now;
    Wr.PutText(wrM, Fmt.Int(pts) & " " & Fmt.LongReal(Tick.ToSeconds(time)) & "\n");
    Wr.Flush(wrM);
    Wr.PutText(Stdio.stdout, "non-Delaunay : " & Fmt.LongReal(Tick.ToSeconds(time)) & " seconds\n");
    Wr.Flush(Stdio.stdout)
  END;
  
  Wr.Close(wrM);
  Wr.Close(wrD)
END Main.
