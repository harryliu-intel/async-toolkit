(* $Id$ *)

MODULE Main;
IMPORT Voronoi;
IMPORT Random;
IMPORT Debug, Fmt;
IMPORT SyphPoint, SyphPointMST;

CONST
  Pts = 100;
  min = 0.0;
  max = 1000.0;
VAR
  array := NEW(UNTRACED REF ARRAY OF Voronoi.Point, Pts);
  rand := NEW(CITRandom.T).init();
  tri : Voronoi.Triple;

PROCEDURE PickSeq(elimDups : BOOLEAN) =
  BEGIN
  END PickSeq;

BEGIN
  LOOP
    VAR
      netseq := PickSeq();
      mst := NEW(SyphPointMST.T).init(netseq);
      len: LONGREAL;
      from, to : SyphPoint.T;
      tot := 0.0d0;
    BEGIN
      FOR i := 0 TO mst.size() - 1 DO
        mst.getLink(i,from,to,len);
        tot := tot + len;
        
        (* update conTbl if non-NIL *)
        IF conTbl # NIL THEN
          ConAddLink(from,to);
          ConAddLink(to,from)
        END
      END;
      sum := sum + FLOAT(tot)
    END
  ELSIF estMethod = EstMethod.DelaunayMST THEN
    
    (* Delaunay triangulation first, then MST *)
    
    VAR
      netseq := NodeSettoSeq(net, elimDups := TRUE);
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

      (*          Debug.Out("Calling Voronoi.Delaunay package..."); *)
      Voronoi.Delaunay();
      WHILE Voronoi.NextTriple(t) DO
        (*
          Debug.Out("tri " & Fmt.Int(t.s1) & " " & 
          Fmt.Int(t.s2) & " " & 
          Fmt.Int(t.s3));
        *)
        
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
        tot := tot + len;
        
        (* update conTbl if non-NIL *)
        IF conTbl # NIL THEN
          ConAddLink(from,to);
          ConAddLink(to,from)
        END
      END; 
      sum := sum + FLOAT(tot)
    END
  END

END Main.
