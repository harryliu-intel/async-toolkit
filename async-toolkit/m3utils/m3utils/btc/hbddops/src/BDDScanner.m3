(* $Id$ *)

MODULE BDDScanner;
IMPORT BDDBDDTbl, BDD;
IMPORT Debug;
IMPORT BDDDecompose, BDDDepender;
IMPORT RefList;
IMPORT BDDSet;
FROM BDDOpsH IMPORT XFormat;
IMPORT BDDCardTbl;
IMPORT BDDSetDef;

TYPE
  Rec = OBJECT
    v, z  : BDD.T;
    vars : BDDSet.T;
  END;

PROCEDURE Do(tbl : BDDBDDTbl.T) : T =

  PROCEDURE RecordStats(v, z : BDD.T; VAR lst : RefList.T) =
    VAR
      vars : BDDSet.T;
    BEGIN
      IF z = NIL THEN
        vars := NEW(BDDSetDef.T).init()
      ELSE
        vars := dep.depends(z);
      END;
      lst := RefList.Cons(NEW(Rec, vars := vars, z := z, v := v), lst)
    END RecordStats;

  VAR 
    v, x : BDD.T;
    dep := NEW(BDDDepender.T).init();
    dec := NEW(BDDDecompose.T).init(dep);
    before, decomp : RefList.T := NIL;
    res : T;
    r : BDDDecompose.Result;
  BEGIN
    Debug.Out("BDDScanner.Do start");
    WITH iter = tbl.iterate() DO
      WHILE iter.next(v, x) DO
        RecordStats(v, x, before);

        IF x # NIL THEN
          r := dec.attempt(x, XFormat(v) & "_DEC_");
          WHILE r # NIL DO
            VAR w : BDD.T; BEGIN
              IF r.v = NIL THEN w := v ELSE w := r.v END;
              RecordStats(w, r.x, decomp);
              r := r.next
            END
          END
        END
      END
    END;
    Debug.Out("BDDScanner.Do done scanning");
    ProcessStats(before, res.pre);
    ProcessStats(decomp, res.dec);
    RETURN res
  END Do;

PROCEDURE ProcessStats(p : RefList.T; VAR stats : Stats) =

  PROCEDURE IncCounts(s : BDDSet.T) =
    VAR
      iter := s.iterate();
      b : BDD.T;
      old, new : CARDINAL;
    BEGIN
      WHILE iter.next(b) DO
        IF NOT cnts.get(b,old) THEN new := 1 ELSE new := old + 1 END;
        
        IF new > maxFanout THEN maxFanout := new; maxFanoutLit := b END;

        EVAL cnts.put(b, new)
      END
    END IncCounts;

  VAR
    n := 0;
    s := 0;
    min := LAST(CARDINAL);
    max := FIRST(CARDINAL);
    cnts := NEW(BDDCardTbl.Default).init();
    maxFanout := 0;
    maxFanoutLit : BDD.T := NIL;
  BEGIN
    WHILE p # NIL DO
      WITH r  = NARROW(p.head, Rec),
           sz = r.vars.size() DO
        IncCounts(r.vars);
        INC(n);
        INC(s, sz);
        min := MIN(min, sz);
        IF sz > max THEN stats.biggest := r.z; stats.biggestLit := r.v END;
        max := MAX(max, sz);
      END;
      p := p.tail
    END;

    stats.nBdds := n;
    stats.meanFanins := FLOAT(s,LONGREAL)/FLOAT(n,LONGREAL);
    stats.minFanins := min;
    stats.maxFanins := max;
    stats.cnts := cnts;
    stats.maxFanout := maxFanout;
    stats.maxFanoutLit := maxFanoutLit;
    
  END ProcessStats;

BEGIN END BDDScanner.
