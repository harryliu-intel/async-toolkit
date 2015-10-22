MODULE Main;

IMPORT SpiceFormat;
IMPORT FileRd;
IMPORT Params;
IMPORT SpiceCircuit;
IMPORT Debug; FROM Debug IMPORT UnNil;
IMPORT Wr, FileWr;
IMPORT Stdio;
IMPORT SpiceObject;
FROM Fmt IMPORT Int, F;
IMPORT Thread;
IMPORT TextCardTbl, TextRefTbl;
IMPORT TextSetDef, TextSet;
IMPORT TextTextSetTbl;
IMPORT TextSeq;
IMPORT TextCard;
IMPORT TextCardArraySort;

<*FATAL Thread.Alerted*>

TYPE OType = { Null, R, C, M, X, Unknown };
CONST ONames = ARRAY OType OF TEXT { "NULL", "R", "C", "M", "X", "UNKNOWN" };

PROCEDURE Visit(nm    : TEXT; 
                wr    : Wr.T; 
                ckt   : SpiceCircuit.T; 
                level : CARDINAL := 0) =
  VAR 
    cnt := ARRAY OType OF CARDINAL { 0, .. };
    subs := NEW(TextSetDef.T).init();
  BEGIN
    FOR i := 1 TO level DO
      Wr.PutChar(wr, '-')
    END;
    Wr.PutChar(wr, ' ');
    Wr.PutText(wr, nm);
    Wr.PutChar(wr, ' ');
    Wr.PutText(wr, UnNil(ckt.name));

    WITH elems = ckt.elements DO
      (* count elements *)
      FOR i := 0 TO elems.size()-1 DO
        WITH obj = elems.get(i) DO
          TYPECASE obj OF
            NULL             => INC(cnt[OType.Null])
          |
            SpiceObject.R    => INC(cnt[OType.R])
          |
            SpiceObject.C    => INC(cnt[OType.C])
          |
            SpiceObject.M    => INC(cnt[OType.M])
          |
            SpiceObject.X    => INC(cnt[OType.X])
          ELSE
                                INC(cnt[OType.Unknown])
          END
        END
      END;
      Wr.PutChar(wr, ' ');
      FOR i := FIRST(OType) TO LAST(OType) DO
        Wr.PutText(wr, ONames[i]);
        Wr.PutChar(wr, ':');
        Wr.PutText(wr, Int(cnt[i]));
        Wr.PutChar(wr, ' ');
      END;
      Wr.PutChar(wr, '\n');
      Wr.Flush(wr);

      FOR i := 0 TO elems.size()-1 DO
        WITH obj = elems.get(i) DO
          TYPECASE obj OF
            NULL => (* skip *)
          |
            SpiceObject.X(x)    => 
            VAR
              ckt : SpiceCircuit.T;
              hadIt := spice.subCkts.get(x.type, ckt);
              seenIt : BOOLEAN;
            BEGIN
              seenIt := subs.insert(x.type);
              Visit(nm & "." & x.name, wr, ckt, level+1)
            END
          ELSE
            (* skip *)
          END
        END
      END
    END
  END Visit;

PROCEDURE DumpOneType(wr : Wr.T; 
                      tn : TEXT; 
                      ckt : SpiceCircuit.T;
                      tbl : TextRefTbl.T;
                      pTbl : TextTextSetTbl.T) =
  VAR
    elems := ckt.elements;
    cntTbl := NEW(TextCardTbl.Default).init();
  BEGIN

    Wr.PutText(wr, tn & " ======>\n");
    FOR i := 0 TO elems.size()-1 DO
      WITH obj = elems.get(i) DO
        TYPECASE obj OF
          NULL => (* skip *)
        |
          SpiceObject.X(x)    => 
          Wr.PutText(wr, F(" %-70s %s\n", x.type, x.name));
          VAR 
            cnt : CARDINAL := 0;
            s : TextSet.T;
          BEGIN
            EVAL cntTbl.get(x.type, cnt);
            INC(cnt);
            EVAL cntTbl.put(x.type, cnt);

            (* make parent linkage *)
            IF NOT pTbl.get(x.type, s) THEN
              s := NEW(TextSetDef.T).init();
              EVAL pTbl.put(x.type,s)
            END;
            EVAL s.insert(tn)
          END
        ELSE
          (* skip *)
        END
      END
    END;

    Wr.PutText(wr, "\n");

    EVAL tbl.put(tn, cntTbl);

  END DumpOneType;


PROCEDURE DumpBrief(wr         : Wr.T; 
                    x          : TextCard.T; 
                    parentTbl  : TextTextSetTbl.T;
                    typeCntTbl : TextRefTbl.T;
                    heightTbl  : TextCardTbl.T) =
  (* print instantiation DAG node in manner of gprof *)
  VAR
    ps : TextSet.T;
    r : REFANY;
    ct, pct : TextCardTbl.T;
    p, c : TEXT;
    cnt : CARDINAL;
  BEGIN
    WITH hadIt = parentTbl.get(x.t, ps) DO 
      IF NOT hadIt THEN
        (* root type *)
        ps := NEW(TextSetDef.T).init();
      END
    END;
    WITH hadIt = typeCntTbl.get(x.t, r) DO <*ASSERT hadIt*> ct := r END;

    WITH iter = ps.iterate() DO
      WHILE iter.next(p) DO
        WITH hadIt = typeCntTbl.get(p, r) DO <*ASSERT hadIt*> pct := r END;
        WITH hadIt = pct.get(x.t, cnt) DO <*ASSERT hadIt*> END;
        Wr.PutText(wr, F("  %-70s (%s) >>\n", p, Int(cnt)));
      END
    END;
    Wr.PutText(wr,     F("%-70s   HEIGHT %s\n", x.t, Int(x.c)));
    WITH iter = ct.iterate() DO
      WHILE iter.next(c, cnt) DO
        VAR h : CARDINAL;
            hadIt := heightTbl.get(c,h);
            leaf := "";
        BEGIN 
          <*ASSERT hadIt*> 
          IF h=0 THEN leaf := "LEAF " ELSE leaf := F("h=%s ",Int(h)) END;
          Wr.PutText(wr, F("  %-70s (%s) %s<<\n", c, Int(cnt), leaf));
        END
      END
    END;
    Wr.PutChar(wr, '\n');
  END DumpBrief;

PROCEDURE DumpGprofFormat(wr : Wr.T;
                          typeCntTbl : TextRefTbl.T;
                          parentTbl : TextTextSetTbl.T) =
  BEGIN
    (* at this point typeCntTbl contains a table for every type; 
       each such table contains a count of how many instances of each
       child type there are in the parent type *)

    (* find types without children *)
    VAR
      heightTbl := NEW(TextCardTbl.Default).init();
      sofar := NEW(TextSeq.T).init();
      jter := typeCntTbl.iterate();
      tn : TEXT;
      r : REFANY;
      pSet : TextSet.T;
      height, cHeight  : CARDINAL;
    BEGIN
      (* initialize tables *)
      WHILE jter.next(tn, r) DO
        WITH des = NARROW(r, TextCardTbl.T) DO
          IF des.size()=0 THEN 
            EVAL heightTbl.put(tn,0);
            sofar.addhi(tn)
          ELSE
            EVAL heightTbl.put(tn, 0)
          END
        END
      END;

      WHILE sofar.size() # 0 DO
        WITH next = sofar.remlo(),
             hadItP = parentTbl.get(next, pSet),
             hadItH = heightTbl.get(next, cHeight) DO
          <*ASSERT hadItH*>
          IF hadItP THEN
            (* not root *)
            WITH kter = pSet.iterate() DO

              WHILE kter.next(tn) DO
                WITH hadIt = heightTbl.get(tn, height) DO
                  <*ASSERT hadIt*>
                  height := MAX(height, cHeight + 1);
                  EVAL heightTbl.put(tn, height);
                  sofar.addhi(tn)
                END
              END
            END
          END
        END
      END;

      (* heighttbl is now complete and correct *)

      VAR
        lter := heightTbl.iterate();
        a    := NEW(REF ARRAY OF TextCard.T, heightTbl.size());
        i    := 0;
        lastH := LAST(CARDINAL);
      BEGIN
        WHILE lter.next(tn, height) DO
          a[i] := TextCard.T { tn, height };
          INC(i)
        END;
        <*ASSERT i = LAST(a^)+1*>
        TextCardArraySort.Sort(a^);

        (* print out "call structure" *)

        FOR k := LAST(a^) TO FIRST(a^) BY -1 DO
          IF a[k].c # lastH THEN
            FOR l := 1 TO 70 DO Wr.PutChar(wr, '=') END;
            Wr.PutChar(wr, '\n');
            Wr.PutChar(wr, '\n');
            lastH := a[k].c
          END;
            
          DumpBrief(wr, a[k], parentTbl, typeCntTbl, heightTbl)
        END
      
      END
    END
  END DumpGprofFormat;

PROCEDURE DumpBriefFlat(wr         : Wr.T; 
                        top        : TEXT; 
                        typeCntTbl : TextRefTbl.T;
                        cnt        : CARDINAL := 1;
                        level      := 0) =
  VAR
    r : REFANY;
    ct : TextCardTbl.T;
    c : TEXT;
    ccnt : CARDINAL;
  BEGIN
    FOR i := 1 TO level DO Wr.PutText(wr, "    ") END;
    Wr.PutText(wr, F("%s  (%s)\n", top, Int(cnt)));
    WITH hadIt = typeCntTbl.get(top, r) DO <*ASSERT hadIt*> ct := r END;
    WITH iter = ct.iterate() DO
      WHILE iter.next(c, ccnt) DO
        DumpBriefFlat(wr, c, typeCntTbl, ccnt, level+1)
      END
    END
  END DumpBriefFlat;

VAR
  rd := FileRd.Open(Params.Get(1));
  top := Params.Get(2);

  topCkt : SpiceCircuit.T;
  spice := SpiceFormat.ParseSpice(rd);
BEGIN
  IF NOT spice.subCkts.get(top, topCkt) THEN
    Debug.Error("Can't find subcircuit def'n \"" & top & "\"")
  END;
  
  WITH wr = FileWr.Open("flat.out") DO
    Visit("TOP", wr, topCkt);
    Wr.Close(wr)
  END;

  VAR 
    wr   := FileWr.Open("hier.out");
    iter := spice.subCkts.iterate();
    type : TEXT;
    ckt : SpiceCircuit.T;
    typeCntTbl := NEW(TextRefTbl.Default).init();
    parentTbl := NEW(TextTextSetTbl.Default).init();
    gwr := FileWr.Open("gprof.out");
    bwr := FileWr.Open("bflat.out");
  BEGIN
    WHILE iter.next(type, ckt) DO
      DumpOneType(wr, type, ckt, typeCntTbl, parentTbl)
    END;
    Wr.Close(wr);

    DumpGprofFormat(gwr, typeCntTbl, parentTbl);
    Wr.Close(gwr);

    DumpBriefFlat(bwr, top, typeCntTbl);
    Wr.Close(bwr)
  END;

END Main.
