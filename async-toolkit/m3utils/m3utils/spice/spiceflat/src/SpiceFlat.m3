MODULE SpiceFlat;

(* spiceflat <spice-deck-filename> <top-cell> *)

IMPORT SpiceCircuit;
IMPORT Debug; FROM Debug IMPORT UnNil;
IMPORT Wr;
IMPORT SpiceObject;
FROM Fmt IMPORT Int, F;
IMPORT Thread;
IMPORT TextCardTbl, TextRefTbl;
IMPORT TextSetDef, TextSet;
IMPORT TextTextSetTbl;
IMPORT TextSeq;
IMPORT TextCard;
IMPORT TextCardArraySort;
IMPORT TextArraySort;
IMPORT TextUtils, Integer, Text;
IMPORT TextSpiceInstanceSetTbl;
IMPORT SpiceInstance, SpiceInstanceSetDef;
IMPORT SpiceInstanceSet;
IMPORT TextTextTbl;
IMPORT TextSpiceCircuitTbl;

<*FATAL Thread.Alerted*>

TYPE  OType =                      {  Null,   R,   C,   M,   X,   Unknown };
CONST ONames = ARRAY OType OF TEXT { "NULL", "R", "C", "M", "X", "UNKNOWN" };

PROCEDURE Visit(nm    : TEXT; 
                wr    : Wr.T; 
                ckt   : SpiceCircuit.T;
                subCkts : TextSpiceCircuitTbl.T;
                level : CARDINAL := 0)
  RAISES { Wr.Failure } =
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
              hadIt := subCkts.get(x.type, ckt);
              seenIt : BOOLEAN;
            BEGIN
              <*ASSERT hadIt*>
              seenIt := subs.insert(x.type);
              Visit(nm & "." & x.name, wr, ckt, subCkts, level+1)
            END
          ELSE
            (* skip *)
          END
        END
      END
    END
  END Visit;

PROCEDURE DumpOneType(wr   : Wr.T; 
                      tn   : TEXT; 
                      ckt  : SpiceCircuit.T;
                      tbl  : TextRefTbl.T;
                      pTbl : TextTextSetTbl.T)
  RAISES { Wr.Failure } =
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
                    heightTbl  : TextCardTbl.T)
  RAISES { Wr.Failure } =
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
                          parentTbl : TextTextSetTbl.T)
  RAISES { Wr.Failure } =
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
                        level      := 0)
  RAISES { Wr.Failure } =
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

  (**********************************************************************)

PROCEDURE AddAlias(symTab : TextTextSetTbl.T; fm, to : TEXT) =
  VAR
    fSet, tSet : TextSet.T;
    a : TEXT;
  BEGIN
    IF symTab.get(fm, fSet) AND symTab.get(to, tSet) THEN
      (* merge case, nasty *)
      WITH new = fSet.union(tSet),
           iter = new.iterate() DO
        WHILE iter.next(a) DO EVAL symTab.put(a, new) END
      END
    ELSIF symTab.get(fm, fSet) OR symTab.get(to, fSet) THEN
      EVAL fSet.insert(to);
      EVAL symTab.put(to, fSet);
      EVAL symTab.put(fm, fSet)
    ELSE
      (* no records at all, add *)
      WITH new = NEW(TextSetDef.T).init() DO
        EVAL new.insert(fm);
        EVAL new.insert(to);
        EVAL symTab.put(fm, new);
        EVAL symTab.put(to, new)
      END
    END
  END AddAlias;
  
PROCEDURE VisitCktNodes(pfx    : TEXT;
                        symTab : TextTextSetTbl.T;
                        ckt    : SpiceCircuit.T;
                        pNms   : TextSeq.T;
                        assocs : TextSpiceInstanceSetTbl.T;
                        me     : SpiceInstance.T;
                        subCkts : TextSpiceCircuitTbl.T) =
  VAR
    sckt : SpiceCircuit.T;
  BEGIN
    IF pNms # NIL THEN
      <*ASSERT pNms.size() = ckt.params.size()*>
      FOR i := 0 TO pNms.size()-1 DO
        AddAlias(symTab, pNms.get(i), pfx & "." & ckt.params.get(i))
      END
    ELSE
      FOR i := 0 TO ckt.params.size()-1 DO
        WITH tn = pfx & "." & ckt.params.get(i) DO
          (* dont lose nodes that have only one place *)
          AddAlias(symTab, tn, tn)
        END
      END
    END;
    FOR i := 0 TO ckt.elements.size()-1 DO
      WITH elem     = ckt.elements.get(i),
           flatName = pfx & "." & elem.name,
           inst     = NEW(SpiceInstance.T).init(flatName, elem, me) DO
        (* associate element, no matter what it is, with nodes connected *)
        FOR i := 0 TO elem.terminals.size()-1 DO
          WITH fn = pfx & "." & elem.terminals.get(i) DO
            Associate(assocs, fn, inst);

            (* and dont lose nodes that are connected only to circuit elements *)
            AddAlias(symTab, fn, fn)
          END
        END;

        TYPECASE elem OF
          SpiceObject.X(x) =>
          WITH nm   = pfx & "." & elem.name,
               gotType = subCkts.get(x.type, sckt),
               fNmSeq  = NEW(TextSeq.T).init() DO
            <*ASSERT gotType*>

            FOR i := 0 TO x.terminals.size()-1 DO
              fNmSeq.addhi(pfx & "." & x.terminals.get(i))
            END;
            
            VisitCktNodes(nm, symTab, sckt, fNmSeq, assocs, inst, subCkts)
          END
        ELSE
          (* skip *)
        END;

      END
    END
  END VisitCktNodes;

PROCEDURE Associate(tbl : TextSpiceInstanceSetTbl.T;
                    tNm : TEXT;
                    obj : SpiceInstance.T) =
  VAR
    s : SpiceInstanceSet.T;
  BEGIN
    (* just record the fact that the instance "touches" the node somehow.
       this is not great, we still have to search to figure out how they
       are associated *)

    IF NOT tbl.get(tNm, s) THEN
      s := NEW(SpiceInstanceSetDef.T).init();
      EVAL tbl.put(tNm, s)
    END;
    EVAL s.insert(obj)
  END Associate;

PROCEDURE Canonicalize(nm : TEXT;
                       VAR canon : TEXT;
                       canonTbl : TextTextTbl.T) : BOOLEAN =
  BEGIN
    (* ground is special case... *)
    IF TextUtils.HaveSuffix(nm, ".vss") THEN canon := "vss"; RETURN TRUE END;

    IF canonTbl.get(nm, canon) THEN
      RETURN TRUE
    ELSE
      (* unknown node, its own best friend *)
      canon := nm;
      RETURN FALSE
    END
  END Canonicalize;
  
PROCEDURE CleanAssocs(tbl      : TextSpiceInstanceSetTbl.T;
                      canonTbl : TextTextTbl.T) : TextSpiceInstanceSetTbl.T =
  VAR
    new := NEW(TextSpiceInstanceSetTbl.Default).init();
    iter := tbl.iterate();
    nm, canon : TEXT;
    s : SpiceInstanceSet.T;
    inst : SpiceInstance.T;
  BEGIN
    WHILE iter.next(nm, s) DO
      EVAL Canonicalize(nm, canon, canonTbl);
      WITH jter = s.iterate() DO
        WHILE jter.next(inst) DO
          Associate(new, canon, inst)
        END
      END
    END;
    tbl := TextSpiceInstanceSetTbl.Default.init(tbl);
    iter := new.iterate();
    RETURN new
  END CleanAssocs;
  
PROCEDURE DumpSymtab(wr : Wr.T;
                     symTab : TextTextSetTbl.T;
                     (*OUT*)canonTbl : TextTextTbl.T)
  RAISES { Wr.Failure } =
  VAR
    iter := symTab.iterate();
    done := NEW(TextSetDef.T).init();
    n : TEXT;
    a : TextSet.T;
    nn, mas, na := 0;
    mca : TEXT := "**NIL**";
  BEGIN
    WHILE iter.next(n, a) DO
      IF NOT done.member(n) THEN
        INC(nn);
        INC(na, a.size());
        WITH ca = DumpSingleList(wr, a, done, canonTbl) DO
          IF a.size() > mas THEN
            mca := ca;
            mas := a.size()
          END
        END
      END;
      Wr.PutChar(wr, '\n')
    END;

    Debug.Out(F("%-10s nets", Int(nn)));
    Debug.Out(F("%-10s aliases", Int(na)));
    Debug.Out(F("%-10s max aliases in net %s", Int(mas), mca));
  END DumpSymtab;

PROCEDURE BuildCanonTbl(aliasTbl : TextTextSetTbl.T;
                        canonTbl : TextTextTbl.T) =

  PROCEDURE DoOneSet(aliases : TextSet.T) =
    VAR
      arr := NEW(REF ARRAY OF TEXT, aliases.size());
      iter := aliases.iterate();
      i := 0;
      q : TEXT;
    BEGIN
      WHILE iter.next(q) DO
        arr[i] := q;
        EVAL done.insert(arr[i]);
        INC(i)
      END;
      <*ASSERT i = NUMBER(arr^)*>
      TextArraySort.Sort(arr^, AliasSortOrder);
      
      FOR i := FIRST(arr^) TO LAST(arr^) DO
        EVAL canonTbl.put(arr[i], arr[0])
      END
    END DoOneSet;
    
  VAR
    done := NEW(TextSetDef.T).init();
    iter := aliasTbl.iterate();
    t : TEXT;
    ts : TextSet.T;
  BEGIN
    WHILE iter.next(t, ts) DO
      IF NOT done.member(t) THEN
        DoOneSet(ts);
        done := done.union(ts)
      END
    END
  END BuildCanonTbl;



PROCEDURE DumpSingleList(wr : Wr.T;
                         aliases : TextSet.T;
                         done : TextSet.T;
                         canonTbl : TextTextTbl.T) : TEXT
  RAISES { Wr.Failure } =
  VAR
    arr := NEW(REF ARRAY OF TEXT, aliases.size());
    iter := aliases.iterate();
    i := 0;
    q : TEXT;
  BEGIN
    WHILE iter.next(q) DO
      arr[i] := q;
      EVAL done.insert(arr[i]);
      INC(i)
    END;
    <*ASSERT i = NUMBER(arr^)*>
    TextArraySort.Sort(arr^, AliasSortOrder);

    FOR i := FIRST(arr^) TO LAST(arr^) DO
      EVAL canonTbl.put(arr[i], arr[0]);
      Wr.PutText(wr, arr[i]);
      IF i # LAST(arr^) THEN
        Wr.PutChar(wr, ' ')
      END
    END;

    RETURN arr[0] (* lead node *)
  END DumpSingleList;

PROCEDURE AliasSortOrder(a, b : TEXT) : [-1..1] =
  BEGIN
    WITH aDots = TextUtils.CountCharOccurences(a, '.'),
         bDots = TextUtils.CountCharOccurences(b, '.') DO
      IF aDots # bDots THEN RETURN
        Integer.Compare(aDots, bDots)
      ELSE
        RETURN Text.Compare(a, b)
      END
    END
  END AliasSortOrder;
  
  (**********************************************************************)

BEGIN END SpiceFlat.
