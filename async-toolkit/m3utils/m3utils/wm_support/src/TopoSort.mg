GENERIC MODULE TopoSort(Elem, ElemSeq, ElemSet, ElemSetDef, ElemRefTbl);
IMPORT Debug;
IMPORT Wx;

REVEAL
  T = Public BRANDED Brand OBJECT
    allNodes : ElemSet.T := NIL;
    succTbl : ElemRefTbl.T;
  METHODS
    debugFmt(e : Elem.T) : TEXT      := DefDebugFmt;
  OVERRIDES
    sort := TopoSort;
    addDependency := AddDependency;
    init := Init;
  END;

PROCEDURE DefDebugFmt(<*UNUSED*>t : T; <*UNUSED*>e : Elem.T) : TEXT =
  BEGIN RETURN "(" & Elem.Brand & ")" END DefDebugFmt;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.allNodes    := NEW(ElemSetDef.T).init();
    t.succTbl     := NEW(ElemRefTbl.Default).init();
    RETURN t
  END Init;

PROCEDURE TopoSort(t : T) : ElemSeq.T =
  (* topological sort of all the nodes, or if syncOnly is true, of
     all the nodes visible from the syncSet (which might be empty) *)

  (* from Wikipedia *)
  PROCEDURE Visit(n : Elem.T) =
    BEGIN

      IF tempMarks.member(n) THEN Debug.Error("not a DAG!") END;
      IF toVisit.member(n) THEN
        EVAL tempMarks.insert(n);
        WITH s = SuccSet(t, n)^ DO
          FOR i := FIRST(s) TO LAST(s) DO
            WITH m = s[i] DO
              IF Verbose THEN 
                Dbg("TopoSort.Visit", t.debugFmt(n) & "->" & t.debugFmt(m)) 
              END;
              Visit(m)
            END
          END
        END;
        EVAL toVisit.delete(n); EVAL tempMarks.delete(n);
        res.addlo(n)
      END
    END Visit;

  VAR
    res       := NEW(ElemSeq.T).init();
    tempMarks := NEW(ElemSetDef.T).init();
    toVisit   := AllNodes(t);
    n : Elem.T;
  BEGIN
    WHILE SelectOne(toVisit, n, notIn := tempMarks) DO
      Visit(n)
    END;
    IF Verbose THEN
      VAR wx := Wx.New(); BEGIN
        FOR i := 0 TO res.size()-1 DO
          Wx.PutChar(wx, ' ');
          Wx.PutText(wx, t.debugFmt(res.get(i)))
        END;
        Dbg("TopoSort", "returning" & Wx.ToText(wx))
      END
    END;
    RETURN res
  END TopoSort;

PROCEDURE AddDependency(t : T; a, b : Elem.T) =
  BEGIN
    EVAL t.allNodes.insert(a);
    EVAL t.allNodes.insert(b);
    InsertInSuccs(t, a, b);
  END AddDependency;

PROCEDURE InsertInSuccs(t : T; a (* pred *), b (* succ *) : Elem.T) =
  (* add b to succ list of a *)
  VAR
    pst : REF Succs;
    np : CARDINAL;
  BEGIN
    pst := SuccSet(t, a);
    np := NUMBER(pst^);
    
    (* check if we already have it -- necessary ? *)
    FOR i := FIRST(pst^) TO LAST(pst^) DO
      IF Elem.Equal(pst[i], b) THEN
        RETURN
      END
    END;
    
    WITH new = NEW(REF Succs, np+1) DO
      SUBARRAY(new^,0,np) := pst^;
      new[np] := b;
      EVAL t.succTbl.put(a, new)
    END;
  END InsertInSuccs;

PROCEDURE SuccSet(t : T; p : Elem.T) : REF Succs =
  VAR
    ss : REFANY;
  BEGIN
    IF NOT t.succTbl.get(p, ss) THEN
      ss := NEW(REF Succs, 0);
      EVAL t.succTbl.put(p, ss)
    END;
    RETURN ss
  END SuccSet;

TYPE
  Successor = Elem.T;

  Succs = ARRAY OF Successor;

PROCEDURE Dbg(tag, err : TEXT) =
  BEGIN
    Debug.Out(Brand & "." & tag & " : " & err)
  END Dbg;

PROCEDURE AllNodes(t : T) : ElemSet.T = BEGIN RETURN t.allNodes END AllNodes;

PROCEDURE SelectOne(s       : ElemSet.T; 
                    VAR res : Elem.T;
                    notIn   : ElemSet.T := NIL) : BOOLEAN =
  VAR 
    e : Elem.T;
    iter := s.iterate();
  BEGIN
    WHILE iter.next(e) DO
      IF notIn = NIL OR NOT notIn.member(e) THEN res := e; RETURN TRUE END
    END;
    RETURN FALSE
  END SelectOne;

CONST Verbose = FALSE;

BEGIN END TopoSort.
