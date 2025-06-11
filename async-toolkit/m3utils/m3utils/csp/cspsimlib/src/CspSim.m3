MODULE CspSim;
IMPORT CspCompiledProcess AS Process;
IMPORT TextFrameTbl;
IMPORT TextPortTbl;
IMPORT TextClosureTbl;
IMPORT Debug;
FROM Fmt IMPORT F;
IMPORT TextArraySort;
IMPORT CspPortObject;

CONST doDebug = FALSE;
      
VAR theProcs := NEW(TextFrameTbl.Default).init();

PROCEDURE GetProcTbl() : TextFrameTbl.T =
  BEGIN RETURN theProcs END GetProcTbl;

PROCEDURE RegisterProcess(fr : Process.Frame) =
  BEGIN
    IF doDebug THEN
      Debug.Out(F("Registering process : %s", fr.name))
    END;
    EVAL theProcs.put(fr.name, fr)
  END RegisterProcess;

VAR theEdges := NEW(TextPortTbl.Default).init();

PROCEDURE GetAllProcNames() : REF ARRAY OF TEXT =
  VAR
    res  := NEW(REF ARRAY OF TEXT, theProcs.size());
    iter := theProcs.iterate();
    k : TEXT;
    f : Process.Frame;
    i    := 0;
  BEGIN
    WHILE iter.next(k, f) DO
      res[i] := k;
      INC(i)
    END;
    TextArraySort.Sort(res^);
    RETURN res
  END GetAllProcNames;

PROCEDURE GetFrame(nm : TEXT) : Process.Frame =
  VAR
    k : TEXT;
    f : Process.Frame;
  BEGIN
    WITH hadIt = theProcs.get(k, f) DO
      <*ASSERT hadIt*>
    END;
    RETURN f
  END GetFrame;


(**********************************************************************)

VAR closureTbl := NEW(TextClosureTbl.Default).init();

PROCEDURE ClosureName(cl : Process.Closure) : TEXT =
   VAR
    nm := cl.fr.name & "/" & cl.name;
  BEGIN
    RETURN nm
  END ClosureName;
  
PROCEDURE RegisterClosure(cl : Process.Closure) =
  VAR
    nm := ClosureName(cl);
  BEGIN
    EVAL closureTbl.put(nm, cl)
  END RegisterClosure;

PROCEDURE RegisterClosures(READONLY cls : ARRAY OF Process.Closure) =
  BEGIN
    FOR i := FIRST(cls) TO LAST(cls) DO
      RegisterClosure(cls[i])
    END
  END RegisterClosures;

(**********************************************************************)
  
PROCEDURE GetPortTbl() : TextPortTbl.T =
  BEGIN RETURN theEdges END GetPortTbl;

PROCEDURE RegisterEdge(edge : CspPortObject.T) =
  BEGIN
    EVAL theEdges.put(edge.nm, edge)
  END RegisterEdge;

BEGIN END CspSim.
