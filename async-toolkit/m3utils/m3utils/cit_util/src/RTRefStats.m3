MODULE RTRefStats;

TYPE
  T = RTHeapRep.RefVisitor OBJECT
    count, bytes: REF ARRAY OF INTEGER;
  OVERRIDES
    visit := Visit;
  END;

PROCEDURE Visit(self: T; tc: Typecode; r: REFANY; size: CARDINAL): BOOLEAN =
  BEGIN
    
    RETURN TRUE;
  END Visit;

PROCEDURE ShowMe() =
  VAR
    n := RTType.MaxTypecode();
    self := NEW(T);
  BEGIN
    RTHeapRep.VisitAllRefs(NEW(MyRefVisitor));
  END ShowMe;

BEGIN
END RTRefStats.
