(* $Id$ *)
MODULE BoolRemap;
IMPORT Bool,BoolBoolTbl;

PROCEDURE Merge(m1, m2 : Map) : Map =
  VAR
    res := Empty();
  BEGIN
    RETURN MergeD(MergeD(res,m1),m2)
  END Merge;

PROCEDURE MergeD(m1, m2 : Map) : Map =
  VAR
    iter := m2.iterate();
    b0,b1 : Bool.T;
  BEGIN
    WHILE iter.next(b0,b1) DO EVAL m1.put(b0,b1) END;
    RETURN m1
  END MergeD;

PROCEDURE Empty() : Map = 
  BEGIN RETURN NEW(BoolBoolTbl.Default).init() END Empty;

BEGIN END BoolRemap.
