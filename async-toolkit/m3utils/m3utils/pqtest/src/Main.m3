MODULE Main;
IMPORT IntPQ;
IMPORT IO, Fmt;

TYPE Elt = IntPQ.Elt OBJECT id : CARDINAL END;

VAR
  pq := NEW(IntPQ.Default).init();
BEGIN
  FOR i := 0 TO 19 DO
    pq.insert(NEW(Elt, priority := 0, id := i))
  END;

  WHILE pq.size() # 0 DO
    WITH min = NARROW(pq.deleteMin(),Elt) DO
      IO.Put(Fmt.Int(min.id) & "\n")
    END
  END
END Main.
