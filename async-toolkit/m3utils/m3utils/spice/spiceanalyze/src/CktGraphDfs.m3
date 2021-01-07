MODULE CktGraphDfs;
IMPORT CktGraph AS G;
IMPORT CktNodeList AS NodeList;
IMPORT CktElement;

PROCEDURE Node(n : G.Node; visitor : NodeVisitor) =
  VAR
    myMark := NEW(G.Mark);

  PROCEDURE Recurse(n : G.Node; path : NodeList.T) =
    (* n is prev node, path is path (of nodes) by which we got here *)

    PROCEDURE VisitElem(e : CktElement.T) =
      BEGIN
        IF e.mark = myMark THEN RETURN END;
        
        e.mark := myMark;
        FOR i := 0 TO e.terminals.size() - 1 DO
          WITH nn = e.terminals.get(i) DO
            IF visitor.visit(path, e, nn) THEN
              Recurse(nn,
                      NodeList.Cons(nn, path))
            END
          END
        END    
      END VisitElem;
    
    VAR
      p := n.elements;
    BEGIN
      (* dfs *)
      WHILE p # NIL DO
        VisitElem(p.head);
        p := p.tail
      END
    END Recurse;
    
  BEGIN
    Recurse(n, NodeList.List1(n))
  END Node;


BEGIN END CktGraphDfs.
