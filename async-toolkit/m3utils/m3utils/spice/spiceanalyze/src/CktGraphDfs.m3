MODULE CktGraphDfs;
IMPORT CktGraph AS G;

PROCEDURE Node(n : G.Node; visitor : NodeVisitor) =
  VAR
    myMark := NEW(G.Mark);

  PROCEDURE Recurse(n : G.Node) =
    VAR
      p := n.elements;
    BEGIN
      (* dfs *)
      WHILE p # NIL DO
        WITH e = p.head DO
          IF e.mark # myMark THEN
            e.mark := myMark;
            FOR i := 0 TO e.terminals.size() - 1 DO
              WITH nn = e.terminals.get(i) DO
                IF nn.mark # myMark THEN
                  nn.mark := myMark;
                  IF visitor.visit(n, e, nn) THEN
                    Recurse(nn)
                  END
                END
              END
            END    
          END
        END;
        p := p.tail
      END
    END Recurse;
    
  BEGIN
    Recurse(n)
  END Node;


BEGIN END CktGraphDfs.
