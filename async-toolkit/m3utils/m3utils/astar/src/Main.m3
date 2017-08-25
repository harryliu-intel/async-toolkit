MODULE Main;
IMPORT IntPQ;
IMPORT Rd, Scan, Params, FileRd;
IMPORT Text;
IMPORT IO, Fmt;

TYPE
  Node = RECORD
    blocked : BOOLEAN;
    e       : E := NIL;
    cost        := LAST(INTEGER);
    fr      : P;
  END;

  P = RECORD
    x, y : INTEGER;
  END;

  E = IntPQ.Elt OBJECT
    p : P;
  END;

  Dir = { N, E, W, S };
  
CONST
  Step = ARRAY Dir OF P { P { 0, 1 }, P { 1, 0 }, P { -1, 0 }, P { 0, -1 } };

PROCEDURE Plus(READONLY a, b : P) : P = 
  BEGIN RETURN P { a.x + b.x, a.y + b.y } END Plus;

PROCEDURE InField(READONLY a : P) : BOOLEAN =
  BEGIN 
    RETURN a.x >= FIRST(field^)   AND a.x <= LAST(field^) 
           AND
           a.y >= FIRST(field[0]) AND a.y <= LAST(field[0])
  END InField;

PROCEDURE Heuristic(READONLY fr, to : P) : CARDINAL =
  BEGIN
    RETURN ABS(to.x-fr.x) + ABS(to.y-fr.y)
  END Heuristic;

PROCEDURE NegatePath(READONLY p : P) =
  BEGIN
    IF p = strt THEN 
      RETURN 
    ELSE
      WITH pt = field[p.x,p.y] DO
        pt.cost := -pt.cost;
        NegatePath(pt.fr)
      END
    END
  END NegatePath;

VAR
  strt, goal : P;
  q := NEW(IntPQ.Default).init();
  rd    := FileRd.Open(Params.Get(1));
  rows  := Scan.Int(Rd.GetLine(rd));
  cols  := Scan.Int(Rd.GetLine(rd));
  field := NEW(REF ARRAY OF ARRAY OF Node, cols, rows);
BEGIN
  FOR r := 0 TO rows-1 DO
    WITH l = Rd.GetLine(rd) DO
      FOR c := 0 TO cols-1 DO
        WITH n  = field[c,r], 
             ch = Text.GetChar(l, c*2) DO
          CASE ch OF
            '_', 'g', 's' => n.blocked := FALSE
          |
            'x'           => n.blocked := TRUE
          ELSE
            <*ASSERT FALSE*>
          END;

          IF ch = 'g' THEN goal := P { c, r } END;
          IF ch = 's' THEN strt := P { c, r } END
        END
      END
    END
  END;

  WITH e = NEW(E, priority := Heuristic(strt, goal), p := strt) DO
    WITH s = field[strt.x,strt.y] DO
      s.e    := e;
      s.cost := 0
    END;
    
    q.insert(e)
  END;

  LOOP
    IF q.size() = 0 THEN EXIT END;

    WITH min = NARROW(q.deleteMin(),E),
         p   = min.p,
         pp  = field[p.x,p.y],
         nc  = pp.cost + 1 DO
      IF p = goal THEN
        EXIT 
      END;

      FOR d := FIRST(Dir) TO LAST(Dir) DO
        WITH n  = Plus(p,Step[d]) DO
          IF InField(n) THEN
            WITH nn = field[n.x,n.y] DO
              IF NOT nn.blocked AND nc < nn.cost THEN
                (* found better route to n *)
                nn.cost := nc;
                nn.fr := p;
                IF nn.e # NIL THEN q.delete(nn.e) END;
                nn.e := NEW(E, priority := nc + Heuristic(n, goal), p := n);
                q.insert(nn.e)
              END
            END
          END
        END
      END
    END
  END;

  WITH gn = field[goal.x,goal.y] DO
    IF gn.cost = LAST(INTEGER) THEN
      IO.Put("No path found!\n")
    ELSE
      IO.Put("Path found, cost=" & Fmt.Int(gn.cost) & "\n");
      NegatePath(goal);

      FOR i := 0 TO rows-1 DO
        FOR j := 0 TO cols-1 DO
          VAR  p  := P { j, i };
               pt := field[j,i];
               ch : CHAR;
          BEGIN
            IF pt.blocked THEN 
              ch := 'x'
            ELSIF p = goal THEN
              ch := 'g'
            ELSIF p = strt THEN
              ch := 's'
            ELSIF pt.cost < 0 THEN
              WITH cs = Fmt.Int(-pt.cost) DO
                ch := Text.GetChar(cs, Text.Length(cs)-1)
              END
            ELSE
              ch := ' '
            END;
            IO.PutChar(ch); IO.PutChar(' ')
          END
        END;
        IO.Put("\n")
      END
    END
  END
  
END Main.
