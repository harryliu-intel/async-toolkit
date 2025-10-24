MODULE Scenario;
IMPORT Fmt, Name;

PROCEDURE Format(r : REFANY) : TEXT =
  BEGIN
    TYPECASE r OF
      NULL => RETURN "NIL"
    |
      T(t) =>
      RETURN
        "(Scenario.T " &
        "(mutex " & Format(t.mutex) & ")\n" &
        "(expr " & Format(t.expr) & ")\n" &
        "(next " & Format(t.next) & ") )"
    |
      Or(or) =>
      RETURN "(or " & Format(or.x0) & " " & Format(or.x1) & ")"
    |
      And(and) =>
      RETURN "(and " & Format(and.x0) & " " & Format(and.x1) & ")"
    |
      B(b) =>
      RETURN " " & Name.Format(b.node) & "=" & Fmt.Bool(b.to) & " " & 
             Format(b.next)
    |
      V(v) =>
      RETURN " " & Name.Format(v.node) & "=" & Fmt.Bool(v.to) 
    ELSE
      <*ASSERT FALSE*>
    END
  END Format;

BEGIN END Scenario.
