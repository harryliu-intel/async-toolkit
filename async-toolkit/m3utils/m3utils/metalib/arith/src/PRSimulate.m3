MODULE PRSimulate EXPORTS PRSimulate, Scenario;
IMPORT Name, ArithP, PRS, Transition, Dsim, TransitionList;
IMPORT PRSClass;
IMPORT Debug, Fmt;
IMPORT Scenario;
IMPORT TransitionPredTbl;
IMPORT TransitionPair;

PROCEDURE Set(set   : PRS.PRS; 
              node  : Name.T; 
              to    : BOOLEAN; 
              at    : ArithP.T) =
  BEGIN SetInternal(set,node,to,at,NIL) END Set;

TYPE 
  PredTbl = TransitionPredTbl.Default OBJECT
  METHODS
    preceded(t, by : Transition.T) : BOOLEAN := Preceded;
    succeeded(t : Transition.T; by : TransitionList.T) : BOOLEAN := Succeeded;
  END;

PROCEDURE Succeeded(tbl : PredTbl;
                    t : Transition.T; 
                    by : TransitionList.T) : BOOLEAN =
  BEGIN
    WHILE by # NIL DO
      IF tbl.preceded(by.head, t) THEN RETURN TRUE END;
      by := by.tail
    END;
    RETURN FALSE
  END Succeeded;

PROCEDURE Preceded(tbl : PredTbl;
                   t   : Transition.T; 
                   by  : Transition.T) : BOOLEAN =

  PROCEDURE Recurse(t : Transition.T) : BOOLEAN =
    VAR
      b : BOOLEAN;
    BEGIN
      WITH k = TransitionPair.T { t, by } DO
        IF tbl.get(k, b) THEN 
          RETURN b 
        ELSE
          VAR p := t.pred; BEGIN
            WHILE p # NIL DO
              IF p.head = by OR Recurse(p.head) THEN b := TRUE; EXIT END;
              p := p.tail
            END;
            b := FALSE
          END;
          EVAL tbl.put(k, b); RETURN b
        END
      END
    END Recurse;

  BEGIN RETURN Recurse(t) END Preceded;

PROCEDURE SetInternal(prs   : PRS.PRS; 
                      node  : Name.T; 
                      to    : BOOLEAN; 
                      at    : ArithP.T;
                      pred  : TransitionList.T) =
  VAR 
    r : REFANY;
  BEGIN
    WITH hadIt = prs.nodes.get(node, r) DO
      IF NOT hadIt THEN
        Debug.Error("PRSimulate.SetInternal: no node named \"" & 
          Name.Format(node) &"\"")
      END
    END;
    
    WITH n = NARROW(r, PRS.Node) DO
      IF n.transitions # NIL AND n.transitions.head.newValue = to THEN
        (* already has right value *)
        RETURN 
      ELSE
        Debug.Out("Setting \"" & Name.Format(node) & "\" to " & Fmt.Bool(to));
        WITH nt = NEW(Transition.T, 
                      name := node, 
                      newValue := to, 
                      pred := NIL, 
                      at := at) DO
          n.transitions := TransitionList.Cons(nt,n.transitions);

          nt.pred := pred;
        END
      END
    END
  END SetInternal;

PROCEDURE Get(set : PRS.PRS; node : Name.T) : Transition.T =
  VAR r : REFANY; BEGIN
    WITH hadIt = set.nodes.get(node, r) DO
      IF NOT hadIt THEN
        Debug.Error("PRSimulate.Get: no node named \"" & 
          Name.Format(node) &"\"")
      END
    END;
    WITH n = NARROW(r, PRS.Node) DO
      IF n.transitions # NIL THEN
        RETURN n.transitions.head
      ELSE
        RETURN NIL
      END
    END
  END Get;

PROCEDURE Value(s : Dsim.Sense) : BOOLEAN =
  TYPE S = Dsim.Sense;
  BEGIN 
    CASE s OF S.Down => RETURN FALSE | S.Up => RETURN TRUE END
  END Value;

PROCEDURE Cycle(prs      : PRS.PRS; 
                scenario : Scenario.T;
                minMult  : LONGREAL) : Result =

  PROCEDURE NonVacuouslyEnabled(rule : Dsim.Rule) : BOOLEAN =
    VAR 
      cp := rule.conjuncts;
    BEGIN
      WITH last = Get(prs, rule.target) DO
        IF last # NIL AND last.newValue = Value(rule.sense) THEN 
          RETURN FALSE 
        END
      END;
      WHILE cp # NIL DO
        WITH c    = NARROW(cp.head,Dsim.Conjunct),
             last = Get(prs, c.input) DO
          IF last = NIL OR last.newValue # Value(c.sense) THEN 
            RETURN FALSE
          END
        END;
        cp := cp.tail
      END;
      RETURN TRUE
    END NonVacuouslyEnabled;

  PROCEDURE ExecuteRule(rule : Dsim.Rule) =
    VAR
      fi := rule.conjuncts;
      enablers, preds : TransitionList.T := NIL;
      
    BEGIN
      alive := TRUE;
      
      WHILE fi # NIL DO
        enablers := TransitionList.Cons(
                        Get(prs,NARROW(fi.head,Dsim.Conjunct).input),
                        enablers);
        fi := fi.tail
      END;

      (* prune preceding transitions out of list *)
      
      VAR p := enablers; BEGIN
        WHILE p # NIL DO
          IF NOT tbl.succeeded(p.head,enablers) THEN
            preds := TransitionList.Cons(p.head,preds)
          END;
          p := p.tail
        END
      END;
      
      WITH value = Value(rule.sense) DO
        SetInternal(prs, 
                    rule.target, 
                    value,
                    prs.tm.transitionTime(rule.target,
                                          value,
                                          preds,
                                          minMult),
                    preds)
      END;

      (* here we should check fanouts for vacuous enablings *)
      (* if anything is vacuously enabled, it's an error *)

    END ExecuteRule;

  PROCEDURE Banned(rule : Dsim.Rule) : BOOLEAN =
    VAR ss := s.mutex;
    BEGIN
      WHILE ss # NIL DO
        IF ss.node = rule.target AND ss.to = Value(rule.sense) THEN
          Debug.Out("Transition blocked in scenario: \"" & 
            Name.Format(ss.node) & "\" to " & Fmt.Bool(ss.to));
          RETURN TRUE
        END;
        ss := ss.next
      END;
      RETURN FALSE
    END Banned;

  PROCEDURE EvaluateScenario(s : Scenario.X) : BOOLEAN =
    BEGIN
      TYPECASE s OF
        NULL => RETURN FALSE
      |
        V(v) => 
        WITH last = Get(prs, v.node) DO
          IF last = NIL THEN 
            RETURN FALSE
          ELSE
            RETURN last.newValue = v.to
          END
        END
      |
        Or(x)   => RETURN EvaluateScenario(x.x0) OR EvaluateScenario(x.x1)
      |
        And(x)  => RETURN EvaluateScenario(x.x0) AND EvaluateScenario(x.x1)
      ELSE
        <*ASSERT FALSE*>
      END
    END EvaluateScenario;

  PROCEDURE AdvanceScenario(rule : Dsim.Rule) =
    BEGIN
      IF EvaluateScenario(s.expr) THEN
        Debug.Out("Advancing scenario: \"" & 
          Name.Format(rule.target) & "\" to " & Fmt.Bool(Value(rule.sense)) &
          " scenario=" & Scenario.Format(s));
        s := s.next
      END
    END AdvanceScenario;

  VAR 
    tbl : PredTbl := NEW(PredTbl).init();
    alive : BOOLEAN; 
    time := 0;
    s := scenario;
  BEGIN
    LOOP
      alive := FALSE;
      VAR p := prs.rules; 
      BEGIN
        WHILE p # NIL DO
          IF NonVacuouslyEnabled(p.head) AND NOT Banned(p.head) THEN
            ExecuteRule(p.head);
            AdvanceScenario(p.head);
            IF s = NIL THEN 
              Debug.Out("End of scenarios reached, stopping simulation.");
              RETURN Result.EndOfScenario
            END
          END;
          p := p.tail
        END
      END;
      INC(time);

      IF NOT alive THEN 
        Debug.Out("Deadlock, stopping simulation");
        RETURN Result.Deadlock
      END
    END
  END Cycle;

VAR forever := NEW(Scenario.T, expr := NIL);

PROCEDURE Forever() : Scenario.T = BEGIN RETURN forever END Forever;

BEGIN END PRSimulate.
