MODULE Example;

(* ring oscillator *)

IMPORT PRS, Name, Dsim, RefList;
IMPORT PRSimulate;
IMPORT ArithP, ArithR;
IMPORT Directives;
IMPORT FileRd, Rd;
IMPORT TimingModel, TransitionList;
FROM ArithP IMPORT GetX, GetY, NewPair;
FROM ArithR IMPORT NewFunc;
FROM ArithOps IMPORT Plus;
FROM Directives IMPORT Data, DelayBlock;
IMPORT Debug;
IMPORT NodePair;
IMPORT Scenario;
IMPORT Pathname;
IMPORT NameSetDef;
IMPORT Atom;

TYPE P = ArithP.T;
     R = ArithR.T;

CONST N = Name.ParseText;

  
PROCEDURE MakeInverter(in, out : PRS.Node; VAR prs : PRS.PRS) =
  VAR
    up := NEW(Dsim.Conjunct, input := in.name, sense := Dsim.Sense.Down);
    dn := NEW(Dsim.Conjunct, input := in.name, sense := Dsim.Sense.Up);
    uprule := NEW(Dsim.Rule, target := out.name, sense := Dsim.Sense.Up,
                  conjuncts := RefList.List1(up));
    dnrule := NEW(Dsim.Rule, target := out.name, sense := Dsim.Sense.Down,
                  conjuncts := RefList.List1(dn));
  BEGIN
    prs.rules := RefList.Cons(uprule, RefList.Cons(dnrule, prs.rules))
  END MakeInverter;

PROCEDURE MakeNand(in0, in1, out : PRS.Node; VAR prs : PRS.PRS) =
  VAR
    up0 := NEW(Dsim.Conjunct, input := in0.name, sense := Dsim.Sense.Down);
    up1 := NEW(Dsim.Conjunct, input := in1.name, sense := Dsim.Sense.Down);
    dn0 := NEW(Dsim.Conjunct, input := in0.name, sense := Dsim.Sense.Up);
    dn1 := NEW(Dsim.Conjunct, input := in1.name, sense := Dsim.Sense.Up);
    rules := ARRAY [0..2] OF Dsim.Rule {
      NEW(Dsim.Rule, target := out.name, sense := Dsim.Sense.Up,
          conjuncts := RefList.List1(up0)),
      NEW(Dsim.Rule, target := out.name, sense := Dsim.Sense.Up,
          conjuncts := RefList.List1(up1)),
      NEW(Dsim.Rule, target := out.name, sense := Dsim.Sense.Down,
                  conjuncts := RefList.List2(dn0, dn1)) };
  BEGIN
    FOR i := FIRST(rules) TO LAST(rules) DO
      prs.rules := RefList.Cons(rules[i], prs.rules)
    END
  END MakeNand;

TYPE
  MyTimingModel = TimingModel.T OBJECT
    tbl : Directives.Table;
  OVERRIDES
    transitionTime := TransitionTime;
  END;

TYPE F = ArithR.F OBJECT data : REF Directives.Characterization; END;

PROCEDURE EvalSlew(f : F; at : LONGREAL) : ArithR.R =
  BEGIN 
    WITH d = Directives.Interpolate(f.data^, at) DO
      RETURN ArithR.NewRange ( d[Data.MinSlew], d[Data.MaxSlew] )
    END
  END EvalSlew;

PROCEDURE EvalDelay(f : F; at : LONGREAL) : ArithR.R =
  BEGIN 
    WITH d = Directives.Interpolate(f.data^, at) DO
      RETURN ArithR.NewRange ( d[Data.MinDelay], d[Data.MaxDelay] )
    END
  END EvalDelay;

PROCEDURE TimingP(inTrans : P; d : DelayBlock) : P =
  BEGIN
    WITH inSlew = GetY(inTrans),
         inTime = GetX(inTrans),

         delay  = NewFunc(NEW(F, data := d.data, eval := EvalDelay), inSlew,
                          "delta(" & NodePair.Format(d.nodes) & ")"),
         slew   = NewFunc(NEW(F, data := d.data, eval := EvalSlew),  inSlew,
                          "sigma(" & NodePair.Format(d.nodes) & ")" ) DO
      RETURN NewPair(Plus(inTime,delay),slew)
    END
  END TimingP;

PROCEDURE CalcTrans(tbl     : Directives.Table;
                    fanin   : Name.T;
                    newIn   : BOOLEAN;
                    fanout  : Name.T;
                    newOut  : BOOLEAN;
                    inTrans : P) : P =
  TYPE S = Dsim.Sense;
  CONST Sense = ARRAY BOOLEAN OF S { S.Down, S.Up };
  VAR r : REFANY;
  BEGIN
    WITH np = NodePair.T { fanout, fanin, Sense[newOut], Sense[newIn] } DO
      IF NOT tbl.get(np, r) THEN
        Debug.Error("No timing data for \"" & Name.Format(fanin) & "\" -> \""&
          Name.Format(fanout) & "\"")
      END;

      RETURN TimingP(inTrans, r)
    END
  END CalcTrans;

PROCEDURE TransitionTime(tm       : MyTimingModel;
                         of       : Name.T;
                         newValue : BOOLEAN;
                         fanins   : TransitionList.T) : ArithP.T =
  VAR 
    n := TransitionList.Length(fanins);
    by  := NEW(REF ARRAY OF R, n);
    val := NEW(REF ARRAY OF P, n);
    i := 0;
    p := fanins;
  BEGIN
    <*ASSERT n > 0 *>
    WHILE p # NIL DO
      by[i]  := ArithP.GetX(p.head.at);
      val[i] := CalcTrans(tm.tbl,
                          p.head.name, p.head.newValue, 
                          of, newValue,
                          p.head.at);
      p := p.tail; INC(i)
    END;

    RETURN ArithP.SelectMin(by,val)
  END TransitionTime;

PROCEDURE DoIt(pn : Pathname.T) : PRS.T =
  VAR 
    rd := FileRd.Open(pn);
    tbl := NEW(Directives.Table).init();
  BEGIN
    Directives.Parse(rd, 
                     NEW(NameSetDef.T).init(),
                     NIL,
                     tbl);
    
    VAR
      tm  := NEW(MyTimingModel, tbl := tbl);
      prs := NEW(PRS.T).init(tm);
      
      a := prs.node(N("a"));
      b := prs.node(N("b"));
      c := prs.node(N("c"));
      r := prs.node(N("_RESET"));
      
    CONST 
      DefaultSlew = 40.0d0;
      
    VAR defaultEpoch := ArithP.NewPair(ArithR.TheEpoch(), 
                                       ArithR.NewConstant(DefaultSlew));
        start        := ArithP.NewPair(ArithR.NewLiteral(Atom.FromText("t0")),
                                       ArithR.NewConstant(DefaultSlew));
    BEGIN
      Rd.Close(rd);
      MakeNand    (a, r, b, prs);
      MakeInverter(b,    c, prs);
      MakeInverter(c,    a, prs);
      
      PRSimulate.Set(prs, N("_RESET"), FALSE, defaultEpoch);
      EVAL PRSimulate.Cycle(prs, Scenario.Forever());
      PRSimulate.Set(prs, N("_RESET"), TRUE, start);
      EVAL PRSimulate.Cycle(prs, NEW(Scenario.T, 
                                     expr := NEW(Scenario.V, 
                                                 node := N("a"), 
                                                 to := TRUE)));
      RETURN prs
    END
  END DoIt;

BEGIN END Example.
