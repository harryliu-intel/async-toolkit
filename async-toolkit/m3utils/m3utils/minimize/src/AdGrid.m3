(* $Id$ *)

MODULE AdGrid EXPORTS AdGrid;
IMPORT AdGridChild;
IMPORT LRPoint, LRVector, LRScalarField, AdGridQ, AdGridQSet, AdGridQSetDef;

TYPE
  Child = AdGridChild.T; (* top left, top right, etc. *)

CONST 
  UL = Child.UL; UR = Child.UR; LL = Child.LL; LR = Child.LR;
  WhichChild = ARRAY BOOLEAN OF ARRAY BOOLEAN OF Child { 
    ARRAY BOOLEAN OF Child {LL,UL}, 
    ARRAY BOOLEAN OF Child {LR, UR} 
  };
  NoChildren = ARRAY Child OF M { NIL, .. };

TYPE 
  Funcs = REF ARRAY OF LRScalarField.T;
  Vals = ARRAY Child OF REF ARRAY OF LONGREAL;

  M = AdGridQ.T OBJECT
    f : Funcs;  (* pointer to functions stored in this mesh *)
    up : M;     (* pointer to my parent UNUSED ??? *) 
    ll, ur : LRPoint.T;  (* my corners *)
    vals : Vals; (* my values (for EACH f) *)
    children : ARRAY Child OF M := NoChildren; (* my children *)
  METHODS
    maxCorner(master : T) : Child := MaxCorner;
    minCorner(master : T) : Child := MinCorner;
  OVERRIDES
    corner := MCorner;
    getLbound := GetLbound;
    getUbound := GetUbound;
    subdivide := SubdivideSet;
  END;

PROCEDURE NewVals(n : CARDINAL) : Vals = 
  VAR
    r : Vals;
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      r[i] := NEW(REF ARRAY OF LONGREAL, n)
    END;
    RETURN r
  END NewVals;

REVEAL 
  T = Public BRANDED Brand OBJECT
    root : M;      (* pointer to root of mesh *)
    me : CARDINAL; (* index of the function I represent *)
    f : Funcs;     (* pointer to the set of functions evaluated in this mesh *)
    prec := 0.01d0; (* precision ??? *)
    next : T := NIL; (* circular list of Ts that share same mesh *)
  OVERRIDES
    evalP := Eval;
    init := Init;
    setPrec := SetPrec;
    eval := EvalLRSF;
    getQuadsContainingLevel := GetLeafTilesContainingLevel;
    mapNewLRSF := MapNewLRSF;
  END;

PROCEDURE EvalF(f : REF ARRAY OF LRScalarField.T;
                v : LRVector.T) : REF ARRAY OF LONGREAL =
  VAR
    r := NEW(REF ARRAY OF LONGREAL, NUMBER(f^));
  BEGIN
    FOR i := FIRST(r^) TO LAST(r^) DO
      r[i] := f[i].eval(v)
    END;
    RETURN r
  END EvalF;

PROCEDURE SetPrec(a : T; prec : LONGREAL) = BEGIN a.prec := prec END SetPrec;

PROCEDURE EvalLRSF(a : T; at : LRVector.T) : LONGREAL =
  BEGIN RETURN a.evalP(LRPoint.T{ at[0], at[1]}, prec := a.prec) END EvalLRSF;

PROCEDURE PointToVector(READONLY p : LRPoint.T) : LRVector.T =
  VAR 
    r := NEW(LRVector.T,2);
  BEGIN
    r[0] := p.x; r[1] := p.y; RETURN r
  END PointToVector;

PROCEDURE MCorner(m : M ; c : Child) : LRPoint.T =
  BEGIN
    WITH llx = m.ll.x,
         urx = m.ur.x,
         lly = m.ll.y,
         ury = m.ur.y,
         xs = ARRAY Child OF LONGREAL { llx, llx, urx, urx }, 
         ys = ARRAY Child OF LONGREAL { lly, ury, ury, lly } DO
      RETURN LRPoint.T { xs[c], ys[c] }
    END
  END MCorner;

PROCEDURE GetLbound(m : M; gridP : REFANY) : LONGREAL =
  BEGIN
    WITH grid = NARROW(gridP,T) DO
      RETURN m.vals[m.minCorner(grid)][grid.me]
    END
  END GetLbound;

PROCEDURE GetUbound(m : M; gridP : REFANY) : LONGREAL =
  BEGIN
    WITH grid = NARROW(gridP,T) DO
      RETURN m.vals[m.maxCorner(grid)][grid.me]
    END
  END GetUbound;

PROCEDURE SubdivideSet(m : M; levels : CARDINAL := 1) : REFANY =
  PROCEDURE Recurse(m : M) =
    BEGIN
      IF m.children = NoChildren THEN
        (* base case *)
        EVAL set.insert(m)
      ELSE
        (* recursion case *)
        FOR i := FIRST(Child) TO LAST(Child) DO
          Recurse(m.children[i])
        END
      END
    END Recurse;
  VAR
    set := NEW(AdGridQSetDef.T).init();
  BEGIN
    SubdivideM(m,levels);
    Recurse(m);
    RETURN set
  END SubdivideSet;

PROCEDURE SubdivideM(m : M; levels : CARDINAL := 1) =
  BEGIN
    IF levels = 0 THEN RETURN END;

    <* ASSERT m.children = ARRAY Child OF M { NIL, .. } *>
    WITH mm = LRPoint.T { (m.ll.x + m.ur.x) / 2.0d0 , 
                          (m.ll.y + m.ur.y) / 2.0d0 } DO
      FOR i := FIRST(Child) TO LAST(Child) DO
        WITH c = m.children[i] DO
          c := NEW(M, up := m, f := m.f);
          c.vals[i] := m.vals[i];
          CASE i OF
            UL => c.ll := LRPoint.T { m.ll.x, mm.y }; 
                  c.ur := LRPoint.T { mm.x, m.ur.y }
          |
            UR => c.ll := mm; c.ur := m.ur
          |
            LL => c.ll := m.ll; c.ur := mm
          |
            LR => c.ll := LRPoint.T { mm.x, m.ll.y }; 
                  c.ur := LRPoint.T { m.ur.x, mm.y }
          END;
        END
      END;
      (* fill in new points *)
      WITH f = m.f,
           ml = m.children[UL].corner(LL), mlz = EvalF(f,PointToVector(ml)),
           mr = m.children[LR].corner(UR), mrz = EvalF(f,PointToVector(mr)),
           mb = m.children[LR].corner(LL), mbz = EvalF(f,PointToVector(mb)),
           mt = m.children[UL].corner(UR), mtz = EvalF(f,PointToVector(mt)),
           mmz = EvalF(f,PointToVector(mm))
       DO
        m.children[UL].vals[LL] := mlz; m.children[LL].vals[UL] := mlz;
        m.children[UL].vals[UR] := mtz; m.children[UR].vals[UL] := mtz;
        m.children[UR].vals[LR] := mrz; m.children[LR].vals[UR] := mrz;
        m.children[LR].vals[LL] := mbz; m.children[LL].vals[LR] := mbz;
        
        m.children[UL].vals[LR] := mmz;
        m.children[UR].vals[LL] := mmz;
        m.children[LR].vals[UL] := mmz;
        m.children[LL].vals[UR] := mmz
      END
    END;
    FOR i := FIRST(Child) TO LAST(Child) DO
      SubdivideM(m.children[i],levels - 1) 
    END
  END SubdivideM;

PROCEDURE Eval(t : T; READONLY at : LRPoint.T; prec : LONGREAL) : LONGREAL =
  PROCEDURE EvalM(m : M) : LONGREAL =
    BEGIN
      WITH mm = LRPoint.T {(m.ll.x+m.ur.x)/2.0d0, (m.ll.y+m.ur.y)/2.0d0},
           wc = WhichChild[at.x > mm.x, at.y > mm.y],
           c = m.children[wc] DO
        IF c # NIL THEN 
          RETURN EvalM(c) 
        ELSE
          (* base case *)
          WITH alpha = (at.x - m.ll.x) / (m.ur.x - m.ll.x),
               beta  = (at.y - m.ll.y) / (m.ur.y - m.ll.y),
               res = beta * 
                       (alpha * m.vals[UR][t.me] + 
                       (1.0d0-alpha)*m.vals[UL][t.me]) +
                     (1.0d0-beta) * 
                       (alpha * m.vals[LR][t.me] + 
                       (1.0d0-alpha)*m.vals[LL][t.me]) DO
            VAR
              maxdiff := 0.0d0;
            BEGIN
              FOR i := FIRST(Child) TO LAST(Child) DO
                maxdiff := MAX(maxdiff, ABS(m.vals[i][t.me]-res))
              END;
              IF maxdiff < prec THEN 
                RETURN res
              ELSE
                SubdivideM(m);
                RETURN EvalM(m)
              END
            END
          END
        END
      END
    END EvalM;

  PROCEDURE Expand(oldRootIs : Child) =
    VAR
      ll, ur : LRPoint.T;
      new : M;
    BEGIN
      CASE oldRootIs OF
        UR =>
          ll := LRPoint.T { 2.0d0*t.root.ll.x - t.root.ur.x, 
                                2.0d0*t.root.ll.y - t.root.ur.y };
          ur := t.root.ur
      |
        LL =>
          ll := t.root.ll;
          ur := LRPoint.T { 2.0d0*t.root.ur.x - t.root.ll.x, 
                                2.0d0*t.root.ur.y - t.root.ll.y };
      ELSE
        <* ASSERT FALSE *>
      END;

      new := NewM(ll, ur, t.f);

      (* subdivide it once *)
      SubdivideM(new);

      t.root.up := new;

      (* and now overwrite the virgin child... *)
      new.children[oldRootIs] := t.root;

      t.root := new;
    END Expand;

  BEGIN
    WITH llx = t.root.ll.x, urx = t.root.ur.x,
         lly = t.root.ll.y, ury = t.root.ur.y DO

      (* first check if we must expand *)
      IF    at.x < llx THEN Expand(Child.UR)
      ELSIF at.x > urx THEN Expand(Child.LL)
      ELSIF at.y < lly THEN Expand(Child.UR)
      ELSIF at.y > ury THEN Expand(Child.LL)
      ELSE                  RETURN EvalM(t.root)
      END;
      RETURN Eval(t,at,prec)
    END
  END Eval;

PROCEDURE NewM(ll, ur : LRPoint.T; f : Funcs) : M =
  VAR
    res := NEW(M, ll := ll, ur := ur, up := NIL, 
               f := f, vals := NewVals(NUMBER(f^)));
  BEGIN
    FOR j := FIRST(f^) TO LAST(f^) DO
      FOR i := FIRST(Child) TO LAST(Child) DO
        res.vals[i][j] := f[j].eval(PointToVector(res.corner(i)))
      END
    END; 
    RETURN res
  END NewM;

PROCEDURE Init(self : T; 
               f : LRScalarField.T; ll, ur : LRPoint.T; initLevels := 0) : T =
  BEGIN
    self.me := 0; (* index of function in f *)
    self.f := NEW(Funcs, 1);
    self.f[0] := f;
    self.root := NewM(ll, ur, self.f);
    self.next := self;
    SubdivideM(self.root,initLevels);
    RETURN self
  END Init;

PROCEDURE GetLeafTilesContainingLevel(self : T; 
                                      level : LONGREAL;
                                      set : AdGridQSet.T) : AdGridQSet.T = 

  VAR
    delSet := NEW(AdGridQSetDef.T).init();
    iter : AdGridQSet.Iterator;
    mp : AdGridQ.T;
  BEGIN
    IF set = NIL THEN set := GetLeafTiles(self) END;
    iter := set.iterate();

    WHILE iter.next(mp) DO WITH m = NARROW(mp,M) DO
      IF NOT( level >= m.vals[m.minCorner(self)][self.me] AND 
              level <= m.vals[m.maxCorner(self)][self.me]) THEN
        EVAL delSet.insert(m)
      END
    END END;
    RETURN set.diff(delSet)
  END GetLeafTilesContainingLevel;

PROCEDURE GetLeafTiles(self : T) : AdGridQSet.T = 

  PROCEDURE Recurse(m : M) =
    BEGIN
      IF m.children = NoChildren THEN
        (* base case *)
        EVAL set.insert(m)
      ELSE
        (* recursion case *)
        FOR i := FIRST(Child) TO LAST(Child) DO
          Recurse(m.children[i])
        END
      END
    END Recurse;

  VAR
    set := NEW(AdGridQSetDef.T).init();
  BEGIN
    Recurse(self.root);
    RETURN set
  END GetLeafTiles;

PROCEDURE MaxCorner(m : M; master : T) : Child =
  VAR
    c := UL;
    v := FIRST(LONGREAL);
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      IF m.vals[i][master.me] >= v THEN v := m.vals[i][master.me]; c := i END
    END;
    RETURN c
  END MaxCorner;

PROCEDURE MinCorner(m : M; master : T) : Child =
  VAR
    c := UL;
    v := LAST(LONGREAL);
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      IF m.vals[i][master.me] <= v THEN v := m.vals[i][master.me]; c := i END
    END;
    RETURN c
  END MinCorner;

PROCEDURE MapNewLRSF(self : T; newf : LRScalarField.T) : T = 

  PROCEDURE Recurse(m : M) =
    VAR
      newVals := NewVals(n);
    BEGIN
      m.f := self.f;
      FOR i := FIRST(Child) TO LAST(Child) DO 
        SUBARRAY(newVals[i]^,0,n - 1) := m.vals[i]^;
        newVals[i][n-1] := newf.eval(PointToVector(m.corner(i)))
      END;
      m.vals := newVals;

      IF m.children # NoChildren THEN
        FOR i := FIRST(Child) TO LAST(Child) DO
          Recurse(m.children[i])
        END
      END
    END Recurse;

  VAR
    n := NUMBER(self.f^) + 1;
    newF := NEW(Funcs, n);
    res : T;
    p : T;
  BEGIN
    SUBARRAY(newF^,0,n - 1) := self.f^;
    newF[n-1] := newf;
    res := NEW(T, root := self.root, me := n - 1, next := self.next);
    self.next := res;

    (* update the Ts that share mesh *)
    p := self;
    REPEAT
      p.f := newF;
      p := p.next
    UNTIL p = self;

    (* update mesh itself *)
    Recurse(self.root);
    RETURN res
  END MapNewLRSF;

BEGIN END AdGrid.
