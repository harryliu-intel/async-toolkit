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
  M = AdGridQ.T OBJECT
    master : T;
    up : M;
    ll, ur : LRPoint.T;
    vals : ARRAY Child OF LONGREAL;
    children : ARRAY Child OF M := NoChildren;
  METHODS
    maxCorner() : Child := MaxCorner;
    minCorner() : Child := MinCorner;
  OVERRIDES
    corner := MCorner;
  END;

REVEAL 
  T = Public BRANDED Brand OBJECT
    root : M;
    f : LRScalarField.T;
    prec := 0.01d0;
  OVERRIDES
    evalP := Eval;
    init := Init;
    setPrec := SetPrec;
    eval := EvalLRSF;
    getQuadsContainingLevel := GetLeafTilesContainingLevel;
  END;

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


PROCEDURE SubdivideM(f : LRScalarField.T; m : M; levels : CARDINAL := 1) =
  BEGIN
    IF levels = 0 THEN RETURN END;

    <* ASSERT m.children = ARRAY Child OF M { NIL, .. } *>
    WITH mm = LRPoint.T { (m.ll.x + m.ur.x) / 2.0d0 , 
                          (m.ll.y + m.ur.y) / 2.0d0 } DO
      FOR i := FIRST(Child) TO LAST(Child) DO
        WITH c = m.children[i] DO
          c := NEW(M, up := m, master := m.master);
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
      WITH ml = m.children[UL].corner(LL), mlz = f.eval(PointToVector(ml)),
           mr = m.children[LR].corner(UR), mrz = f.eval(PointToVector(mr)),
           mb = m.children[LR].corner(LL), mbz = f.eval(PointToVector(mb)),
           mt = m.children[UL].corner(UR), mtz = f.eval(PointToVector(mt)),
           mmz = f.eval(PointToVector(mm))
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
      SubdivideM(f,m.children[i],levels - 1) 
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
                       (alpha * m.vals[UR] + 
                       (1.0d0-alpha)*m.vals[UL]) +
                     (1.0d0-beta) * 
                       (alpha * m.vals[LR] + 
                       (1.0d0-alpha)*m.vals[LL]) DO
            VAR
              maxdiff := 0.0d0;
            BEGIN
              FOR i := FIRST(Child) TO LAST(Child) DO
                maxdiff := MAX(maxdiff, ABS(m.vals[i]-res))
              END;
              IF maxdiff < prec THEN 
                RETURN res
              ELSE
                SubdivideM(t.f, m);
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

      new := NewM(t.f, ll, ur);

      (* subdivide it once *)
      SubdivideM(t.f,new);

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

PROCEDURE NewM(f : LRScalarField.T; ll, ur : LRPoint.T; master : T) : M =
  VAR
    res := NEW(M, ll := ll, ur := ur, up := NIL, master := master);
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      res.vals[i] := f.eval(PointToVector(res.corner(i)))
    END;
    RETURN res
  END NewM;

PROCEDURE Init(self : T; 
               f : LRScalarField.T; ll, ur : LRPoint.T; initLevels := 0) : T =
  BEGIN
    self.f := f;
    self.root := NewM(f, ll, ur, self);
    SubdivideM(f,self.root,initLevels);
    RETURN self
  END Init;

PROCEDURE GetLeafTilesContainingLevel(self : T; 
                                      level : LONGREAL) : AdGridQSet.T = 

  PROCEDURE GetLeafTilesContainingLevelM(m : M) =
    BEGIN
      IF m.children = NoChildren THEN
        (* base case *)
        IF level >= m.vals[m.minCorner()] AND 
           level <= m.vals[m.maxCorner()] THEN
          EVAL set.insert(m)
        END
      ELSE
        (* recursion case *)
        FOR i := FIRST(Child) TO LAST(Child) DO
          GetLeafTilesContainingLevelM(m.children[i])
        END
      END
    END GetLeafTilesContainingLevelM;

  VAR
    set := NEW(AdGridQSetDef.T).init();
  BEGIN
    GetLeafTilesContainingLevelM(self.root);
    RETURN set
  END GetLeafTilesContainingLevel;

PROCEDURE MaxCorner(m : M) : Child =
  VAR
    c := UL;
    v := FIRST(LONGREAL);
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      IF m.vals[i] >= v THEN v := m.vals[i]; c := i END
    END;
    RETURN c
  END MaxCorner;

PROCEDURE MinCorner(m : M) : Child =
  VAR
    c := UL;
    v := LAST(LONGREAL);
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      IF m.vals[i] <= v THEN v := m.vals[i]; c := i END
    END;
    RETURN c
  END MinCorner;

BEGIN END AdGrid.
