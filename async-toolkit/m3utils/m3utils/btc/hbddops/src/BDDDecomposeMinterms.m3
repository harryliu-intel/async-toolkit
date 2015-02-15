(* $Id$ *)

MODULE BDDDecomposeMinterms EXPORTS BDDDecompose;
IMPORT BDD, SopBDD, SopBDDRep;
FROM Fmt IMPORT Int;

PROCEDURE Minterms(<*UNUSED*>t   : T; 
                   x             : BDD.T; 
                   pfx           : TEXT) : Result =
  VAR
    sop := SopBDD.ConvertBool(x).invariantSimplify(BDD.True(),
                                                   BDD.True(),
                                                   BDD.True());
    c := 0;
    res : Result := NEW(Result, v := BDD.New(pfx & "_ROOT"), next := NIL);
    root := res;
    rx := BDD.False();
    cx : BDD.T;
  BEGIN

    (* Brute-force decomposition of large boolean expressions.

       we just expand the s-o-p

       an AND plane of ANDs for each conjunct
       followed by an OR of all the ANDs.
    *)
    
    FOR i := FIRST(sop.rep^) TO LAST(sop.rep^) DO
      WITH conj = sop.rep[i],
           vv   = BDD.New(pfx & "_" & Int(c)) (* AND plane output *) DO
        cx := BDD.True();
        FOR l := FIRST(conj^) TO LAST(conj^) DO
          cx := BDD.And(B(conj[l]),cx)
        END;
        res := NEW(Result, v := vv, x := cx, next := res);
        rx := BDD.Or(rx, vv);
        INC(c)
      END
    END;

    root.x := rx;
    RETURN res
  END Minterms;

PROCEDURE B(l : SopBDD.Literal) : BDD.T =
  BEGIN
    IF l.mode THEN RETURN l.var ELSE RETURN BDD.Not(l.var) END
  END B;

BEGIN END BDDDecomposeMinterms.
