MODULE MemoTranSeq;
IMPORT TranSeq;

REVEAL
  T = TranSeq.T BRANDED OBJECT
    q : CARDINAL := 0;
  END;
  (* we assume that the first transition is at t=0 AND that we never request
     negative times *)

PROCEDURE Interpolate(s : T; t : LONGREAL) : LONGREAL =
  VAR 
    n := s.size();
  BEGIN
    (* seek s.q such that:
       s.q   points to the previous transition at or before t
    *)
       
    <*ASSERT t >= 0.0d0 *>

    IF t < s.get(s.q).t THEN s.q := 0 END; (* rewind *)

    WHILE s.q < n-1 AND s.get(s.q+1).t <= t DO INC(s.q) END;

    <*ASSERT s.get(s.q).t <= t*> 
    <*ASSERT s.q = n-1 OR s.get(s.q+1).t > t *>

    WITH tran = s.get(s.q) DO
      IF t > tran.t + tran.rf THEN 
        RETURN tran.v 
      ELSE
        VAR 
          pv := 0.0d0;
        BEGIN
          IF s.q # 0 THEN pv := s.get(s.q-1).v END;
          RETURN pv + (tran.v-pv) * (t - tran.t) / tran.rf
        END
      END
    END
  END Interpolate;

BEGIN END MemoTranSeq.
