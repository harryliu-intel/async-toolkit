(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LambSequencer;
IMPORT LambCommandSeq AS CommandSeq;
IMPORT LambVerb AS Verb, BitInteger;
FROM Fmt IMPORT Int, F;
IMPORT RefSeq, Debug;

CONST SI = BitInteger.Small;

PROCEDURE Compile(prog          : CommandSeq.T;
                  VAR      seq  : ARRAY Verb.T OF REF ARRAY OF BitInteger.T) =

  PROCEDURE Clear() =
    BEGIN FOR i := FIRST(buf) TO LAST(buf) DO buf[i] := SI(0) END END Clear;

  PROCEDURE Push(dbg : TEXT) =
    VAR
      dbgStr := F("cycle %5s %10s : ", Int(tmp[FIRST(tmp)].size()), dbg);
    BEGIN 
      FOR i := FIRST(buf) TO LAST(buf) DO 
        dbgStr := dbgStr & F(" %s %s", 
                             Verb.Names[i], 
                             BitInteger.Format(buf[i]))
      END;

      Debug.Out(dbgStr);

      FOR i := FIRST(buf) TO LAST(buf) DO tmp[i].addhi(buf[i]) END 
    END Push;

  TYPE
    V = Verb.T;
  VAR
    tmp : ARRAY V OF RefSeq.T;
    buf : ARRAY V OF BitInteger.T;
  BEGIN
    FOR i := FIRST(tmp) TO LAST(tmp) DO tmp[i] := NEW(RefSeq.T).init() END;
    
    Clear();
    FOR i := 0 TO prog.size()-1 DO

      WITH ordr = prog.get(i) DO
        CASE ordr.v OF
          V.Nop   => (* skip *)
        |
          V.Read  => buf[V.Read] := SI(1); <*ASSERT NARROW(buf[V.Read], BitInteger.SmallPromise).v = 1*>
                     buf[V.Radr] := ordr.p0
        |
          V.Writ  => buf[V.Writ]  := SI(1); 
                     buf[V.Wdata] := ordr.p0; 
                     buf[V.Wadr]  := ordr.p1
        |
          V.RdWr  => buf[V.Writ]  := SI(1);
                     buf[V.Read]  := SI(1);
                     buf[V.Wdata] := ordr.p0; 
                     buf[V.Wadr]  := ordr.p1;
                     buf[V.Radr]  := ordr.p2       
        ELSE
          <*ASSERT FALSE*>
        END;
        Push(Verb.Names[ordr.v]);

        Clear();

        FOR i := 1 TO Verb.Nops[ordr.v] DO Push("*end-nop*") END (* push nops as req'd *)

      END
    END;

    Push("*coda*"); (* a few more cycles for good measure *) 
    Push("*last*");

    FOR v := FIRST(seq) TO LAST(seq) DO
      seq[v] := NEW(REF ARRAY OF BitInteger.T, tmp[v].size());

      FOR i := 0 TO tmp[v].size()-1 DO
        seq[v][i] := tmp[v].get(i)
      END
    END
  END Compile;

BEGIN END LambSequencer.
