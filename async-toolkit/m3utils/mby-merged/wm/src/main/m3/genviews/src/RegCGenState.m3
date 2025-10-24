(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE RegCGenState;
IMPORT RegGenState;
IMPORT Wr, Thread, OSError, Debug, Wx;
FROM Fmt IMPORT F;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    p          := GsP;
    main       := GsMain;
    scm        := GsScm;
    noteDep    := NoteDep;
    put        := PutGS;
    newSymbol  := NewSymbol;
  END;

PROCEDURE NoteDep(gs : T; toSym : TEXT) =
  BEGIN
    (* note that curSym depends on toSym *)
    <*ASSERT gs.curSym # NIL*>
    Debug.Out(F("%s depends on %s", gs.curSym, toSym));
    gs.topo.addDependency(toSym, gs.curSym)
  END NoteDep;
      
PROCEDURE NewSymbol(gs : T; nm : TEXT) : BOOLEAN
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    res : BOOLEAN;
  BEGIN
    (* is it OK to generate this symbol?  
       if we had it before -> NOT OK
       if we did not have it before -> OK
     *)
    res := RegGenState.T.newSymbol(gs, nm);

    IF res AND gs.phase = 0 THEN
      gs.curWx := Wx.New();
      WITH hadIt = gs.wxTbl.put(nm, gs.curWx) DO <*ASSERT NOT hadIt*> END
    END;
    gs.curSym := nm;
    
    RETURN res
  END NewSymbol;
  
PROCEDURE PutGS(gs : T; sec : Section; txt : TEXT) =
  BEGIN
    IF sec = Section.Maintype THEN
      Wx.PutText(gs.curWx, txt)
    ELSE
      IF gs.wx[sec] = NIL THEN
        gs.wx[sec] := Wx.New()
      END;
      Wx.PutText(gs.wx[sec], txt)
    END
  END PutGS;

PROCEDURE GsP(gs  : T;
              sec : Section;
              fmt : TEXT;
              t1, t2, t3, t4, t5 : TEXT) =
  BEGIN gs.put(sec, F(fmt, t1, t2, t3, t4, t5)) END GsP;

PROCEDURE GsMain(gs : T; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL)= 
  BEGIN gs.p(Section.Maintype, fmt, t1, t2, t3, t4, t5) END GsMain;

PROCEDURE GsScm(gs : T; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL)= 
  BEGIN gs.p(Section.Scheme, fmt, t1, t2, t3, t4, t5) END GsScm;


BEGIN END RegCGenState.
