(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE GenScmUtils;
IMPORT TextSeq, RdlArray, RegCGenState;
FROM Fmt IMPORT F;
IMPORT BigInt;
IMPORT IdStyles;

PROCEDURE DefineConstantC(sDecls : TextSeq.T;
                          nmC, val : TEXT) =
  BEGIN
    WITH nmS =
         IdStyles.Convert(nmC,
                          IdStyles.Case.Lower     , IdStyles.Case.Lower,
                          IdStyles.Sep. Underscore, IdStyles.Sep.Hyphen) DO
      sDecls.addhi(F("(define %s  %s)", nmS, val))
    END
  END DefineConstantC;

PROCEDURE FmtConstant(sDecls : TextSeq.T; val : TEXT; nm, sfx : TEXT) =
  BEGIN
    WITH snmC = F("%s__%s", nm, sfx) DO
      DefineConstantC(sDecls, snmC, val);
    END
  END FmtConstant;
  
PROCEDURE FmtArrSz(sDecls : TextSeq.T; a : RdlArray.Single; nm : TEXT) =
  BEGIN
    IF a = NIL THEN
      RETURN
    ELSE
      FmtConstant(sDecls, BigInt.Format(a.n.x), nm, "n")
    END
  END FmtArrSz;

PROCEDURE PutSDecls(gs : RegCGenState.T; sDecls : TextSeq.T) =
  BEGIN
    FOR i := 0 TO sDecls.size()-1 DO
      gs.scm(sDecls.get(i)); gs.scm("\n")
    END;
    gs.main("\n")
  END PutSDecls;

BEGIN END GenScmUtils.
