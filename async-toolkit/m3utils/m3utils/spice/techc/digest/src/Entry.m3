(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Entry;
IMPORT LongrealType;
IMPORT Scan;
IMPORT Wx;
FROM Fmt IMPORT F;

PROCEDURE Compare(a, b : T) : [ -1 .. 1 ] =
  BEGIN
    RETURN LongrealType.Compare(Scan.LongReal(a[CsvCols.Volt]),
                                Scan.LongReal(b[CsvCols.Volt]))
  END Compare;

PROCEDURE CompareLR(a, b : T; col : CsvCols) : [ -1 .. 1 ] =
  BEGIN
    RETURN LongrealType.Compare(Scan.LongReal(a[col]),
                                Scan.LongReal(b[col]))
  END CompareLR;

PROCEDURE Format(t : T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(CsvCols) TO LAST(CsvCols) DO
      Wx.PutText(wx, F("%s=%s", CsvColNames[i], t[i]));
      IF i # LAST(CsvCols) THEN
        Wx.PutChar(wx, ' ')
      END
    END;
    RETURN Wx.ToText(wx)
  END Format;

BEGIN END Entry.
