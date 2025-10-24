(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE IndexBits;
IMPORT Word;
IMPORT Wx;
FROM Fmt IMPORT Int;

PROCEDURE FromArray(n : CARDINAL; stride : Word.T) : T =
  VAR
    p, q : Word.T := 0;
    res := Zero;
  BEGIN
    FOR i := 0 TO n-1 DO
      p := Word.Plus(p, stride);
      q := Word.Or(q, p)
    END;
    FOR i := 0 TO LAST(I) DO
      IF Word.Extract(q, i, 1) = 1 THEN
        res := res + T { i }
      END
    END;
    RETURN res
  END FromArray;

PROCEDURE FromReg(bitSz : CARDINAL) : T =
  VAR
    byteSz := (bitSz - 1) DIV 8 + 1;
    res := Zero;
  BEGIN
    FOR i := 0 TO LAST(I) DO
      IF Word.Shift(1,i) < byteSz THEN
        res := res + T { i }
      ELSE
        RETURN res
      END
    END;
    RETURN res
  END FromReg;

PROCEDURE Hi(t : T) : [ -1..LAST(I) ] =
  VAR
    hi := -1;
  BEGIN
    FOR i := FIRST(I) TO LAST(I) DO
      IF i IN t THEN hi := i END
    END;
    RETURN hi
  END Hi;

PROCEDURE Lo(t : T) : [ 0..LAST(I)+1 ] =
  VAR
    lo := LAST(I) + 1;
  BEGIN
    FOR i := LAST(I) TO FIRST(I) BY -1 DO
      IF i IN t THEN lo := i END
    END;
    RETURN lo
  END Lo;

PROCEDURE FormatMask(bits : [0..64]; t : T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, Int(bits));
    Wx.PutText(wx, "'b");
    FOR i := bits-1 TO 0 BY -1 DO
      IF i IN t THEN
        Wx.PutChar(wx, '0')
      ELSE
        Wx.PutChar(wx, '1')
      END
    END;
    RETURN Wx.ToText(wx)
  END FormatMask;

PROCEDURE FormatBaseQ(bits : [0..64]; t : T; base : Word.T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    <*ASSERT Word.RightShift(base, bits) = 0*>
    Wx.PutText(wx, Int(bits));
    Wx.PutText(wx, "'b");
    FOR i := bits-1 TO 0 BY -1 DO
      IF i IN t THEN
        Wx.PutChar(wx, '?')
      ELSE
        CASE Word.Extract(base, i, 1) OF
          0 =>  Wx.PutChar(wx, '0')
        |
          1 =>  Wx.PutChar(wx, '1')
        END
      END
    END;
    RETURN Wx.ToText(wx)
  END FormatBaseQ;

BEGIN END IndexBits.
