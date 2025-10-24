(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

MODULE DatabaseUtils;
IMPORT Text, TextRd, TextWr, Rd, Wr, UnsafeWr;
IMPORT Thread, Fmt;


(* need function for formatting, too... *)
(* this should take name of col, label of col in HTML, and then format *)
(* also alignment directives? *)

(* we need this function to make sure that database args dont include *)
(* special characters that can override security protections *)
(* especially the single quote *)

PROCEDURE Sanitize(string : TEXT) : TEXT =
  BEGIN 
    IF Text.FindChar(string,'\'') < 0 THEN
      RETURN string 
    ELSE (* someone trying to hack *)
      RETURN NIL
    END
  END Sanitize;

(************************************************************)

PROCEDURE Text2ByteA(data : TEXT) : ByteA =
  <* FATAL Rd.Failure, Thread.Alerted *>
  VAR
    rd := NEW(TextRd.T).init(data);
  BEGIN
    RETURN Rd2ByteA(rd)
  END Text2ByteA;

PROCEDURE ByteA2Text(data : ByteA) : TEXT =
  <* FATAL Wr.Failure *>
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    ByteA2Wr(wr, data);
    RETURN TextWr.ToText(wr)
  END ByteA2Text;

PROCEDURE Rd2ByteA(rd : Rd.T) : ByteA RAISES { Rd.Failure, Thread.Alerted } =
  <* FATAL Wr.Failure *>
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    Text2ByteARdWr(rd,wr);
    RETURN TextWr.ToText(wr)
  END Rd2ByteA;

PROCEDURE OctalChar(c : CHAR; VAR a : ARRAY [0..2] OF CHAR) =
  VAR
    cc := ORD(c);
  BEGIN
    FOR i := LAST(a) TO FIRST(a) BY -1 DO
      WITH digit = cc MOD 8 DO
        a[i] := VAL(digit + ORD('0'),CHAR);
        cc := cc DIV 8
      END
    END
  END OctalChar;

PROCEDURE Text2ByteARdWr(rd : Rd.T; wr : Wr.T) RAISES { Rd.Failure,
                                                         Thread.Alerted,
                                                         Wr.Failure } =
  VAR
    buff : ARRAY [0..2] OF CHAR;
  BEGIN
    TRY
      LOOP
        VAR
          c := Rd.GetChar(rd);
        BEGIN
          CASE ORD(c) OF
            0, (* 1..31, *) 39, 92 (*, 127..255*) => 
              Wr.PutText(wr, BackSlashEs); 
              OctalChar(c,buff);
              FOR i := FIRST(buff) TO LAST(buff) DO
                Wr.PutChar(wr, buff[i])
              END
          ELSE
            Wr.PutChar(wr, c)
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(rd); Wr.Flush(wr)
    END
  END Text2ByteARdWr;  

PROCEDURE Text2ByteAArray(READONLY in : ARRAY OF CHAR;
                          VAR out : ARRAY OF CHAR) : CARDINAL =
  PROCEDURE Put(c : CHAR) =
    BEGIN out[outp] := c; INC(outp) END Put;
  VAR
    buff : ARRAY [0..2] OF CHAR;
    outp := 0;
  BEGIN
    FOR inp := 0 TO LAST(in) DO
      WITH c = in[inp] DO
        CASE ORD(c) OF
          0, (* 1..31, *) 39, 92 (*, 127..255*) => 
            Put(BackSlash);
            Put(BackSlash);
            OctalChar(c,buff);
            FOR i := FIRST(buff) TO LAST(buff) DO
              Put(buff[i])
            END
        ELSE
          Put(c)
        END
      END
    END;
    RETURN outp
  END Text2ByteAArray;
   
  

PROCEDURE FilterUnprintable(t : TEXT) : TEXT =
  <* FATAL Thread.Alerted, Rd.Failure, Wr.Failure *>
  VAR
    rd := NEW(TextRd.T).init(t);
    wr := NEW(TextWr.T).init();
  BEGIN
    TRY
      LOOP
        
        VAR
          c := Rd.GetChar(rd);
        BEGIN
          CASE ORD(c) OF
            0, 1..31, (*39, 92 , *) 127..255 => Wr.PutText(wr, BackSlashEs & 
              Fmt.Pad(length := 3,  padChar := '0',
                      text := Fmt.Int(ORD(c), base := 8)))
          ELSE
            Wr.PutChar(wr, c)
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => RETURN TextWr.ToText(wr)
    END
  END FilterUnprintable;


PROCEDURE ByteA2Wr(wr : Wr.T; data : ByteA) RAISES { Wr.Failure } =
  <* FATAL Rd.Failure, Thread.Alerted *>
  VAR
    rd := NEW(TextRd.T).init(data);
  BEGIN
    ByteA2TextRdWr(rd,wr)
  END ByteA2Wr;

PROCEDURE OctalToChar(READONLY c : ARRAY [0..2] OF CHAR) : CHAR =
  (* lots of things can be FATAL here! *)
  VAR 
    a : ARRAY [0..2] OF CARDINAL;
  BEGIN
    a[0] := ORD(c[0]) - ORD('0');
    a[1] := ORD(c[1]) - ORD('0');
    a[2] := ORD(c[2]) - ORD('0');

    RETURN VAL(a[0]*64+a[1]*8+a[2], CHAR)
  END OctalToChar;

PROCEDURE ByteA2TextRdWr(rd : Rd.T; wr : Wr.T) RAISES { Wr.Failure,
                                                        Rd.Failure,
                                                        Thread.Alerted } =
  VAR
    buffer : ARRAY [0..3] OF CHAR;
    p : CARDINAL;
    c : CHAR;
  BEGIN
    TRY
      LOCK wr DO
        LOOP
          c := Rd.GetChar(rd);
          IF c = BackSlash THEN
            buffer[0] := c;
            p := 1;
            LOOP
              buffer[p] := Rd.GetChar(rd); 
              IF p = 1 AND buffer[1] = BackSlash THEN
                UnsafeWr.FastPutChar(wr, BackSlash); p := 0; EXIT
              ELSIF NOT buffer[p] IN OctalDigits THEN
                UnsafeWr.FastPutString(wr, SUBARRAY(buffer,0,p+1)); 
                p := 0; EXIT
              ELSIF p = 3 THEN
                UnsafeWr.FastPutChar(wr, 
                           OctalToChar(SUBARRAY(buffer,1,3)));
                p := 0;
                EXIT
              END;
              INC(p)
            END;
            <* ASSERT p = 0 *>
          ELSE
            UnsafeWr.FastPutChar(wr, c)
          END
        END
      END
    EXCEPT
      Rd.EndOfFile =>
        IF p # 0 THEN 
          Wr.PutText(wr, Text.FromChars(SUBARRAY(buffer,0,p+1)))
        END;
        Rd.Close(rd);
        Wr.Flush(wr)
    END
  END ByteA2TextRdWr;


CONST 
  OctalDigits = SET OF CHAR { '0'..'7' };
  BackSlash = '\\';
  BackSlashEs = "\\\\";

BEGIN END DatabaseUtils.


