(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SpiceFormat;
IMPORT Rd;
IMPORT Debug;
IMPORT Text;
IMPORT SpiceCircuitList, TextSpiceCircuitTbl;
IMPORT TextSeq;
IMPORT SpiceObjectSeq;
IMPORT SpiceCircuit;
IMPORT SpiceObjectParse;
FROM SpiceFileFormat IMPORT White;
IMPORT Thread;
IMPORT SpiceError;
IMPORT Pathname;
FROM SpiceParse IMPORT HavePrefix;
FROM Fmt IMPORT F, Int;
IMPORT OSError, FileRd;
IMPORT AL;
IMPORT TextTextTbl;
IMPORT CitTextUtils;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      
VAR Verbose := Debug.DebugThis("SpiceFormat");

(* The way we do EOF is an absolute mess.

   It's partly because of the file format, and partly a result of poor 
   programming by me.

   Remember that we don't know that a line is complete until we read the
   first character on the NEXT line, to see if it is a continuation 
   character.

   Also we want to be able to tolerate the case that the last line does
   not end in a carriage return.

   It is nasty.  Should not have used Rd.GetSubLine.

   Maybe re-code this using the generic fast lexer instead.
*)

PROCEDURE DebugOut(READONLY a : ARRAY OF CHAR) =
  VAR
    txt := Text.FromChars(a);
  BEGIN
    Debug.Out(txt)
  END DebugOut;
  
PROCEDURE GetSingleLine(rd       : Rd.T; 
                        VAR p    : [-1..LAST(CARDINAL)]; 
                        VAR buff : REF ARRAY OF CHAR;
                        VAR lNo  : CARDINAL ) : [ -1..LAST(CARDINAL) ]
  RAISES { Rd.Failure, Thread.Alerted } =
  VAR
    len : CARDINAL;
  BEGIN
    IF buff = NIL OR NUMBER(buff^) = 0 THEN
      buff := NEW(REF ARRAY OF CHAR, 1)
    END;
    LOOP
      len := Rd.GetSubLine(rd, SUBARRAY(buff^, p, NUMBER(buff^)-p));
      IF Verbose THEN
        Debug.Out(F("GetSingleLine: lNo %s len %s", Int(lNo), Int(len)))
      END;
      IF Rd.EOF(rd) THEN
        IF len # 0 THEN
          (* on the last line *)
          IF Verbose THEN Debug.Out("GetSingleLine: at EOF") END;
          (*success*)
          INC(lNo);
          IF p + len # 0 AND buff[p + len - 1] = '\r' THEN
            IF Verbose THEN Debug.Out("GetSingleLine: strip CR") END;
            DEC(len) (* strip carriage return *)
          END;
          RETURN p + len
        ELSE
          RETURN -1
        END
      ELSIF buff[p + len - 1] = '\n' THEN
        IF Verbose THEN Debug.Out("GetSingleLine: success") END;
        (*success*)
        INC(lNo);
        DEC(len);
        IF p+len # 0 AND buff[p+len-1] = '\r' THEN
          IF Verbose THEN Debug.Out("GetSingleLine: strip CR") END;
          DEC(len) (* strip carriage return *)
        END;
        RETURN p + len

      ELSE
        (*failure to finish the line*)
        IF Verbose THEN Debug.Out("GetSingleLine: failure to finish") END;
        WITH new = NEW(REF ARRAY OF CHAR, NUMBER(buff^)*2) DO
          SUBARRAY(new^, 0, NUMBER(buff^)) := buff^;
          p := NUMBER(buff^);
          buff := new
        END
      END
    END
  END GetSingleLine;

PROCEDURE GetLine(rd : Rd.T;
                  VAR buff : REF ARRAY OF CHAR;
                  VAR lNo : CARDINAL) : [-1..LAST(CARDINAL)]
  RAISES { Rd.Failure, Thread.Alerted } =
  VAR 
    p : [ -1 .. LAST(CARDINAL) ] := 0;
  BEGIN
    (* there are two ways of hitting EOF here ... *)
    LOOP
      p := GetSingleLine(rd, p, buff, lNo);

      IF p = -1 THEN RETURN p END; (* check for EOF *)
      
      LOOP
        (* eat the next live character.
           
           it can still be an EOF because we can get another line at EOF..
           argh what a mess.

           if a + keep getting stuff, else return here *)

        TRY
          WITH c = Rd.GetChar(rd) DO
            CASE c OF
              '+' =>
              (* careful here.  we have just read a complete line, which is
                 fine, but now there's a continuation character.  We must 
                 insert a space, because the + is equivalent to a space!

                 There will always be space, because the GetSingleLine will
                 have recently read a carriage return to get into this state *)

              <*ASSERT p <= LAST(buff^)*>
              buff[p] := ' ';
              INC(p);
              
              EXIT (* fetch another line *)
            ELSE
              Rd.UnGetChar(rd); RETURN p
            END
          END
        EXCEPT
          Rd.EndOfFile => RETURN p
        END
      END
    END
  END GetLine;

<*UNUSED*>
PROCEDURE AddToBuff(VAR buff : REF ARRAY OF CHAR;
                    VAR p    : CARDINAL;
                    c        : CHAR) =
  (* this code seems to have been inlined *)
  BEGIN
    IF p = NUMBER(buff^) THEN
      WITH new = NEW(REF ARRAY OF CHAR, 2*NUMBER(buff^)) DO
        SUBARRAY(new^, 0, NUMBER(buff^)) := buff^;
        buff := new
      END
    END;
    buff[p] := c;
    INC(p)
  END AddToBuff;


TYPE
  Private = T OBJECT
    circuit : SpiceCircuitList.T; (* currently parsing *)
  END;

  FileLookup = RECORD
    tgtFileName, newSearchDir : Pathname.T;
    rd : Rd.T;
  END;
  
PROCEDURE ResolvePath(cwd, fn : Pathname.T; search : TextSeq.T) : FileLookup
  RAISES { SpiceError.E, OSError.E } =

  PROCEDURE Dbg(path : Pathname.T) =
    BEGIN
      Debug.Out("ResolvePath trying " & path)
    END Dbg;
    
  BEGIN
    IF Text.Length(fn) = 0 THEN
      RAISE SpiceError.E(SpiceError.Data {
      msg := "Empty filename in .INCLUDE",
      lNo := 0
      })
    END;
    IF Text.GetChar(fn, 0) = '/' THEN
      (* absolute name *)
      RETURN FileLookup { tgtFileName  := fn,
                          newSearchDir := Pathname.Prefix(fn),
                          rd           := FileRd.Open(fn) }
    ELSE
      FOR i := 0 TO search.size() - 1 DO
        WITH dir = search.get(i),
             fn = dir & "/" & fn DO
          Dbg(fn);
          TRY
            RETURN FileLookup { tgtFileName  := fn,
                                newSearchDir := dir,
                                rd           := FileRd.Open(fn) }
          EXCEPT
            OSError.E => (* skip *)
          END
        END
      END;
      
      WITH primary = cwd & "/" & fn DO
        Dbg(primary);
        TRY
          RETURN FileLookup { tgtFileName  := primary,
                              newSearchDir := Pathname.Prefix(primary),
                              rd           := FileRd.Open(primary) }
        EXCEPT
          OSError.E(x) =>
          Debug.Warning(F("SpiceFormat.ResolvePath didn't find file %s in correct place %s : OSError.E : %s",
                          fn, primary, AL.Format(x)))
        END
      END;
      WITH secondary = "." & "/" & fn DO
        Dbg(secondary);
        RETURN FileLookup { tgtFileName  := secondary,
                            newSearchDir := Pathname.Prefix(secondary),
                            rd           := FileRd.Open(secondary) }
      END
    END
  END ResolvePath;

PROCEDURE ParseOptionLine(READONLY line : ARRAY OF CHAR; search : TextSeq.T) =
  VAR 
    p   : CARDINAL := 0;
    str : TEXT;
  BEGIN
    WHILE SpiceObjectParse.GetWord(line, p, str) DO
      <*ASSERT str # NIL*>
      WITH eqPos = Text.FindChar(str, '=') DO
        IF eqPos = -1 THEN
          (* it's an unbound option *)
          EVAL options.put(str, "1") (* hmm, dunno *)
        ELSE
          (* it's actually a value binding *)
          WITH k = Text.Sub(str,         0, eqPos),
               v = Text.Sub(str, eqPos + 1, LAST(CARDINAL)) DO
            Debug.Out(F("Options binding k=\"%s\" v=\"%s\"", k, v));
            EVAL options.put(k, v);

            IF TE("search", CitTextUtils.ToLower(k)) THEN
              search.addhi(Unquote(v))
            END
          END
        END
      END
    END
  END ParseOptionLine;

PROCEDURE Unquote(txt : TEXT) : TEXT =
  BEGIN
    CASE Text.GetChar(txt, 0) OF
      '\'' =>
      WITH n = Text.Length(txt),
           l = Text.GetChar(txt, n - 1),
           s = Text.Sub(txt, 1, n - 2) DO
        <*ASSERT l = '\''*>
        RETURN s
      END
    |
      '"' =>
      WITH n = Text.Length(txt),
           l = Text.GetChar(txt, n - 1),
           s = Text.Sub(txt, 1, n - 2) DO
        <*ASSERT l = '"'*>
        RETURN s
      END
    ELSE
      RETURN txt
    END
  END Unquote;
  
VAR options := NEW(TextTextTbl.Default).init();
  
PROCEDURE ParseSpice(rd : Rd.T; currentSearchDir, fn : Pathname.T) : T
  RAISES { SpiceError.E, Rd.Failure } =

  VAR
    res := NEW(Private,
               subCkts := NEW(TextSpiceCircuitTbl.Default).init(),
               subCktNames := NEW(TextSeq.T).init());


  PROCEDURE Recurse(rd : Rd.T; currentSearchDir, fn : Pathname.T)
    RAISES { SpiceError.E, Rd.Failure } =
    VAR
      buff := NEW(REF ARRAY OF CHAR, 100);
      p : CARDINAL;
      lNo : CARDINAL := 0;
      search := NEW(TextSeq.T).init();
    BEGIN
      LOOP
        WITH len = GetLine(rd, buff, lNo) DO
          IF len = -1 THEN 
            IF Verbose THEN Debug.Out("ParseSpice: EOF") END;
            RETURN 
          END;
          IF Verbose THEN
            Debug.Out("ParseSpice: " & Text.FromChars(SUBARRAY(buff^, 0, len)))
          END;

          p := 0;
          WHILE p < len AND buff[p] IN White DO
            INC(p)
          END;

          IF HavePrefix(buff^, p, ".INCLUDE") THEN 
            Debug.Out(".INCLUDE : ");
            DebugOut(SUBARRAY(buff^, p, len - p));
            VAR
              q := LAST(CARDINAL);
              lookup : FileLookup;
            BEGIN
              IF buff[p] # '\'' AND buff[p] # '"' THEN
                RAISE SpiceError.E(SpiceError.Data {
                msg := "Can't find filename in .INCLUDE", lNo := lNo, fn := fn
                })
              END;
              INC(p);
              FOR i := p TO len - 1 DO
                IF buff[i] = '\'' OR buff[i] = '"' THEN
                  q := i; EXIT
                END
              END;
              IF Verbose THEN
                Debug.Out(F("Filename, p=%s q=%s", Int(p), Int(q)))
              END;
              (* if q finite, success *)
              IF q = LAST(CARDINAL) THEN
                RAISE SpiceError.E(SpiceError.Data {
                msg := "Can't find filename in .INCLUDE", lNo := lNo, fn := fn
                })
              END;
              WITH fn     = Text.FromChars(SUBARRAY(buff^, p, q - p)) DO
                TRY
                  TRY
                    lookup := ResolvePath(currentSearchDir, fn, search);
                  EXCEPT
                    SpiceError.E(e) =>
                    VAR f := e; BEGIN
                      f.fn := fn;
                      f.lNo := lNo;
                      RAISE SpiceError.E(f)
                    END
                  END;

                  IF Verbose THEN
                    Debug.Out(F("ParseSpice: found .INCLUDE file %s to parse resolving to %s",
                                fn,
                                lookup.tgtFileName))
                  END;
                  Recurse(lookup.rd, lookup.newSearchDir, lookup.tgtFileName)
                EXCEPT
                  OSError.E => Debug.Error(F("Can't find .INCLUDE file %s",fn))
                END
              END
            END
          ELSIF HavePrefix(buff^, p, ".OPTION") THEN
            Debug.Out(".OPTION : ");
            DebugOut(SUBARRAY(buff^, p, len - p));
            ParseOptionLine(SUBARRAY(buff^, p, len - p), search)


          ELSIF HavePrefix(buff^, p, ".LIB") THEN
            Debug.Out(".LIB : ");
            DebugOut(SUBARRAY(buff^, p, len - p))
          ELSE
            TRY
              WHILE p < len AND buff[p] IN White DO INC(p) END;
              VAR
                warning : TEXT := NIL;
              BEGIN
                SpiceObjectParse.ParseLine(res.circuit,
                                           res.subCkts,
                                           res.subCktNames,
                                           SUBARRAY(buff^, p, len - p),
                                           warning);
                IF warning # NIL THEN
                  Debug.Warning(F("SpiceFormat.ParseSpice : file %s line %s : %s",
                                  fn, Int(lNo), warning))
                END
              END
                
              
            EXCEPT
              SpiceError.E(e) =>
              VAR f := e; BEGIN
                f.lNo := lNo;
                f.fn := fn;
                RAISE SpiceError.E(f)
              END
            END
          END
        END
      END
    END Recurse;
    
  BEGIN 
    res.circuit := NIL;       

    WITH root = NEW(SpiceCircuit.T, 
                    name     := NIL, 
                    params   := NEW(TextSeq.T).init(),
                    elements := NEW(SpiceObjectSeq.T).init()) DO
      res.circuit := SpiceCircuitList.Cons(root, res.circuit)
    END;

    Recurse(rd, currentSearchDir, fn);
    res.topCkt := res.circuit.head; 
    RETURN res
  END ParseSpice;

BEGIN END SpiceFormat.
