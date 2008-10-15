(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeInputPort;
IMPORT AL, Rd;
FROM SchemeUtils IMPORT Error, Warn, Cons, List2, ListToVector;
FROM Scheme IMPORT Object, String, E;
IMPORT SchemeLongReal;
FROM SchemeSymbol IMPORT SymEq;
IMPORT SchemeBoolean, SchemeSymbol;
IMPORT CharSeq;
IMPORT Text;
IMPORT Scan, FloatMode, Lex, TextUtils, Wx;
FROM SchemeChar IMPORT IChr, Character, Delims, White, NumberChars;
IMPORT Thread;

TYPE Boolean = SchemeBoolean.T;

<* FATAL Thread.Alerted *>

REVEAL
  T = Public BRANDED Brand OBJECT
    rd : Rd.T;
    isPushedToken, isPushedChar := FALSE;
    pushedToken : Object := NIL;
    pushedChar : INTEGER := -1;
  METHODS

    nextToken() : Object RAISES { E } := NextToken;

    readTail(dotOK : BOOLEAN) : Object RAISES { E } := ReadTail;

  OVERRIDES
    getCh    :=  GetCh; 
    init     :=  Init;
    readChar :=  ReadChar;
    peekChar :=  PeekChar;
    pushChar :=  PushChar;
    popChar  :=  PopChar;
    peekCh   :=  PeekCh;
    read     :=  Read;
    close    :=  Close;
  END;

PROCEDURE Init(t : T; rd : Rd.T) : T = 
  BEGIN t.rd := rd; RETURN t END Init;

PROCEDURE GetCh(t : T) : INTEGER =
  BEGIN
    TRY
      RETURN ORD(Rd.GetChar(t.rd))
    EXCEPT
      Rd.EndOfFile => RETURN ChEOF 
    |
      Rd.Failure(err) =>
      EVAL Warn("Rd.Failure : " & AL.Format(err));
      RETURN ChEOF
    END
  END GetCh;

PROCEDURE ReadChar(t : T) : Object =
  BEGIN
(*
    TRY
*)
      IF t.isPushedChar THEN
        t.isPushedChar := FALSE;
        IF t.pushedChar = ChEOF THEN 
          RETURN EOF
        ELSE
          RETURN IChr(t.pushedChar)
        END
      ELSE
        WITH ch = t.getCh() DO
          IF ch = ChEOF THEN RETURN EOF ELSE RETURN IChr(ch) END
        END
      END
(*
    EXCEPT
      Rd.Failure(err) =>
        EVAL Warn("On input, exception: " & AL.Format(err));
        RETURN EOF
    END
*)
  END ReadChar;

PROCEDURE PeekChar(t : T) : Object =
  BEGIN
    WITH p = t.peekCh() DO
      IF p = ChEOF THEN RETURN EOF ELSE RETURN IChr(p) END
    END
  END PeekChar;

PROCEDURE PushChar(t : T; ch : INTEGER) : INTEGER =
  BEGIN
    t.isPushedChar := TRUE;
    t.pushedChar := ch;
    RETURN ch 
  END PushChar;

PROCEDURE PopChar(t : T) : INTEGER =
  BEGIN
    t.isPushedChar := FALSE;
    RETURN t.pushedChar
  END PopChar;

PROCEDURE PeekCh(t : T) : INTEGER =
  BEGIN
(*
    TRY
*)
      IF t.isPushedChar THEN
        RETURN t.pushedChar
      ELSE
        RETURN t.pushChar(t.getCh())
      END
(*
    EXCEPT
      Rd.Failure(err) => 
        EVAL Warn("On input, exception: " & AL.Format(err));
        RETURN ChEOF
    END
*)
  END PeekCh;

PROCEDURE Read(t : T) : Object RAISES { E } =

  CONST Symbol = SchemeSymbol.Symbol;

  BEGIN
(*
    TRY
*)
      WITH token = t.nextToken() DO
        IF    SymEq(token, "(") THEN
          RETURN t.readTail(FALSE)
        ELSIF SymEq(token, ")") THEN
          EVAL Warn("Extra ) ignored."); RETURN t.read()
        ELSIF SymEq(token, ".") THEN
          EVAL Warn("Extra . ignored."); RETURN t.read()
        ELSIF SymEq(token, "'") THEN
          RETURN List2(Symbol("quote"), t.read())
        ELSIF SymEq(token, "`") THEN
          RETURN List2(Symbol("quasiquote"), t.read())
        ELSIF SymEq(token, ",") THEN
          RETURN List2(Symbol("unquote"), t.read())
        ELSIF SymEq(token, ",@") THEN
          RETURN List2(Symbol("unquote-splicing"), t.read())
        ELSE
          RETURN token
        END
      END
(*
    EXCEPT
      Rd.Failure(err) => 
        EVAL Warn("On input, exception: " & AL.Format(err));
        RETURN EOF
    END
*)
  END Read;

PROCEDURE Close(t : T) : Boolean RAISES { E } =
  BEGIN
    TRY
      Rd.Close(t.rd);
      RETURN SchemeBoolean.True();
    EXCEPT
      Rd.Failure(err) => RETURN Error("IOException: " & AL.Format(err))
    END
  END Close;

PROCEDURE IsEOF(x : Object) : BOOLEAN = BEGIN RETURN x = EOF END IsEOF;

PROCEDURE ReadTail(t : T; 
                   <*UNUSED*>dotOK : BOOLEAN) : Object RAISES { E } =
  VAR token := t.nextToken();
  BEGIN
    IF    token = EOF THEN
      RETURN Error("EOF during read.")
    ELSIF SymEq(token,")") THEN
      RETURN NIL
    ELSIF SymEq(token,".") THEN
      WITH result = t.read() DO
        token := t.nextToken();
        IF NOT SymEq(token, ")") THEN
          EVAL Warn("Where's the ')'?  Got " & SchemeSymbol.ToText(token) &
            " after .")
        END;
        RETURN result
      END
    ELSE
      t.isPushedToken := TRUE;
      t.pushedToken := token;
      RETURN Cons(t.read(), t.readTail(TRUE))
    END
  END ReadTail;

PROCEDURE NextToken(t : T) : Object RAISES { E } =

  CONST Symbol = SchemeSymbol.Symbol;

  VAR ch : INTEGER;

  BEGIN
    IF t.isPushedToken THEN
      t.isPushedToken := FALSE;
      RETURN t.pushedToken
    ELSIF t.isPushedChar THEN
      ch := t.popChar()
    ELSE
      ch := t.getCh()
    END;

    WHILE ch # ChEOF AND VAL(ch,CHAR) IN White DO ch := t.getCh() END;

    CASE ch OF
      ChEOF => RETURN EOF;
    |
      ORD('('), ORD(')'), ORD('\''), ORD('`') => 
        RETURN Symbol(Text.FromChar(VAL(ch,CHAR)))
    |
      ORD(',') =>
        ch := t.getCh();
        IF ch = ORD('@') THEN
          RETURN Symbol(",@") 
        ELSE
          EVAL t.pushChar(ch); RETURN Symbol(",")
        END
    |
      ORD(';') =>
        WHILE ch # ChEOF AND ch # ORD('\n') AND ch # ORD('\r') DO
          ch := t.getCh()
        END;
        RETURN t.nextToken()
    |
      ORD(QMC) =>
        WITH buff = NEW(CharSeq.T).init() DO
          LOOP
            ch := t.getCh();
            IF ch = ORD(QMC) OR ch = ChEOF THEN EXIT END;
            IF ch = ORD(BSC) THEN
              buff.addhi(VAL(t.getCh(),CHAR))
            ELSE
              buff.addhi(VAL(ch,CHAR))
            END
          END;
          IF ch = ChEOF THEN EVAL Warn("EOF inside of a string.") END;
          RETURN CharSeqToArray(buff)
        END
    |
      ORD('#') =>
        ch := t.getCh();
        CASE ch OF
          ORD('t'), ORD('T') => RETURN SchemeBoolean.True()
        |
          ORD('f'), ORD('F') => RETURN SchemeBoolean.False()
        |
          ORD('(') =>
            EVAL t.pushChar(ch);
            RETURN ListToVector(t.read())
        | 
          ORD(BSC) =>
            ch := t.getCh();
            IF VAL(ch,CHAR) IN SET OF CHAR { 's', 'S', 'n', 'N' } THEN
              EVAL t.pushChar(ch);
              WITH token = t.nextToken() DO
                IF    SymEq(token, "space") THEN RETURN Character(' ') 
                ELSIF SymEq(token, "newline") THEN RETURN Character('\n')
                ELSE
                  (* this isn't right.. if we're parsing "#\n", for
                     instance, we'd be pushing the token "n"... *)
                  (*
                  t.isPushedToken := TRUE;
                  t.pushedToken := token;
                  *)

                  (* lop off start of token *)
                  WITH txt = SchemeSymbol.ToText(token) DO
                    IF Text.Length(txt) > 1 THEN
                      t.isPushedToken := TRUE;
                      t.pushedToken := SchemeSymbol.Symbol(Text.Sub(txt,1))
                    END
                  END;

                  RETURN IChr(ch)
                END
              END
            ELSE
              RETURN IChr(ch)
            END
        |
          ORD('e'), ORD('i'), ORD('d') => RETURN t.nextToken()
        |
          ORD('b'), ORD('o'), ORD('x') =>
            EVAL Warn("#" & Text.FromChar(VAL(ch,CHAR)) & 
                      " not implemented, ignored.");
            RETURN t.nextToken()
        ELSE
          EVAL Warn("#" & Text.FromChar(VAL(ch,CHAR)) & 
                    " not recognized, ignored.");
          RETURN t.nextToken()
        END
    ELSE
      WITH wx = Wx.New(),
           c  = VAL(ch,CHAR) DO
        REPEAT
          Wx.PutChar(wx, VAL(ch, CHAR));
          ch := t.getCh()
        UNTIL
          ch = ChEOF OR VAL(ch,CHAR) IN White OR VAL(ch,CHAR) IN Delims;

        EVAL t.pushChar(ch);

        IF c IN NumberChars THEN
          WITH txt = Wx.ToText(wx) DO
            TRY
              WITH lr = Scan.LongReal(txt), 
                   lrp = NEW(SchemeLongReal.T) DO
                lrp^ := lr;
                RETURN lrp
              END
            EXCEPT
              Lex.Error, FloatMode.Trap => 
                Wx.PutText(wx, txt) (* restore it *)
            END
          END
        END;

        RETURN Symbol(TextUtils.ToLower(Wx.ToText(wx)))
      END
    END
  END NextToken;

PROCEDURE CharSeqToArray(seq : CharSeq.T) : String =
  BEGIN
    WITH res = NEW(String, seq.size()) DO
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := seq.get(i)
      END;
      RETURN res
    END
  END CharSeqToArray;


CONST BSC = '\\'; QMC = '"';

BEGIN
  EOF := SchemeSymbol.Symbol("#!EOF");
END SchemeInputPort.
