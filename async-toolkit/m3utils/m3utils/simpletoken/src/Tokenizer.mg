(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC MODULE Tokenizer(Defs);
FROM Defs IMPORT BufSiz, Special, White, Ident1, Ident2, CComments,
                 DoString, StringQuote;

IMPORT Thread, Rd;
IMPORT Text, TextUtils;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT IO;
IMPORT Compiler;

CONST TL = Compiler.ThisLine;
      TF = Compiler.ThisFile;
      
VAR doDebug := Debug.DebugThis(TextUtils.FilterIdent(Brand));

PROCEDURE Refill(VAR buf : Buffer; VAR st : State)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax }  =
  BEGIN
    (* existing token starts at s, b is at or past end *)
    
    (* shift down the old token 
       st.s <- 0
    *)
    WITH bufRem       = BufSiz - st.s DO
      SUBARRAY(buf, 0, bufRem) := SUBARRAY(buf, st.s, bufRem);
      DEC(st.b, st.s);
      DEC(st.e, st.s);
      st.s := 0;
    END;
    
    (* refill buffer *)
    
    IF BufSiz - st.b < 2 THEN RAISE
      (* token too long -- need 2 for comments *)
      Syntax(E{TF(),TL()})  
    END;

    (* we can fit some characters, let's get them *)
    
    WITH len = Rd.GetSub(st.rd, SUBARRAY(buf, st.e, BufSiz - st.e)) DO
      IF len = 0 THEN RAISE Rd.EndOfFile END;
      INC(st.bytes, len);
      st.e := st.e + len;
    END;
    
    <*ASSERT st.b # st.e AND st.b # st.e - 1*>
  END Refill;

PROCEDURE NextToken(VAR buf : Buffer; VAR st : State) 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax } =

  PROCEDURE PutPos() =
    BEGIN
      IO.Put("\r");
      IO.Put(Int(st.lineno));
      IO.Put(" lines ");
      IO.Put(Int(Rd.Index(st.rd)));
      IO.Put(" bytes")
    END PutPos;

  PROCEDURE Get(charsNeeded : CARDINAL)
    RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax }  =
    BEGIN
      IF st.b > st.e - charsNeeded THEN Refill(buf, st) END
    END Get;
    
  PROCEDURE Dbg() =
    BEGIN
      IF st.string THEN
        Debug.Out(F("Got string %s",
                    Text.FromChars(SUBARRAY(buf,
                                            st.s,
                                            st.b - st.s))))
      ELSE
        Debug.Out(F("Got token <%s>",
                    Text.FromChars(SUBARRAY(buf,
                                            st.s,
                                            st.b - st.s))))
      END;
    END Dbg;
  CONST
    SpecialPlusWS = Special + White;
    BackSlash     = '\\';
  BEGIN
    (* we should have consumed previous token *)
    <*ASSERT NOT st.haveTok*>

    st.string := FALSE;

    (* ensure we have text *)
    Get(2);

    (* read token from b onwards *)
    WHILE buf[st.b] IN White OR
      CComments AND buf[st.b] = '/' AND buf[st.b+1] = '*' DO
      WHILE buf[st.b] IN White DO

        IF buf[st.b] = '\n' THEN
          INC(st.lineno);
          IF st.progress AND st.lineno MOD 10000 = 0 THEN
            PutPos()
          END
        END;
        
        INC(st.b); (* skip *)
        INC(st.s);
        Get(2)
      END;

      IF CComments AND buf[st.b] = '/' AND buf[st.b + 1] = '*' THEN
        INC(st.b,2);
        Get(2);

        WHILE buf[st.b] # '*' OR buf[st.b + 1] # '/' DO
          IF buf[st.b] = '\n' THEN
            INC(st.lineno)
          END;
          
          INC(st.b); (* skip *)
          INC(st.s);
          Get(2);
        END;
        INC(st.b, 2);
        INC(st.s, 2);
        Get(2)
      END
      
    END;

    (* buf[b] is NOT whitespace : we are at start of token *)
    st.s := st.b;

    (* check for single character token *)
    <*ASSERT st.b # st.e*>
    IF    buf[st.b] IN Special THEN
      INC(st.b);
      st.haveTok := TRUE;
      IF doDebug THEN Dbg() END;
      RETURN 
    END;

    <*ASSERT st.b # st.e*>
    (* we are now not at whitespace nor at a special
       either at 
       -- a string 
       -- a number 
       -- an identifier 
    *)
    IF DoString AND buf[st.b] = StringQuote THEN
      VAR
        state : CARDINAL := 0;
      BEGIN
        INC(st.b); (* skip opening quote *)
        Get(1);
        st.haveTok := TRUE;
        
        LOOP
          IF    state = 0 AND buf[st.b] = BackSlash THEN
            state := 1
          ELSIF state = 1 THEN
            (* just skip this char *)
            state := 0
          ELSIF buf[st.b] = StringQuote THEN
            st.string := TRUE
          END;
          INC(st.b); 
          Get(1);
          IF st.string THEN
            IF doDebug THEN Dbg() END;
            RETURN
          END
        END
      END          
    END;
    
    (* we are now at a normal character, so we can execute the 
       following loop at least once *)
    REPEAT
      INC(st.b); 
      Get(1)
    UNTIL buf[st.b] IN SpecialPlusWS;

    (* we are at the end of a token *)
    st.haveTok := TRUE;
    IF doDebug THEN Dbg() END;
  END NextToken;

  (************************************************************)
  
PROCEDURE GetAny(VAR buf : ARRAY OF CHAR; VAR st : State;
                 VAR tok : Token) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    IF NOT st.haveTok THEN NextToken(buf, st) END;
    
    tok.s := st.s;
    tok.n := st.b - st.s;
    st.haveTok := FALSE;
    RETURN NOT st.haveTok
  END GetAny;

PROCEDURE GetExact(VAR buf : ARRAY OF CHAR; VAR st : State;
                   READONLY str : ARRAY OF CHAR) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    IF NOT st.haveTok THEN NextToken(buf, st) END;

    st.haveTok := SUBARRAY(buf, st.s, st.b - st.s) # str;
    RETURN NOT st.haveTok
  END GetExact;

PROCEDURE GetIdent(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR str : Token) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    IF NOT st.haveTok THEN NextToken(buf, st) END;

    IF NOT buf[st.s] IN Ident1 THEN RETURN FALSE END;

    FOR q := st.s + 1 TO st.b - 1 DO
      IF NOT buf[st.s] IN Ident2 THEN RETURN FALSE END
    END;

    str := Token { st.s, st.b - st.s };
    st.haveTok := FALSE;
    RETURN NOT st.haveTok
  END GetIdent;
      
PROCEDURE GetString(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR str : Token) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    IF NOT st.haveTok THEN NextToken(buf, st) END;

    IF NOT st.string THEN RETURN FALSE END;

    str := Token { st.s, st.b - st.s };
    st.haveTok := FALSE;
    RETURN NOT st.haveTok
  END GetString;
      
PROCEDURE GetInt(VAR buf : ARRAY OF CHAR; VAR st : State;
                 VAR int : INTEGER) : BOOLEAN 
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    neg := FALSE;
    q : CARDINAL;
    
    res := 0;
  BEGIN
    IF NOT st.haveTok THEN NextToken(buf, st) END;
    
    q := st.s;
    
    IF buf[q] = '-' THEN neg := TRUE; INC(q) END;
    
    IF q = st.b THEN RETURN FALSE END;
    
    WHILE q < st.b DO
      WITH c = buf[q] DO
        IF '0' <= c AND c <= '9' THEN
          res := res * 10 + ORD(c) - ORD('0');
          INC(q)
        ELSE
          RETURN FALSE
        END
      END
    END;
    
    IF neg THEN int := -res ELSE int := res END;
    
    st.haveTok := FALSE;
    RETURN TRUE
  END GetInt;
  
PROCEDURE GetFloat(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR lr : LONGREAL) : BOOLEAN 
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    neg := FALSE;
    q : CARDINAL;
    
    whole := 0;
    frac := 0.0d0;
  BEGIN
    IF NOT st.haveTok THEN NextToken(buf, st) END;
    
    q := st.s;
    
    IF buf[q] = '-' THEN neg := TRUE; INC(q) END;
    
    IF q = st.b THEN RETURN FALSE END;
    
    WHILE q < st.b DO
      WITH c = buf[q] DO
        IF '0' <= c AND c <= '9' THEN
          whole := whole * 10 + ORD(c) - ORD('0');
          INC(q)
        ELSIF buf[q] = '.' THEN
          EXIT
        ELSE
          RETURN FALSE
        END
      END
    END;
    
    (* done reading the integer part -- two poss.
       
       1. either done here
       2. we are at the radix point
    *)
    
    IF buf[q] = '.' THEN
      INC(q);(* skip *)
      
      (* now read fractional part *)
      VAR
        pos := 10.0d0; (* this is exact, 0.1 is not *)
      BEGIN
        WHILE q < st.b DO
          WITH c = buf[q] DO
            IF '0' <= c AND c <= '9' THEN
              frac := frac + FLOAT(ORD(c) - ORD('0'),LONGREAL) / pos;
              pos := pos * 10.0d0;
              INC(q);
            ELSE
              RETURN FALSE
            END
          END
        END
      END
    END;

    IF neg THEN lr := -(FLOAT(whole,LONGREAL) + frac)
    ELSE        lr :=   FLOAT(whole,LONGREAL) + frac
    END;

    st.haveTok := FALSE;
    RETURN TRUE
  END GetFloat;

BEGIN END Tokenizer.
