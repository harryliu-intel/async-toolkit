MODULE BraceParse;
IMPORT Rd, Thread;
FROM Fmt IMPORT F, Int;
IMPORT Debug;
IMPORT Text;
IMPORT NetKeywords AS N;
IMPORT Compiler;
IMPORT IO;

CONST BufSiz = 16384;
TYPE  Buffer = ARRAY [ 0 .. BufSiz-1 ] OF CHAR;

TYPE SC = SET OF CHAR;
     CA = ARRAY OF CHAR;
     
CONST WhiteSpace = SC { ' ', '\t', '\n', '\r' };
      Special    = SC { '{', '}', '=' };
      Digit      = SC { '0' .. '9' };
      Lower      = SC { 'a' .. 'z' };
      Upper      = SC { 'A' .. 'Z' };
      Letter     = Lower + Upper;
      Ident1     = Letter + SC { '_' };
      Ident      = Ident1 + Digit;

      LB = CA { '{' };
      RB = CA { '}' };
      EQ = CA { '=' };

CONST TL = Compiler.ThisLine;
      
VAR doDebug := Debug.GetLevel() >= 10;
    
PROCEDURE Parse(rd : Rd.T)
  RAISES { Rd.Failure, Thread.Alerted } =

  VAR
    buf       : Buffer;
    totBytes  : CARDINAL := 0;

    b : CARDINAL := 0;         (* buffer pointer  *)
    e : CARDINAL := 0;         (* end of buffer   *)
    s : CARDINAL := BufSiz;    (* start of token  *)

    lev := 0;

    lineno := 1;

  PROCEDURE Refill()
    RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax }  =
    BEGIN
      (* existing token starts at s, b is at end *)
      
      (* shift down the old token *)
      WITH tokSoFar = BufSiz - s DO
        SUBARRAY(buf, 0, tokSoFar) := SUBARRAY(buf, s, tokSoFar);
        s := 0;
        b := tokSoFar
      END;

      (* refill buffer *)

      IF BufSiz - b < 2 THEN RAISE 
        Syntax(TL())  (* token too long -- need 2 for comments *)
      END;
      WITH len = Rd.GetSub(rd, SUBARRAY(buf, b, BufSiz - b)) DO
        IF len = 0 THEN RAISE Rd.EndOfFile END;
        INC(totBytes, len);
        e := b + len;
      END;

      <*ASSERT b # e AND b # e - 1*>
    END Refill;

  VAR haveTok := FALSE;

  PROCEDURE NextToken() 
    RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax } =

    PROCEDURE Dbg() =
      BEGIN
        Debug.Out(F("Lev %s got token %s",
                    Int(lev),
                    Text.FromChars(SUBARRAY(buf,
                                            s,
                                            b - s))))
      END Dbg;

    BEGIN
      (* we should have consumed previous token *)
      <*ASSERT NOT haveTok*>
      
      (* ensure we have text *)
      IF b = e THEN Refill() END;
      
      (* read token from b onwards *)
      WHILE buf[b] IN WhiteSpace OR
            buf[b] = '/' AND buf[b+1] = '*' DO
        WHILE buf[b] IN WhiteSpace DO

          IF buf[b] = '\n' THEN
            INC(lineno);
            IF lineno MOD 10000 = 0 THEN
              IO.Put("\r");
              IO.Put(Int(lineno));
              IO.Put(" lines ");
              IO.Put(Int(Rd.Index(rd)));
              IO.Put(" bytes")
            END
          END;
        
          INC(b); (* skip *)
        
          IF b = e THEN Refill() END;
        END;

        IF buf[b] = '/' AND buf[b+1] = '*' THEN
          INC(b,2);

          WHILE buf[b] # '*' AND buf[b+1] # '/' DO
            IF buf[b] = '\n' THEN
              INC(lineno)
            END;
        
            INC(b); (* skip *)
        
            IF b = e THEN Refill() END;
          END;
          INC(b,2)
        END
            
      END;

      (* buf[b] is NOT whitespace : we are at start of token *)
      s := b;

      (* check for single character token *)
      <*ASSERT b # e*>
      IF    buf[b] IN Special THEN
        IF    buf[b] = '{' THEN INC(lev)
        ELSIF buf[b] = '}' THEN DEC(lev)
        END;
        
        INC(b);
        haveTok := TRUE;
        IF doDebug THEN Dbg() END;
        RETURN 
      END;

      <*ASSERT b # e*>
      WHILE NOT buf[b] IN Special + WhiteSpace DO
        INC(b); 

        IF b = e THEN Refill() END
      END;

      (* we are at the end of a token *)
      haveTok := TRUE;
      IF doDebug THEN Dbg() END;
    END NextToken;

    (************************************************************)
    
  PROCEDURE GetAny(VAR tok : Token) : BOOLEAN
    RAISES ANY =
    BEGIN
      IF NOT haveTok THEN NextToken() END;
      
      tok.s := s;
      tok.n := b - s;
      haveTok := FALSE;
      RETURN NOT haveTok
    END GetAny;

  PROCEDURE GetExact(READONLY str : ARRAY OF CHAR) : BOOLEAN
    RAISES ANY =
    BEGIN
      IF NOT haveTok THEN NextToken() END;

      haveTok := SUBARRAY(buf, s, b - s) # str;
      RETURN NOT haveTok
    END GetExact;

  PROCEDURE GetIdent(VAR str : Token) : BOOLEAN
    RAISES ANY =
    BEGIN
      IF NOT haveTok THEN NextToken() END;

      IF NOT buf[s] IN Ident1 THEN RETURN FALSE END;

      FOR q := s + 1 TO b - 1 DO
        IF NOT buf[s] IN Ident THEN RETURN FALSE END
      END;

      str := Token { s, b - s };
      haveTok := FALSE;
      RETURN NOT haveTok
    END GetIdent;

  PROCEDURE GetInt(VAR int : INTEGER) : BOOLEAN 
    RAISES ANY =
    VAR
      neg := FALSE;
      q : CARDINAL;

      res := 0;
    BEGIN
      IF NOT haveTok THEN NextToken() END;

      q := s;

      IF buf[q] = '-' THEN neg := TRUE; INC(q) END;

      IF q = b THEN RETURN FALSE END;

      WHILE q < b DO
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

      haveTok := FALSE;
      RETURN TRUE
    END GetInt;

  PROCEDURE GetFloat(VAR lr : LONGREAL) : BOOLEAN 
    RAISES ANY =
    VAR
      neg := FALSE;
      q : CARDINAL;

      whole := 0;
      frac := 0.0d0;
    BEGIN
      IF NOT haveTok THEN NextToken() END;

      q := s;

      IF buf[q] = '-' THEN neg := TRUE; INC(q) END;

      IF q = b THEN RETURN FALSE END;

      WHILE q < b DO
        WITH c = buf[q] DO
          IF '0' <= c AND c <= '9' THEN
            whole := whole * 10 + ORD(c) - ORD('0');
            INC(q)
          ELSE
            EXIT
          END
        END
      END;

      (* done reading the integer part *)

      IF q # b THEN
        IF buf[q] = '.' THEN
          INC(q);
          (* read fractional part *)
          WHILE q < b DO
            VAR
              pos := 10.0d0; (* this is exact, 0.1 is not *)
            BEGIN
              WITH c = buf[q] DO
                IF '0' <= c AND c <= '9' THEN
                  frac := frac + FLOAT(ORD(c) - ORD('0'),LONGREAL) / pos;
                  pos := pos * 10.0d0;
                  INC(q);
                ELSE
                  EXIT
                END
              END
            END
          END
        ELSE
          RETURN FALSE
        END
      END;

      IF neg THEN lr := -(FLOAT(whole,LONGREAL) + frac)
      ELSE        lr :=   FLOAT(whole,LONGREAL) + frac
      END;

      haveTok := FALSE;
      RETURN TRUE
    END GetFloat;
    
    (************************************************************)

  <*UNUSED*>PROCEDURE Trivial() 
    RAISES ANY =
    VAR
      tok : Token;
    BEGIN
      WHILE GetAny(tok) DO
        IF doDebug THEN
          Debug.Out(F("Lev %s got token %s",
                      Int(lev),
                      Text.FromChars(SUBARRAY(buf,
                                              tok.s,
                                              tok.n))))
        END
      END
    END Trivial;

    (************************************************************)

  PROCEDURE GetPin() : BOOLEAN 
    RAISES ANY =
    VAR
      exName, inName : Token;
    BEGIN
      IF NOT GetExact(N.PINkw) THEN RETURN FALSE END;

      LOOP
        IF GetExact(RB) THEN
          RETURN TRUE
        ELSE
          IF NOT GetIdent(exName) THEN RAISE Syntax(TL()) END;
          IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
          IF NOT GetIdent(inName) THEN RAISE Syntax(TL()) END;
        END
      END
    END GetPin;

  PROCEDURE GetCoord() : BOOLEAN 
    RAISES ANY =
    VAR
      axis : Token;
      pos : LONGREAL;
    BEGIN
      IF NOT GetExact(N.COORDkw) THEN RETURN FALSE END;

      (* at least two coords *)
      IF NOT GetIdent(axis) THEN RAISE Syntax(TL()) END;
      IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
      IF NOT GetFloat(pos) THEN RAISE Syntax(TL()) END;
      IF NOT GetIdent(axis) THEN RAISE Syntax(TL()) END;
      IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
      IF NOT GetFloat(pos) THEN RAISE Syntax(TL()) END;

      LOOP
        IF GetExact(RB) THEN
          RETURN TRUE
        ELSIF GetIdent(axis) THEN 
          IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
          IF NOT GetFloat(pos) THEN RAISE Syntax(TL()) END;
        ELSE
          RAISE Syntax(TL())
        END
      END
    END GetCoord;

  PROCEDURE GetPort() : BOOLEAN 
    RAISES ANY =
    VAR
      portNm : Token;
    BEGIN
      IF NOT GetExact(N.PORTkw) THEN RETURN FALSE END;

      LOOP
        IF    GetIdent(portNm) THEN
        ELSIF GetExact(RB) THEN
          RETURN TRUE
        ELSE
          RAISE Syntax(TL())
        END
      END
    END GetPort;

  PROCEDURE GetInst() : BOOLEAN 
    RAISES ANY =
    VAR
      instNm, typeNm : Token;
    BEGIN 
      IF NOT GetExact(N.INSTkw) THEN RETURN FALSE END;
      IF NOT GetIdent(instNm) THEN RAISE Syntax(TL()) END;
      IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
      IF NOT GetIdent(typeNm) THEN RAISE Syntax(TL()) END;
      LOOP
        IF GetExact(LB) THEN
          IF GetType() THEN
          ELSIF GetCoord() THEN
          ELSIF GetProp() THEN
          ELSIF GetPin() THEN
          ELSE
            RAISE Syntax(TL())
          END
        ELSIF GetExact(RB) THEN
          RETURN TRUE
        END
      END
    END GetInst;
    
  PROCEDURE GetType() : BOOLEAN 
    RAISES ANY =
    VAR
      typeType : Token;
    BEGIN
      IF NOT GetExact(N.TYPEkw) THEN RETURN FALSE END;

      IF NOT GetIdent(typeType) THEN RAISE Syntax(TL()) END;
      IF NOT GetExact(RB) THEN RAISE Syntax(TL()) END;
      RETURN TRUE
    END GetType;
    
  PROCEDURE GetProp() : BOOLEAN 
    RAISES ANY =
    VAR
      propNm : Token;
      propVal : LONGREAL;
    BEGIN
      IF NOT GetExact(N.PROPkw) THEN RETURN FALSE END;

      LOOP
        IF    GetExact(RB) THEN
          RETURN TRUE
        ELSIF GetIdent(propNm) AND GetExact(EQ) AND GetFloat(propVal) THEN
          (* here's the action! *)
        ELSE
          RAISE Syntax(TL())
        END
      END   
    END GetProp;
    
  PROCEDURE GetCell() : BOOLEAN 
    RAISES ANY =
    VAR
      nm : Token;
    BEGIN
      IF NOT GetExact(N.CELLkw) THEN RETURN FALSE END;

      IF NOT GetIdent(nm) THEN RETURN FALSE END;

      LOOP
        IF GetExact(LB) THEN
          IF    GetPort() THEN
          ELSIF GetProp() THEN
          ELSIF GetInst() THEN
          ELSE
            RAISE Syntax(TL()) 
          END
        ELSIF GetExact(RB) THEN
          RETURN TRUE
        ELSE
          RAISE Syntax(TL())
        END
      END
    END GetCell;
    
  PROCEDURE GetVersion() : BOOLEAN 
    RAISES ANY =
    VAR
      v0, v1, v2 : INTEGER;
    BEGIN
      IF NOT GetExact(N.VERSIONkw) THEN RETURN FALSE END;

      IF NOT GetInt(v0) THEN RAISE Syntax(TL()) END;
      IF NOT GetInt(v1) THEN RAISE Syntax(TL()) END;
      IF NOT GetInt(v2) THEN RAISE Syntax(TL()) END;

      IF NOT GetExact(RB) THEN RAISE Syntax(TL()) END;
      RETURN TRUE
    END GetVersion;

  PROCEDURE GetNetlist() : BOOLEAN 
    RAISES ANY =
    VAR
      nm : Token;
    BEGIN
      IF NOT GetExact(N.NETLISTkw) THEN RETURN FALSE END;

      IF NOT GetIdent(nm) THEN RETURN FALSE END;
      
      LOOP
        IF GetExact(RB) THEN
          RETURN TRUE
        ELSIF GetExact(LB) THEN
          IF    GetVersion() THEN
          ELSIF GetCell() THEN
          ELSE RAISE Syntax(TL())
          END
        ELSE
          RAISE Syntax(TL())
        END
      END
    END GetNetlist;
    
  PROCEDURE ParseTop() : BOOLEAN 
    RAISES ANY =
    BEGIN
      IF NOT GetExact(LB) THEN RETURN FALSE END;

      TRY
        IF GetNetlist() THEN
          RETURN TRUE
        ELSE
          RETURN FALSE
        END
      EXCEPT
        Rd.EndOfFile => RAISE Syntax(TL())
      END

    END ParseTop;

    (* complete set of exceptions is

    RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax } 

    *)
    
  BEGIN
    TRY
      (* Trivial() *)
      
      IF NOT ParseTop() THEN
        RAISE Syntax(TL())
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    |
      Rd.Failure(x) => RAISE Rd.Failure(x)
    |
      Thread.Alerted => RAISE Thread.Alerted
    |
      Syntax(line) =>
      Debug.Error(F("?Syntax error on line %s of input (%s:%s)",
                    Int(lineno),
                    Compiler.ThisFile(),
                    Int(line)))
    ELSE
      Debug.Error("Unexpected exception");
      <*ASSERT FALSE*>
    END;

    Debug.Out(F("Read %s bytes", Int(totBytes)))
  END Parse;

TYPE Token = RECORD s, n : CARDINAL END;

EXCEPTION Syntax(CARDINAL);
          
BEGIN
END BraceParse.
