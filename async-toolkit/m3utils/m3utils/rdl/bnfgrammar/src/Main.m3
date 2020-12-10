MODULE Main;
IMPORT Stdio, Rd;
IMPORT Thread;
IMPORT Compiler;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT BnfTokenizer;
FROM BnfTokenizer IMPORT Syntax, GetIdent, GetExact, E, Token, GetString;
IMPORT AL;
IMPORT ParseParams;
IMPORT OSError;
IMPORT FileRd;
IMPORT ExceptionInfo;

VAR doDebug := Debug.GetLevel() >= 10 AND Debug.This("BnfGrammar");

TYPE AC = ARRAY OF CHAR;

CONST TL = Compiler.ThisLine;
      TF = Compiler.ThisFile;
      TE = Text.Equal;
            
VAR
  st  : BnfTokenizer.State;
  buf : BnfTokenizer.Buffer;
  tok : BnfTokenizer.Token;

  (* oddly the usage suggests that | binds tighter than , 

     so we have

     ( a, b, c | ( d , e ) ) | e
                  -------
                  factor
             -------------
               term
       -  -                    
       terms                   
     ----------------------    -
       factor                factor
  *)

CONST LParen  = AC { '(' };
      RParen  = AC { ')' };

      LBrace  = AC { '{' };
      RBrace  = AC { '}' };

      LSquare = AC { '[' };
      RSquare = AC { ']' };

      Pipe    = AC { '|' };
      Comma   = AC { ',' };
      Equal   = AC { '=' };
      Semi    = AC { ';' };

PROCEDURE GetFactor() : BOOLEAN
  RAISES ANY =
  VAR
    ident : Token;
  BEGIN
    IF    GetIdent(buf, st, ident) THEN
      RETURN TRUE
    ELSIF GetString(buf, st, ident) THEN
      RETURN TRUE
    ELSIF GetExact(buf, st, LParen) THEN  
      IF NOT GetExpression() THEN RAISE Syntax(E{TF(),TL()}) END;
      IF NOT GetExact(buf, st, RParen) THEN RAISE Syntax(E{TF(),TL()}) END;
      RETURN TRUE
    ELSIF GetExact(buf, st, LBrace) THEN  
      IF NOT GetExpression() THEN RAISE Syntax(E{TF(),TL()}) END;
      IF NOT GetExact(buf, st, RBrace) THEN RAISE Syntax(E{TF(),TL()}) END;
      RETURN TRUE
    ELSIF GetExact(buf, st, LSquare) THEN  
      IF NOT GetExpression() THEN RAISE Syntax(E{TF(),TL()}) END;
      IF NOT GetExact(buf, st, RSquare) THEN RAISE Syntax(E{TF(),TL()}) END;
      RETURN TRUE
    ELSE
      RAISE Syntax(E{TF(),TL()})
    END
  END GetFactor;
  
PROCEDURE GetTerm() : BOOLEAN 
  RAISES ANY =
  BEGIN
    IF NOT GetFactor() THEN
      RETURN FALSE
    END;
    WHILE GetExact(buf, st, Pipe) DO
      IF NOT GetFactor() THEN RAISE Syntax(E{TF(),TL()}) END
    END;
    RETURN TRUE
  END GetTerm;
  
PROCEDURE GetExpression() : BOOLEAN 
  RAISES ANY =
  BEGIN
    IF NOT GetTerm() THEN
      RETURN FALSE
    END;
    WHILE GetExact(buf, st, Comma) DO
      IF NOT GetTerm() THEN RAISE Syntax(E{TF(),TL()}) END
    END;
    RETURN TRUE
  END GetExpression;
  
PROCEDURE GetSyntaxRule() : BOOLEAN
  RAISES ANY =
  BEGIN
    IF NOT GetIdent(buf, st, tok) THEN
      RETURN FALSE
    END;
    IF NOT GetExact(buf, st, Equal) THEN
      RAISE Syntax(E{TF(),TL()})
    END;
    IF NOT GetExpression() THEN
      RAISE Syntax(E{TF(),TL()})
    END;
    IF NOT GetExact(buf, st, Semi) THEN
      RAISE Syntax(E{TF(),TL()})
    END;
    RETURN TRUE
  END GetSyntaxRule;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
BEGIN
  TRY
    IF pp.keywordPresent("-f") THEN
      WITH fn = pp.getNext() DO
        IF TE(fn,"-") THEN
          st.rd := Stdio.stdin
        ELSE
          TRY
            st.rd := FileRd.Open(fn)
          EXCEPT
            OSError.E(e) => Debug.Error(F("Main.m3: trouble opening \"%s\" : OSError.E : %s",
                                          fn,
                                          AL.Format(e)))
          END
        END
      END
    END;
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF st.rd = NIL THEN Debug.Error("Must specify input with -f") END;

  TRY
    WHILE GetSyntaxRule() DO
    END
  EXCEPT
    Syntax(e) =>
    Debug.Error(F("Syntax error on input line %s [%s:%s]",
                  Int(st.lineno), e.file, Int(e.line)))
  |
    Rd.EndOfFile => (* done *)
  |
    Thread.Alerted => Debug.Error("Thread.Alerted")
  |
    Rd.Failure(x) => Debug.Error("Rd.Failure : " & AL.Format(x))
  ELSE
    Debug.Error("Unexpected exception\n" & ExceptionInfo.Fmt(Compiler.ThisException()));
    <*ASSERT FALSE*>
  END
END Main.
