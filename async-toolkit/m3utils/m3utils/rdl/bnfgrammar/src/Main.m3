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
IMPORT Bnf;
IMPORT BnfSeq;
IMPORT TextBnfTbl;
IMPORT TextSet, TextSetDef;
IMPORT Wx;
IMPORT CharNames;

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

PROCEDURE Tok2Text(READONLY tok : Token) : TEXT =
  BEGIN
    RETURN Text.FromChars(SUBARRAY(buf, tok.s, tok.n))
  END Tok2Text;

VAR stringSet : TextSet.T := NEW(TextSetDef.T).init();
    
PROCEDURE GetFactor(VAR factor : Bnf.T) : BOOLEAN
  RAISES ANY =
  VAR
    ident : Token;
  BEGIN
    IF    GetIdent(buf, st, ident) THEN
      factor := NEW(Bnf.Ident, ident := Tok2Text(ident));
      RETURN TRUE
    ELSIF GetString(buf, st, ident) THEN
      WITH str = Tok2Text(ident) DO
        factor := NEW(Bnf.String, string := str);
        EVAL stringSet.insert(str)
      END;
      RETURN TRUE
    ELSIF GetExact(buf, st, LParen) THEN
      IF NOT GetExpression(factor) THEN RAISE Syntax(E{TF(),TL()}) END;
      IF NOT GetExact(buf, st, RParen) THEN RAISE Syntax(E{TF(),TL()}) END;
      RETURN TRUE
    ELSIF GetExact(buf, st, LBrace) THEN
      VAR
        expr : Bnf.T;
      BEGIN
        IF NOT GetExpression(expr) THEN RAISE Syntax(E{TF(),TL()}) END;
        IF NOT GetExact(buf, st, RBrace) THEN RAISE Syntax(E{TF(),TL()}) END;
        factor := NEW(Bnf.ListOf, elem := expr);
        RETURN TRUE
      END
    ELSIF GetExact(buf, st, LSquare) THEN  
      VAR
        expr : Bnf.T;
      BEGIN
        IF NOT GetExpression(expr) THEN RAISE Syntax(E{TF(),TL()}) END;
        IF NOT GetExact(buf, st, RSquare) THEN RAISE Syntax(E{TF(),TL()}) END;
        factor := NEW(Bnf.Optional, elem := expr);
        RETURN TRUE
      END
    ELSE
      RAISE Syntax(E{TF(),TL()})
    END
  END GetFactor;
  
PROCEDURE GetTerm(VAR term : Bnf.T) : BOOLEAN 
  RAISES ANY =
  VAR
    factor : Bnf.T;
    seq := NEW(BnfSeq.T).init();
  BEGIN
    IF NOT GetFactor(factor) THEN
      RETURN FALSE
    END;
    seq.addhi(factor);
    WHILE GetExact(buf, st, Pipe) DO
      IF NOT GetFactor(factor) THEN RAISE Syntax(E{TF(),TL()}) END;
      seq.addhi(factor);
    END;
    IF seq.size() = 1 THEN
      term := seq.get(0)
    ELSE
      WITH res = NEW(Bnf.Disjunction,
                     elems := NEW(REF ARRAY OF Bnf.T, seq.size()))  DO
        FOR i := FIRST(res.elems^) TO LAST(res.elems^) DO
          res.elems[i] := seq.get(i)
        END;
        term := res
      END;
    END;
    <*ASSERT term # NIL*>
    RETURN TRUE
  END GetTerm;
  
PROCEDURE GetExpression(VAR expr : Bnf.T) : BOOLEAN 
  RAISES ANY =
  VAR
    term : Bnf.T;
    seq := NEW(BnfSeq.T).init();
  BEGIN
    IF NOT GetTerm(term) THEN
      RETURN FALSE
    END;
    seq.addhi(term);
    WHILE GetExact(buf, st, Comma) DO
      IF NOT GetTerm(term) THEN RAISE Syntax(E{TF(),TL()}) END;
      seq.addhi(term);
    END;
    IF seq.size() = 1 THEN
      expr := seq.get(0)
    ELSE
      WITH res = NEW(Bnf.Sequence,
                     elems := NEW(REF ARRAY OF Bnf.T, seq.size()))  DO
        FOR i := FIRST(res.elems^) TO LAST(res.elems^) DO
          res.elems[i] := seq.get(i)
        END;
        expr := res
      END
    END;
    <*ASSERT expr # NIL*>
    RETURN TRUE
  END GetExpression;
  
PROCEDURE GetSyntaxRule(VAR nm : TEXT; VAR expr : Bnf.T) : BOOLEAN
  RAISES ANY =
  BEGIN
    IF NOT GetIdent(buf, st, tok) THEN
      RETURN FALSE
    END;
    nm := Tok2Text(tok);
    IF NOT GetExact(buf, st, Equal) THEN
      RAISE Syntax(E{TF(),TL()})
    END;
    IF NOT GetExpression(expr) THEN
      RAISE Syntax(E{TF(),TL()})
    END;
    IF NOT GetExact(buf, st, Semi) THEN
      RAISE Syntax(E{TF(),TL()})
    END;
    RETURN TRUE
  END GetSyntaxRule;

  (**********************************************************************)

PROCEDURE PatchTypes(rn : TEXT) =
  VAR
    rootType : Bnf.T;
    fails, found := NEW(TextSetDef.T).init();
  BEGIN
    (* do we want to start from the root type or walk the whole symbol 
       table? *)
    IF NOT symtab.get(rn, rootType) THEN
      Debug.Error("Unknown root type : " & rn)
    END;

    <*ASSERT rootType # NIL*>

    Bnf.VisitPre(rootType, NEW(PatchVisitor,
                               fails := fails,
                               found := found));

    IF doDebug THEN
      Debug.Out(F("No mapping for %s type names:", Int(fails.size())));
      VAR
        iter := fails.iterate();
        txt : TEXT;
      BEGIN
        WHILE iter.next(txt) DO
          Debug.Out("No syntax definition for " & txt)
        END
      END;

      EVAL found.insert(rn);
      
      VAR
        iter := symtab.iterate();
        nm : TEXT;
        bnf : Bnf.T;
      BEGIN
        WHILE iter.next(nm, bnf) DO
          IF NOT found.member(nm) THEN
            Debug.Warning("Syntax rule not used : " & nm)
          END
        END
      END
    END
        
  END PatchTypes;

TYPE
  PatchVisitor = Bnf.Visitor OBJECT
    fails : TextSet.T;
    found : TextSet.T;
  OVERRIDES
    visit := PatchVisit
  END;

PROCEDURE MapString(str : TEXT) : TEXT =
  VAR
    in := Text.Length(str);
    wx := Wx.New();
    to : TEXT;
  BEGIN
    FOR i := 1 TO in - 2 DO
      WITH c = Text.GetChar(str, i) DO
        IF CharNames.Map(c, to) THEN
          Wx.PutText(wx, to)
        ELSE
          Wx.PutChar(wx, c)
        END
      END
    END;
    RETURN Wx.ToText(wx)
  END MapString;

PROCEDURE PatchVisit(v : PatchVisitor; bnf : Bnf.T) =
  VAR
    mapping : Bnf.T;
  BEGIN
    TYPECASE bnf OF
      Bnf.Ident(id) =>
      <*ASSERT id # NIL*>
      <*ASSERT id.ident # NIL*>
      <*ASSERT symtab # NIL*>

      IF id.def = NIL THEN

        Debug.Out("Seeking mapping for ident " & id.ident);
      
        IF NOT symtab.get(id.ident, mapping) THEN
          EVAL v.fails.insert(id.ident)
        ELSE
          (* if we get here, we are updating the definition of ourselves
             with the link to the definition of our name *)
          EVAL v.found.insert(id.ident);
          id.def := mapping;
          Bnf.VisitPre(mapping, v)
        END
      END
    ELSE
      (* skip *)
    END
  END PatchVisit;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  symtab := NEW(TextBnfTbl.Default).init();
  rootType : TEXT := NIL;
BEGIN
  TRY
    IF pp.keywordPresent("-r") THEN
      rootType := pp.getNext()
    END;
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
    VAR
      nm   : TEXT;
      expr : Bnf.T;
    BEGIN
      WHILE GetSyntaxRule(nm, expr) DO
        <*ASSERT nm # NIL*>
        <*ASSERT expr # NIL*>
        EVAL symtab.put(nm, expr)
      END
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
  END;

  Debug.Out("Parsing done");
  
  IF doDebug THEN
    Debug.Out(F("Main.m3:Got %s unique strings", Int(stringSet.size())));
    VAR
      iter := stringSet.iterate();
      str : TEXT;
    BEGIN
      WHILE iter.next(str) DO
        WITH mapStr = MapString(str) DO
          Debug.Out("String " & str & " -> " & mapStr)
        END
      END
    END
  END;

  IF rootType # NIL THEN
    PatchTypes(rootType)
  END

END Main.
