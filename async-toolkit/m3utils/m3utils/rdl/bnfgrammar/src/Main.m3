MODULE Main;

(* 
   "BNF" parser for automated generation of parser using Karl's 
   parser system

   Author : Mika Nystrom <mika.nystroem@intel.com>
   December, 2020
*)

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
IMPORT TextBnfTbl, TextBnf;
IMPORT TextSet, TextSetDef;
IMPORT Wx;
IMPORT CharNames;
IMPORT Wr;
IMPORT Pathname;
IMPORT FileWr;
IMPORT TextRefTbl;
IMPORT TextBnfSeq;

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

<*FATAL Thread.Alerted*>
      
PROCEDURE Tok2Text(READONLY tok : Token) : TEXT =
  BEGIN
    RETURN Text.FromChars(SUBARRAY(buf, tok.s, tok.n))
  END Tok2Text;

VAR stringTbl : TextRefTbl.T := NEW(TextRefTbl.Default).init();

PROCEDURE MakeString(str : TEXT) : Bnf.String =
  VAR
    res : REFANY;
  BEGIN
    IF NOT stringTbl.get(str,res) THEN
      res := NEW(Bnf.String, string := str).init();
      EVAL stringTbl.put(str, res)
    END;
    RETURN res
  END MakeString;
  
PROCEDURE GetFactor(VAR factor : Bnf.T) : BOOLEAN
  RAISES ANY =
  VAR
    ident : Token;
  BEGIN
    IF    GetIdent(buf, st, ident) THEN
      factor := NEW(Bnf.Ident, ident := Tok2Text(ident)).init();
      RETURN TRUE
    ELSIF GetString(buf, st, ident) THEN
      WITH str = Tok2Text(ident) DO
        factor := MakeString(str)
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
        factor := NEW(Bnf.ListOf, elem := expr).init();
        RETURN TRUE
      END
    ELSIF GetExact(buf, st, LSquare) THEN  
      VAR
        expr : Bnf.T;
      BEGIN
        IF NOT GetExpression(expr) THEN RAISE Syntax(E{TF(),TL()}) END;
        IF NOT GetExact(buf, st, RSquare) THEN RAISE Syntax(E{TF(),TL()}) END;
        factor := NEW(Bnf.Optional, elem := expr).init();
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
      VAR
        elems := NEW(REF ARRAY OF Bnf.T, seq.size());
      BEGIN
        FOR i := FIRST(elems^) TO LAST(elems^) DO
          elems[i] := seq.get(i)
        END;
        term := NEW(Bnf.Disjunction, elems := elems).init();
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
      VAR
        elems := NEW(REF ARRAY OF Bnf.T, seq.size());
      BEGIN
        FOR i := FIRST(elems^) TO LAST(elems^) DO
          elems[i] := seq.get(i)
        END;
        expr := NEW(Bnf.Sequence, elems := elems).init();
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

PROCEDURE MapPrintable(str : TEXT) : TEXT =
  VAR
    in := Text.Length(str);
  BEGIN
    RETURN Text.Sub(str, 1, in - 2)
  END MapPrintable;
  
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

PROCEDURE OpenF(gramName : TEXT; outDir : Pathname.T; ext : TEXT) : Wr.T =
  VAR
    fn := gramName & "." & ext;
    qn := outDir & "/" & fn;
  BEGIN
    TRY
      RETURN FileWr.Open(qn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Trouble opening \"%s\" for writing : OSError.E : %s",
                    qn, AL.Format(x)));
      <*ASSERT FALSE*>
    END
  END OpenF;

PROCEDURE CloseF(wr : Wr.T; gramName : TEXT; outDir : Pathname.T; ext : TEXT) =
  VAR
    fn := gramName & "." & ext;
    qn := outDir & "/" & fn;
  BEGIN
    TRY
      Wr.Close(wr)
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error(F("Trouble closing \"%s\" for writing : Wr.Failure : %s",
                    qn, AL.Format(x)));
      <*ASSERT FALSE*>
    END
  END CloseF;

  (**********************************************************************)
  
PROCEDURE GenTokFile(wr : Wr.T)
  RAISES { Wr.Failure } =
  VAR
    iter := stringTbl.iterate();
    str : TEXT;
    dummy : REFANY;
  BEGIN
    IF tokHeader # NIL THEN
      Wr.PutText(wr, tokHeader);
      Wr.PutText(wr, "\n")
    END;

    WHILE iter.next(str, dummy) DO
      Wr.PutText(wr, F("%const %s\n", Str2Token(str)))
    END
  END GenTokFile;

PROCEDURE Str2Token(str : TEXT) : TEXT =
  BEGIN RETURN "T_" & MapString(str) END Str2Token;
  
PROCEDURE GenLexFile(wr : Wr.T)
  RAISES { Wr.Failure } =
  VAR
    iter := stringTbl.iterate();
    str : TEXT;
    dummy : REFANY;
  BEGIN
    IF lexHeader # NIL THEN
      Wr.PutText(wr, lexHeader);
      Wr.PutText(wr, "\n")
    END;

    WHILE iter.next(str, dummy) DO
      Wr.PutText(wr, F("T_%s \"%s\"\n", MapString(str), MapPrintable(str)))
    END
  END GenLexFile;

PROCEDURE CopySymtab() : TextBnfSeq.T =
  VAR
    new := NEW(TextBnfSeq.T).init();
    iter := symtab.iterate();
    nm : TEXT;
    x : Bnf.T;
  BEGIN
    WHILE iter.next(nm, x) DO
      new.addhi(TextBnf.T { nm, x })
    END;
    RETURN new
  END CopySymtab;

PROCEDURE PerformParseEdit(seq : TextBnfSeq.T; editor : Bnf.Editor) =
  VAR
    i := 0;
  BEGIN
    WHILE i # seq.size() DO
      WITH e = seq.get(i) DO
        seq.put(i, TextBnf.T { e.t, editor(e.b, seq, MapString) });
      END;
      INC(i)
    END
  END PerformParseEdit;

PROCEDURE GenYacFile(wr : Wr.T; seq : TextBnfSeq.T)
  RAISES { Wr.Failure } =
  VAR
    k := 0;
  BEGIN
    Wr.PutText(wr, F("%start %s\n", "%s", rootType));

    IF yaccHeader # NIL THEN
      Wr.PutText(wr, yaccHeader);
      Wr.PutText(wr, "\n")
    END;
    
    WHILE k < seq.size() DO
      WITH e = seq.get(k) DO

        TYPECASE e.b OF
          Bnf.Disjunction(dis) =>
          Debug.Out(F("%s disjunctive rule, %s disjuncts", e.t, Int(NUMBER(dis.elems^))))
        |
          Bnf.Sequence(seq) =>
          Debug.Out(F("%s sequence rule, %s steps", e.t, Int(NUMBER(seq.elems^))))
        ELSE
          Debug.Out(F("%s simple rule", e.t))
        END;
        
        Wr.PutText(wr, "\n" & e.t & ":\n");
        TYPECASE e.b OF Bnf.Disjunction(dis) =>
          FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
            DumpYaccRule(wr,
                         NameDisjunct(dis.elems[i], i),
                         dis.elems[i])
          END
        ELSE
          DumpYaccRule(wr,
                       "x",
                       e.b)
        END
      END;
      INC(k)
    END
  END GenYacFile;

PROCEDURE FormatBnf(x : Bnf.T) : TEXT =
  BEGIN
    TYPECASE x OF
      Bnf.String(str) => RETURN Str2Token(str.string)
    |
      Bnf.Sequence(seq) =>
      VAR
        wx := Wx.New();
      BEGIN
        FOR i := FIRST(seq.elems^) TO LAST(seq.elems^) DO
          Wx.PutChar(wx, ' ');
          Wx.PutText(wx, FormatBnf(seq.elems[i]));
        END;
        RETURN Wx.ToText(wx)
      END
    |
      Bnf.Ident(id) => RETURN id.ident
    |
      Bnf.Optional(opt) =>
      RETURN "(*optional* " & FormatBnf(opt.elem) & ")"
    |
      Bnf.ListOf(listof) =>
      RETURN "(*listof* " & FormatBnf(listof.elem) & ")"
    |
      Bnf.Disjunction(dis) =>
      VAR
        wx := Wx.New();
      BEGIN
        Wx.PutText(wx, "(*disjunction* ");
        FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
          Wx.PutChar(wx, ' ');
          Wx.PutText(wx, FormatBnf(dis.elems[i]));
        END;
        Wx.PutText(wx, ")");
        RETURN Wx.ToText(wx)
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END FormatBnf;

PROCEDURE DumpYaccRule(wr : Wr.T; nm : TEXT; expr : Bnf.T)
  RAISES { Wr.Failure } =
  BEGIN
    Wr.PutText(wr, F("  %s %s\n", nm, FormatBnf(expr)));

    
  END DumpYaccRule;

PROCEDURE NameDisjunct(x : Bnf.T; i : CARDINAL) : TEXT =
  BEGIN
    TYPECASE x OF
      Bnf.String(str) => RETURN MapString(str.string)
    |
      Bnf.Ident(id) => RETURN id.ident
    ELSE
      RETURN "dis" & Int(i)
    END
  END NameDisjunct;
  
  (**********************************************************************)

PROCEDURE WriteFiles(gramName : TEXT;
                     outDir   : Pathname.T;
                     seq : TextBnfSeq.T)
  RAISES { Wr.Failure } =
  VAR
    derQn := outDir & "/derived.m3m";

    derWr : Wr.T;
    
    tokWr := OpenF(gramName, outDir, "t");
    lexWr := OpenF(gramName, outDir, "l");
    yacWr := OpenF(gramName, outDir, "y");
  BEGIN
    TRY
      derWr := FileWr.Open(derQn);
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Trouble opening \"%s\" for writing : OSError.E : %s",
                    derQn, AL.Format(x)))
    END;

    Wr.PutText(derWr, F("Token(\"%s\")\n", gramName));
    Wr.PutText(derWr, F("Lexer(\"%s\",\"%s\")\n", gramName, gramName));
    Wr.PutText(derWr, F("Parser(\"%s\",\"%s\")\n", gramName, gramName));
    
    GenTokFile(tokWr);
    GenLexFile(lexWr);
    GenYacFile(yacWr, seq);

    Wr.Close(derWr);
    
    CloseF(tokWr, gramName, outDir, "t");
    CloseF(lexWr, gramName, outDir, "l");
    CloseF(yacWr, gramName, outDir, "y");
  END WriteFiles;

PROCEDURE RenameAllIdents(seq : TextBnfSeq.T;
                          fromSet : TextSet.T;
                          to : TEXT;
                          protected : TextSet.T) =
  VAR
    iter := fromSet.iterate();
    f : TEXT;
    ti := Bnf.MakeIdent(to);
    rule : TextBnf.T;
  BEGIN
    (* make all the substitutions *)
    WHILE iter.next(f) DO
      IF NOT protected.member(f) AND NOT TE(f, to) THEN
        Debug.Out(F("Renaming %s -> %s", f, to));
        FOR i := 0 TO seq.size() - 1 DO
          rule := seq.get(i);
          rule.b := Bnf.Substitute(rule.b, Bnf.MakeIdent(f), ti);
          seq.put(i, rule)
        END
      END
    END;

    (* now remove the named rules that we no longer need *)
    FOR i := seq.size() - 1 TO 0 BY -1 DO
      WITH rule = seq.get(i) DO
        IF NOT protected.member(rule.t) AND
           NOT TE(rule.t, to) AND
           fromSet.member(rule.t) THEN
          (* we need to remove rule i 
             swap with last and lop off last
          *)
          WITH last = seq.get(seq.size() - 1) DO
            seq.put(i, last)
          END;

          EVAL seq.remhi()
        END
      END
    END;
          
  END RenameAllIdents;
  
PROCEDURE EliminateOneIdentical(seq : TextBnfSeq.T) : BOOLEAN =
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      VAR
        set := NEW(TextSetDef.T).init();
      BEGIN
        FOR j := i + 1 TO seq.size() - 1 DO
          WITH ie = seq.get(i),
               je = seq.get(j) DO
            IF ie.b = je.b THEN
              Debug.Out(F("Rules the same: %s %s", ie.t, je.t));
              EVAL set.insert(ie.t);
              EVAL set.insert(je.t);
            END
          END
        END(*ROF*);

        IF set.size() > 1 THEN
          (* found an equivalence class,

             now we chose canonical names as follows:

             if no protected names, canonical is shortest name

             if protected names, canonical is shortest protected name
          *)
          VAR canonical : TEXT;
              myProtected := set.intersection(protected);
          BEGIN
            IF myProtected.size() = 0 THEN
              canonical := GetShortest(set)
            ELSE
              canonical := GetShortest(myProtected)
            END;
            RenameAllIdents(seq, set, canonical, protected)
          END;
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END EliminateOneIdentical;

PROCEDURE GetShortest(set : TextSet.T) : TEXT =
  VAR
    len := LAST(CARDINAL);
    res : TEXT := NIL;
    t   : TEXT;
    iter := set.iterate();
  BEGIN
    WHILE iter.next(t) DO
      IF Text.Length(t) < len THEN
        res := t;
        len := Text.Length(t)
      END
    END;
    RETURN res
  END GetShortest;

PROCEDURE EliminateIdenticals(seq : TextBnfSeq.T) =
  BEGIN
    WHILE EliminateOneIdentical(seq) DO (* skip *) END
  END EliminateIdenticals;
  
PROCEDURE EditParseTree(seq : TextBnfSeq.T) =
  CONST
    Phases = ARRAY OF Bnf.Editor { Bnf.RemoveNestedSequences,
                                   Bnf.DistributeAll,
                                   Bnf.RemoveSeqLists,
                                   Bnf.RemoveIdentLists,
                                   Bnf.RemoveNestedSequences,
                                   Bnf.RemoveSingletonSequences,
                                   Bnf.RemoveOptionalStringIdent,
                                   Bnf.RemoveRemainingOptionals
    };
  BEGIN
    (* perform parse edits *)

      Debug.Out(F("=====================   BEGIN EDIT PARSE TREE   ======================="));
    DebugDumpTree("start.tree", seq);
    IF doElimIdenticalRules THEN EliminateIdenticals(seq) END;
    FOR i := FIRST(Phases) TO LAST(Phases) DO

      Debug.Out(F("============================  EDIT PASS %s  ==========================", Int(i)));
      PerformParseEdit(seq, Phases[i]);
      DebugDumpTree("result" & Int(i)  & ".tree", seq);
      IF doElimIdenticalRules THEN EliminateIdenticals(seq) END;

    END;
      Debug.Out(F("======================   END EDIT PARSE TREE   ========================"));
    DebugDumpTree("stop" & ".tree", seq);
  END EditParseTree;    


PROCEDURE DebugDumpTree(fn : Pathname.T; seq : TextBnfSeq.T) =
  <*FATAL OSError.E, Wr.Failure*>
  VAR
    wr := FileWr.Open(fn);
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH q = seq.get(i) DO
        Wr.PutChar(wr, '\n');
        Wr.PutChar(wr, '\n');
        Wr.PutText(wr, q.t);
        Wr.PutChar(wr, '=');
        Wr.PutChar(wr, '\n');
        Wr.PutText(wr, Bnf.DebugBnf(q.b, 0))
      END
    END;
    Wr.Close(wr)
  END DebugDumpTree;

PROCEDURE ReadFile(fn : Pathname.T) : TEXT =
  VAR
    rd : Rd.T;
    wx := Wx.New();
  BEGIN
    TRY
      rd := FileRd.Open(fn);
    EXCEPT
      OSError.E (x) =>
      Debug.Error(F("Couldn't open \"%s\" for reading : OSError.E : %s",
                    fn, AL.Format(x)));
      <*ASSERT FALSE*>
    END;

    TRY
      LOOP
        VAR
          line := Rd.GetLine(rd);
        BEGIN
          Wx.PutText(wx, line);
          Wx.PutChar(wx, '\n')
        END
      END
    EXCEPT
      Rd.EndOfFile =>
      TRY Rd.Close(rd); EXCEPT ELSE <*ASSERT FALSE*> END;
      RETURN Wx.ToText(wx)
    |
      Rd.Failure (x) =>
      Debug.Error(F("Trouble reading \"%s\" : Rd.Failure : %s",
                    fn, AL.Format(x)));
      <*ASSERT FALSE*>
    END
  END ReadFile;

VAR
  pp                    := NEW(ParseParams.T).init(Stdio.stderr);
  symtab                := NEW(TextBnfTbl.Default).init();
  rootType : TEXT       := NIL;
  outDir   : Pathname.T := NIL;
  gramName : TEXT       := NIL;
  yaccHeader : TEXT     := NIL;
  lexHeader  : TEXT     := NIL;
  tokHeader  : TEXT     := NIL;
  protected             := NEW(TextSetDef.T).init(); (* protected rules *)
  doElimIdenticalRules  := TRUE;
  
BEGIN
  TRY
    IF pp.keywordPresent("-r") THEN
      rootType := pp.getNext()
    END;
    IF pp.keywordPresent("-n") THEN
      gramName := pp.getNext()
    ELSE
      gramName := rootType
    END;
    IF pp.keywordPresent("-d") THEN
      outDir := pp.getNext()
    END;

    IF pp.keywordPresent("-Hy") OR pp.keywordPresent("-yaccheader") THEN
      yaccHeader := ReadFile(pp.getNext())
    END;
    
    IF pp.keywordPresent("-Hl") OR pp.keywordPresent("-lexheader") THEN
      lexHeader := ReadFile(pp.getNext())
    END;
    
    IF pp.keywordPresent("-Ht") OR pp.keywordPresent("-tokheader") THEN
      tokHeader := ReadFile(pp.getNext())
    END;

    IF pp.keywordPresent("-n") THEN
      doElimIdenticalRules := FALSE
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
  Debug.Out(Int(symtab.size()) & " grammar rules");
  
  IF doDebug THEN
    Debug.Out(F("Main.m3:Got %s unique strings", Int(stringTbl.size())));
    VAR
      iter := stringTbl.iterate();
      str : TEXT;
      dummy : REFANY;
    BEGIN
      WHILE iter.next(str, dummy) DO
        WITH mapStr = MapString(str) DO
          Debug.Out("String " & str & " -> " & mapStr)
        END
      END
    END
  END;

  IF rootType # NIL THEN
    PatchTypes(rootType)
  END;

  IF outDir = NIL THEN
    Debug.Error("Must specify output directory with -d")
  END;

  WITH seq = CopySymtab() DO
    EditParseTree(seq);
  
    WriteFiles(gramName, outDir, seq)
  END

END Main.
