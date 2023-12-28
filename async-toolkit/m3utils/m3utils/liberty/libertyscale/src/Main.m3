MODULE Main;

(* 

   libertyscale : Simple scaling of Liberty timing files

   Uses the Liberty parser from the Barefoot LAMB project to
   understand the Liberty files.

   Author : mika.nystroem@intel.com

   December, 2023

*)

IMPORT LibertyParse;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT OSError;
IMPORT Rd;
IMPORT FileRd;
IMPORT AL;
FROM Fmt IMPORT F, LongReal, Bool;
IMPORT Text;
IMPORT LibertyComponent;
IMPORT LibertyComponentChildren;
IMPORT RTName;
IMPORT LibertySorI;
IMPORT FloatMode;
IMPORT TextReader;
IMPORT Scan;
IMPORT Lex;
IMPORT LibertyHead;
IMPORT LibertyGroup;
IMPORT TextWr;
IMPORT Process;
IMPORT Wr;
IMPORT SeekRd;
IMPORT Thread;
IMPORT LibertySimpleAttr;
IMPORT FileWr;
IMPORT Params;
IMPORT ProcUtils;
IMPORT TextList;
<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      LR = LongReal;

CONST HelpText = "";
      
VAR
  pp       := NEW(ParseParams.T).init(Stdio.stderr);
  rd       : Rd.T;
  wr       : Wr.T := Stdio.stdout;
  lib      : LibertyComponent.T;
  scaleFac := 1.0d0;
  Verbose  := Debug.DebugThis("libertyscale");

TYPE
  Visitor = OBJECT METHODS
    visit(x : LibertyComponent.T) : Visitor;
    (* return visitor to apply to children *)
  END;

  Executor = OBJECT METHODS
    execute(x : LibertyComponent.T);
    (* do *something* to a component *)
  END;
  
PROCEDURE VisitAll(x : LibertyComponent.T; v : Visitor) =
  BEGIN
    WITH newv = v.visit(x) DO
      
      IF newv # NIL AND x.canHaveChildren() THEN
        WITH children = x.children() DO
          FOR i := 0 TO children.size() - 1 DO
            WITH child = children.get(i) DO
              VisitAll(child, newv)
            END
          END
        END
      END
    END
  END VisitAll;

TYPE
  ChainedVisitor = Visitor OBJECT
    next     : Visitor  := NIL;
    executor : Executor := NIL;
  END;
  
  TaggedVisitor = ChainedVisitor OBJECT
    tag         : TEXT;
    needAttr    : TEXT := NIL;
    needAttrVal : TEXT := NIL;
  OVERRIDES
    visit := TaggedVisit;
  END;

  HeadVisitor = ChainedVisitor OBJECT
    tag         : TEXT;
  OVERRIDES
    visit := HeadVisit;
  END;

  OrVisitor = Visitor OBJECT
    disjuncts : REF ARRAY OF Visitor;
  METHODS
    init2(a, b : Visitor) : OrVisitor := InitOV2;
  OVERRIDES
    visit := OrVisit;
  END;

  StringVisitor = Visitor OBJECT
    editor : StringEditor;
  OVERRIDES
    visit := StringVisit;
  END;

  StringEditor = OBJECT METHODS
    edit(str : TEXT) : TEXT;
  END;
  
PROCEDURE StringVisit(sv : StringVisitor; c : LibertyComponent.T) : Visitor =
  BEGIN
    TYPECASE c OF
      LibertySorI.String(str) =>
      str.val := sv.editor.edit(str.val)
    ELSE
      (*skip*)
    END;
    RETURN sv
  END StringVisit;
    
PROCEDURE InitOV2(ov : OrVisitor; a, b : Visitor) : OrVisitor =
  VAR
    arr := NEW(REF ARRAY OF Visitor, 2);
  BEGIN
    arr[0] := a;
    arr[1] := b;
    ov.disjuncts := arr;
    RETURN ov
  END InitOV2;

PROCEDURE OrVisit(ov : OrVisitor; c : LibertyComponent.T) : Visitor =
  BEGIN
    FOR i := FIRST(ov.disjuncts^) TO LAST(ov.disjuncts^) DO
      WITH thisV = ov.disjuncts[i],
           newV  = thisV.visit(c) DO
        IF newV # thisV THEN RETURN newV END
      END
    END;
    RETURN ov
  END OrVisit;

PROCEDURE HeadVisit(hv : HeadVisitor; c : LibertyComponent.T) : Visitor =
  VAR
    success : BOOLEAN;
  BEGIN
    TYPECASE c OF
      LibertyHead.T(lh) =>
      success := TE(lh.ident, hv.tag);
      WITH sstr    = ARRAY BOOLEAN OF TEXT { "", " SUCCESS" }[success] DO
        IF Verbose THEN
          Debug.Out(F("Seeking tag %s in LibertyHead object with tag %s %s : %s",
                      hv.tag, lh.ident, sstr, lh.format()));
          IF success THEN
            Debug.Out("Success with head: obj:\n" & lh.debugDump())
          END
        END;
        
        IF success THEN
          IF hv.executor # NIL THEN
            hv.executor.execute(c)
          END;
          RETURN hv.next
        END
      END
    ELSE
      (* skip *)
    END;
    RETURN hv
  END HeadVisit;
  
PROCEDURE TaggedVisit(tv : TaggedVisitor; c : LibertyComponent.T) : Visitor =
  VAR
    success : BOOLEAN;
  BEGIN
    TYPECASE c OF
      LibertyGroup.T(lg) =>

      success :=  TE(lg.head.ident, tv.tag);
      WITH sstr    = ARRAY BOOLEAN OF TEXT { "", " SUCCESS" }[success] DO
        IF Verbose THEN
          Debug.Out(F("Seeking tag %s in object with head tag %s %s",
                      tv.tag, lg.head.ident, sstr))
        END;
          
          
        IF success AND tv.needAttr # NIL THEN
          success := FALSE;
          Debug.Out("Seeking attr " & tv.needAttr);
          FOR i := 0 TO lg.statements.size() - 1 DO
            WITH s = lg.statements.get(i) DO
              TYPECASE s OF
                LibertySimpleAttr.T(sa) =>
                IF TE(sa.ident, tv.needAttr) THEN
                  IF Verbose THEN
                    Debug.Out(F("matched attr %s type %s : %s", sa.ident, RTName.GetByTC(TYPECODE(sa.attrValExpr)), sa.attrValExpr.format()))
                  END;
                  IF tv.needAttrVal = NIL THEN
                    success := TRUE
                  ELSE
                    success := TE(sa.attrValExpr.format(), tv.needAttrVal);
                    IF Verbose THEN
                      Debug.Out(F("tv.needAttrVal %s, success %s",
                                  tv.needAttrVal, Bool(success)))
                    END;
                  END;
                  EXIT
                END
              ELSE
                (* skip *)
              END
            END
          END
        END;
        
        IF success THEN
          IF tv.executor # NIL THEN
            tv.executor.execute(c)
          END;
          RETURN tv.next
        ELSE
          RETURN tv
        END
      END
    ELSE
      RETURN tv
    END
  END TaggedVisit;

TYPE
  StringScaler = StringEditor OBJECT
    mult : LONGREAL;
  OVERRIDES
    edit := StringScalerEdit;
  END;

PROCEDURE StringScalerEdit(ss : StringScaler; in : TEXT) : TEXT =
  VAR
    reader := NEW(TextReader.T).init(in);
    lst    := reader.shatter(",", "");
    wr     := TextWr.New();
    p      := lst;
  BEGIN
    IF Verbose THEN
      Debug.Out(F("StringScalerEdit: in=\"%s\"", in))
    END;
    WHILE p # NIL DO
      TRY
        WITH val       = Scan.LongReal(p.head),
             scaledVal = val * ss.mult DO
          Wr.PutText(wr, LR(scaledVal))
        END
      EXCEPT
        Lex.Error, FloatMode.Trap =>
        Debug.Error("Couldn't parse values number \"" & lst.head & "\"")
      END;
      
      IF p.tail # NIL THEN
        Wr.PutText(wr, ", ")
      END;
      
      p := p.tail
    END;
    WITH out = TextWr.ToText(wr) DO
      IF Verbose THEN
        Debug.Out(F("StringScalerEdit: out=\"%s\"", out))
      END;
      RETURN out
    END
  END StringScalerEdit;

VAR
  timingType : TEXT := NIL;
  timingValues : TextList.T := NIL;
  
BEGIN
  TRY
    IF pp.keywordPresent("-help") OR pp.keywordPresent("--help") THEN
      Wr.PutText(Stdio.stderr, HelpText);
      Process.Exit(0)
    END;

    IF pp.keywordPresent("-factor") THEN
      scaleFac := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-timing_type") THEN
      timingType := pp.getNext()
    END;

    IF timingType = NIL THEN
      Debug.Error("?must specify -timing_type")
    END;

    WHILE pp.keywordPresent("-values") DO
      timingValues := TextList.Cons(pp.getNext(), timingValues)
    END;
    
    IF pp.keywordPresent("-i") THEN
      WITH fn = pp.getNext() DO
        IF TE(fn, "-") THEN
          rd := SeekRd.Stdin()
        ELSE
          TRY
            rd := FileRd.Open(fn)
          EXCEPT
            OSError.E(e) =>
            Debug.Error(F("Couldn't open liberty file \"%s\" : OSError.E : %s\n%s",
                          fn, AL.Format(e), HelpText))
          END
        END
      END
    END;

    IF pp.keywordPresent("-o") THEN
      WITH fn = pp.getNext() DO
        IF TE(fn, "-") THEN
          wr := Stdio.stdout
        ELSE
          TRY
            wr := FileWr.Open(fn)
          EXCEPT
            OSError.E(e) =>
            Debug.Error(F("Couldn't open output file \"%s\" : OSError.E : %s\n%s",
                          fn, AL.Format(e), HelpText))
          END
        END
      END
    END;

    
    pp.skipParsed();
    pp.finish()

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line\n" & HelpText)
  END;

  TRY
    Debug.Out("Parsing lib...");
    lib := LibertyParse.Parse(rd);
    Debug.Out("Done parsing lib.");
    Rd.Close(rd)
  EXCEPT
    Rd.Failure(e) =>
    Debug.Error(F("I/O error while parsing liberty : Rd.Failure : %s\n%s",
                  AL.Format(e), HelpText))
  END;

  VAR
    valuesChanger    : HeadVisitor;
    valuesVisitors   := NEW(REF ARRAY OF Visitor, TextList.Length(timingValues));
  BEGIN
    valuesChanger := NEW(HeadVisitor,
                            tag  := "values",
                            next := NEW(StringVisitor,
                                        editor := NEW(StringScaler,
                                                      mult := scaleFac)));

    FOR i := FIRST(valuesVisitors^) TO LAST(valuesVisitors^) DO
      valuesVisitors[i] := NEW(TaggedVisitor,
                               tag := TextList.Nth(timingValues,i),
                               next := valuesChanger)
    END;
    

    VAR
      ocvRiseOrFallVisitor := NEW(OrVisitor, disjuncts := valuesVisitors);

      timingTagVisitor := NEW(TaggedVisitor,
                              tag         := "timing",
                              needAttr    := timingType,
                              needAttrVal := "min_pulse_width",
                              next        := ocvRiseOrFallVisitor);
      cellTagVisitor   := NEW(TaggedVisitor,
                              tag  := "cell"  ,
                              next := timingTagVisitor);
      
    BEGIN
      VisitAll(lib, cellTagVisitor)
    END
  END;

  Wr.PutText(wr, "/* DO NOT EDIT : generated by \n");
  Wr.PutChar(wr, '\n');
  Wr.PutText(wr, "  ");
  FOR i := 0 TO Params.Count - 1 DO
    Wr.PutText(wr, " ");
    Wr.PutText(wr, Params.Get(i))
  END;
  Wr.PutChar(wr, '\n');
  Wr.PutChar(wr, '\n');

  Wr.PutText(wr, "   Run at ");
  ProcUtils.RunText("/usr/bin/date", stdout := ProcUtils.WriteHere(wr)).wait();
  Wr.PutChar(wr, '\n');
  
  Wr.PutText(wr, "   cwd : " & Process.GetWorkingDirectory() & "\n");
  Wr.PutChar(wr, '\n');

  Wr.PutText(wr, "*/\n\n");
  
  lib.write(wr);
  Wr.Close(wr)

END Main.
