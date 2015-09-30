MODULE Main;
IMPORT FileRd;
IMPORT Rd, Text, Name, NameNameTbl, NameList, NameNameListTbl;
IMPORT IO, Fmt, Process;
IMPORT ParseParams, Stdio, Pathname;

IMPORT Thread;
IMPORT SchemeM3;
IMPORT SchemeNavigatorEnvironment;
IMPORT SchemeStubs;
IMPORT Params, Scheme, IP, AL, ReadLine;
FROM SchemeReadLine IMPORT MainLoop;
IMPORT Debug, OSError, ReadLineError, NetObj;
IMPORT Dsim;
IMPORT NameRefTbl;
IMPORT NameSet, NameSetDef;
IMPORT TextUtils;
IMPORT Wr;
IMPORT SchemeSymbol, SchemeString;

<*FATAL Thread.Alerted*>

CONST MaxLine = 8*1024;

PROCEDURE FindChar(READONLY a : ARRAY OF CHAR; c : CHAR) : CARDINAL =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF a[i] = c THEN RETURN i END
    END;
    <*ASSERT FALSE*>
  END FindChar;

PROCEDURE Error(msg : TEXT) = 
  BEGIN Process.Crash("ERROR : " & msg) END Error;

PROCEDURE Build(instanceFn : Pathname.T) =
  VAR
    line : ARRAY [0..MaxLine-1] OF CHAR;

    instanceRd := FileRd.Open(instanceFn);
    lines := 0;
  BEGIN

    instanceTypeTbl := NEW(NameNameTbl.Default).init();
    typeInstanceTbl := NEW(NameNameListTbl.Default).init();

    LOOP
      WITH cnt = Rd.GetSubLine(instanceRd, line)-1 (* newline *),
           div = FindChar(line, ' ') DO

        IF cnt = -1 THEN EXIT END;

        IF div = -1 THEN
          Error("Bad line " & Fmt.Int(lines+1) & " : " & 
            Text.FromChars(SUBARRAY(line,0,cnt)))
        END;

        WITH type = SUBARRAY(line, 0, div),
             ins  = SUBARRAY(line, div+1, cnt-(div+1)),
             iNam = Name.ParseChars(ins),
             tNam = Name.ParseChars(type) DO

          IF tNam = NIL THEN
            Error("tNam NIL : line " & Fmt.Int(lines+1) & " : " & 
              Text.FromChars(SUBARRAY(line,0,cnt)) & " ins=\"" & Text.FromChars(ins) & "\" type=\"" & Text.FromChars(type) & "\"")
          END;

          EVAL instanceTypeTbl.put(iNam,tNam);

          VAR soFar : NameList.T;
          BEGIN
            EVAL typeInstanceTbl.get(tNam, soFar);
            soFar := NameList.Cons(iNam, soFar);
            EVAL typeInstanceTbl.put(tNam, soFar)
          END;

          INC (lines);
          
          IF lines MOD 100000 = 0 THEN IO.Put(Fmt.Int(lines) & "\n") END
        END
        
      END
    END(*LOOP*);
    Rd.Close(instanceRd)
  END Build;

PROCEDURE ReadIntoSet(fn : Pathname.T; set : NameSet.T) =
  VAR rd : Rd.T;
  BEGIN
    rd := FileRd.Open(fn);
    TRY
      LOOP
        VAR line := Rd.GetLine(rd);
            name := Name.ParseText(TextUtils.FilterOut(line));
        BEGIN
          EVAL set.insert(name)
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;
    Rd.Close(rd);
  END ReadIntoSet;

EXCEPTION Uncaught;

PROCEDURE IError(t : TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "ERROR: " & Debug.UnNil(t) & "\n");
    Wr.Flush(Stdio.stderr);
    <*NOWARN*>RAISE Uncaught 
  END IError;

TYPE 
  SchemeVar = OBJECT 
    sym : SchemeSymbol.T; 
    val : TEXT;
    next : SchemeVar 
  END;

VAR
  instanceTypeTbl : NameNameTbl.T;
  typeInstanceTbl : NameNameListTbl.T;

  dsimTypes, dsimDecls : NameRefTbl.T;
  
  dsimTopLevelWires : NameRefTbl.T := NIL;
  dsimTop : Dsim.Define;

  routedSet : NameSet.T := NIL;
  topName : Pathname.T := NIL;
  dir : Pathname.T := NIL;
  
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  sv : SchemeVar := NIL;
BEGIN
  TRY
    IF pp.keywordPresent("-dir") THEN dir := pp.getNext()  END;
    IF pp.keywordPresent("-top") THEN topName := pp.getNext() END;

    IF dir = NIL OR topName = NIL THEN
      Debug.Error("Must at least specify -top and -dir") 
    END;

    IF pp.keywordPresent("-dsim") THEN
      dsimTypes := NEW(NameRefTbl.Default).init();
      dsimDecls := NEW(NameRefTbl.Default).init();
      WITH rd = FileRd.Open(dir & "/" & topName & ".dsim") DO
        dsimTop:= Dsim.Parse(rd,dsimTypes,dsimDecls,dsimTopLevelWires);
        Rd.Close(rd)
      END;

      instanceTypeTbl := NEW(NameNameTbl.Default).init();
      typeInstanceTbl := NEW(NameNameListTbl.Default).init();

(*
      Dsim.Flatten(dsimTop, dsimTypes, instanceTypeTbl, typeInstanceTbl)
*)

    END;
    IF pp.keywordPresent("-build") THEN
      Build(dir & "/" & topName & ".instances")
    END;
    IF pp.keywordPresent("-routed") THEN
      routedSet := NEW(NameSetDef.T).init();
      ReadIntoSet(dir & "/" & topName & ".routed", routedSet)
    END;

    WHILE pp.keywordPresent("-defscm") DO
      WITH sym = SchemeSymbol.FromText(pp.getNext()),
           val = pp.getNext()                         DO
        sv := NEW(SchemeVar, sym := sym, val := val, next := sv)
      END
    END;

    pp.skipParsed()
  EXCEPT
    ParseParams.Error => Error("Can't parse command line")
  END;

  Debug.RegisterErrorHook(IError);

  SchemeStubs.RegisterStubs();

  IO.Put("pp.next=" & Fmt.Int(pp.next) & " Params.Count=" & Fmt.Int(Params.Count) & "\n");

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count-pp.next+1) DO
    arr[0] := "require";
    FOR i := pp.next TO Params.Count-1 DO arr[i-pp.next+1] := Params.Get(i) END;
    TRY
      WITH scm = NEW(SchemeM3.T).init(ARRAY OF Pathname.T { "require" },
                                      globalEnv := 
                                NEW(SchemeNavigatorEnvironment.T).initEmpty()) DO
        scm.bind("instance-types",instanceTypeTbl);
        scm.bind("type-instances",typeInstanceTbl);
        scm.bind("dsim-types", dsimTypes);
        scm.bind("dsim-decls", dsimDecls);
        scm.bind("dsim-wires", dsimTopLevelWires);
        scm.bind("dsim-top",   dsimTop);
        scm.bind("routed-set", routedSet);
        scm.bind("top-name",   Name.ParseText(topName));
        scm.bind("top-dir",    SchemeString.FromText(dir));

        WHILE sv # NIL DO
          scm.bind(sv.sym, scm.loadEvalText(sv.val));
          sv := sv.next
        END;

        FOR i := 1 TO LAST(arr^) DO
          EVAL scm.loadFile(SchemeString.FromText(arr[i]))
        END;


        MainLoop(NEW(ReadLine.Default).init(), scm)
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    |
      IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
    |
      OSError.E(err) => 
        Debug.Error("Caught NetObj.Error : " & AL.Format(err))
    |
      ReadLineError.E(err) => 
        Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
    |
      NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
                                        AL.Format(err))
    END
  END

END Main.
