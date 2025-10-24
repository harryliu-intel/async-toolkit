MODULE Main;
IMPORT rdlLexExt;
IMPORT rdlParseExt;
IMPORT Stdio;
IMPORT ParseParams;
IMPORT Params;
IMPORT Debug; 
IMPORT TextSetDef;
IMPORT RegAddrmap;
IMPORT Pathname;
IMPORT GenViews, GenViewsM3, GenViewsScala, GenViewsC, GenViewsCApi;
IMPORT GenViewsSvFulcrum;
IMPORT GenViewsScheme;
IMPORT Text;
IMPORT Rd, FileRd;
IMPORT ParseError;
IMPORT Word;

CONST TE = Text.Equal;

CONST Usage = "-top <top map name> [-L|-language m3|scala|c[-api]|scheme|sv-fulcrum] [-f -|<field-addr-file>] [-i -|<rdl-file>]";

PROCEDURE DoUsage() : TEXT =
  BEGIN RETURN Params.Get(0) & ": usage: " & Usage END DoUsage;

TYPE Lang = { M3, Scala, C, Scheme, CApi, SvFulcrum };

CONST LangNames = ARRAY Lang OF TEXT { "m3", "scala", "c", "scheme", "c-api", "sv-fulcrum" };
  
VAR
  lexer  := NEW(rdlLexExt.T, userDefProperties := NEW(TextSetDef.T).init());
  parser := NEW(rdlParseExt.T, lexer := lexer);
  rd     := Stdio.stdin;
  tgtmap : RegAddrmap.T := NIL;
  tgtmapNm : TEXT;
  outDir : Pathname.T := "build/src";
  gv : GenViews.T;
  lang := Lang.M3;
  fieldAddrRd : Rd.T := NIL;
  scmFiles : REF ARRAY OF TEXT;

  (* the following are specific for SvFulcrum, should be moved to a 
     different file ? *)
  addrBits : GenViewsSvFulcrum.AddrBits := LAST(GenViewsSvFulcrum.AddrBits);
  baseAddressBytes : Word.T := 0;
  packageName : TEXT := NIL;
  outFileName : Pathname.T := NIL;
  
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-top") THEN
        tgtmapNm := pp.getNext()
      END;
      IF pp.keywordPresent("-o") THEN
        outDir := pp.getNext()
      END;
      IF pp.keywordPresent("-L") OR pp.keywordPresent("-language") THEN
        VAR langT := pp.getNext();
            success := FALSE;
        BEGIN
          FOR i := FIRST(Lang) TO LAST(Lang) DO
            IF TE(langT, LangNames[i]) THEN
              lang := i;
              success := TRUE
            END
          END;
          IF NOT success THEN
            Debug.Error("Unsupported target language : " & langT & "\n" & DoUsage())
          END;
        END
      END;

      IF lang IN SET OF Lang { Lang.Scheme, Lang.SvFulcrum } THEN
        IF pp.keywordPresent("-f") THEN
          WITH ifn = pp.getNext() DO
            IF TE(ifn, "-") THEN
              fieldAddrRd := Stdio.stdin
            ELSE
              Debug.Out("Opening fieldAddrRd: " & ifn);
              fieldAddrRd := FileRd.Open(ifn)
            END
          END
        END
      END;

      IF lang = Lang.SvFulcrum THEN
        (* this should perhaps be processed inside GenViewsSvFulcrum 
           and not here *)
        IF pp.keywordPresent("-bits") THEN
          addrBits := pp.getNextInt()
        END;
        IF pp.keywordPresent("-baseaddr") THEN
          baseAddressBytes := pp.getNextInt()
        END;
        IF pp.keywordPresent("-packagename") THEN
          packageName := pp.getNext()
        ELSE
          Debug.Error("Must specify -packagename")
        END;
        IF pp.keywordPresent("-of") THEN
          outFileName := pp.getNext()
        ELSE
          Debug.Error("Must specify -of")
        END
      END;

      IF pp.keywordPresent("-i") THEN
        WITH ifn = pp.getNext() DO
          IF TE(ifn, "-") THEN
            rd := Stdio.stdin
          ELSE
            rd := FileRd.Open(ifn)
          END
        END
      END;
      
      pp.skipParsed();

      IF lang = Lang.Scheme THEN
        WITH nFiles = NUMBER(pp.arg^) - pp.next DO
          scmFiles := NEW(REF ARRAY OF TEXT, nFiles);
          FOR i := 0 TO nFiles-1 DO
            scmFiles[i] := pp.getNext()
          END
        END;
      END;
      
      pp.finish()
    END;
    IF tgtmapNm = NIL THEN RAISE ParseParams.Error END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;

  CASE lang OF
    Lang.M3    => gv := NEW(GenViewsM3.T)
  |
    Lang.Scala => gv := NEW(GenViewsScala.T)
  |
    Lang.C     => gv := NEW(GenViewsC.T)
  |
    Lang.CApi  => gv := NEW(GenViewsCApi.T)
  |
    Lang.Scheme => gv := NEW(GenViewsM3.T)
  |
    Lang.SvFulcrum => gv := NEW(GenViewsM3.T)
  END;  

  EVAL lexer.setRd(rd);
  (* set lexer input *)
  
  EVAL parser.setLex(lexer).parse();
  (* set parser lexer and call the parse method *)

  Debug.Out("Done parsing.");

  WITH rdlTgt = parser.defSymtab.lookup(tgtmapNm) DO
    IF rdlTgt = NIL THEN
      Debug.Error("Top symbol not found")
    END;
    TRY
      tgtmap := gv.decorate(rdlTgt, "", "").comp
    EXCEPT
      ParseError.E(txt) =>
      Debug.Error("While processing \"" & tgtmapNm & "\" : ParseError.E : " &
        txt)
    END
  END;

  CASE lang OF
    Lang.Scheme =>
    NEW(GenViewsScheme.T,
        scmFiles := scmFiles,
        fieldAddrRd := fieldAddrRd).gen(tgtmap, outDir)
  |
    Lang.SvFulcrum =>
    NEW(GenViewsSvFulcrum.T,
        fieldAddrRd      := fieldAddrRd,
        baseAddressBytes := baseAddressBytes,
        addrBits         := addrBits,
        packageName      := packageName,
        outFileName      := outFileName).gen(tgtmap, outDir)
  ELSE
    gv.gen(tgtmap, outDir)
  END
END Main.
