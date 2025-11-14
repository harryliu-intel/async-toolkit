(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE MODULE CopyrightAdder EXPORTS Main;

IMPORT Rd, Wr, FileRd, FileWr, OSError, Text, Params,
       ParseParams, Stdio, FS, Debug, Thread, Ustat, Unix, M3toC, Ctypes;
FROM Fmt IMPORT Int, Bool;

<*FATAL Thread.Alerted*>

TYPE
  Style = RECORD
    isLineStyle : BOOLEAN;
    startStr    : TEXT;
    endStr      : TEXT;  (* empty string for line styles *)
  END;
  
  Mapping = RECORD
    extension : TEXT;
    style     : Style;
  END;

CONST
  (* Style definitions *)
  Modula3Style   = Style{FALSE, "(* ", " *)"};
  HashStyle      = Style{TRUE, "# ", ""};
  FortranStyle   = Style{TRUE, "C ", ""};
  CStyle         = Style{FALSE, "/* ", " */"};
  SlashStyle     = Style{TRUE, "// ", ""};
  PercentStyle   = Style{TRUE, "% ", ""};
  SemicolonStyle = Style{TRUE, "; ", ""};
  
  (* How many bytes to read when checking for existing copyright *)
  CopyrightCheckBytes = 2048;
  
  (* File extension to style mappings *)
  Mappings = ARRAY OF Mapping{
    Mapping{".m3",    Modula3Style},
    Mapping{".i3",    Modula3Style}, 
    Mapping{".ig",    Modula3Style},
    Mapping{".mg",    Modula3Style},
    Mapping{".sh",    HashStyle},
    Mapping{".csh",   HashStyle},
    Mapping{".awk",   HashStyle},
    Mapping{".zsh",   HashStyle}, 
    Mapping{".py",    HashStyle},
    Mapping{".pl",    HashStyle}, 
    Mapping{".pm",    HashStyle}, 
    Mapping{".perl",  HashStyle},
    Mapping{".rb",    HashStyle}, 
    Mapping{".tcl",   HashStyle},
    Mapping{".mk",    HashStyle}, 
    Mapping{".c",     CStyle},
    Mapping{".h",     CStyle},
    Mapping{".p4",    CStyle},
    Mapping{".f",     FortranStyle},
    Mapping{".cpp",   SlashStyle},
    Mapping{".java",  SlashStyle}, 
    Mapping{".scala", SlashStyle},
    Mapping{".js",    SlashStyle}, 
    Mapping{".go",    SlashStyle},
    Mapping{".v",     SlashStyle}, 
    Mapping{".sv",    SlashStyle},
    Mapping{".vh",    SlashStyle}, 
    Mapping{".svh",   SlashStyle},
    Mapping{".sva",   SlashStyle}, 
    Mapping{".cast",  SlashStyle},
    Mapping{".tex",   PercentStyle},
    Mapping{".tmpl",  PercentStyle},
    Mapping{".scm",   SemicolonStyle},
    Mapping{".il",    SemicolonStyle}
  };

VAR
  overrideFileType : TEXT    := NIL;
  targetFilename   : TEXT    := NIL;
  copyrightYear    : TEXT    := NIL;
  copyrightLine1   : TEXT    := NIL;
  copyrightLine2   : TEXT    := NIL;
  customStyle      : Style;
  useCustomStyle   : BOOLEAN := FALSE;
  dryRun           : BOOLEAN := FALSE;

PROCEDURE GetFileExtension(filename : TEXT; VAR extension : TEXT): BOOLEAN =
  VAR
    lastDot : INTEGER;
  BEGIN
    lastDot := Text.FindCharR(filename, '.');
    IF lastDot = -1 THEN
      extension := "";
      RETURN FALSE;
    ELSE
      extension := Text.Sub(filename, lastDot, Text.Length(filename) - lastDot);
      RETURN TRUE;
    END;
  END GetFileExtension;

PROCEDURE GetStyle(filename : TEXT; overrideType : TEXT; VAR style : Style): BOOLEAN =
  VAR
    extension : TEXT;
  BEGIN
    (* Use custom style if specified *)
    IF useCustomStyle THEN
      style := customStyle;
      RETURN TRUE;
    END;
    
    (* Use override type if provided *)
    IF overrideType # NIL THEN
      extension := overrideType;
      IF NOT (Text.Length(extension) > 0 AND Text.GetChar(extension, 0) = '.') THEN
        extension := "." & extension;
      END;
    ELSE
      IF NOT GetFileExtension(filename, extension) THEN
        (* No extension found *)
        RETURN FALSE;
      END;
    END;
    
    (* Look up extension in mappings *)
    FOR i := FIRST(Mappings) TO LAST(Mappings) DO
      IF Text.Equal(Mappings[i].extension, extension) THEN
        style := Mappings[i].style;
        RETURN TRUE;
      END;
    END;
    
    (* Not found *)
    RETURN FALSE;
  END GetStyle;

PROCEDURE FindSubstring(text : TEXT; pattern : TEXT): INTEGER =
  VAR
    textLen : INTEGER;
    patLen  : INTEGER;
    found   : BOOLEAN;
  BEGIN
    textLen := Text.Length(text);
    patLen  := Text.Length(pattern);
    
    IF patLen = 0 THEN RETURN 0; END;
    IF patLen > textLen THEN RETURN -1; END;
    
    FOR i := 0 TO textLen - patLen DO
      found := TRUE;
      FOR j := 0 TO patLen - 1 DO
        IF Text.GetChar(text, i + j) # Text.GetChar(pattern, j) THEN
          found := FALSE;
          EXIT;
        END;
      END;
      IF found THEN RETURN i; END;
    END;
    RETURN -1;
  END FindSubstring;

PROCEDURE CheckForExistingCopyright(filename : TEXT; line1 : TEXT; line2 : TEXT): BOOLEAN RAISES {OSError.E, Rd.Failure} =
  VAR
    rd     : FileRd.T;
    header : TEXT;
  BEGIN
    (* Read first few KB to check for existing copyright - should be plenty *)
    rd := FileRd.Open(filename);
    TRY
      header := Rd.GetText(rd, CopyrightCheckBytes);
    FINALLY
      Rd.Close(rd);
    END;
    
    (* Check if either processed line exists in the header *)
    RETURN FindSubstring(header, line1) # -1 OR FindSubstring(header, line2) # -1;
  END CheckForExistingCopyright;

PROCEDURE SubstituteYear(text : TEXT; year : TEXT): TEXT =
  VAR
    result : TEXT;
    pos    : INTEGER;
  BEGIN
    result := text;
    pos    := FindSubstring(result, "%Y");
    WHILE pos # -1 DO
      result := Text.Sub(result, 0, pos) & year & 
                Text.Sub(result, pos + 2, Text.Length(result) - pos - 2);
      pos    := FindSubstring(result, "%Y");
    END;
    RETURN result;
  END SubstituteYear;

PROCEDURE GenerateCopyright(style : Style; year : TEXT; line1 : TEXT; line2 : TEXT): TEXT =
  VAR
    processedLine1 : TEXT;
    processedLine2 : TEXT;
  BEGIN
    processedLine1 := SubstituteYear(line1, year);
    processedLine2 := SubstituteYear(line2, year);
    
    IF style.isLineStyle THEN
      RETURN style.startStr & processedLine1 & "\n" &
             style.startStr & processedLine2 & "\n\n";
    ELSE
      RETURN style.startStr & processedLine1 & style.endStr & "\n" &
             style.startStr & processedLine2 & style.endStr & "\n\n";
    END;
  END GenerateCopyright;

<*UNUSED*>
PROCEDURE DumpPp(pp : ParseParams.T) =
  <*FATAL Wr.Failure*>
  BEGIN
    FOR i := 0 TO NUMBER(pp.arg^) - 1 DO
      Wr.PutText(Stdio.stderr, "pp.arg[" & Int(i) & "] : " & pp.arg[i] & " parsed=" & Bool(pp.parsed[i]) & "\n")
    END
  END DumpPp;
  
PROCEDURE ParseCommandLine(): BOOLEAN =
  VAR
    pp       : ParseParams.T;
    typeStr  : TEXT;
    startStr : TEXT;
    endStr   : TEXT;
    hasYear  : BOOLEAN := FALSE;
    hasLine1 : BOOLEAN := FALSE;
    hasLine2 : BOOLEAN := FALSE;
  BEGIN
    pp := NEW(ParseParams.T).init(Stdio.stderr);
    
    TRY
      IF pp.keywordPresent("-year") THEN
        copyrightYear := pp.getNext();
        hasYear       := TRUE;
      END;
      
      IF pp.keywordPresent("-line1") THEN
        copyrightLine1 := pp.getNext();
        hasLine1       := TRUE;
      END;
      
      IF pp.keywordPresent("-line2") THEN
        copyrightLine2 := pp.getNext();
        hasLine2       := TRUE;
      END;
      
      IF pp.keywordPresent("-type") THEN
        overrideFileType := pp.getNext();
        IF FALSE THEN
          Wr.PutText(Stdio.stderr,
                     "Type is overriden to " & overrideFileType & "\n")
        END;
      END;
      
      IF pp.keywordPresent("-dry-run") THEN
        dryRun := TRUE;
      END;
      
      (* Check for custom style *)
      IF pp.keywordPresent("-style") THEN
        typeStr  := pp.getNext();
        startStr := pp.getNext();
        endStr   := pp.getNext();
        
        IF Text.Equal(typeStr, "line") THEN
          customStyle    := Style{TRUE, startStr, endStr};
          useCustomStyle := TRUE;
        ELSIF Text.Equal(typeStr, "block") THEN
          customStyle    := Style{FALSE, startStr, endStr};
          useCustomStyle := TRUE;
        ELSE
          Debug.Error("Style must be 'line' or 'block', not '" & typeStr & "'");
          <*ASSERT FALSE*>
        END;
      END;

      pp.skipParsed();
      
      (* Get the filename *)
      targetFilename := pp.getNext();
      pp.finish();
      
      (* Check required parameters *)
      IF targetFilename = NIL THEN
        RETURN FALSE;
      END;
      
      IF NOT hasYear THEN
        RETURN FALSE;
      END;
      
      IF NOT hasLine1 THEN
        RETURN FALSE;
      END;
      
      IF NOT hasLine2 THEN
        RETURN FALSE;
      END;
      
      RETURN TRUE;
    EXCEPT
    | ParseParams.Error =>
      RETURN FALSE;
    END;
  END ParseCommandLine;

PROCEDURE CheckForShebang(content           : TEXT;
                          VAR shebang       : TEXT;
                          VAR restOfContent : TEXT) =
  VAR
    firstLineEnd : INTEGER;
  BEGIN
    shebang       := "";
    restOfContent := content;
    
    IF Text.Length(content) >= 2 AND Text.Equal(Text.Sub(content, 0, 2), "#!") THEN
      (* Find the end of the first line *)
      firstLineEnd := Text.FindChar(content, '\n');
      IF firstLineEnd # -1 THEN
        shebang       := Text.Sub(content, 0, firstLineEnd + 1);
        restOfContent := Text.Sub(content, firstLineEnd + 1, Text.Length(content) - firstLineEnd - 1);
      END;
    END;
  END CheckForShebang;

PROCEDURE AddCopyrightToFile(filename : TEXT; fileType : TEXT)
  RAISES {OSError.E, Wr.Failure, Rd.Failure} =
  VAR
    rd             : FileRd.T;
    wr             : FileWr.T;
    content        : TEXT;
    tempFilename   : TEXT;
    shebang        : TEXT;
    restOfContent  : TEXT;
    style          : Style;
    header         : TEXT;
    processedLine1 : TEXT;
    processedLine2 : TEXT;
    fileStat       : Ustat.struct_stat;
    fileStatPtr    : Ustat.struct_stat_star;
    cFilename      : Ctypes.const_char_star;
    cTempFilename  : Ctypes.const_char_star;
  BEGIN
    (* Determine style for this file *)
    IF NOT GetStyle(filename, fileType, style) THEN
      Debug.Error("Unknown file type for " & filename & " : use -type <extension> or -style to specify.");
      RETURN;
    END;
    
    processedLine1 := SubstituteYear(copyrightLine1, copyrightYear);
    processedLine2 := SubstituteYear(copyrightLine2, copyrightYear);
    
    (* Check if copyright already exists by reading just the beginning of the file *)
    IF CheckForExistingCopyright(filename, processedLine1, processedLine2) THEN
      Debug.Out("Notice already exists in " & filename);
      RETURN;
    END;
    
    (* If dry-run, just report what would be done *)
    IF dryRun THEN
      Debug.Out("Would add copyright to " & filename);
      RETURN;
    END;
    
    (* Get original file permissions before modifying *)
    cFilename   := M3toC.SharedTtoS(filename);
    fileStatPtr := NEW(Ustat.struct_stat_star);
    IF Ustat.stat(cFilename, fileStatPtr) # 0 THEN
      Debug.Error("Cannot get file permissions for " & filename);
      RETURN;
    END;
    fileStat := fileStatPtr^;  (* Dereference pointer and copy struct *)
    
    (* Now read the entire file since we're going to modify it *)
    rd := FileRd.Open(filename);
    TRY
      content := Rd.GetText(rd, LAST(CARDINAL));
    FINALLY
      Rd.Close(rd);
    END;
    
    header := GenerateCopyright(style, copyrightYear, copyrightLine1, copyrightLine2);
    
    (* Check for shebang line *)
    CheckForShebang(content, shebang, restOfContent);
    
    (* Create temporary filename *)
    tempFilename := filename & ".tmp";
    
    (* Write content to temp file in proper order *)
    wr := FileWr.Open(tempFilename);
    TRY
      (* Write shebang first if it exists *)
      IF Text.Length(shebang) > 0 THEN
        Wr.PutText(wr, shebang);
      END;
      (* Then write header *)
      Wr.PutText(wr, header);
      (* Then write rest of content *)
      Wr.PutText(wr, restOfContent);
      Wr.Flush(wr);
    FINALLY
      Wr.Close(wr);
    END;
    
    (* Set permissions on temp file to match original *)
    cTempFilename := M3toC.SharedTtoS(tempFilename);
    IF Unix.chmod(cTempFilename, fileStat.st_mode) # 0 THEN
      Debug.Error("Cannot set permissions on " & tempFilename);
      RETURN;
    END;
    
    (* Replace original file with temp file *)
    FS.Rename(tempFilename, filename);
    Debug.Out("Statement added to " & filename & "\n");
  END AddCopyrightToFile;

PROCEDURE PrintUsage() RAISES {Wr.Failure} =
  
  PROCEDURE W(text : TEXT) RAISES {Wr.Failure} =
    BEGIN
      Wr.PutText(Stdio.stderr, text);
    END W;
  
  BEGIN
    W("Usage: " & Params.Get(0) & " -year <year> -line1 <text> -line2 <text> [options] <filename>\n");
    W("Adds a two-line statement to the top of the specified file.\n");
    W("\nRequired:\n");
    W("  -year <year>      Year (e.g., 2025)\n");
    W("  -line1 <text>     First line (use %Y for year placeholder)\n");
    W("  -line2 <text>     Second line (use %Y for year placeholder)\n");
    W("\nOptions:\n");
    W("  -type <filetype>  Override file type (e.g., 'sh', 'py', 'c')\n");
    W("  -dry-run          Show what would be done without making changes\n");
    W("  -style <line|block> <start> <end>\n");
    W("                    Custom style\n");
    W("                    Examples:\n");
    W("                      -style line \"# \" \"\"\n");
    W("                      -style block \"/* \" \" */\"\n");
    W("\nExamples:\n");
    W("  " & Params.Get(0) & " -year 2025 -line1 \"Copyright (c) %Y ACME Corp.\" -line2 \"All rights reserved.\" script.py\n");
    W("  " & Params.Get(0) & " -year 2025 -line1 \"Copyright %Y John Doe\" -line2 \"Licensed under MIT\" -type sh config.txt\n");
    W("  " & Params.Get(0) & " -dry-run -year 2025 -line1 \"Copyright %Y\" -line2 \"All rights reserved\" *.py\n");
    W("\nSupported file types:\n");
    W("  .m3, .i3, .ig, .mg  Modula-3 files       (* ... *)\n");
    W("  .sh, .zsh           Shell files          # ...\n");
    W("  .py                 Python files         # ...\n");
    W("  .pl, .perl, .pm     Perl files           # ...\n");
    W("  .rb                 Ruby files           # ...\n");
    W("  .tcl                Tcl files            # ...\n");
    W("  .mk                 Makefiles            # ...\n");
    W("  .c, .h, .p4         C files              /* ... */\n");
    W("  .cpp, .java         C++/Java files       // ...\n");
    W("  .scala              Scala files          // ...\n");
    W("  .js, .go            JavaScript/Go files  // ...\n");
    W("  .v, .sv             Verilog files        // ...\n");
    W("  .vh, .svh, .sva     Verilog headers/SVA  // ...\n");
    W("  .cast               Cast files           // ...\n");
    W("  .tex                LaTeX files          % ...\n");
    W("  .tmpl               Template files       % ...\n");
    W("  .scm                Scheme files         ; ...\n");
    W("  .il                 SKILL files          ; ...\n");
  END PrintUsage;

PROCEDURE DoIt() =
  BEGIN
    TRY
      (* Parse command line arguments *)
      IF NOT ParseCommandLine() THEN
        PrintUsage();
        RETURN;
      END;
      
      AddCopyrightToFile(targetFilename, overrideFileType);
    EXCEPT
    | OSError.E =>
      Debug.Error("Failed to process file. Please check file permissions and path.");
    | Wr.Failure =>
      Debug.Error("Write operation failed.");
    | Rd.Failure =>
      Debug.Error("Read operation failed.");
    END;
  END DoIt;

BEGIN
  DoIt();
END CopyrightAdder.
