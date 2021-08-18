MODULE NameControl;
IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT CardSeq;
IMPORT RegEx;
IMPORT Text;
IMPORT Pathname;
IMPORT Wr;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT OSError;
IMPORT FileWr;
IMPORT AL;
IMPORT TextUtils;
IMPORT Thread;
IMPORT TextCardTbl;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

PROCEDURE MakeIdxMap(fsdbNames     : TextCardTbl.T;
                     restrictNodes : TextSet.T;
                     regExList     : RegExList.T;
                     names         : TextSeq.T) : CardSeq.T =
  VAR
    res := NEW(CardSeq.T).init();
    c := 0;
    success : BOOLEAN;
    iter := fsdbNames.iterate();
    maxId := -1;
    arr : REF ARRAY OF TEXT;
    n : TEXT;
    id : CARDINAL;
    
  BEGIN
    WHILE iter.next(n, id) DO
      maxId := MAX(id, maxId)
    END;

    arr := NEW(REF ARRAY OF TEXT, maxId + 1);

    FOR i := FIRST(arr^) TO LAST(arr^) DO arr[i] := NIL END;
    
    iter := fsdbNames.iterate();
    WHILE iter.next(n, id) DO
      arr[id] := n
    END;

    EVAL names.init();
    FOR i := FIRST(arr^) TO LAST(arr^) DO names.addhi(arr[i]) END;

    FOR i := 0 TO names.size() - 1 DO
      IF names.get(i) = NIL THEN
        success := FALSE
      ELSIF TE(names.get(i), "TIME") THEN
        success := TRUE
      ELSIF restrictNodes = NIL AND regExList = NIL THEN
        success := TRUE
      ELSIF restrictNodes # NIL AND restrictNodes.member(names.get(i)) THEN
        success := TRUE
      ELSE
        success := FALSE;
        VAR
          p := regExList;
        BEGIN
          WHILE p # NIL DO
            IF RegEx.Execute(p.head,
                             names.get(i)) # -1 THEN
              success := TRUE;
              EXIT
            END;
            p := p.tail
          END
        END
      END;

      IF success THEN
        res.addhi(c);
        INC(c)
      ELSE
        res.addhi(NoMapping)
      END
    END;

    RETURN res
  END MakeIdxMap;
  
PROCEDURE SanitizeNames(idxMap : CardSeq.T;
                        names  : TextSeq.T) =
  VAR
    store := NEW(TextSeq.T).init();
  BEGIN
    FOR i := 0 TO names.size() - 1 DO
      IF idxMap.get(i) # NoMapping THEN store.addhi(names.get(i)) END
    END;

    EVAL names.init();

    FOR i := 0 TO store.size() - 1 DO
      names.addhi(store.get(i))
    END
  END SanitizeNames;

PROCEDURE CountActiveNames(seq : CardSeq.T) : CARDINAL =
  VAR
    res : CARDINAL := 0;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      IF seq.get(i) # NoMapping THEN
        INC(res)
      END
    END;
    RETURN res
  END CountActiveNames;
  
PROCEDURE FormatFN(i : CARDINAL) : TEXT =
  BEGIN RETURN F("%08s", Int(i)) END FormatFN;

PROCEDURE WriteNames(wd, ofn       : Pathname.T;

                     names         : TextSeq.T;

                     idxMap        : CardSeq.T;
                     (* map of input node to output node *)
                     
                     maxFiles      : CARDINAL;

                     VAR nFiles    : CARDINAL;

                     VAR wdWr      : REF ARRAY OF Wr.T;

                     VAR wdPth     : REF ARRAY OF Pathname.T;

                     includeIdNames: BOOLEAN) : CARDINAL =
  VAR
    anWr, wr : Wr.T;
    nFn := ofn & ".names";
    anFn := ofn & ".allnames";

    nNodes := names.size();
    (* this is the number of names in the file, not the number of names
       written? *)

    aNodes := CountActiveNames(idxMap);
  BEGIN
    nFiles := MIN(aNodes, maxFiles); (* note that nNodes includes TIME *)
    
    Debug.Out(F("%s nodes (incl. TIME), %s nodes active",
                Int(nNodes),
                Int(aNodes)));
    Debug.Out(F("%s files", Int(nFiles)));
    
    TRY
      wdWr := NEW(REF ARRAY OF Wr.T, nFiles);
      wdPth := NEW(REF ARRAY OF Pathname.T, nFiles);
      
      TRY
        wr := FileWr.Open(nFn)
      EXCEPT
        OSError.E(x) => Debug.Error("Unable to open names file \"" & nFn & "\" : OSError.E : " & AL.Format(x))
      END;

      TRY
        anWr := FileWr.Open(anFn)
      EXCEPT
        OSError.E(x) => Debug.Error("Unable to open allnames file \"" & anFn & "\" : OSError.E : " & AL.Format(x))
      END;

      (* open temp files *)
      FOR i := 0 TO nFiles - 1 DO
        WITH fn = wd & "/" & FormatFN(i) DO
          TRY
            WITH wr2 = FileWr.Open(fn) DO
              wdWr[i] := wr2;
              wdPth[i] := fn
            END
          EXCEPT
            OSError.E(x) =>
            Debug.Warning("Unable to temp file \"" & fn & "\" : OSError.E : " & AL.Format(x))
          END
        END
      END;

      (* write names file *)
      FOR i := 0 TO names.size() - 1 DO
        WITH nm = TextUtils.ReplaceChar(UnNil(names.get(i)), ':', '_') DO
          (* aplot has trouble with colons in node names, so rename those,
             sorry about any clashes ... *)
          IF idxMap.get(i) # NoMapping THEN
            
            Wr.PutText(wr, nm);

            IF includeIdNames THEN
              Wr.PutText(wr, F("=NAMES%s", Int(i)))
            END;
            
            Wr.PutChar(wr, '\n')

          END;

          Wr.PutText(anWr, nm);
          Wr.PutChar(anWr, '\n')

        END
      END;
      Wr.Close(wr)
    EXCEPT
      Wr.Failure(x) => Debug.Error("Unable to write names file \"" & nFn & "\" : Wr.Failure : " & AL.Format(x))
    END;
    RETURN aNodes
  END WriteNames;

PROCEDURE UnNil(txt : TEXT) : TEXT =
  BEGIN
    IF txt = NIL THEN RETURN
      "NULLTEXT"
    ELSE
      RETURN txt
    END
  END UnNil;

PROCEDURE FileIndex(nFiles, nNodes, nodeIndex : CARDINAL) : CARDINAL =
  BEGIN
    IF nodeIndex = 0 THEN
      RETURN 0 (* TIME node on its own *)
    ELSE
      WITH nonTimeFiles = nFiles - 1,
           nonTimeIndex = nodeIndex - 1,
           nodesPerFile = (nNodes DIV nonTimeFiles) + 1 DO
        RETURN nonTimeIndex DIV nodesPerFile + 1
      END
    END
  END FileIndex;

BEGIN END NameControl.
