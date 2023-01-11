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
IMPORT CitTextUtils AS TextUtils;
IMPORT Thread;
IMPORT CardTextSetTbl;
IMPORT TextSeqSeq;
IMPORT TextArraySort;
IMPORT Cardinal;
IMPORT CitTextUtils;
IMPORT Wx;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

VAR Verbose := Debug.DebugThis("NameControl");
    
(*

/*** translate from GDS2 name to CAST name and strip v(.) ***/
char *gds2cast(char *name) {
  if (translate) {
    int strip=0;
    char *s=name, *t=name;
    if ( *s=='\'') { s++; strip++; }
    if (!strncasecmp(s,"v(",2)) { s+=2; strip++; }
    while( *s) {
      if      (!strncmp(s,"_D_",3))   { s+=3; *(t++) = '.'; }
      else if (!strncmp(s,"_l_",3))   { s+=3; *(t++) = '['; }
      else if (!strncmp(s,"_r_",3))   { s+=3; *(t++) = ']'; }
      else if (!strncmp(s,"_C_",3))   { s+=3; *(t++) = ','; }
      else if (!strncmp(s,"_U_",3))   { s+=3; *(t++) = '_'; }
      else if (!strncmp(s,"_3a_",4))  { s+=4; *(t++) = ':'; }
      else                            { *(t++) = *(s++); }
    }
    *(t-strip)=0;
  }
  return name;
}

*)

PROCEDURE Gds2Cast(name : TEXT) : TEXT =
  CONST
    SQ = '\'';
  TYPE
    M = RECORD t : TEXT; c : CHAR; n := LAST(CARDINAL) END;
  VAR 
    ToConv:= ARRAY [0..5] OF M {
              M { "_D_" , '.' },
              M { "_l_" , '[' },
              M { "_r_" , ']' },
              M { "_C_" , ',' },
              M { "_U_" , '_' },
              M { "_3a_", ':' }
             };

  VAR
    wx := Wx.New();
    n, i  : CARDINAL;
    mapped : BOOLEAN;
  BEGIN
    (* v() is stripped elsewhere *)
    FOR m := FIRST(ToConv) TO LAST(ToConv) DO
      ToConv[m].n := Text.Length(ToConv[m].t)
    END;

    IF Text.GetChar(name, 0) = SQ THEN
      name := Text.Sub(name, 1, Text.Length(name) - 2)
    END;

    n := Text.Length(name);

    i := 0;
    WHILE i < n DO
      mapped := FALSE;
      FOR m := FIRST(ToConv) TO LAST(ToConv) DO
       WITH tr = ToConv[m] DO
         IF TE(Text.Sub(name, i, tr.n), tr.t) THEN
           Wx.PutChar(wx, tr.c);
           INC(i, tr.n);
           mapped := TRUE;
           EXIT
         END
       END
      END;
      IF NOT mapped THEN              
        Wx.PutChar(wx, Text.GetChar(name, i));
        INC(i)
      END
    END;
    RETURN Wx.ToText(wx)
  END Gds2Cast;

PROCEDURE SetToSeq(set : TextSet.T; translate, noX : BOOLEAN) : TextSeq.T =
  VAR
    iter := set.iterate();
    txt : TEXT;
    a   := NEW(REF ARRAY OF TEXT, set.size());
    res := NEW(TextSeq.T).init();
    j  := 0;
  BEGIN
    WHILE iter.next(txt) DO
      IF noX THEN
        txt := CitTextUtils.FilterOut(txt, SET OF CHAR { 'X' })
      END;
      IF translate THEN
        a[j] := Gds2Cast(txt)
      ELSE
        a[j] := txt
      END;
      INC(j)
    END;

    TextArraySort.Sort(a^, cmp := CompareText);
    
    FOR i := 0 TO NUMBER(a^) - 1 DO
      res.addhi(a[i])
    END;
    RETURN res
  END SetToSeq;

PROCEDURE CountDots(a : TEXT) : CARDINAL =
  VAR
    res := 0;
    p := 0;
    q0, q1 : CARDINAL;
  BEGIN
    LOOP
      q0 := LAST(CARDINAL);
      q1 := LAST(CARDINAL);
      IF CitTextUtils.FindSub(a, ".", q0, p) OR
         CitTextUtils.FindSub(a, "_D_", q1, p) THEN
        p := MIN(q0, q1) + 1;
        INC(res)
      ELSE
        EXIT
      END
    END;
    RETURN res
  END CountDots;
  
PROCEDURE CompareText(a, b : TEXT) : [-1..1] =
  BEGIN
    WITH adots = CountDots(a),
         bdots = CountDots(b) DO
      IF adots = bdots THEN
        RETURN Text.Compare(a, b)
      ELSE
        RETURN Cardinal.Compare(adots, bdots)
      END
    END
  END CompareText;

PROCEDURE MakeIdxMap(fsdbNames     : CardTextSetTbl.T;
                     restrictNodes : TextSet.T;
                     regExList     : RegExList.T;
                     maxNodes      : CARDINAL;
                     names         : TextSeqSeq.T;
                     translate, noX: BOOLEAN) : CardSeq.T =
  VAR
    res   := NEW(CardSeq.T).init();
    c     := 0;
    maxId := -1;
    iter  := fsdbNames.iterate();

    success : BOOLEAN;
    arr     : REF ARRAY OF TextSeq.T;
    id      : CARDINAL;
    set     : TextSet.T;
    seq     : TextSeq.T;
    
  BEGIN
    WHILE iter.next(id, set) DO
      maxId := MAX(id, maxId)
    END;

    arr := NEW(REF ARRAY OF TextSeq.T, maxId + 1);

    FOR i := FIRST(arr^) TO LAST(arr^) DO arr[i] := NIL END;
    
    iter := fsdbNames.iterate();
    WHILE iter.next(id, set) DO
      arr[id] := SetToSeq(set, translate, noX)
    END;

    EVAL names.init();
    FOR i := FIRST(arr^) TO LAST(arr^) DO names.addhi(arr[i]) END;

    FOR i := 0 TO names.size() - 1 DO

      (* set up useful auxiliary variables *)
      
      seq := names.get(i);

      IF seq # NIL THEN
        WITH hadIt = fsdbNames.get(i, set) DO
          <*ASSERT hadIt*>
        END
      END;


      IF names.get(i) = NIL THEN
        success := FALSE
      ELSIF set.member("TIME") THEN
        success := TRUE
      ELSIF restrictNodes = NIL AND regExList = NIL THEN
        success := TRUE
      ELSIF restrictNodes # NIL AND
            restrictNodes.intersection(set).size() # 0 THEN
        success := TRUE
      ELSE
        IF Verbose THEN
          Debug.Out("NameControl : regExList length " &
            Int(RegExList.Length(regExList)))
        END;
        
        success := FALSE;
        VAR
          p := regExList;
        BEGIN
          WHILE p # NIL DO
            FOR j := 0 TO seq.size() - 1 DO
              WITH alias = seq.get(j) DO
                IF Verbose THEN
                  Debug.Out("NameControl.MakeIdxMap : matching regex to " &
                    alias)
                END;
                IF RegEx.Execute(p.head, alias) # -1 THEN
                  success := TRUE;
                  EXIT
                END
              END
            END;
            
            IF success THEN EXIT END;
            p := p.tail
          END
        END
      END;

      IF success AND res.size() < maxNodes THEN
        res.addhi(c);
        INC(c)
      ELSE
        res.addhi(NoMapping)
      END
    END;

    RETURN res
  END MakeIdxMap;
  
PROCEDURE SanitizeNames(idxMap : CardSeq.T;
                        names  : TextSeqSeq.T) =
  VAR
    store := NEW(TextSeqSeq.T).init();
  BEGIN
    FOR i := 0 TO names.size() - 1 DO
      IF idxMap.get(i) # NoMapping THEN store.addhi(names.get(i)) END
    END;

    EVAL names.init();

    FOR i := 0 TO store.size() - 1 DO
      names.addhi(store.get(i))
    END
  END SanitizeNames;

PROCEDURE CountActiveNodes(seq : CardSeq.T) : CARDINAL =
  VAR
    res : CARDINAL := 0;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      IF seq.get(i) # NoMapping THEN
        INC(res)
      END
    END;
    RETURN res
  END CountActiveNodes;
  
PROCEDURE FormatFN(i : CARDINAL) : TEXT =
  BEGIN RETURN F("%08s", Int(i)) END FormatFN;

PROCEDURE PutNames(wr             : Wr.T;
                   i              : CARDINAL;
                   seq            : TextSeq.T;
                   includeIdNames : BOOLEAN)
  RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    rawNm, nm : TEXT;
  BEGIN
    FOR j := 0 TO seq.size() - 1 DO
      IF j # 0 THEN Wr.PutChar(wr, '=') END;
      
      rawNm := seq.get(j);
      nm := TextUtils.ReplaceChar(UnNil(rawNm), ':', '_');
      
      (* aplot has trouble with colons in node names, so rename those,
         sorry about any clashes ... *)
      
      Wr.PutText(wr, nm);
    END;

    IF includeIdNames THEN
      Wr.PutText(wr, F("=NAMES%s", Int(i)))
    END;
  
    Wr.PutChar(wr, '\n')
  END PutNames;

PROCEDURE WriteNames(wd, ofn       : Pathname.T;

                     names         : TextSeqSeq.T;

                     idxMap        : CardSeq.T;
                     (* map of input node to output node *)
                     
                     maxFiles      : CARDINAL;

                     VAR nFiles    : CARDINAL;

                     VAR wdWr      : REF ARRAY OF Wr.T;

                     VAR wdPth     : REF ARRAY OF Pathname.T;

                     includeIdNames: BOOLEAN) : CARDINAL =
  VAR
    anWr, wr : Wr.T;
    nFn  := ofn & ".names";
    anFn := ofn & ".allnames";

    nNodes := names.size();
    (* this is the number of names in the file, not the number of names
       written? *)

    aNodes := CountActiveNodes(idxMap);
  BEGIN
    nFiles := MIN(aNodes, maxFiles); (* note that nNodes includes TIME *)
    
    Debug.Out(F("%s nodes (incl. TIME), %s nodes active",
                Int(nNodes),
                Int(aNodes)));
    Debug.Out(F("%s files", Int(nFiles)));
    
    TRY
      wdWr  := NEW(REF ARRAY OF Wr.T,       nFiles);
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
        IF idxMap.get(i) # NoMapping THEN
          PutNames(wr, i, names.get(i), includeIdNames)
        END;
        PutNames(anWr, i, names.get(i), includeIdNames)
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
