UNSAFE MODULE XMLParseStream;
FROM Ctypes IMPORT const_char_star;
IMPORT XMLParseImpl;
IMPORT Debug, Fmt; FROM Fmt IMPORT F, Int;
IMPORT xmlParserContext;
IMPORT xmlParser; FROM xmlParser IMPORT xmlNullCopy, xmlLenCopy, xmlCopyFree;
IMPORT M3toC;
IMPORT Pathname;
IMPORT Text;

TYPE
  Result = OBJECT nxt : Result END;
  
  Start = Result OBJECT el : const_char_star END;

  Attr = Result OBJECT tag, attr : const_char_star END;

  End  = Result BRANDED "End" OBJECT END;

  CharData = Result OBJECT len : CARDINAL; data : const_char_star END;

REVEAL
  T = Public BRANDED OBJECT
    state : State;
  END;

  FileStream = T OBJECT METHODS
    init(pn : Pathname.T) : FileStream;
  END BRANDED OBJECT
    path : Pathname.T;
  OVERRIDES
    init := Init;
    parse := Parse;
  END;

VAR DoDebug := Debug.GetLevel() > 10;

PROCEDURE SetResult(c : xmlParserContext.T; state : State; to : Result) =
  BEGIN
    IF NOT state.stopped THEN
      WITH stopRes = xmlParser.xmlParseStopParser(c, 1) DO
        IF stopRes = xmlParser.XML_STATUS_ERROR THEN
          Debug.Error("stop " & Fmt.Int(stopRes))
        END
      END;
      state.stopped := TRUE
    END;
    to.nxt := state.result;
    state.result := to
  END SetResult;

(* called from C *)
PROCEDURE DoStart(c : xmlParserContext.T; s : REFANY; el : const_char_star) =
  VAR
    state := NARROW(s, State);
  BEGIN
    IF DoDebug THEN Debug.Out("Start " & M3toC.CopyStoT(el)) END;
    INC(state.depth);
    IF state.ignoreUntil < state.depth THEN
      IF DoDebug THEN Debug.Out("Start ignoring...") END;
      RETURN
    END;
    SetResult(c, state, NEW(Start, el := xmlNullCopy(el)))
  END DoStart;

(* called from C *)
PROCEDURE DoAttr(c : xmlParserContext.T; s : REFANY; tag, attr : const_char_star) =
  VAR
    state := NARROW(s, State);
  BEGIN
    IF DoDebug THEN Debug.Out("Attr") END;
    IF state.ignoreUntil < state.depth THEN RETURN END;
    SetResult(c, state, NEW(Attr, tag := xmlNullCopy(tag), attr := xmlNullCopy(attr)))
  END DoAttr;

PROCEDURE DoEnd(c : xmlParserContext.T; s : REFANY) =
  VAR
    state := NARROW(s, State);
  BEGIN
    IF DoDebug THEN Debug.Out("End @ " & Fmt.Int(state.depth)) END;
    DEC(state.depth);
    IF state.ignoreUntil = state.depth THEN
      IF DoDebug THEN Debug.Out("DoEnd stop ignoring") END;
      state.ignoreUntil := LAST(CARDINAL) (* done ignoring element *)
    ELSIF state.ignoreUntil < state.depth THEN
      IF DoDebug THEN Debug.Out("DoEnd actively ignoring") END;
      RETURN
    END;
    SetResult(c, state, NEW(End));
  END DoEnd;

(* called from C *)
PROCEDURE DoCharData(c : xmlParserContext.T;
                     s : REFANY;
                     len : CARDINAL;
                     data : const_char_star) =
  VAR
    state := NARROW(s, State);
  BEGIN
    IF DoDebug THEN Debug.Out("CharData") END;
    IF state.ignoreUntil < state.depth THEN RETURN END;
    SetResult(c, state, NEW(CharData, len := len, data := xmlLenCopy(data, len)));
  END DoCharData;

PROCEDURE DoPost(<*UNUSED*>c : xmlParserContext.T; s : REFANY) =
  VAR
    state := NARROW(s, State);
  BEGIN
    IF DoDebug THEN Debug.Out("---Post---") END;

    state.stopped := FALSE;
    
    VAR
      q : Result := NIL;
      p := state.result;
      n : Result;
      cnt := 0;
    BEGIN
      (* reverse the list of stuff *)
      WHILE p # NIL DO
        n := p.nxt;
        p.nxt := q;
        q := p;
        p := n;
        INC(cnt)
      END;
      state.result := NIL;

      IF DoDebug THEN Debug.Out(Fmt.Int(cnt) & " items in DoPost") END;

      (* now traverse it *)
      p := q;
      WHILE p # NIL DO
        VAR
          disp := Disp.Continue;
        BEGIN
          TYPECASE p OF
            Start(z) =>
            disp := state.obj.start(M3toC.CopyStoT(z.el))
          |
            Attr(z) =>
            disp := state.obj.attr(M3toC.CopyStoT(z.tag), M3toC.CopyStoT(z.attr))
          |
            End =>
            state.obj.end()
          |
            CharData(z) =>
            IF DoDebug THEN
              Debug.Out(F("DoPost, CharData len %s buff %s",
                          Int(z.len), Buff2Text(z.len, z.data)))
            END;
            WITH lastcharP = z.data + z.len - 1,
                 c         = LOOPHOLE(lastcharP, UNTRACED REF CHAR)^ DO

              IF DoDebug THEN Debug.Out("DoPost CharData tail ASCII " &
                Int(ORD(c))) END;
              
              IF c = '\000' THEN
                IF DoDebug THEN Debug.Out("DoPost stripping null from CharData") END;
                DEC(z.len)
              END
            END;
            
            VAR charCopy := NEW(REF ARRAY OF CHAR, z.len);
                x := LOOPHOLE(z.data,ADDRESS);
                i := 0;
            BEGIN
              WHILE x < z.data + z.len DO
                WITH cp = LOOPHOLE(x,UNTRACED REF CHAR) DO
                  charCopy[i] := cp^
                END;
                INC(x);
                INC(i)
              END;
              disp := state.obj.charData(charCopy^)
            END
          ELSE
              <*ASSERT FALSE*>
          END;
          CASE disp OF
            Disp.Continue => (* skip *)
          |
            Disp.Abort    =>
            IF DoDebug THEN Debug.Out("DoPost start ignoring") END;
            state.ignoreUntil := state.depth-1
          |
            Disp.Pop      =>
            IF DoDebug THEN Debug.Out("DoPost start POP ignoring") END;
            state.ignoreUntil := state.depth-2
          END
        END;
        p := p.nxt
      END;

      xmlCopyFree()
      
    END
  END DoPost;

PROCEDURE Buff2Text(len : CARDINAL; data : const_char_star) : TEXT =
  VAR
    charCopy := NEW(REF ARRAY OF CHAR, len);
    x := LOOPHOLE(data,ADDRESS);
    i := 0;
  BEGIN
    WHILE x < data + len DO
      WITH cp = LOOPHOLE(x,UNTRACED REF CHAR) DO
        charCopy[i] := cp^
      END;
      INC(x);
      INC(i)
    END;
    RETURN Text.FromChars(charCopy^)
  END Buff2Text;
  
TYPE
  State = OBJECT
    depth := 0;
    ignoreUntil := LAST(CARDINAL);
    result : Result := NIL;
    stopped := FALSE;
    obj : T;
  END;

PROCEDURE Init(self : FileStream; pn : Pathname.T) : FileStream =
  BEGIN
    self.state := NEW(State, obj := self);
    self.path := pn;
    RETURN self
  END Init;

PROCEDURE Parse(self : FileStream) =
  BEGIN
    XMLParseImpl.DoItImpl(self.path,
                          self.state,
                          DoStart,
                          DoAttr,
                          DoEnd,
                          DoCharData,
                          DoPost)
  END Parse;

BEGIN END XMLParseStream.
