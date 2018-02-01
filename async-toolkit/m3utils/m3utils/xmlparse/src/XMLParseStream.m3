UNSAFE MODULE XMLParseStream;

(* this module adapts the C callbacks from expat to Modula-3 OOP callbacks *)

FROM Ctypes IMPORT const_char_star;
IMPORT XMLParseImpl;
IMPORT Debug, Fmt; FROM Fmt IMPORT F, Int;
IMPORT xmlParserContext;
IMPORT xmlParser; FROM xmlParser IMPORT xmlNullCopy, xmlLenCopy, xmlCopyFree;
IMPORT M3toC;
IMPORT Pathname;
IMPORT Text;

TYPE
  Result = OBJECT depth : CARDINAL; nxt : Result END;
  
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

(* the read loop in XMLParse does the following:

   parse some stuff, wait for expat's XML_Parse to return, call Post (below).

   what we do is that if we hit atag we're interested in, we stop the
   parser and return.  This means that the expat parser ALMOST
   immediately returns in a stopped state, causing our parse loop to
   call Post below.

   We can't do all the work in the callbacks because during XML_Parse,
   thread switching and garbage collection are turned off!  Well we
   could, but we'd run out of memory pretty quickly, and we don't want
   that to happen, do we?

   So instead we create a record of the callback from expat, using a 
   Result object, and force XML_Parse to return.  We then handle the 
   Result record in the Post method, when Modula-3 is operating normally.

   2/1/18 the handling of ignoreUntil is extremely tricky!

   The parser calls DoStart, ..., DoEnd synchronously.  If we are
   ignoring, we ignore at this point.

   None of these can set ignoreUntil, but end can clear ignoreUntil.

   These are mapped to results and queued.

   Then DoPost starts running.
   The results are taken off the queue and passed to various callbacks.

   The callbacks can set ignoreUntil (by returning Disp.Abort).

   There is a race condition, where several tags can get through if
   the parser returns more than one thing at a time to Post.

   The difficult situation to handle is a Start and End tag together.

   The Start and End tags are not ignored.

   The callback from Start sets ignoreUntil.

   It is now too late to release ignoreUntil from the C callback.

   Instead a second check is inserted in DoPost to clear ignoreUntil in
   this special case.
*)
    
PROCEDURE SetResult(c : xmlParserContext.T; state : State; to : Result) =
  (* stop XML parser and set result *)
  BEGIN
    to.depth := state.depth;
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

(* called from C, thread switching and GC turned off *)
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

(* called from C, thread switching and GC turned off *)
PROCEDURE DoAttr(c : xmlParserContext.T; s : REFANY; tag, attr : const_char_star) =
  VAR
    state := NARROW(s, State);
  BEGIN
    IF DoDebug THEN Debug.Out("Attr") END;
    IF state.ignoreUntil < state.depth THEN RETURN END;
    SetResult(c, state, NEW(Attr, tag := xmlNullCopy(tag), attr := xmlNullCopy(attr)))
  END DoAttr;

(* called from C, thread switching and GC turned off *)
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


(* called from C, thread switching and GC turned off *)
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
  (* this routine is called at the end of the read/parse loop that interacts
     with C, and it is called in a pure Modula-3 environment, with
     thread switching and garbage collection turned on *)
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
            (* really we should check ignoreUntil here, in case something
               got through the first-level parser *)
            IF DoDebug THEN Debug.Out("DoPost Start") END;
            disp := state.obj.start(M3toC.CopyStoT(z.el))
          |
            Attr(z) =>
            (* really we should check ignoreUntil here, in case something
               got through the first-level parser *)
            IF DoDebug THEN Debug.Out("DoPost Attr") END;
            disp := state.obj.attr(M3toC.CopyStoT(z.tag), M3toC.CopyStoT(z.attr))
          |
            End =>
            IF DoDebug THEN Debug.Out("DoPost End") END;
            IF state.ignoreUntil = p.depth THEN
              IF DoDebug THEN Debug.Out("End stop ignoring") END;
              state.ignoreUntil := LAST(CARDINAL) (* done ignoring element *)
            END;
            (* really we should check ignoreUntil here, in case something
               got through the first-level parser *)
            state.obj.end()
          |
            CharData(z) =>
            (* really we should check ignoreUntil here, in case something
               got through the first-level parser *)
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
          IF DoDebug THEN Debug.Out("DoPost disp = " & DispNames[disp]) END;
          CASE disp OF
            Disp.Continue => (* skip *)
          |
            Disp.Abort    =>
            state.ignoreUntil := p.depth-1;

            (* there's a race condition here.
               if we set ignoreUntil and there are things still in the queue
               we have to ignore them actively somehow... *)
            
            IF DoDebug THEN Debug.Out("DoPost start ignoring until " & Fmt.Int(state.ignoreUntil) & " state.depth = " & Fmt.Int(state.depth) & " result.depth = " & Fmt.Int(p.depth)) END;

          |
            Disp.Pop      =>
            IF DoDebug THEN Debug.Out("DoPost start POP ignoring") END;
            state.ignoreUntil := p.depth-2
          END
        END;
        p := p.nxt
      END;

      xmlCopyFree()
      
    END
  END DoPost;

PROCEDURE Buff2Text(len : CARDINAL; data : const_char_star) : TEXT =
  (* this routine is actually only used for debugging *)
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
  (* the state of our parsing *)
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
