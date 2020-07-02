MODULE DefFormat;
IMPORT Rd;
FROM DefLexer IMPORT String, State, Buffer, Digit;
IMPORT Debug;
IMPORT Text;
IMPORT DefParseTrie;
IMPORT ParseProc;
IMPORT DefLexer;
IMPORT DefPoint, DefPointSeq;
IMPORT Fmt;
FROM ParseError IMPORT E;

REVEAL
  T = Public BRANDED Brand OBJECT
    buff  : Buffer;
    token : String;
    state : State;
    eop   := FALSE; (* done parsing *)

    version : TEXT;

  METHODS
    getCard(VAR c : CARDINAL) : BOOLEAN := GetCard;
    error() := Error;
    getIdentifier(VAR txt : TEXT) : BOOLEAN := GetIdentifier;
    tokMustBeChar(c : CHAR; dbg : TEXT := NIL) RAISES { E } := 
                                              TokMustBeChar;
    tokMustNotBeChar(c : CHAR; dbg : TEXT := NIL) RAISES { E } := 
                                                 TokMustNotBeChar;
    tokMustBeChars(READONLY a : ARRAY OF CHAR; dbg : TEXT := NIL) RAISES { E } := 
                                                                 TokMustBeChars;
  END;

CONST DQ = '"';

PROCEDURE TokMustBeChar(t : T; c : CHAR; dbg : TEXT) RAISES { E } =
  BEGIN
    IF NOT GetChar(t, c) THEN
      RAISE E("TokMustBeChar: "&BrackOrEmpty(dbg)&" expected '" & Text.FromChar(c) & 
        "' but got \"" & S2T(t.buff, t.token) & "\"")
    END
  END TokMustBeChar;

PROCEDURE GetChar(t : T; c : CHAR) : BOOLEAN =
  BEGIN
    IF t.token.n = 1 AND t.buff[t.token.start] = c THEN
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetChar;

PROCEDURE GetCharSet(t : T; s : SET OF CHAR; VAR c : CHAR) : BOOLEAN =
  BEGIN
    IF t.token.n = 1 AND t.buff[t.token.start] IN s THEN
      c := t.buff[t.token.start];
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetCharSet;

PROCEDURE GetToken(t : T; READONLY tok : ARRAY OF CHAR) : BOOLEAN =
  BEGIN
    IF SUBARRAY(t.buff, t.token.start, t.token.n) = tok THEN
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetToken;

PROCEDURE TokMustNotBeChar(t : T; c : CHAR; dbg : TEXT) RAISES { E } =
  BEGIN
    IF t.token.n = 1 AND t.buff[t.token.start] = c THEN
      RAISE E("TokMustNotBeChar: "&BrackOrEmpty(dbg)&" illegal \"" & 
            S2T(t.buff, t.token) & "\"")
    ELSE
      Next(t)
    END
  END TokMustNotBeChar;

PROCEDURE TokMustBeChars(t : T; READONLY a : ARRAY OF CHAR; dbg : TEXT) RAISES { E } =
  BEGIN
    IF t.token.n # NUMBER(a) OR 
       SUBARRAY(t.buff, t.token.start, t.token.n) # a THEN
      RAISE E("TokMustBeChars: "&BrackOrEmpty(dbg)&" expected '" & Text.FromChars(a) & 
        "' but got \"" & S2T(t.buff, t.token) & "\"")
    END;
    Next(t)
  END TokMustBeChars;

PROCEDURE BrackOrEmpty(txt : TEXT) : TEXT =
  BEGIN
    IF txt = NIL THEN RETURN "" ELSE RETURN "["&txt&"]" END
  END BrackOrEmpty;

PROCEDURE GetCard(t : T; VAR c : CARDINAL) : BOOLEAN =
  VAR 
    res := 0;
  BEGIN
    FOR i := t.token.start TO t.token.start+t.token.n-1 DO
      WITH num = ORD(t.buff[i])-ORD('0') DO
        IF num < 0 OR num > 9 THEN RETURN FALSE END;
        res := 10*res+num
      END
    END;
    c := res;
    Next(t);
    D("Card"); RETURN TRUE
  END GetCard;

PROCEDURE MustBeCard(t : T; VAR c : CARDINAL) RAISES { E } = 
  BEGIN
    IF NOT GetCard(t, c) THEN
      RAISE E ("MustBeCard, expected number")
    END
  END MustBeCard;

PROCEDURE MustBeInt(t : T; VAR i : INTEGER) RAISES { E } =
  (* kind of ugly *)
  VAR
    j : CARDINAL;
  BEGIN
    IF GetCard(t, j) THEN
      i := j;
      RETURN 
    ELSIF t.token.n = 1 AND t.buff[t.token.start] = '+' THEN
      Next(t); 
      MustBeCard(t, j);
      i := j;
      RETURN
    ELSIF t.token.n = 1 AND t.buff[t.token.start] = '-' THEN
      Next(t); 
      IF NOT GetCard(t, j) THEN
        RAISE E("MustBeInt, expected number")
      END;
      i := -j;
      RETURN
    ELSE
      RAISE E("MustBeInt, expected integer")
    END
  END MustBeInt;

PROCEDURE MustBePoint(t : T; VAR p : DefPoint.T; dbg : TEXT := NIL) RAISES { E } =
  BEGIN
    t.tokMustBeChar('(');
    MustBeInt(t, p.x);
    MustBeInt(t, p.y);
    t.tokMustBeChar(')',dbg);
  END MustBePoint;
  
PROCEDURE Error(t : T) =
  BEGIN
    Debug.Error("PARSE ERROR, lately reading: " & 
      Text.FromChars(SUBARRAY(t.buff,
                              t.token.start,
                              t.token.n)))
  END Error;
  
PROCEDURE Next(t : T) =
  BEGIN 
    t.eop := NOT DefLexer.GetToken(t.buff, t.state, t.token) ;
    Debug.Out("Token \"" & S2T(t.buff, t.token) & "\"")
  END Next;
  
PROCEDURE GetIdentifier(t : T; VAR ident : TEXT) : BOOLEAN =
  VAR
    ok := FALSE;
  BEGIN
    (* check its not a special character or a number *)
    IF    t.token.n = 0 THEN 
      RETURN FALSE
    ELSIF t.buff[t.token.start] IN t.state.special THEN
      <*ASSERT t.token.n = 1*>
      RETURN FALSE
    ELSE
      FOR i := t.token.start TO t.token.start + t.token.n - 1 DO
        IF NOT t.buff[i] IN Digit THEN 
          ok := TRUE
        END
      END
    END;

    IF NOT ok THEN RETURN FALSE END;
      
    ident := Text.FromChars(SUBARRAY(t.buff, t.token.start, t.token.n));
    Next(t);
    D("Identifier"); 
    RETURN TRUE
  END GetIdentifier;
  
  <*NOWARN*>PROCEDURE D(what : TEXT) = BEGIN (*IO.Put(what & "\n")*) END D;

PROCEDURE Parse(rd : Rd.T) : T RAISES { E } =

  VAR
    (* parsing *)
    t := NEW(T);
  BEGIN
    
    t.state.rd := rd;

    Next(t); (* establish lookahead *)

    TRY
      WHILE NOT t.state.eof DO
        (* note that this type of "block" is a bit different because
           it doesn't end with an END statement *)
        ParseBlock(ARRAY OF CHAR {} ,
                   topDisp, 
                   t, 
                   t)        
      END
    EXCEPT
      E(x) => RAISE E ("DefFormat.Parse: Error" & x & ", line " & Fmt.Int(t.state.line))
    END;

    RETURN t

  END Parse;

PROCEDURE S2T(READONLY buff : Buffer; s : String) : TEXT =
  BEGIN RETURN Text.FromChars(SUBARRAY(buff, s.start, s.n)) END S2T;

CONST A2T = Text.FromChars;

(**********************************************************************)

PROCEDURE ParseVersion(t : T; ref : REFANY) RAISES { E } =
  BEGIN
    <*ASSERT t = ref*>
    t.version := S2T(t.buff, t.token);
    Debug.Out("ParseVersion, t.version = " & t.version);
    Next(t);
    t.tokMustBeChar(';',"VERSION")
  END ParseVersion;

PROCEDURE ParseDividerChar(t : T; ref : REFANY) RAISES { E } =
  BEGIN
    <*ASSERT t = ref*>
    WITH chars = SUBARRAY(t.buff, t.token.start, t.token.n) DO
      IF NUMBER(chars) # 3 OR chars[0] # DQ OR chars[2] # DQ THEN
        Debug.Error("DefFormat.ParseDividerChar ?syntax error")
      END;
      DefLexer.DividerChar(t.state, chars[1])
    END;
    Next(t);
    t.tokMustBeChar(';',"DIVIDERCHAR")
  END ParseDividerChar;

PROCEDURE ParseBusbitChars(t : T; ref : REFANY) RAISES { E } =
  BEGIN
    <*ASSERT t = ref*>
    WITH chars = SUBARRAY(t.buff, t.token.start, t.token.n) DO
      IF NUMBER(chars) # 4 OR chars[0] # DQ OR chars[3] # DQ THEN
        Debug.Error("DefFormat.ParseBusbitChars ?syntax error")
      END;
      DefLexer.BusbitChars(t.state, SUBARRAY(chars, 1, 2))
    END;
    Next(t);
    t.tokMustBeChar(';',"BUSBITCHARS")
  END ParseBusbitChars;

(**********************************************************************)

TYPE
  Design = OBJECT
    name : TEXT;
    distUnits : CARDINAL;
    diearea : DefPointSeq.T;
    end := FALSE;
  END;

PROCEDURE ParseDesign(t : T; ref : REFANY) RAISES { E } =
  VAR 
    des := NEW(Design);
  BEGIN
    <*ASSERT t = ref*>
    des.name := S2T(t.buff, t.token);
    Next(t);
    t.tokMustBeChar(';',"DESIGN");
    
    LOOP
      Debug.Out("DefFormat.ParseDesign kw=" & S2T(t.buff, t.token));
      WITH f = designDisp.get(SUBARRAY(t.buff, t.token.start, t.token.n)) DO
        IF f = NIL THEN
          RAISE E("DefFormat.ParseDesign, syntax error \"" & S2T(t.buff, t.token) & "\"")
        ELSE
          Next(t);
          f(t, des)
        END
      END
    END
  END ParseDesign;

PROCEDURE ParseDesignUnits(t : T; ref : REFANY) RAISES { E } =
  BEGIN
    WITH des = NARROW(ref, Design) DO
      t.tokMustBeChars(DISTANCEa^);
      t.tokMustBeChars(MICRONSa^);
      
      IF NOT t.getCard(des.distUnits) THEN
        RAISE E("ParseDesignUnits: ?syntax error: \"" & S2T(t.buff, t.token) & "\"")
      END;
      t.tokMustBeChar(';',"DESIGNUNITS")
    END
  END ParseDesignUnits;

PROCEDURE ParseDieArea(t : T; ref : REFANY) RAISES { E } =
  BEGIN
    WITH des = NARROW(ref, Design),
         seq = NEW(DefPointSeq.T).init() DO
      LOOP
        IF SUBARRAY(t.buff, t.token.start, t.token.n) = Semia^ THEN
          IF seq.size() < 2 THEN
            RAISE E("DIEAREA with fewer than 2 points")
          END;
          des.diearea := seq;
          Next(t);
          RETURN
        END;
        VAR 
          p : DefPoint.T;
        BEGIN
          MustBePoint(t, p, "DIEAREA");
          seq.addhi(p)
        END
      END
    END
  END ParseDieArea;

PROCEDURE ParseRow(t : T; ref : REFANY) RAISES { E } = 
  VAR
    rowName, siteName, siteOrient : TEXT;
    origX, origY : INTEGER;
    numX, numY : CARDINAL := 1;
    stepX, stepY : INTEGER := 0;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      IF NOT t.getIdentifier(rowName) OR NOT t.getIdentifier(siteName) THEN
        RAISE E("ParseRow, rowName, siteName")
      END;

      MustBeInt(t, origX);
      MustBeInt(t, origY);

      IF NOT t.getIdentifier(siteOrient) THEN
        RAISE E("ParseRow, siteOrient")
      END;

      IF GetToken(t, Semia^) THEN
        RETURN
      END;

      IF GetToken(t, DOa^) THEN
        IF NOT GetCard(t, numX) THEN RAISE E("ParseRow, numX") END;
        
        IF NOT GetToken(t, BYa^) THEN RAISE E("ParseRow, DO...BY") END;

        IF NOT GetCard(t, numY) THEN  RAISE E("ParseRow, numY") END;
        
        IF GetToken(t, Semia^) THEN
          RETURN
        ELSIF GetToken(t, STEPa^) THEN

          MustBeInt(t, stepX);
          MustBeInt(t, stepY)
        END;

        IF GetToken(t, Semia^) THEN
          RETURN
        ELSE
          <*ASSERT FALSE*> (* ROW-PROPERTY not implemented yet *)
        END
      END
    END
  END ParseRow;

PROCEDURE ParseTracks(t : T; ref : REFANY) RAISES { E } =
  VAR
    xy : CHAR;
    numTracks, space, mask : CARDINAL;
    start : INTEGER;
  BEGIN
    IF NOT GetCharSet(t, SET OF CHAR { 'X', 'Y' }, xy) THEN
      RAISE E("ParseTracks, xy")
    END;

    MustBeInt(t, start);
    
    IF NOT GetToken(t, DOa^) THEN
      RAISE E("ParseTracks, ?DO")
    END;

    MustBeCard(t, numTracks);

    IF NOT GetToken(t, STEPa^) THEN
      RAISE E("ParseTracks, ?STEP")
    END;

    MustBeCard(t, space);

    IF GetToken(t, MASKa^) THEN
      MustBeCard(t, mask);
      
      IF NOT GetToken(t, SAMEMASKa^) THEN
        RAISE E ("ParseTracks, ?MASK..SAMEMASK")
      END
    END;

    IF NOT GetToken(t, LAYERa^) THEN
      RAISE E ("ParseTracks, ?LAYER")
    END;

    (* now pointing to layer name *)
    
    t.tokMustNotBeChar(';');

    (* do not support multiple layers at this time *)

    t.tokMustBeChar(';')
  END ParseTracks;

(**********************************************************************)

PROCEDURE ParsePropertyDefinitions(t : T; ref : REFANY) 
  RAISES { E } =
  BEGIN
    WITH des = NARROW(ref, Design) DO
      ParseBlock(PROPERTYDEFINITIONSa^, propDisp, t, ref)
    END
  END ParsePropertyDefinitions;

PROCEDURE ParseBlock(READONLY type : ARRAY OF CHAR;
                     keywords      : DefParseTrie.T;
                     t             : T;
                     ref           : REFANY) RAISES { E } =
  BEGIN
    LOOP
      Debug.Out(A2T(type) & " kw=" & S2T(t.buff, t.token));
      WITH nxt = SUBARRAY(t.buff, t.token.start, t.token.n) DO
        IF nxt = ENDa^ THEN
          Next(t);

          WITH nxt2 = SUBARRAY(t.buff, t.token.start, t.token.n) DO
            IF nxt2 = type THEN 
              Next(t);
              RETURN
            ELSE
              RAISE E ("? END " & A2T(nxt2) & " ending block " & A2T(type))
            END
          END
        END;

        WITH f = keywords.get(nxt) DO
          IF f = NIL THEN
            RAISE E(A2T(type) & 
                  ", syntax error \"" & S2T(t.buff, t.token) & "\"")
          ELSE
            Next(t);
            f(t, ref)
          END
        END
      END
    END
  END ParseBlock;

(**********************************************************************)

PROCEDURE IgnorePropertyDefinition(t : T; ref : REFANY) 
  RAISES { E } =
  BEGIN
    (* skip name *) 
    Next(t); 

    (* skip type *)
    Next(t);

    WITH nxt = SUBARRAY(t.buff, t.token.start, t.token.n) DO
      IF nxt = Semia^ THEN 
        (* skip *)
      ELSIF nxt = RANGEa^ THEN
        Next(t);
        t.tokMustNotBeChar(';');
        t.tokMustNotBeChar(';');
      ELSE
        (* was def value *)
        Next(t);
      END
    END;
    t.tokMustBeChar(';')
  END IgnorePropertyDefinition;

(**********************************************************************)

VAR 
  topDisp    := NEW(DefParseTrie.T).init(NIL);
  designDisp := NEW(DefParseTrie.T).init(NIL);
  propDisp   := NEW(DefParseTrie.T).init(NIL);


PROCEDURE AddKeyword(to : DefParseTrie.T; 
                     kw : TEXT;
                     f  : ParseProc.T) =
  VAR
    buff := MakeCA(kw);
  BEGIN
    EVAL to.put(buff^, f)
  END AddKeyword;

PROCEDURE MakeCA(txt : TEXT) : REF ARRAY OF CHAR =
  VAR
    buff := NEW(REF ARRAY OF CHAR, Text.Length(txt));
  BEGIN
    Text.SetChars(buff^, txt);
    RETURN buff
  END MakeCA;

VAR
  DISTANCEa := MakeCA("DISTANCE");
  MICRONSa  := MakeCA("MICRONS");

  REALa     := MakeCA("REAL");
  STRINGa   := MakeCA("STRING");
  INTEGERa  := MakeCA("INTEGER");
  PROPERTYDEFINITIONSa := MakeCA("PROPERTYDEFINITIONS");
  Semia     := MakeCA(";");
  RANGEa    := MakeCA("RANGE");
  ENDa      := MakeCA("END");
  DOa       := MakeCA("DO");
  BYa       := MakeCA("BY");
  STEPa       := MakeCA("STEP");
  MASKa       := MakeCA("MASK");
  LAYERa       := MakeCA("LAYER");
  SAMEMASKa       := MakeCA("SAMEMASK");

BEGIN 

  AddKeyword(topDisp, "VERSION",             ParseVersion);
  AddKeyword(topDisp, "DIVIDERCHAR",         ParseDividerChar);
  AddKeyword(topDisp, "BUSBITCHARS",         ParseBusbitChars);
  AddKeyword(topDisp, "DESIGN",              ParseDesign);

  AddKeyword(designDisp, "UNITS",               ParseDesignUnits);
  AddKeyword(designDisp, "PROPERTYDEFINITIONS", ParsePropertyDefinitions);
  AddKeyword(designDisp, "DIEAREA",             ParseDieArea);
  AddKeyword(designDisp, "ROW",                 ParseRow);
  AddKeyword(designDisp, "TRACKS",              ParseTracks);
  AddKeyword(designDisp, "GCELLGRID",           NIL);
  AddKeyword(designDisp, "VIAS",                NIL);
  AddKeyword(designDisp, "NONDEFAULTRULES",     NIL);
  AddKeyword(designDisp, "REGIONS",             NIL);
  AddKeyword(designDisp, "COMPONENTMASKSHIFT",  NIL);
  AddKeyword(designDisp, "COMPONENTS",          NIL);
  AddKeyword(designDisp, "PINS",                NIL);
  AddKeyword(designDisp, "BLOCKAGES",           NIL);
  AddKeyword(designDisp, "FILLS",               NIL);
  AddKeyword(designDisp, "SPECIALNETS",         NIL);
  AddKeyword(designDisp, "NETS",                NIL);
  AddKeyword(designDisp, "GROUPS",              NIL);

  AddKeyword(propDisp,   "DESIGN",              IgnorePropertyDefinition);
  AddKeyword(propDisp,   "SPECIALNET",          IgnorePropertyDefinition);
  AddKeyword(propDisp,   "NET",                 IgnorePropertyDefinition);
  AddKeyword(propDisp,   "COMPONENTPIN",        IgnorePropertyDefinition);
  AddKeyword(propDisp,   "NONDEFAULTRULE",      IgnorePropertyDefinition);
  AddKeyword(propDisp,   "COMPONENT",           IgnorePropertyDefinition);
  
  

END DefFormat.
