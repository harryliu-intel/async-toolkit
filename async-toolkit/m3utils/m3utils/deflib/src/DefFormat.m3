MODULE DefFormat;
IMPORT Rd;
FROM DefLexer IMPORT String, State, Buffer, Digit;
IMPORT Debug;
IMPORT Text;
IMPORT DefParseTrie;
IMPORT ParseProc, ParseProcRec;
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

    lately := ParseProcRec.Default;

  METHODS
    getCard(VAR c : CARDINAL) : BOOLEAN := GetCard;
    error() := Error;
    getIdentifier(VAR txt : TEXT) : BOOLEAN := GetIdentifier;
  END;

CONST DQ = '"';

PROCEDURE MustBeSingle(t : T; VAR c : CHAR) RAISES { E } =
  BEGIN
    IF NOT GetSingle(t, c) THEN
      RAISE E("MustBeSingle: "&BrackOrEmpty(t.lately.nm)&" expected single but got \"" & S2T(t.buff, t.token) & "\"")
    END
  END MustBeSingle;

PROCEDURE GetSingle(t : T; VAR c : CHAR) : BOOLEAN =
  BEGIN
    IF t.token.n = 1 THEN
      c := t.buff[t.token.start];
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetSingle;

PROCEDURE GetChar(t : T; c : CHAR) : BOOLEAN =
  BEGIN
    IF t.token.n = 1 AND t.buff[t.token.start] = c THEN
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetChar;

PROCEDURE MustBeChar(t : T;  c : CHAR) RAISES { E } =
  BEGIN
    IF NOT GetChar(t, c) THEN
      RAISE E("MustBeChar: "&BrackOrEmpty(t.lately.nm)&" expected '"&Text.FromChar(c)&"' but got \"" & S2T(t.buff, t.token) & "\"")
    END
  END MustBeChar;

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

PROCEDURE MustBeToken(t : T; READONLY tok : ARRAY OF CHAR) RAISES { E } =
  BEGIN
    IF NOT GetToken(t, tok) THEN
      RAISE E("MustBeToken: "&BrackOrEmpty(t.lately.nm)&" expected '" & A2T(tok) & 
        "' but got \"" & S2T(t.buff, t.token) & "\"")
    END
  END MustBeToken;

PROCEDURE MustNotBeChar(t : T; c : CHAR) RAISES { E } =
  BEGIN
    IF t.token.n = 1 AND t.buff[t.token.start] = c THEN
      RAISE E("MustNotBeChar: "&BrackOrEmpty(t.lately.nm)&" illegal \"" & 
            S2T(t.buff, t.token) & "\"")
    ELSE
      Next(t)
    END
  END MustNotBeChar;

PROCEDURE MustBeChars(t : T; READONLY a : ARRAY OF CHAR) RAISES { E } =
  BEGIN
    IF t.token.n # NUMBER(a) OR 
       SUBARRAY(t.buff, t.token.start, t.token.n) # a THEN
      RAISE E("MustBeChars: "&BrackOrEmpty(t.lately.nm)&" expected '" & Text.FromChars(a) & 
        "' but got \"" & S2T(t.buff, t.token) & "\"")
    END;
    Next(t)
  END MustBeChars;

PROCEDURE MustBeCharSet(t : T; s : SET OF CHAR; VAR c : CHAR) RAISES { E } =
  BEGIN
    IF t.token.n # 1 OR NOT t.buff[t.token.start] IN s THEN
      RAISE E("MustBeCharSet: unexpected \"" & S2T(t.buff, t.token) & "\"")
    END;
    c := t.buff[t.token.start];
    Next(t)
  END MustBeCharSet;

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
      RAISE E ("MustBeCard, "&BrackOrEmpty(t.lately.nm)&" expected number")
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
        RAISE E("MustBeInt, "&BrackOrEmpty(t.lately.nm)&" expected number")
      END;
      i := -j;
      RETURN
    ELSE
      RAISE E("MustBeInt, "&BrackOrEmpty(t.lately.nm)&" expected integer")
    END
  END MustBeInt;

PROCEDURE MustBePoint(t : T; VAR p : DefPoint.T) RAISES { E } =
  BEGIN
    MustBeChar(t,'(');
    MustBeInt(t, p.x);
    MustBeInt(t, p.y);
    MustBeChar(t,')');
  END MustBePoint;

PROCEDURE GetPoint(t : T; VAR p : DefPoint.T) : BOOLEAN RAISES { E } =
  (* a bit of a hack because of the LL(1) capability here *)
  BEGIN
    IF NOT GetChar(t, '(') THEN
      RETURN FALSE
    END;
    MustBeInt(t, p.x);
    MustBeInt(t, p.y);
    MustBeChar(t,')');
    RETURN TRUE
  END GetPoint;
  
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
  (* we could use a char buffer instead of TEXT here to reduce mem alloc *)

  (* this needs to handle multiple arcs and arraying! *)

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

PROCEDURE MustBeIdentifier(t : T; VAR ident : TEXT) RAISES { E } =
  BEGIN
    IF NOT GetIdentifier(t, ident) THEN
      RAISE E ("MustBeIdentifier: " & BrackOrEmpty(t.lately.nm) & " expected identifier here : " & S2T(t.buff, t.token))
    END;
  END MustBeIdentifier;

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
        ParseBlock(t,
                   topDisp, 
                   t)        
      END
    EXCEPT
      E(x) => RAISE E ("DefFormat.Parse: Error " & x & ", line " & Fmt.Int(t.state.line))
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
    MustBeChar(t,';')
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
    MustBeChar(t,';')
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
    MustBeChar(t,';')
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
    MustBeChar(t,';');
    
    ParseBlock(t, designDisp, des);
  END ParseDesign;

PROCEDURE ParseDesignUnits(t : T; ref : REFANY) RAISES { E } =
  BEGIN
    WITH des = NARROW(ref, Design) DO
      MustBeChars(t, DISTANCEa^);
      MustBeChars(t, MICRONSa^);
      
      MustBeCard(t, des.distUnits);
      MustBeChar(t,';')
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
          MustBePoint(t, p);
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
      MustBeChars(t, SAMEMASKa^);
    END;

    IF NOT GetToken(t, LAYERa^) THEN
      RAISE E ("ParseTracks, ?LAYER")
    END;

    (* now pointing to layer name *)
    
    MustNotBeChar(t,';');

    (* do not support multiple layers at this time *)

    MustBeChar(t,';')
  END ParseTracks;

PROCEDURE ParseGCellGrid(t : T; ref : REFANY) RAISES { E } = 
  VAR
    c : CHAR;
    start : INTEGER;
    numRowsCols : CARDINAL;
    step : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      MustBeCharSet(t, SET OF CHAR { 'X', 'Y' }, c);
      MustBeInt(t, start);
      MustBeChars(t, DOa^);
      MustBeCard(t, numRowsCols);
      MustBeChars(t, STEPa^);
      MustBeCard(t, step);
      MustBeChar(t, ';');
    END
  END ParseGCellGrid;

PROCEDURE ParseVias(t : T; ref : REFANY) RAISES { E } = 
  VAR
    numVias : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      MustBeCard(t, numVias);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, numVias, ParseVia);
    END
  END ParseVias;

PROCEDURE ParseNonDefaultRules(t : T; ref : REFANY) RAISES { E } = 
  VAR
    numNonDefaultRules : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      MustBeCard(t, numNonDefaultRules);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, numNonDefaultRules, ParseNonDefaultRule);
    END
  END ParseNonDefaultRules;

PROCEDURE ParseRegions(t : T; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      MustBeCard(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParseRegion);
    END
  END ParseRegions;

PROCEDURE ParseComponents(t : T; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      MustBeCard(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParseComponent);
    END
  END ParseComponents;

PROCEDURE ParseMinusBlock(t : T; ref : REFANY; cnt : CARDINAL; f : ParseProc.T) 
  RAISES { E } =
  BEGIN
    FOR i := 0 TO cnt - 1 DO
      MustBeChar(t, '-');
      f(t, ref)
    END;

    (* parse END statement, e.g., END VIAS for VIAS block *)
    MustBeChars(t, ENDa^);    
    MustBeChars(t, t.lately.ca^)
  END ParseMinusBlock;

PROCEDURE ParseVia(t : T; ref : REFANY) RAISES { E } =
  VAR
    nm : TEXT;
    c  : CHAR;
  BEGIN
    MustBeIdentifier(t, nm);
    LOOP
      MustBeSingle(t, c);
      CASE c OF 
        ';' => RETURN
      |
        '+' =>
        
        IF GetToken(t, RECTa^) THEN
          VAR 
            p1, p2 : DefPoint.T;
            layer : TEXT;
            maskNo : CARDINAL;
          BEGIN
            MustBeIdentifier(t, layer);
            IF GetChar(t, '+') THEN
              MustBeToken(t, MASKa^);
              MustBeCard(t, maskNo)
            END;
            MustBePoint(t, p1);
            MustBePoint(t, p2)
          END
        ELSE
          RAISE E("ParseVia: expected RECT")
        END
      ELSE
        RAISE E("ParseVia: unexpected '" & Text.FromChar(c) & "'")
      END
    END
  END ParseVia;

PROCEDURE ParseComponent(t : T; ref : REFANY) RAISES { E } =
  VAR
    c : CHAR;
    compName, modelName, macroName : TEXT;
    p : DefPoint.T;
    left, bottom, right, top, haloDist, weight : CARDINAL;
    prop : PropertyBinding;
    orient, minLayer, maxLayer, regionName : TEXT;
  BEGIN
    MustBeIdentifier(t, compName);
    MustBeIdentifier(t, modelName);
    LOOP
      MustBeSingle(t, c);
      CASE c OF
        ';' => RETURN
      |
        '+' =>
        IF    GetToken(t, EEQMASTERa^) THEN
          MustBeIdentifier(t, macroName)
        ELSIF GetToken(t, SOURCEa^) THEN
          IF    GetToken(t, NETLISTa^) THEN
          ELSIF GetToken(t, DISTa^) THEN
          ELSIF GetToken(t, USERa^) THEN
          ELSIF GetToken(t, TIMINGa^) THEN
          ELSE
            RAISE E ("ParseComponent ?SOURCE")
          END
        ELSIF GetToken(t, FIXEDa^) THEN
          MustBePoint(t, p);
          MustBeIdentifier(t, orient) (* should be a special func *)
        ELSIF GetToken(t, COVERa^) THEN
          MustBePoint(t, p);
          MustBeIdentifier(t, orient) (* should be a special func *)
        ELSIF GetToken(t, PLACEDa^) THEN
          MustBePoint(t, p);
          MustBeIdentifier(t, orient) (* should be a special func *)
        ELSIF GetToken(t, UNPLACEDa^) THEN
        ELSIF GetToken(t, HALOa^) THEN
          IF GetToken(t, SOFTa^) THEN
          END;
          MustBeCard(t, left);
          MustBeCard(t, bottom);
          MustBeCard(t, right);
          MustBeCard(t, top)
        ELSIF GetToken(t, ROUTEHALOa^) THEN
          MustBeCard(t, haloDist);
          MustBeIdentifier(t, minLayer);
          MustBeIdentifier(t, maxLayer)
        ELSIF GetToken(t, WEIGHTa^) THEN
          MustBeCard(t, weight)
        ELSIF GetToken(t, REGIONa^) THEN
          MustBeIdentifier(t, regionName)
        ELSIF GetProperty(t, ref, prop) THEN
        ELSE
          RAISE E("ParseComponent unexpected text")
        END
      ELSE
        RAISE E("ParseComponent: unexpected '" & Text.FromChar(c) & "'")
      END
    END          
  END ParseComponent;

PROCEDURE ParseNonDefaultRule(t : T; ref : REFANY) RAISES { E } =
  VAR
    nm : TEXT;
    c : CHAR;
    prop : PropertyBinding;
  BEGIN
    MustBeIdentifier(t, nm);
    LOOP
      MustBeSingle(t, c);
      CASE c OF 
        ';' => RETURN
      |
        '+' =>
        IF    GetToken(t, HARDSPACINGa^) THEN
        ELSIF GetToken(t, LAYERa^) THEN
          VAR
            layer : TEXT;
            width, diagWidth, spacing, wireExt : CARDINAL;
          BEGIN
            MustBeIdentifier(t, layer);
            MustBeToken(t, WIDTHa^);
            MustBeCard(t, width);
            IF    GetToken(t, DIAGWIDTHa^) THEN
              MustBeCard(t, diagWidth)
            ELSIF GetToken(t, SPACINGa^) THEN
              MustBeCard(t, spacing)
            ELSIF GetToken(t, WIREEXTa^) THEN
              MustBeCard(t, wireExt)
            ELSE
              RAISE E("ParseNonDefaultRule: LAYER: unknown keyword")
            END
          END
        ELSIF GetToken(t, VIAa^) THEN
          VAR
            viaName : TEXT;
          BEGIN
            MustBeIdentifier(t, viaName)
          END
        ELSIF GetToken(t, VIARULEa^) THEN
          VAR
            viaRuleName : TEXT;
          BEGIN
            MustBeIdentifier(t, viaRuleName)
          END
        ELSIF GetToken(t, MINCUTSa^) THEN
          VAR 
            cutLayerName : TEXT;
            numCuts : CARDINAL;
          BEGIN
            MustBeIdentifier(t, cutLayerName);
            MustBeCard(t, numCuts)
          END
        ELSIF GetProperty(t, ref, prop) THEN
        ELSE
          RAISE E("ParseNonDefaultRule: unknown keyword")
        END
         
      ELSE
        RAISE E("ParseNonDefaultRule: unexpected '" & Text.FromChar(c) & "'")
      END
    END
        
  END ParseNonDefaultRule;

TYPE
  PropertyBinding = RECORD
    property : TEXT; (* should really point to a Property.T *)
    binding  : NULL; (* needs to be a type-tagged value *)
  END;

PROCEDURE GetProperty(t : T; 
                      ref : REFANY; 
                      VAR pb : PropertyBinding) : BOOLEAN
  RAISES { E } =
  BEGIN
    IF GetToken(t, PROPERTYa^) THEN
      MustBeIdentifier(t, pb.property);
      Next(t); (* should put it in binding *)
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetProperty;

PROCEDURE ParseRegion(t : T; ref : REFANY) RAISES { E } =
  VAR
    nm : TEXT;
    c  : CHAR;
    p, q : DefPoint.T;
    prop : PropertyBinding;
  BEGIN
    MustBeIdentifier(t, nm);
    LOOP
      WHILE GetPoint(t, p) DO (* points are in pairs *)
        MustBePoint(t, q)
      END;

      MustBeSingle(t, c);

      CASE c OF 
        ';' => RETURN
      |
        '+' =>
        IF GetToken(t, TYPEa^) THEN
          IF    GetToken(t, FENCEa^) THEN
          ELSIF GetToken(t, GUIDEa^) THEN
          ELSE
            RAISE E("ParseRegion TYPE")
          END
        ELSIF GetProperty(t, ref, prop) THEN
        ELSE
          RAISE E("ParseRegion")
        END
      ELSE
        RAISE E("ParseRegion: unexpected '" & Text.FromChar(c) & "'")
      END
    END
  END ParseRegion;        
        
(**********************************************************************)

PROCEDURE ParsePropertyDefinitions(t : T; ref : REFANY) 
  RAISES { E } =
  BEGIN
    WITH des = NARROW(ref, Design) DO
      ParseBlock(t, propDisp, ref)
    END
  END ParsePropertyDefinitions;

PROCEDURE ParseBlock(t             : T;
                     keywords      : DefParseTrie.T;
                     ref           : REFANY) RAISES { E } =
  BEGIN
    LOOP
      Debug.Out(t.lately.nm & " kw=" & S2T(t.buff, t.token));
      WITH nxt = SUBARRAY(t.buff, t.token.start, t.token.n) DO
        IF nxt = ENDa^ THEN
          Next(t);

          WITH nxt2 = SUBARRAY(t.buff, t.token.start, t.token.n) DO
            IF nxt2 = t.lately.ca^ THEN 
              Next(t);
              RETURN
            ELSE
              RAISE E ("? END " & A2T(nxt2) & " ending block " & t.lately.nm)
            END
          END
        END;

        WITH rec = keywords.get(nxt),
             f   = rec.f DO
          IF f = NIL THEN
            RAISE E(BrackOrEmpty(t.lately.nm) & 
                  ", syntax error \"" & S2T(t.buff, t.token) & "\"")
          ELSE
            Next(t);
            VAR old := t.lately; BEGIN
              t.lately := rec;
              f(t, ref);
              t.lately := old
            END
          END
        END
      END
    END
  END ParseBlock;

PROCEDURE ParseComponentMaskShift(t : T; ref : REFANY) RAISES { E } = 
  VAR
    id : TEXT;
  BEGIN
    WHILE GetIdentifier(t, id) DO (* skip *) END;
    MustBeChar(t, ';')
  END ParseComponentMaskShift;

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
        MustNotBeChar(t,';');
        MustNotBeChar(t,';');
      ELSE
        (* was def value *)
        Next(t);
      END
    END;
    MustBeChar(t,';')
  END IgnorePropertyDefinition;

(**********************************************************************)

VAR 
  topDisp, designDisp, propDisp    := 
      NEW(DefParseTrie.T).init(ParseProcRec.Default);

PROCEDURE AddKeyword(to : DefParseTrie.T; 
                     kw : TEXT;
                     f  : ParseProc.T) =
  VAR
    buff := MakeCA(kw);
  BEGIN
    EVAL to.put(buff^, ParseProcRec.T { nm := kw, f := f, ca := buff})
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
  RECTa       := MakeCA("RECT");
  VIAa       := MakeCA("VIA");
  VIARULEa       := MakeCA("VIARULE");
  WIREEXTa       := MakeCA("WIREEXT");
  LAYERa       := MakeCA("LAYER");
  SAMEMASKa       := MakeCA("SAMEMASK");
  SPACINGa    := MakeCA("SPACING");
  HARDSPACINGa    := MakeCA("HARDSPACING");
  WIDTHa    := MakeCA("WIDTH");
  MINCUTSa    := MakeCA("MINCUTS");
  PROPERTYa    := MakeCA("PROPERTY");
  DIAGWIDTHa    := MakeCA("DIAGWIDTH");
  TYPEa    := MakeCA("TYPE");
  FENCEa    := MakeCA("FENCE");
  GUIDEa    := MakeCA("GUIDE");
  EEQMASTERa    := MakeCA("EEQMASTER");
  SOURCEa    := MakeCA("SOURCE");
  NETLISTa    := MakeCA("NETLIST");
  DISTa    := MakeCA("DIST");
  USERa    := MakeCA("USER");
  TIMINGa    := MakeCA("TIMING");
  FIXEDa    := MakeCA("FIXED");
  COVERa    := MakeCA("COVER");
  PLACEDa    := MakeCA("PLACED");
  UNPLACEDa    := MakeCA("UNPLACED");
  HALOa    := MakeCA("HALO");
  SOFTa    := MakeCA("SOFT");
  ROUTEHALOa    := MakeCA("ROUTEHALO");
  WEIGHTa    := MakeCA("WEIGHT");
  REGIONa    := MakeCA("REGION");
  
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
  AddKeyword(designDisp, "GCELLGRID",           ParseGCellGrid);
  AddKeyword(designDisp, "VIAS",                ParseVias);
  AddKeyword(designDisp, "NONDEFAULTRULES",     ParseNonDefaultRules);
  AddKeyword(designDisp, "REGIONS",             ParseRegions);
  AddKeyword(designDisp, "COMPONENTMASKSHIFT",  ParseComponentMaskShift);
  AddKeyword(designDisp, "COMPONENTS",          ParseComponents);
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
