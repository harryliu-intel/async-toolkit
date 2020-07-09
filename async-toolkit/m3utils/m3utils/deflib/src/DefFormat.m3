MODULE DefFormat;
IMPORT Rd;
IMPORT RecursiveParser;

FROM RecursiveParser IMPORT GetToken, Next, MustBeToken, MustNotBeChar,  
                            MustBeChars, MustBeCharSet, BrackOrEmpty, 
                            GetChar, MustBeChar, GetCharSet, MustBeSingle,
                            S2T, A2T, PeekToken
                            ;

IMPORT RecursiveParserRep;
IMPORT Debug;
IMPORT Text;
IMPORT ParseTrie;
IMPORT ParseProc, ParseProcRec;
IMPORT DefLexer;
IMPORT DefPoint, DefPointSeq;
IMPORT Fmt;
FROM ParseError IMPORT E;
IMPORT DefTokens AS K;
IMPORT DefDirection;
IMPORT DefOrientation;
IMPORT DefUse;
IMPORT DefInt;
IMPORT DefCard;
IMPORT DefIdent;
IMPORT DefName;
IMPORT DefRoutingPoint;
IMPORT DefPattern;
IMPORT DefSource;
IMPORT DefString;
IMPORT DefShape;
IMPORT IO;

TYPE R = RecursiveParser.T;

REVEAL
  T = Public BRANDED Brand OBJECT
    version : TEXT;
  METHODS
  END;

CONST DQ = '"';

VAR d := FALSE;

PROCEDURE D(what : TEXT) = 
  BEGIN 
    IF d THEN IO.Put(what & "\n") END
  END D;

PROCEDURE Parse(rd : Rd.T) : T RAISES { E } =

  VAR
    (* parsing *)
    t := NEW(T, lexer := NEW(DefLexer.T));
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
      E(x) => RAISE E (Fmt.F("DefFormat.Parse: Error %s, line %s, lately parsing \"%s\"",
                             x, 
                             Fmt.Int(t.state.line),
                             Text.FromChars(SUBARRAY(t.buff, 
                                                     t.token.start, 
                                                     t.token.n))))
    END;

    RETURN t

  END Parse;

(**********************************************************************)

PROCEDURE ParseVersion(t : R; ref : REFANY) RAISES { E } =
  BEGIN
    <*ASSERT t = ref*>
    NARROW(t,T).version := S2T(t.buff, t.token);
    Debug.Out("ParseVersion, t.version = " & NARROW(t,T).version);
    Next(t);
    MustBeChar(t,';')
  END ParseVersion;

PROCEDURE ParseDividerChar(t : R; ref : REFANY) RAISES { E } =
  BEGIN
    <*ASSERT t = ref*>
    WITH chars = SUBARRAY(t.buff, t.token.start, t.token.n) DO
      IF NUMBER(chars) # 3 OR chars[0] # DQ OR chars[2] # DQ THEN
        Debug.Error("DefFormat.ParseDividerChar ?syntax error")
      END;
      DefLexer.DividerChar(NARROW(t,T).lexer,chars[1])
    END;
    Next(t);
    MustBeChar(t,';')
  END ParseDividerChar;

PROCEDURE ParseBusbitChars(t : R; ref : REFANY) RAISES { E } =
  BEGIN
    <*ASSERT t = ref*>
    WITH chars = SUBARRAY(t.buff, t.token.start, t.token.n) DO
      IF NUMBER(chars) # 4 OR chars[0] # DQ OR chars[3] # DQ THEN
        Debug.Error("DefFormat.ParseBusbitChars ?syntax error")
      END;
      DefLexer.BusbitChars(NARROW(t,T).lexer, SUBARRAY(chars, 1, 2))
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

PROCEDURE ParseDesign(t : R; ref : REFANY) RAISES { E } =
  VAR 
    des := NEW(Design);
  BEGIN
    <*ASSERT t = ref*>
    des.name := S2T(t.buff, t.token);
    Next(t);
    MustBeChar(t,';');
    
    ParseBlock(t, designDisp, des);
  END ParseDesign;

PROCEDURE ParseDesignUnits(t : R; ref : REFANY) RAISES { E } =
  BEGIN
    WITH des = NARROW(ref, Design) DO
      MustBeChars(t, K.T_DISTANCE);
      MustBeChars(t, K.T_MICRONS);
      
      DefCard.MustBe(t, des.distUnits);
      MustBeChar(t,';')
    END
  END ParseDesignUnits;

CONST T_Semi = ARRAY OF CHAR { ';' };
CONST T_Plus = ARRAY OF CHAR { '+' };
CONST T_OParen = ARRAY OF CHAR { '(' };
CONST T_CParen = ARRAY OF CHAR { ')' };
CONST T_Asterisk = ARRAY OF CHAR { '*' };

PROCEDURE ParseDieArea(t : R; ref : REFANY) RAISES { E } =
  BEGIN
    WITH des = NARROW(ref, Design),
         seq = NEW(DefPointSeq.T).init() DO
      LOOP
        IF SUBARRAY(t.buff, t.token.start, t.token.n) = T_Semi THEN
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
          DefPoint.MustBe(t, p);
          seq.addhi(p)
        END
      END
    END
  END ParseDieArea;

PROCEDURE ParseRow(t : R; ref : REFANY) RAISES { E } = 
  VAR
    rowName, siteName, siteOrient : DefIdent.T;
    origX, origY : INTEGER;
    numX, numY : CARDINAL := 1;
    stepX, stepY : INTEGER := 0;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      IF NOT DefIdent.Get(t, rowName) OR NOT DefIdent.Get(t, siteName) THEN
        RAISE E("ParseRow, rowName, siteName")
      END;

      DefInt.MustBe(t, origX);
      DefInt.MustBe(t, origY);

      IF NOT DefIdent.Get(t, siteOrient) THEN
        RAISE E("ParseRow, siteOrient")
      END;

      IF GetToken(t, T_Semi) THEN
        RETURN
      END;

      IF GetToken(t, K.T_DO) THEN
        IF NOT DefCard.Get(t, numX) THEN RAISE E("ParseRow, numX") END;
        
        IF NOT GetToken(t, K.T_BY) THEN RAISE E("ParseRow, DO...BY") END;

        IF NOT DefCard.Get(t, numY) THEN  RAISE E("ParseRow, numY") END;
        
        IF GetToken(t, T_Semi) THEN
          RETURN
        ELSIF GetToken(t, K.T_STEP) THEN

          DefInt.MustBe(t, stepX);
          DefInt.MustBe(t, stepY)
        END;

        IF GetToken(t, T_Semi) THEN
          RETURN
        ELSE
          <*ASSERT FALSE*> (* ROW-PROPERTY not implemented yet *)
        END
      END
    END
  END ParseRow;

PROCEDURE ParseTracks(t : R; ref : REFANY) RAISES { E } =
  VAR
    xy : CHAR;
    numTracks, space, mask : CARDINAL;
    start : INTEGER;
  BEGIN
    IF NOT GetCharSet(t, SET OF CHAR { 'X', 'Y' }, xy) THEN
      RAISE E("ParseTracks, xy")
    END;

    DefInt.MustBe(t, start);
    
    IF NOT GetToken(t, K.T_DO) THEN
      RAISE E("ParseTracks, ?DO")
    END;

    DefCard.MustBe(t, numTracks);

    IF NOT GetToken(t, K.T_STEP) THEN
      RAISE E("ParseTracks, ?STEP")
    END;

    DefCard.MustBe(t, space);

    IF GetToken(t, K.T_MASK) THEN
      DefCard.MustBe(t, mask);
      MustBeChars(t, K.T_SAMEMASK);
    END;

    IF NOT GetToken(t, K.T_LAYER) THEN
      RAISE E ("ParseTracks, ?LAYER")
    END;

    (* now pointing to layer name *)
    
    MustNotBeChar(t,';');

    (* do not support multiple layers at this time *)

    MustBeChar(t,';')
  END ParseTracks;

PROCEDURE ParseGCellGrid(t : R; ref : REFANY) RAISES { E } = 
  VAR
    c : CHAR;
    start : INTEGER;
    numRowsCols : CARDINAL;
    step : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      MustBeCharSet(t, SET OF CHAR { 'X', 'Y' }, c);
      DefInt.MustBe(t, start);
      MustBeChars(t, K.T_DO);
      DefCard.MustBe(t, numRowsCols);
      MustBeChars(t, K.T_STEP);
      DefCard.MustBe(t, step);
      MustBeChar(t, ';');
    END
  END ParseGCellGrid;

PROCEDURE ParseVias(t : R; ref : REFANY) RAISES { E } = 
  VAR
    numVias : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, numVias);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, numVias, ParseVia);
    END
  END ParseVias;

PROCEDURE ParseNonDefaultRules(t : R; ref : REFANY) RAISES { E } = 
  VAR
    numNonDefaultRules : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, numNonDefaultRules);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, numNonDefaultRules, ParseNonDefaultRule);
    END
  END ParseNonDefaultRules;

PROCEDURE ParseRegions(t : R; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParseRegion);
    END
  END ParseRegions;

PROCEDURE ParseComponents(t : R; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParseComponent);
    END
  END ParseComponents;

PROCEDURE ParsePins(t : R; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParsePin);
    END
  END ParsePins;

PROCEDURE ParseBlockages(t : R; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParseBlockage);
    END
  END ParseBlockages;

PROCEDURE ParseFills(t : R; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParseFill);
    END
  END ParseFills;

PROCEDURE ParseSpecialNets(t : R; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParseSpecialNet);
    END
  END ParseSpecialNets;

PROCEDURE ParseNets(t : R; ref : REFANY) RAISES { E } = 
  VAR
    num : CARDINAL;
  BEGIN
    WITH des = NARROW(ref, Design) DO
      DefCard.MustBe(t, num);
      MustBeChar(t, ';');

      ParseMinusBlock(t, ref, num, ParseNet);
    END
  END ParseNets;

PROCEDURE ParseMinusBlock(t : T; ref : REFANY; cnt : CARDINAL; f : ParseProc.T) 
  RAISES { E } =
  VAR
    j : CARDINAL;
  BEGIN
    TRY
      FOR i := 0 TO cnt - 1 DO
        j := i;
        IF GetToken(t, K.T_END) THEN
          (* Hmm... *)
          Debug.Warning("ParseMinusBlock of " & Text.FromChars(t.lately.ca^) & " is short: " & Fmt.Int(j) & "/" & Fmt.Int(cnt-1));
          MustBeChars(t, t.lately.ca^);
          RETURN
        END;
        MustBeChar(t, '-');
        f(t, ref)
      END;
      
      (* parse END statement, e.g., END VIAS for VIAS block *)
      MustBeChars(t, K.T_END);    
      MustBeChars(t, t.lately.ca^)
    EXCEPT
      E(x) => RAISE E("ParseMinusBlock ("& Fmt.Int(j) & "/" & Fmt.Int(cnt-1) &"): " & x)
    END
  END ParseMinusBlock;

PROCEDURE ParseVia(t : R; ref : REFANY) RAISES { E } =
  VAR
    nm : DefIdent.T;
    c  : CHAR;
  BEGIN
    DefIdent.MustBe(t, nm);
    LOOP
      MustBeSingle(t, c);
      CASE c OF 
        ';' => RETURN
      |
        '+' =>
        
        IF GetToken(t, K.T_RECT) THEN
          VAR 
            p1, p2 : DefPoint.T;
            layer : DefIdent.T;
            maskNo : CARDINAL;
          BEGIN
            DefIdent.MustBe(t, layer);
            IF GetChar(t, '+') THEN
              MustBeToken(t, K.T_MASK);
              DefCard.MustBe(t, maskNo)
            END;
            DefPoint.MustBe(t, p1);
            DefPoint.MustBe(t, p2)
          END
        ELSE
          RAISE E("ParseVia: expected RECT")
        END
      ELSE
        RAISE E("ParseVia: unexpected '" & Text.FromChar(c) & "'")
      END
    END
  END ParseVia;

PROCEDURE ParseComponent(t : R; ref : REFANY) RAISES { E } =
  VAR
    c : CHAR;
    compName : DefName.T;
    modelName, macroName : DefIdent.T;
    p : DefPoint.T;
    left, bottom, right, top, haloDist, weight : CARDINAL;
    prop : PropertyBinding;
    orient, minLayer, maxLayer, regionName : DefIdent.T;
  BEGIN
    DefName.MustBe(t, compName);
    DefIdent.MustBe(t, modelName);
    LOOP
      MustBeSingle(t, c);
      CASE c OF
        ';' => RETURN
      |
        '+' =>
        IF    GetToken(t, K.T_EEQMASTER) THEN
          DefIdent.MustBe(t, macroName)
        ELSIF GetToken(t, K.T_SOURCE) THEN
          IF    GetToken(t, K.T_NETLIST) THEN
          ELSIF GetToken(t, K.T_DIST) THEN
          ELSIF GetToken(t, K.T_USER) THEN
          ELSIF GetToken(t, K.T_TIMING) THEN
          ELSE
            RAISE E ("ParseComponent ?SOURCE")
          END
        ELSIF GetToken(t, K.T_FIXED) THEN
          DefPoint.MustBe(t, p);
          DefIdent.MustBe(t, orient) (* should be a special func *)
        ELSIF GetToken(t, K.T_COVER) THEN
          DefPoint.MustBe(t, p);
          DefIdent.MustBe(t, orient) (* should be a special func *)
        ELSIF GetToken(t, K.T_PLACED) THEN
          DefPoint.MustBe(t, p);
          DefIdent.MustBe(t, orient) (* should be a special func *)
        ELSIF GetToken(t, K.T_UNPLACED) THEN
        ELSIF GetToken(t, K.T_HALO) THEN
          IF GetToken(t, K.T_SOFT) THEN
          END;
          DefCard.MustBe(t, left);
          DefCard.MustBe(t, bottom);
          DefCard.MustBe(t, right);
          DefCard.MustBe(t, top)
        ELSIF GetToken(t, K.T_ROUTEHALO) THEN
          DefCard.MustBe(t, haloDist);
          DefIdent.MustBe(t, minLayer);
          DefIdent.MustBe(t, maxLayer)
        ELSIF GetToken(t, K.T_WEIGHT) THEN
          DefCard.MustBe(t, weight)
        ELSIF GetToken(t, K.T_REGION) THEN
          DefIdent.MustBe(t, regionName)
        ELSIF GetProperty(t, ref, prop) THEN
        ELSE
          RAISE E("ParseComponent unexpected text")
        END
      ELSE
        RAISE E("ParseComponent: unexpected '" & Text.FromChar(c) & "'")
      END
    END          
  END ParseComponent;

PROCEDURE ParsePin(t : R; ref : REFANY) RAISES { E } =
  VAR
    c : CHAR;
    netName, pinName, compName : DefName.T;
    p : DefPoint.T;
    left, bottom, right, top, haloDist, weight : CARDINAL;
    prop : PropertyBinding;
    orient, minLayer, maxLayer, regionName : DefIdent.T;

    parsingPort := FALSE;
    (* DAMN!!! grammar isnt LL(1) for PIN .. + PORT + .... *)
  BEGIN
    DefName.MustBe(t, pinName);
    MustBeChar(t, '+');
    MustBeToken(t, K.T_NET);
    DefName.MustBe(t, netName);

    LOOP
      MustBeSingle(t, c);
      CASE c OF
        ';' => RETURN
      |
        '+' =>
        IF parsingPort THEN
          IF    GetToken(t, K.T_LAYER) THEN
            VAR
              layer : DefIdent.T;
              p0, p1 : DefPoint.T;
            BEGIN
              DefIdent.MustBe(t, layer);
              DefPoint.MustBe(t, p0);
              DefPoint.MustBe(t, p1)
            END;
            D("Layer")
          ELSIF GetToken(t, K.T_FIXED) THEN
            VAR
              p : DefPoint.T;
              dir : DefOrientation.T;
            BEGIN
              DefPoint.MustBe(t, p);
              DefOrientation.MustBe(t, dir)
            END;
            D("Fixed")
          ELSIF GetToken(t, K.T_POLYGON) THEN
            <*ASSERT FALSE*>
          ELSIF GetToken(t, K.T_VIA) THEN
            <*ASSERT FALSE*>
          ELSIF GetToken(t, K.T_PORT) THEN
            D("Port");
            (* next port *) (* TBD *)
          ELSE 
            parsingPort := FALSE
          END
        END;

        IF NOT parsingPort THEN
          (* there are many others too *)
          IF    GetToken(t, K.T_DIRECTION) THEN
            VAR
              dir : DefDirection.T;
            BEGIN
              DefDirection.MustBe(t, dir)
            END
          ELSIF GetToken(t, K.T_SPECIAL) THEN
          ELSIF GetToken(t, K.T_PORT) THEN
            parsingPort := TRUE;
            (*d := TRUE;*)
          ELSIF GetToken(t, K.T_USE) THEN
            VAR 
              use : DefUse.T;
            BEGIN
              DefUse.MustBe(t, use)
            END
          ELSIF GetToken(t, K.T_LAYER) THEN
            VAR 
              layer : DefIdent.T;
              p0, p1 : DefPoint.T;
            BEGIN
              DefIdent.MustBe(t, layer);
              DefPoint.MustBe(t, p0);
              DefPoint.MustBe(t, p1)
            END
          ELSIF GetToken(t, K.T_FIXED) THEN
            VAR 
              p : DefPoint.T;
              o : DefOrientation.T;
            BEGIN
              DefPoint.MustBe(t, p);
              DefOrientation.MustBe(t, o)
            END
          ELSE
            RAISE E("ParsePin unexpected text")
          END
        END
      ELSE
        RAISE E("ParsePin: unexpected '" & Text.FromChar(c) & "'")
      END(*ESAC*)

    END(*POOL*)          
  END ParsePin;

PROCEDURE ParseBlockage(t : R; ref : REFANY) RAISES { E } =
  BEGIN
    IF    GetToken(t, K.T_LAYER) THEN
      VAR 
        layer : DefIdent.T;
      BEGIN
        DefIdent.MustBe(t, layer);
      END
    ELSIF GetToken(t, K.T_PLACEMENT) THEN
    ELSE
      RAISE E("ParseBlockage of unexpected type")
    END;
    LOOP
      IF GetToken(t, T_Semi) THEN
        RETURN
      ELSIF GetToken(t, T_Plus) THEN
        IF    GetToken(t, K.T_COMPONENT) THEN
          VAR
            compName : DefName.T;
          BEGIN
            DefName.MustBe(t, compName)
          END
        ELSIF GetToken(t, K.T_SOFT) THEN
          (* skip *)
        ELSIF GetToken(t, K.T_SLOTS) THEN
          (* skip *)
        ELSIF GetToken(t, K.T_FILLS) THEN
          (* skip *)
        ELSIF GetToken(t, K.T_PUSHDOWN) THEN
          (* skip *)
        ELSIF GetToken(t, K.T_EXCEPTPGNET) THEN
          (* skip *)
        ELSIF GetToken(t, K.T_SPACING) THEN
          VAR
            spacing : CARDINAL;
          BEGIN
            DefCard.MustBe(t, spacing)
          END
        ELSIF GetToken(t, K.T_DESIGNRULEWIDTH) THEN
          VAR
            effectiveWidth : CARDINAL;
          BEGIN
            DefCard.MustBe(t, effectiveWidth)
          END
        ELSE
          RAISE E("ParseBlockage: unexpected option")
        END
      ELSIF GetToken(t, K.T_POLYGON) THEN
        VAR
          p : DefPoint.T;
        BEGIN
          WHILE DefPoint.Get(t, p) DO
          END
        END
      ELSIF GetToken(t, K.T_RECT) THEN
        VAR
          p0, p1 : DefPoint.T;
        BEGIN
          DefPoint.MustBe(t, p0);
          DefPoint.MustBe(t, p1)
        END
      ELSE
        RAISE E("ParseBlockage: unexpected token")
      END
    END
  END ParseBlockage;

PROCEDURE ParseFill(t : R; ref : REFANY) RAISES { E } =

  PROCEDURE GetShapes() RAISES { E } =
    BEGIN
      LOOP
        IF GetToken(t, K.T_RECT) THEN
          VAR 
            p0, p1 : DefPoint.T;
          BEGIN
            DefPoint.MustBe(t, p0);
            DefPoint.MustBe(t, p1)
          END
        ELSIF GetToken(t, K.T_POLYGON) THEN
          VAR
            p : DefPoint.T;
          BEGIN
            WHILE DefPoint.Get(t, p) DO END
          END
        ELSIF GetToken(t, T_Semi) THEN
          RETURN
        ELSE
          RAISE E("ParseFill getting shapes")
        END
      END
    END GetShapes;

  BEGIN
    IF    GetToken(t, K.T_VIA) THEN
      VAR
        viaName : DefIdent.T;
      BEGIN
        DefIdent.MustBe(t, viaName);
        
        IF GetToken(t, T_Plus) THEN
          MustBeToken(t, K.T_OPC);
        END;
        GetShapes()
      END
    ELSIF GetToken(t, K.T_LAYER) THEN
      VAR
        layerName : DefIdent.T;
        p : DefPoint.T;
      BEGIN
        DefIdent.MustBe(t, layerName);

        WHILE GetToken(t, T_Plus) DO
          IF    GetToken(t, K.T_OPC) THEN
          ELSIF GetToken(t, K.T_MASK) THEN
            VAR
              mask : CARDINAL;
            BEGIN
              DefCard.MustBe(t, mask)
            END
          END
        END;
        GetShapes()
      END
    ELSE
      RAISE E ("ParseFill ??")
    END
  END ParseFill;

PROCEDURE ParseSpecialNet(t : R; ref : REFANY) RAISES { E } =
  VAR
    netName := DefName.MustGet(t);
    c : CHAR;
  BEGIN
    (* parse the special comp/net syntax here *)
    WHILE GetToken(t, T_OParen) DO
      IF GetToken(t, T_Asterisk) THEN
      ELSIF GetToken(t, K.T_PIN) THEN
      ELSE
        VAR compName := DefIdent.MustGet(t); BEGIN END;
      END;
      VAR pinName := DefName.MustGet(t); BEGIN END;

      IF GetToken(t, T_Plus) THEN
        MustBeToken(t, K.T_SYNTHESIZED)
      END;

      MustBeToken(t, T_CParen);
    END;

    LOOP
      MustBeSingle(t, c);
      CASE c OF 
        ';' => RETURN
      |
        '+' =>
        IF    GetSpecialWiring(t, ref) THEN
        ELSIF GetToken(t, K.T_VOLTAGE) THEN
          <*ASSERT FALSE*> (* real number? *)
        ELSIF GetToken(t, K.T_SOURCE) THEN
          VAR src := DefSource.MustGet(t); BEGIN END
        ELSIF GetToken(t, K.T_FIXEDBUMP) THEN
          (* skip *)
        ELSIF GetToken(t, K.T_ORIGINAL) THEN
          VAR netName := DefName.MustGet(t); BEGIN END
        ELSIF GetToken(t, K.T_USE) THEN
          VAR use := DefUse.MustGet(t); BEGIN END
        ELSIF GetToken(t, K.T_PATTERN) THEN 
          VAR pattern := DefPattern.MustGet(t); BEGIN END
        ELSIF GetToken(t, K.T_ESTCAP) THEN
          <*ASSERT FALSE*> (* real number? *)
        ELSIF GetToken(t, K.T_WEIGHT) THEN
          VAR weight := DefCard.MustGet(t); BEGIN END
        ELSIF GetToken(t, K.T_PROPERTY) THEN
          VAR propName  : DefIdent.T;
          BEGIN
            WHILE DefIdent.Get(t, propName) DO
              (* here we really should look up the type of the property
                 named propName and parse the value of that type, 
                 but for now we just skip it *)
              Next(t)
            END
          END
        ELSE
          RAISE E("ParseSpecialNet???")
        END
      ELSE
        RAISE E("ParseSpecialNet character ???")
      END
    END
  END ParseSpecialNet;

PROCEDURE GetSpecialWiring(t : R; ref : REFANY) : BOOLEAN RAISES { E } =
  (* assume '+' has already been consumed *)

  PROCEDURE GetSubDetails() : BOOLEAN RAISES { E } =
    BEGIN
      IF NOT GetToken(t, T_Plus) THEN RETURN FALSE END;

      IF    GetToken(t, K.T_SHAPE) THEN
        VAR shape := DefShape.MustGet(t); BEGIN END
      ELSIF GetToken(t, K.T_STYLE) THEN
        VAR styleNum := DefCard.MustGet(t); BEGIN END
      ELSIF GetToken(t, K.T_MASK) THEN
        VAR maskNum := DefCard.MustGet(t); BEGIN END
      ELSIF GetToken(t, K.T_RECT) THEN
        (* this is just for GetAlternativeDetails -- not in grammar?? *)
        VAR 
          layer := DefIdent.MustGet(t);
          p0, p1 : DefPoint.T; 
        BEGIN
          p0 := DefPoint.MustGet(t);
          p1 := DefPoint.MustGet(t)
        END;
        RETURN FALSE (* what a hack! *)
      ELSE 
        RAISE E("GetSpecialWiring.GetSubDetails???")
      END;

      RETURN TRUE
    END GetSubDetails;

  PROCEDURE GetRoutingPointSeq() RAISES { E } =
    VAR 
      rp := DefRoutingPoint.Initial;
    BEGIN
      LOOP
        IF    DefRoutingPoint.Get(t, rp) THEN
        ELSIF GetToken(t, K.T_MASK) THEN
          VAR maskId := DefCard.MustGet(t); BEGIN END
        ELSE
          RETURN
        END
      END
    END GetRoutingPointSeq;

  PROCEDURE GetDetails() RAISES { E } =
    (* we are just past the "FIXED" keyword *)
    VAR
      layerName  := DefIdent.MustGet(t);
      routeWidth := DefCard.MustGet(t);
      viaName : DefIdent.T;
    BEGIN
      IF GetSubDetails() THEN
      END;
      GetRoutingPointSeq();
      (* could end in a via name *)
      IF DefIdent.Get(t, viaName) THEN END;

      WHILE GetToken(t, K.T_NEW) DO
        layerName  := DefIdent.MustGet(t);
        routeWidth := DefCard.MustGet(t);
        IF GetSubDetails() THEN
        END;
        GetRoutingPointSeq();
        (* could end in a via name *)
        IF DefIdent.Get(t, viaName) THEN END;
      END
    END GetDetails;

  PROCEDURE GetAlternativeDetails() RAISES { E } =
    BEGIN
      WHILE GetSubDetails() DO END;
    END GetAlternativeDetails;

  VAR
    dummyId : DefIdent.T;
  BEGIN
    IF    GetToken(t, K.T_POLYGON) THEN
      VAR
        layerName := DefIdent.MustGet(t);
        p : DefPoint.T;
      BEGIN
        WHILE DefPoint.Get(t, p) DO
        END
      END
    ELSIF GetToken(t, K.T_RECT) THEN
      VAR
        layerName := DefIdent.MustGet(t);
        p0 := DefPoint.MustGet(t);
        p1 := DefPoint.MustGet(t);
      BEGIN
      END
    ELSIF GetToken(t, K.T_COVER) THEN
      GetDetails()
    ELSIF GetToken(t, K.T_FIXED) THEN
      GetDetails()
    ELSIF GetToken(t, K.T_ROUTED) THEN
      (* so the grammar claims here we need to have

         layerName routeWidth 

         but at least some files do not have that.  
      *)
      IF DefIdent.Peek(t, dummyId) THEN
        GetDetails()
      ELSE
        GetAlternativeDetails()
      END
    ELSIF GetToken(t, K.T_SHIELD) THEN
      VAR
        shieldNetName := DefName.MustGet(t);
      BEGIN
        IF PeekToken(t, T_Plus) THEN
          (* not in the grammar...?? *)
          GetAlternativeDetails() 
        ELSE
          GetDetails()
        END
      END
    ELSE
      RETURN FALSE
    END;
    RETURN TRUE
  END GetSpecialWiring;

PROCEDURE ParseNet(t : R; ref : REFANY) RAISES { E } =
  VAR
    netName := DefName.MustGet(t);
  BEGIN
  END ParseNet;


PROCEDURE ParseNonDefaultRule(t : R; ref : REFANY) RAISES { E } =
  VAR
    nm := DefIdent.MustGet(t);
    c : CHAR;
    prop : PropertyBinding;
  BEGIN
    LOOP
      MustBeSingle(t, c);
      CASE c OF 
        ';' => RETURN
      |
        '+' =>
        IF    GetToken(t, K.T_HARDSPACING) THEN
        ELSIF GetToken(t, K.T_LAYER) THEN
          VAR
            layer : DefIdent.T;
            width, diagWidth, spacing, wireExt : CARDINAL;
          BEGIN
            DefIdent.MustBe(t, layer);
            MustBeToken(t, K.T_WIDTH);
            DefCard.MustBe(t, width);
            IF    GetToken(t, K.T_DIAGWIDTH) THEN
              DefCard.MustBe(t, diagWidth)
            ELSIF GetToken(t, K.T_SPACING) THEN
              DefCard.MustBe(t, spacing)
            ELSIF GetToken(t, K.T_WIREEXT) THEN
              DefCard.MustBe(t, wireExt)
            ELSE
              RAISE E("ParseNonDefaultRule: LAYER: unknown keyword")
            END
          END
        ELSIF GetToken(t, K.T_VIA) THEN
          VAR
            viaName := DefIdent.MustGet(t);
          BEGIN

          END
        ELSIF GetToken(t, K.T_VIARULE) THEN
          VAR
            viaRuleName := DefIdent.MustGet(t);
          BEGIN

          END
        ELSIF GetToken(t, K.T_MINCUTS) THEN
          VAR 
            cutLayerName := DefIdent.MustGet(t);
            numCuts := DefCard.MustGet(t);
          BEGIN
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
  Value = NULL; (* TBD *)

  PropertyBinding = RECORD
    property : DefIdent.T; (* should really point to a Property.T *)
    binding  : Value; (* needs to be a type-tagged value *)
  END;

PROCEDURE GetProperty(t : T; 
                      ref : REFANY; 
                      VAR pb : PropertyBinding) : BOOLEAN
  RAISES { E } =
  BEGIN
    IF GetToken(t, K.T_PROPERTY) THEN
      DefIdent.MustBe(t, pb.property);
      Next(t); (* should put it in binding *)
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetProperty;

PROCEDURE ParseRegion(t : R; ref : REFANY) RAISES { E } =
  VAR
    nm : DefIdent.T;
    c  : CHAR;
    p, q : DefPoint.T;
    prop : PropertyBinding;
  BEGIN
    DefIdent.MustBe(t, nm);
    LOOP
      WHILE DefPoint.Get(t, p) DO (* points are in pairs *)
        DefPoint.MustBe(t, q)
      END;

      MustBeSingle(t, c);

      CASE c OF 
        ';' => RETURN
      |
        '+' =>
        IF GetToken(t, K.T_TYPE) THEN
          IF    GetToken(t, K.T_FENCE) THEN
          ELSIF GetToken(t, K.T_GUIDE) THEN
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
        
(*********************************************************************)

PROCEDURE ParsePropertyDefinitions(t : R; ref : REFANY) 
  RAISES { E } =
  BEGIN
    WITH des = NARROW(ref, Design) DO
      ParseBlock(t, propDisp, ref)
    END
  END ParsePropertyDefinitions;

PROCEDURE ParseBlock(t             : T;
                     keywords      : ParseTrie.T;
                     ref           : REFANY) RAISES { E } =
  BEGIN
    LOOP
      Debug.Out(t.lately.nm & " kw=" & S2T(t.buff, t.token));
      WITH nxt = SUBARRAY(t.buff, t.token.start, t.token.n) DO
        IF nxt = K.T_END THEN
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

PROCEDURE ParseComponentMaskShift(t : R; ref : REFANY) RAISES { E } = 
  VAR
    id : DefIdent.T;
  BEGIN
    WHILE DefIdent.Get(t, id) DO (* skip *) END;
    MustBeChar(t, ';')
  END ParseComponentMaskShift;

(**********************************************************************)

PROCEDURE IgnorePropertyDefinition(t : R; ref : REFANY) 
  RAISES { E } =
  BEGIN
    (* skip name *) 
    Next(t); 

    (* skip type *)
    Next(t);

    IF GetToken(t, T_Semi) THEN 
      (* skip *)
    ELSIF GetToken(t, K.T_RANGE) THEN
      MustNotBeChar(t,';');
    ELSE
      (* was def value *)
      MustNotBeChar(t,';');
      MustBeChar(t,';')
    END;
  END IgnorePropertyDefinition;

(**********************************************************************)

VAR 
  topDisp, designDisp, propDisp    := 
      NEW(ParseTrie.T).init(ParseProcRec.Default);

PROCEDURE AddKeyword(to : ParseTrie.T; 
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
  AddKeyword(designDisp, "PINS",                ParsePins);
  AddKeyword(designDisp, "BLOCKAGES",           ParseBlockages);
  AddKeyword(designDisp, "FILLS",               ParseFills);
  AddKeyword(designDisp, "SPECIALNETS",         ParseSpecialNets);
  AddKeyword(designDisp, "NETS",                ParseNets);
  AddKeyword(designDisp, "GROUPS",              NIL);

  AddKeyword(propDisp,   "DESIGN",              IgnorePropertyDefinition);
  AddKeyword(propDisp,   "SPECIALNET",          IgnorePropertyDefinition);
  AddKeyword(propDisp,   "NET",                 IgnorePropertyDefinition);
  AddKeyword(propDisp,   "COMPONENTPIN",        IgnorePropertyDefinition);
  AddKeyword(propDisp,   "NONDEFAULTRULE",      IgnorePropertyDefinition);
  AddKeyword(propDisp,   "COMPONENT",           IgnorePropertyDefinition);
  
  

END DefFormat.
