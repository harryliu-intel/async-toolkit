MODULE DefFormat;
IMPORT Rd;
FROM DefLexer IMPORT GetToken, String, State, Buffer, BufSize;
IMPORT Debug;
IMPORT Text;
IMPORT DefParseTrie;
IMPORT ParseProc;
IMPORT DefLexer;

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
    getIdentifier(VAR name : Name) : BOOLEAN := GetIdentifier;
    tokMustBeChar(c : CHAR) := TokMustBeChar;
    tokMustBeChars(READONLY a : ARRAY OF CHAR) := TokMustBeChars;
  END;

TYPE Name = REFANY;

CONST DQ = '"';

PROCEDURE TokMustBeChar(t : T; c : CHAR) =
  BEGIN
    IF t.token.n # 1 OR t.buff[t.token.start] # c THEN
      Debug.Error("TokMustBeChar: expected '" & Text.FromChar(c) & 
        "' but got \"" & S2T(t.buff, t.token) & "\"")
    END;
    Next(t)
  END TokMustBeChar;

PROCEDURE TokMustBeChars(t : T; READONLY a : ARRAY OF CHAR) =
  BEGIN
    IF t.token.n # NUMBER(a) OR 
       SUBARRAY(t.buff, t.token.start, t.token.n) # a THEN
      Debug.Error("TokMustBeChars: expected '" & Text.FromChars(a) & 
        "' but got \"" & S2T(t.buff, t.token) & "\"")
    END;
    Next(t)
  END TokMustBeChars;

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
  
PROCEDURE Error(t : T) =
  BEGIN
    Debug.Error("PARSE ERROR, lately reading: " & 
      Text.FromChars(SUBARRAY(t.buff,
                              t.token.start,
                              t.token.n)))
  END Error;
  
PROCEDURE Next(t : T) =
  BEGIN 
    t.eop := NOT GetToken(t.buff, t.state, t.token) ;
    Debug.Out("Token \"" & S2T(t.buff, t.token) & "\"")
  END Next;
  
PROCEDURE GetIdentifier(t : T; VAR name : Name) : BOOLEAN =
  BEGIN
    name := NIL;
    Next(t);
    D("Identifier"); RETURN TRUE
  END GetIdentifier;
  
  <*NOWARN*>PROCEDURE D(what : TEXT) = BEGIN (*IO.Put(what & "\n")*) END D;

PROCEDURE Parse(rd : Rd.T) : T =

  VAR
    (* parsing *)
    t := NEW(T);
  BEGIN
    
    t.state.rd := rd;

    Next(t); (* establish lookahead *)

    LOOP
      Debug.Out("DefFormat.Parse kw=" & S2T(t.buff, t.token));
      WITH f = topDisp.get(SUBARRAY(t.buff, t.token.start, t.token.n)) DO
        IF f = NIL THEN
          Debug.Error("DefFormat.Parse, syntax error \"" & S2T(t.buff, t.token) & "\"")
        ELSE
          Next(t);
          f(t, t)
        END
      END
                           
    END;

    RETURN t

  END Parse;

PROCEDURE S2T(READONLY buff : Buffer; s : String) : TEXT =
  BEGIN RETURN Text.FromChars(SUBARRAY(buff, s.start, s.n)) END S2T;

(**********************************************************************)

PROCEDURE ParseVersion(t : T; ref : REFANY) =
  BEGIN
    <*ASSERT t = ref*>
    t.version := S2T(t.buff, t.token);
    Debug.Out("ParseVersion, t.version = " & t.version);
    Next(t);
    t.tokMustBeChar(';')
  END ParseVersion;

PROCEDURE ParseDividerChar(t : T; ref : REFANY) =
  BEGIN
    <*ASSERT t = ref*>
    WITH chars = SUBARRAY(t.buff, t.token.start, t.token.n) DO
      IF NUMBER(chars) # 3 OR chars[0] # DQ OR chars[2] # DQ THEN
        Debug.Error("DefFormat.ParseDividerChar ?syntax error")
      END;
      DefLexer.DividerChar(t.state, chars[1])
    END;
    Next(t);
    t.tokMustBeChar(';')
  END ParseDividerChar;

PROCEDURE ParseBusbitChars(t : T; ref : REFANY) =
  BEGIN
    <*ASSERT t = ref*>
    WITH chars = SUBARRAY(t.buff, t.token.start, t.token.n) DO
      IF NUMBER(chars) # 4 OR chars[0] # DQ OR chars[3] # DQ THEN
        Debug.Error("DefFormat.ParseBusbitChars ?syntax error")
      END;
      DefLexer.BusbitChars(t.state, SUBARRAY(chars, 1, 2))
    END;
    Next(t);
    t.tokMustBeChar(';')
  END ParseBusbitChars;

(**********************************************************************)

TYPE
  Design = OBJECT
    name : TEXT;
    distUnits : CARDINAL;
    end := FALSE;
  END;

PROCEDURE ParseDesign(t : T; ref : REFANY) =
  VAR 
    des := NEW(Design);
  BEGIN
    <*ASSERT t = ref*>
    des.name := S2T(t.buff, t.token);
    Next(t);
    t.tokMustBeChar(';');
    
    LOOP
      Debug.Out("DefFormat.ParseDesign kw=" & S2T(t.buff, t.token));
      WITH f = designDisp.get(SUBARRAY(t.buff, t.token.start, t.token.n)) DO
        IF f = NIL THEN
          Debug.Error("DefFormat.ParseDesign, syntax error \"" & S2T(t.buff, t.token) & "\"")
        ELSE
          Next(t);
          f(t, des)
        END
      END
    END
  END ParseDesign;

PROCEDURE ParseDesignUnits(t : T; ref : REFANY) =
  BEGIN
    WITH des = NARROW(ref, Design) DO
      t.tokMustBeChars(DISTANCEa^);
      t.tokMustBeChars(MICRONSa^);
      
      IF NOT t.getCard(des.distUnits) THEN
        Debug.Error("ParseDesignUnits: ?syntax error: \"" & S2T(t.buff, t.token) & "\"")
      END;
      t.tokMustBeChar(';')
    END
  END ParseDesignUnits;
    
(**********************************************************************)
PROCEDURE ParsePropertyDefinitions(t : T; ref : REFANY) =
  BEGIN
  END ParsePropertyDefinitions;

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

BEGIN 

  AddKeyword(topDisp, "VERSION",             ParseVersion);
  AddKeyword(topDisp, "DIVIDERCHAR",         ParseDividerChar);
  AddKeyword(topDisp, "BUSBITCHARS",         ParseBusbitChars);
  AddKeyword(topDisp, "DESIGN",              ParseDesign);

  AddKeyword(designDisp, "UNITS",               ParseDesignUnits);
  AddKeyword(designDisp, "PROPERTYDEFINITIONS", NIL);
  AddKeyword(designDisp, "DIEAREA",             NIL);
  AddKeyword(designDisp, "ROW",                 NIL);
  AddKeyword(designDisp, "TRACKS",              NIL);
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
  AddKeyword(designDisp, "END",                 NIL);

  AddKeyword(propDisp,   "DESIGN",              NIL);
  AddKeyword(propDisp,   "SPECIALNET",          NIL);
  AddKeyword(propDisp,   "NET",                 NIL);
  AddKeyword(propDisp,   "COMPONENTPIN",        NIL);
  AddKeyword(propDisp,   "NONDEFAULTRULE",      NIL);
  AddKeyword(propDisp,   "COMPONENT",           NIL);
  
  

END DefFormat.
