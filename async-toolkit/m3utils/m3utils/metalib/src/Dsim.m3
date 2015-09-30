MODULE Dsim;
IMPORT Rd;
IMPORT (*IO, *)Text;
(*IMPORT Fmt;*)
IMPORT Name, NameSeq, NameList;
IMPORT Debug;
IMPORT RefList;
IMPORT NameRefTbl;
IMPORT NameSet, NameSetList;
IMPORT NameNameTbl, NameNameListTbl;
FROM Lexer IMPORT GetToken, String, State, BufSize;

CONST DQ='"';

PROCEDURE MergeDsimBodies(a, b : DsimBody) : DsimBody =
  (* merge rules of b into a *)
  BEGIN
    a.rules := RefList.Append(a.rules,b.rules); RETURN a
  END MergeDsimBodies;

PROCEDURE Parse(rd : Rd.T; 
                types : NameRefTbl.T; (* type definitions Name -> Define *)
                decls : NameRefTbl.T; (* inst definitions Name -> Decl   *)
                VAR topLevelWires : NameRefTbl.T
  ) : Define  =

  PROCEDURE GetCard(VAR c : CARDINAL) : BOOLEAN =
    VAR res := 0;
    BEGIN
      FOR i := token.start TO token.start+token.n-1 DO
        WITH num = ORD(buff[i])-ORD('0') DO
          IF num < 0 OR num > 9 THEN RETURN FALSE END;
          res := 10*res+num
        END
      END;
      c := res;
      Next();
      D("Card"); RETURN TRUE
    END GetCard;

  PROCEDURE Error() =
    BEGIN
      Debug.Error("PARSE ERROR, lately reading: " & Text.FromChars(SUBARRAY(buff,token.start,token.n)))
    END Error;

  PROCEDURE Next() =
    BEGIN eop := NOT GetToken(buff,state,token) END Next;

  (*************************************************************)

  PROCEDURE GetIdentifier(VAR name : Name.T) : BOOLEAN =
    BEGIN
      IF NOT ( buff[token.start] = DQ AND buff[token.start+token.n-1]=DQ ) THEN
        RETURN FALSE
      ELSE
        name := Name.ParseChars(SUBARRAY(buff,token.start+1,token.n-2));
        Next();
        D("Identifier"); RETURN TRUE
      END
    END GetIdentifier;

  PROCEDURE GetKeywordSet(k : SET OF Keyword; VAR kw : Keyword) : BOOLEAN =
    (* inefficient for now *)
    BEGIN
      FOR i := FIRST(Keyword) TO LAST(Keyword) DO
        IF i IN k AND GetKeyword(i) THEN kw:=i; D("KeywordSet");RETURN TRUE END
      END;
      RETURN FALSE
    END GetKeywordSet;

  PROCEDURE GetKeyword(kw : Keyword) : BOOLEAN =
    BEGIN
     IF SUBARRAY(buff,token.start,token.n) = KeywordChars[kw]^ THEN
        Next();
        D("Keyword");
        RETURN TRUE
      ELSE
        RETURN FALSE
      END
    END GetKeyword;
    
  PROCEDURE GetSpecial(c : CHAR) : BOOLEAN =
    BEGIN
      IF token.n = 1 AND buff[token.start] = c THEN
        Next(); D("Special"); RETURN TRUE
      ELSE
        RETURN FALSE
      END
    END GetSpecial;
      
  PROCEDURE GetParenList(VAR seq : NameSeq.T; 
                         set : NameSet.T := NIL) : BOOLEAN =
    VAR name : Name.T;
    BEGIN
      IF NOT GetSpecial('(') THEN
        RETURN FALSE
      ELSE
        IF seq = NIL AND set = NIL THEN
          seq := NEW(NameSeq.T).init()
        END;

        IF GetSpecial(')') THEN (* empty list *)
          D("ParenList"); RETURN TRUE 
        END;

        WHILE GetIdentifier(name) DO
          IF set # NIL THEN 
            EVAL set.insert(name) 
          ELSE
            seq.addhi(name)
          END;
          IF GetSpecial(')') THEN 
            D("ParenList"); RETURN TRUE 
          ELSIF NOT GetSpecial(',') THEN
            Error(); <*ASSERT FALSE*>
          END
        END
      END;
      Error(); <*ASSERT FALSE*>
    END GetParenList;

  PROCEDURE GetDecl(VAR decl : Decl) : BOOLEAN =
    VAR type : Name.T;
    BEGIN
      IF NOT GetTypeName(type) THEN 
        RETURN FALSE
      ELSE
        decl := NEW(Decl, type := type, args := NIL);
        IF GetIdentifier(decl.id) AND GetParenList(decl.args) THEN
          D("Decl"); RETURN TRUE
        ELSE
          Error(); <*ASSERT FALSE*>
        END
      END
    END GetDecl;

  PROCEDURE GetRule(VAR rule : Rule) : BOOLEAN =
    TYPE K = Keyword;
    CONST 
      SkipWords = SET OF K { K.Env, K.After, K.Metastab, K.Unstab, K.Isochronic };
    VAR kw : K;
        x  : CARDINAL;
        started := FALSE;
        conjunct : Conjunct;
        id : Name.T;
    BEGIN
      rule := NEW(Rule);

      LOOP
        IF GetKeywordSet(SkipWords,kw) THEN 
          started := TRUE;
          rule.attrs := rule.attrs + SET OF K { kw }
        ELSIF  GetCard(x) THEN
          started := TRUE;
          rule.delay := x
        ELSE
          EXIT
        END
      END;
      
      LOOP 
        IF GetSpecial('~') THEN
          started := TRUE;
          conjunct := NEW(Conjunct, sense := Sense.Down);
          IF GetIdentifier(id) THEN
            conjunct.input := id
          ELSE
            Error()
          END;
          rule.conjuncts := RefList.Cons(conjunct,rule.conjuncts);
        ELSIF GetIdentifier(id) THEN
          started := TRUE;
          conjunct := NEW(Conjunct, sense := Sense.Up, input := id);
          rule.conjuncts := RefList.Cons(conjunct,rule.conjuncts);
        ELSE
          EXIT
        END;
        IF GetSpecial('-') THEN EXIT END;
        IF NOT GetSpecial('&') THEN 
          Error()
        END
      END;

      IF NOT started THEN (* nothing parsed *) RETURN FALSE END;

      IF NOT GetSpecial('>') THEN Error() END;
      
      IF NOT GetIdentifier(rule.target) THEN
        Error();
      END;

      IF GetSpecial('+') THEN
        rule.sense := Sense.Up
      ELSIF GetSpecial('-') THEN
        rule.sense := Sense.Down
      ELSE
        Error()
      END;
      D("Rule");
      RETURN TRUE
    END GetRule;

  PROCEDURE GetDsimBody(VAR body : DsimBody) : BOOLEAN =
    VAR pr : Rule;
    BEGIN
      IF NOT GetKeyword(Keyword.Dsim) THEN
        RETURN FALSE
      ELSE
        body := NEW(DsimBody);
        IF GetSpecial('{') THEN
          WHILE NOT GetSpecial('}') DO
            IF GetRule(pr) THEN
              body.rules := RefList.Cons(pr,body.rules)
            ELSE
              Error()
            END
          END;
          D("DsimBody");
          RETURN TRUE
        ELSE
          Error(); <*ASSERT FALSE*>
        END
      END
    END GetDsimBody;

  PROCEDURE GetDefineBody(VAR define : Define(* already alloc'd *)) : BOOLEAN =
    VAR 
      dsimBody : DsimBody;
    BEGIN
      WHILE NOT (GetSpecial('}') OR eop) DO
        IF GetDsimBody(dsimBody) THEN
          IF define.dsimBody = NIL THEN
            define.dsimBody := dsimBody
          ELSE
            define.dsimBody := MergeDsimBodies(define.dsimBody, dsimBody)
          END
        ELSIF GetWire(wire) THEN
          AddWireToTbl(wire,define.wires)
        ELSIF GetSkipStuff() THEN
          (* skip *)
        ELSIF GetDecl(decl) THEN
          define.decls := RefList.Cons(decl,define.decls)
        ELSE
          Error()
        END
      END;
      RETURN TRUE
    END GetDefineBody;

  PROCEDURE GetTypeName(VAR name : Name.T) : BOOLEAN =
    BEGIN
      IF NOT GetIdentifier(name) THEN 
        RETURN FALSE
      ELSE
        (* strip last component per Harry *)
        name := Name.Parent(name);
        D("TypeName"); RETURN TRUE
      END
    END GetTypeName;

  PROCEDURE GetDefine(VAR define : Define) : BOOLEAN =

    BEGIN
      IF NOT GetKeyword(Keyword.Define) THEN
        RETURN FALSE
      ELSE
        define := NEW(Define, 
                      dsimBody := NIL,
                      wires := NEW(NameRefTbl.Default).init(),
                      args := NIL);
        IF  GetTypeName(define.typeName) AND
            GetParenList (define.args)     AND
            GetSpecial('{') AND
            GetDefineBody(define) THEN
          D("Define");
          RETURN TRUE
        ELSE
          Error(); <*ASSERT FALSE*>
        END
      END
    END GetDefine;

  PROCEDURE GetWire(VAR wire : Wire) : BOOLEAN =
    VAR noseq : NameSeq.T; (* unused*)
    BEGIN
      IF NOT GetKeyword(Keyword.Wire) THEN
        RETURN FALSE
      ELSE
        wire := NEW(Wire, nodes := NEW(NameSetList.T).init());
        IF GetParenList(noseq,wire.nodes) THEN
          D("Wire");
          RETURN TRUE
        ELSE
          Error(); <*ASSERT FALSE*>
        END
      END
    END GetWire;

  PROCEDURE GetSkipStuff() : BOOLEAN =
    VAR seq : NameSeq.T := NIL;
    BEGIN
      IF GetKeyword(Keyword.Exclcc) OR 
         GetKeyword(Keyword.Exclhi) OR 
         GetKeyword(Keyword.Excllo)    THEN
        IF GetParenList(seq) THEN
          D("SkipStuff"); RETURN TRUE 
        ELSE 
          Error() ; <*ASSERT FALSE*>
        END
      ELSE 
        RETURN FALSE
      END
    END GetSkipStuff;

  <*NOWARN*>PROCEDURE D(what : TEXT) = BEGIN (*IO.Put(what & "\n")*) END D;

  VAR
    (* parsing *)
    buff : ARRAY [0..BufSize-1] OF CHAR;
    eop  := FALSE; (* done parsing *)
    token : String;
    define : Define;
    decl   : Decl;
    wire   : Wire;
    state  : State;
  BEGIN
    
    state.rd := rd;

    Next(); (* establish lookahead *)
    
    (* syntax for top level is a bit different since define itself
       cant be nested *)

    topLevelWires := NEW(NameRefTbl.Default).init();

    WHILE NOT eop DO
      (* at top level *)
      IF    GetDefine(define) THEN
        EVAL types.put(define.typeName,define)
      ELSIF GetDecl(decl) THEN
        EVAL decls.put(decl.id,decl)
      ELSIF GetWire(wire) THEN
        AddWireToTbl(wire,topLevelWires)
      ELSIF GetSkipStuff() THEN
        (* skip *)
      END
    END;

    (* make fake top-level *)
    VAR
      top := NEW(Define, 
                 typeName := NIL, 
                 args     := NIL, 
                 dsimBody := NIL,
                 wires    := topLevelWires);
      n : Name.T;
      r : REFANY;
    BEGIN
      WITH iter = decls.iterate() DO
        WHILE iter.next(n, r) DO
          top.decls := RefList.Cons(r, top.decls)
        END
      END;
      RETURN top
    END
  END Parse;

PROCEDURE AddWireToTbl(wire : Wire; tbl : NameRefTbl.T) =
  BEGIN
    WITH iter = wire.nodes.iterate() DO
      VAR n : Name.T; 
          old : REFANY; 
      BEGIN
        WHILE iter.next(n) DO
          IF tbl.get(n, old) THEN
            EVAL wire.nodes.unionD(old)
          END;
          EVAL tbl.put(n, wire.nodes)
        END
      END
    END
  END AddWireToTbl;

PROCEDURE Flatten(define          : Define; 
                  types           : NameRefTbl.T;
                  instanceTypeTbl : NameNameTbl.T;
                  typeInstanceTbl : NameNameListTbl.T;
                  root            : Name.T := NIL ) =
  VAR
    p := define.decls;
  BEGIN
    WHILE p # NIL DO
      WITH decl = NARROW(p.head,Decl),
           id   = Name.Append(root, decl.id),
           tn   = decl.type                   DO
        EVAL instanceTypeTbl.put(id, tn);
        VAR
          lst : NameList.T := NIL;
        BEGIN
          EVAL typeInstanceTbl.get(tn, lst);
          lst := NameList.Cons(id, lst);
          EVAL typeInstanceTbl.put(tn, lst)
        END;
        VAR
          r : REFANY;
        BEGIN
          WITH hadIt = types.get(tn,r) DO
            <*ASSERT hadIt*>
            Flatten(r, types, instanceTypeTbl, typeInstanceTbl, id)
          END
        END;
        p := p.tail
      END
    END
  END Flatten;


VAR KeywordChars : ARRAY Keyword OF REF ARRAY OF CHAR;


BEGIN 
  FOR i := FIRST(Keyword) TO LAST(Keyword) DO
    WITH t = KeywordTexts[i],
         l = Text.Length(t) DO
      VAR a := NEW(REF ARRAY OF CHAR, l);
      BEGIN
        Text.SetChars(a^,t);
        KeywordChars[i] := a
      END
    END
  END

END Dsim.
