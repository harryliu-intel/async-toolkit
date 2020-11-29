MODULE BraceParse;

(*

  Parser for Synopsys ICV ".net" output format.

  Author : Mika Nystrom <mika.nystroem@intel.com>
  November 2020

*)

IMPORT Rd, Thread;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Debug;
IMPORT Text;
IMPORT NetKeywords AS N;
IMPORT Compiler;
IMPORT IO;
IMPORT CellRec, CellRecClass;
IMPORT Atom, Subcell;
IMPORT CharsAtomTbl;
IMPORT MosInfo, MosInfoCardTbl;
IMPORT AtomCellTbl;
IMPORT CardPair;
IMPORT SubcellSeq;
IMPORT OpenCharArrayRefTbl;
IMPORT ExceptionInfo;
IMPORT AtomSetDef;

(* 
   an interesting extension of the buffering code would be to make the
   buffer variable size (REF ARRAY OF CHAR), starting it at, say, 16K, 
   and growing it as needed to accomodate larger objects.
   
   doing this we could for example decree that an entire CELL or 
   INST is in memory at the same time, and use offsets into the buffer to
   build it all at the end of parsing the CELL or INST rather than having
   to do everything on-the-fly 
*)

TYPE SC = SET OF CHAR;
     CA = ARRAY OF CHAR;
     
CONST WhiteSpace = SC { ' ', '\t', '\n', '\r' };
      Special    = SC { '{', '}', '=' };
      Digit      = SC { '0' .. '9' };
      Lower      = SC { 'a' .. 'z' };
      Upper      = SC { 'A' .. 'Z' };
      Letter     = Lower + Upper;
      Ident1     = Letter + SC { '_' };
      Ident      = Ident1 + Digit;
      SpecialPlusWS = Special + WhiteSpace;

      LB = CA { '{' };
      RB = CA { '}' };
      EQ = CA { '=' };

TYPE InstanceType = { MOS, Cell, Res, BJT, Unknown };

     FetProps = RECORD
       l : CARDINAL;    (* length in micro-microns (picometers) *)
       nfin : CARDINAL; (* # of fins *)
     END;

CONST TL = Compiler.ThisLine;
      
VAR doDebug := Debug.GetLevel() >= 10 AND Debug.This("BraceParse");
    
PROCEDURE AtomFromChars(atomTbl : CharsAtomTbl.T;
                        READONLY chars : ARRAY OF CHAR) : Atom.T =
  VAR
    atom : Atom.T;
  BEGIN
    IF atomTbl.get(chars, atom) THEN
      RETURN atom
    ELSE
      atom := Atom.FromText(Text.FromChars(chars));
      EVAL atomTbl.put(chars, atom);
      RETURN atom
    END
  END AtomFromChars;
  
PROCEDURE Parse(rd : Rd.T; transistorCells : OpenCharArrayRefTbl.T) : T
  RAISES { Rd.Failure, Thread.Alerted } =

  VAR
    buf       : Buffer;
    totBytes  : CARDINAL := 0;

    b : CARDINAL := 0;         (* buffer pointer  *)
    e : CARDINAL := 0;         (* end of buffer   *)
    s : CARDINAL := BufSiz;    (* start of token  *)

    lev := 0;

    lineno := 1;

    warnSet := NEW(AtomSetDef.T).init();
    atomTbl := NEW(CharsAtomTbl.Default).init();

  PROCEDURE Refill()
    RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax }  =
    BEGIN
      (* existing token starts at s, b is at end *)
      
      (* shift down the old token *)
      WITH tokSoFar = BufSiz - s DO
        SUBARRAY(buf, 0, tokSoFar) := SUBARRAY(buf, s, tokSoFar);
        s := 0;
        b := tokSoFar
      END;

      (* refill buffer *)

      IF BufSiz - b < 2 THEN RAISE 
        Syntax(TL())  (* token too long -- need 2 for comments *)
      END;
      WITH len = Rd.GetSub(rd, SUBARRAY(buf, b, BufSiz - b)) DO
        IF len = 0 THEN RAISE Rd.EndOfFile END;
        INC(totBytes, len);
        e := b + len;
      END;

      <*ASSERT b # e AND b # e - 1*>
    END Refill;

  VAR haveTok := FALSE;

  PROCEDURE PutPos() =
    BEGIN
      IO.Put("\r");
      IO.Put(Int(lineno));
      IO.Put(" lines ");
      IO.Put(Int(Rd.Index(rd)));
      IO.Put(" bytes")
    END PutPos;
    
  PROCEDURE NextToken() 
    RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax } =

    PROCEDURE Dbg() =
      BEGIN
        Debug.Out(F("Lev %s got token %s",
                    Int(lev),
                    Text.FromChars(SUBARRAY(buf,
                                            s,
                                            b - s))))
      END Dbg;

    BEGIN
      (* we should have consumed previous token *)
      <*ASSERT NOT haveTok*>
      
      (* ensure we have text *)
      IF b = e THEN Refill() END;
      
      (* read token from b onwards *)
      WHILE buf[b] IN WhiteSpace OR
            buf[b] = '/' AND buf[b+1] = '*' DO
        WHILE buf[b] IN WhiteSpace DO

          IF buf[b] = '\n' THEN
            INC(lineno);
            IF lineno MOD 10000 = 0 THEN
              PutPos()
            END
          END;
        
          INC(b); (* skip *)
        
          IF b = e THEN Refill() END;
        END;

        IF buf[b] = '/' AND buf[b+1] = '*' THEN
          INC(b,2);

          WHILE buf[b] # '*' AND buf[b+1] # '/' DO
            IF buf[b] = '\n' THEN
              INC(lineno)
            END;
        
            INC(b); (* skip *)
        
            IF b = e THEN Refill() END;
          END;
          INC(b,2)
        END
            
      END;

      (* buf[b] is NOT whitespace : we are at start of token *)
      s := b;

      (* check for single character token *)
      <*ASSERT b # e*>
      IF    buf[b] IN Special THEN
        IF    buf[b] = '{' THEN INC(lev)
        ELSIF buf[b] = '}' THEN DEC(lev)
        END;
        
        INC(b);
        haveTok := TRUE;
        IF doDebug THEN Dbg() END;
        RETURN 
      END;

      <*ASSERT b # e*>

      (* we are now at a normal character, so we can execute the 
         following loop at least once *)
      REPEAT
        INC(b); 

        IF b = e THEN Refill() END
      UNTIL buf[b] IN SpecialPlusWS;

      (* we are at the end of a token *)
      haveTok := TRUE;
      IF doDebug THEN Dbg() END;
    END NextToken;

    (************************************************************)
    
  PROCEDURE GetAny(VAR tok : Token) : BOOLEAN
    RAISES ANY =
    BEGIN
      IF NOT haveTok THEN NextToken() END;
      
      tok.s := s;
      tok.n := b - s;
      haveTok := FALSE;
      RETURN NOT haveTok
    END GetAny;

  PROCEDURE GetExact(READONLY str : ARRAY OF CHAR) : BOOLEAN
    RAISES ANY =
    BEGIN
      IF NOT haveTok THEN NextToken() END;

      haveTok := SUBARRAY(buf, s, b - s) # str;
      RETURN NOT haveTok
    END GetExact;

  PROCEDURE GetIdent(VAR str : Token) : BOOLEAN
    RAISES ANY =
    BEGIN
      IF NOT haveTok THEN NextToken() END;

      IF NOT buf[s] IN Ident1 THEN RETURN FALSE END;

      FOR q := s + 1 TO b - 1 DO
        IF NOT buf[s] IN Ident THEN RETURN FALSE END
      END;

      str := Token { s, b - s };
      haveTok := FALSE;
      RETURN NOT haveTok
    END GetIdent;

  PROCEDURE GetInt(VAR int : INTEGER) : BOOLEAN 
    RAISES ANY =
    VAR
      neg := FALSE;
      q : CARDINAL;

      res := 0;
    BEGIN
      IF NOT haveTok THEN NextToken() END;

      q := s;

      IF buf[q] = '-' THEN neg := TRUE; INC(q) END;

      IF q = b THEN RETURN FALSE END;

      WHILE q < b DO
        WITH c = buf[q] DO
          IF '0' <= c AND c <= '9' THEN
            res := res * 10 + ORD(c) - ORD('0');
            INC(q)
          ELSE
            RETURN FALSE
          END
        END
      END;

      IF neg THEN int := -res ELSE int := res END;

      haveTok := FALSE;
      RETURN TRUE
    END GetInt;

  PROCEDURE GetFloat(VAR lr : LONGREAL) : BOOLEAN 
    RAISES ANY =
    VAR
      neg := FALSE;
      q : CARDINAL;

      whole := 0;
      frac := 0.0d0;
    BEGIN
      IF NOT haveTok THEN NextToken() END;

      q := s;

      IF buf[q] = '-' THEN neg := TRUE; INC(q) END;

      IF q = b THEN RETURN FALSE END;

      WHILE q < b DO
        WITH c = buf[q] DO
          IF '0' <= c AND c <= '9' THEN
            whole := whole * 10 + ORD(c) - ORD('0');
            INC(q)
          ELSIF buf[q] = '.' THEN
            EXIT
          ELSE
            RETURN FALSE
          END
        END
      END;

      (* done reading the integer part -- two poss.
         
         1. either done here
         2. we are at the radix point
      *)

      IF buf[q] = '.' THEN
        INC(q);(* skip *)
        
        (* now read fractional part *)
        VAR
          pos := 10.0d0; (* this is exact, 0.1 is not *)
        BEGIN
          WHILE q < b DO
            WITH c = buf[q] DO
              IF '0' <= c AND c <= '9' THEN
                frac := frac + FLOAT(ORD(c) - ORD('0'),LONGREAL) / pos;
                pos := pos * 10.0d0;
                INC(q);
              ELSE
                RETURN FALSE
              END
            END
          END
        END
      END;

      IF neg THEN lr := -(FLOAT(whole,LONGREAL) + frac)
      ELSE        lr :=   FLOAT(whole,LONGREAL) + frac
      END;

      haveTok := FALSE;
      RETURN TRUE
    END GetFloat;
    
    (************************************************************)

  <*UNUSED*>PROCEDURE Trivial() 
    RAISES ANY =
    VAR
      tok : Token;
    BEGIN
      WHILE GetAny(tok) DO
        IF doDebug THEN
          Debug.Out(F("Lev %s got token %s",
                      Int(lev),
                      Text.FromChars(SUBARRAY(buf,
                                              tok.s,
                                              tok.n))))
        END
      END
    END Trivial;

    (************************************************************)

  PROCEDURE GetPin() : BOOLEAN 
    RAISES ANY =
    VAR
      exName, inName : Token;
    BEGIN
      IF NOT GetExact(N.PINkw) THEN RETURN FALSE END;

      LOOP
        IF GetExact(RB) THEN
          RETURN TRUE
        ELSE
          IF NOT GetIdent(exName) THEN RAISE Syntax(TL()) END;
          IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
          IF NOT GetIdent(inName) THEN RAISE Syntax(TL()) END;
        END
      END
    END GetPin;

  PROCEDURE GetCoord() : BOOLEAN 
    RAISES ANY =
    VAR
      axis : Token;
      pos : LONGREAL;
    BEGIN
      IF NOT GetExact(N.COORDkw) THEN RETURN FALSE END;

      (* at least two coords *)
      IF NOT GetIdent(axis) THEN RAISE Syntax(TL()) END;
      IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
      IF NOT GetFloat(pos) THEN RAISE Syntax(TL()) END;
      IF NOT GetIdent(axis) THEN RAISE Syntax(TL()) END;
      IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
      IF NOT GetFloat(pos) THEN RAISE Syntax(TL()) END;

      LOOP
        IF GetExact(RB) THEN
          RETURN TRUE
        ELSIF GetIdent(axis) THEN 
          IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
          IF NOT GetFloat(pos) THEN RAISE Syntax(TL()) END;
        ELSE
          RAISE Syntax(TL())
        END
      END
    END GetCoord;

  PROCEDURE GetPort() : BOOLEAN 
    RAISES ANY =
    VAR
      portNm : Token;
    BEGIN
      IF NOT GetExact(N.PORTkw) THEN RETURN FALSE END;

      LOOP
        IF    GetIdent(portNm) THEN
        ELSIF GetExact(RB) THEN
          RETURN TRUE
        ELSE
          RAISE Syntax(TL())
        END
      END
    END GetPort;

  PROCEDURE GetInst(parent : CellRec.T; subcells : SubcellSeq.T) : BOOLEAN 
    RAISES ANY =
    VAR
      instNm, typeNm   : Token;
      instBuf, typeBuf : Buffer;
      type             := InstanceType.Unknown;
      props            := FetProps { 0, 0 };
      isTransistorCell := FALSE;
      dummy            : REFANY;
    BEGIN 
      IF NOT GetExact(N.INSTkw) THEN RETURN FALSE END;
      IF NOT GetIdent(instNm) THEN RAISE Syntax(TL()) END;
      SUBARRAY(instBuf, 0, instNm.n) := SUBARRAY(buf, instNm.s, instNm.n);
      IF NOT GetExact(EQ) THEN RAISE Syntax(TL()) END;
      IF NOT GetIdent(typeNm) THEN RAISE Syntax(TL()) END;
      SUBARRAY(typeBuf, 0, typeNm.n) := SUBARRAY(buf, typeNm.s, typeNm.n);

      (* check whether type name is in the special list *)
      IF transistorCells.get(SUBARRAY(buf, typeNm.s, typeNm.n), dummy) THEN
        isTransistorCell := TRUE
      END;
      
      LOOP
        IF GetExact(LB) THEN
          IF    GetType(type) THEN
            (* if stated type is one of the special Transistor Cell types
               then override whatever the type says it is and set it to 
               MOS *)
            IF isTransistorCell THEN
              type := InstanceType.MOS
            END
          ELSIF GetCoord() THEN
          ELSIF GetProp(props) THEN
          ELSIF GetPin() THEN
          ELSE
            RAISE Syntax(TL())
          END
        ELSIF GetExact(RB) THEN
          EXIT
        END
      END;

      CASE type OF
        InstanceType.MOS =>
        VAR
          type    := AtomFromChars(atomTbl, SUBARRAY(typeBuf, 0, typeNm.n));
          mosInfo := MosInfo.T { type, props.l };
          oldCnt  : CardPair.T;
        BEGIN
          IF doDebug THEN
            Debug.Out("got a MOS with fins " & Int(props.nfin))
          END;
          IF parent.mosTbl.get(mosInfo, oldCnt) THEN
            EVAL parent.mosTbl.put(mosInfo, CardPair.T {oldCnt.k1 + 1,
                                                        oldCnt.k2 + props.nfin})
          ELSE
            EVAL parent.mosTbl.put(mosInfo, CardPair.T { 1, props.nfin })
          END
        END

      |
        InstanceType.Cell =>
        VAR
          sub : Subcell.T;
          cellRec : CellRec.T;
          fail := FALSE;
        BEGIN

          WITH subtype = AtomFromChars(atomTbl, SUBARRAY(typeBuf, 0, typeNm.n)),
               hadIt = t.cellTbl.get(subtype, cellRec) DO
            IF NOT hadIt THEN
              fail := TRUE;

              IF NOT warnSet.member(subtype) THEN
                EVAL warnSet.insert(subtype);
                Debug.Warning("Cannot find subcell type " & Atom.ToText(subtype));
              END;
              fail := TRUE;
            END;
            sub.type := cellRec
          END;

          IF NOT fail THEN
            Subcell.EncodeName(t.longNames,
                               SUBARRAY(instBuf, 0, instNm.n),
                               sub.instance);

            subcells.addhi(sub)
          END
        END
      |
        InstanceType.Res, InstanceType.BJT =>
        (* skip *)
      ELSE
        Debug.Warning("?unknown instance type on line " & Int(lineno))
      END;
      
      RETURN TRUE
    END GetInst;
    
  PROCEDURE GetType(VAR type : InstanceType) : BOOLEAN 
    RAISES ANY =
    VAR
      typeDummy : Token;
    BEGIN
      IF NOT GetExact(N.TYPEkw) THEN RETURN FALSE END;

      IF GetExact(N.MOSkw) THEN
        type := InstanceType.MOS
      ELSIF GetExact(N.CELLkw) THEN
        type := InstanceType.Cell
      ELSIF GetExact(N.RESkw) THEN
        type := InstanceType.Res
      ELSIF GetExact(N.BJTkw) THEN
        type := InstanceType.BJT
      ELSIF GetIdent(typeDummy) THEN
        type := InstanceType.Unknown
      ELSE
        RAISE Syntax(TL())
      END;

      IF NOT GetExact(RB) THEN RAISE Syntax(TL()) END;
      RETURN TRUE
    END GetType;
    
  PROCEDURE GetProp(VAR props : FetProps) : BOOLEAN 
    RAISES ANY =

    PROCEDURE GetPropAssign() RAISES ANY =
      BEGIN
        IF NOT GetExact(EQ) OR NOT GetFloat(propVal) THEN
          RAISE Syntax(TL())
        END
      END GetPropAssign;
      
    VAR
      propNm  : Token;
      propVal : LONGREAL;
    BEGIN
      IF NOT GetExact(N.PROPkw) THEN RETURN FALSE END;

      LOOP
        IF    GetExact(RB) THEN
          RETURN TRUE
        ELSIF GetExact(N.lkw) OR GetExact(N.lrkw) THEN
          GetPropAssign();
          props.l := ROUND(propVal * 1.0d6);
          IF doDebug THEN
            Debug.Out(F("Got l = %s -> %s", LongReal(propVal), Int(props.l)))
          END;
        ELSIF GetExact(N.nfinkw) THEN
          GetPropAssign();
          props.nfin := ROUND(propVal);

          IF doDebug THEN
            Debug.Out(F("nfin %s", Int(props.nfin)))
          END
        ELSIF GetIdent(propNm) THEN
          GetPropAssign()
        ELSE
          RAISE Syntax(TL())
        END
      END   
    END GetProp;
    
  PROCEDURE GetCell() : BOOLEAN 
    RAISES ANY =
    VAR
      nm : Token;

      cell : CellRec.T;
      props : FetProps; (* unused *)

      cellNm : Atom.T;
      subcells : SubcellSeq.T;
    BEGIN
      IF NOT GetExact(N.CELLkw) THEN RETURN FALSE END;

      IF NOT GetIdent(nm) THEN RAISE Syntax(TL()) END;

      cellNm := AtomFromChars(atomTbl, SUBARRAY(buf, nm.s, nm.n));

      cell := NEW(CellRec.T,
                  nm       := AtomFromChars(atomTbl, SUBARRAY(buf, nm.s, nm.n)),
                  subcells := NIL,
                  mosTbl   := NEW(MosInfoCardTbl.Default).init());
      subcells := NEW(SubcellSeq.T).init();
      LOOP
        IF GetExact(LB) THEN
          IF    GetPort() THEN
          ELSIF GetProp(props) THEN
          ELSIF GetInst(cell, subcells) THEN
          ELSE
            RAISE Syntax(TL()) 
          END
        ELSIF GetExact(RB) THEN
          EXIT
        ELSE
          RAISE Syntax(TL())
        END
      END;

      (* copy sequence to cell *)
      cell.subcells := NEW(REF ARRAY OF Subcell.T, subcells.size());
      FOR i := FIRST(cell.subcells^) TO LAST(cell.subcells^) DO
        cell.subcells[i] := subcells.get(i)
      END;
      
      WITH hadIt = t.cellTbl.put(cellNm, cell) DO
        IF hadIt THEN RAISE Syntax(TL()) END
      END;

      RETURN TRUE
    END GetCell;
    
  PROCEDURE GetVersion() : BOOLEAN 
    RAISES ANY =
    VAR
      v0, v1, v2 : INTEGER;
    BEGIN
      IF NOT GetExact(N.VERSIONkw) THEN RETURN FALSE END;

      IF NOT GetInt(v0) THEN RAISE Syntax(TL()) END;
      IF NOT GetInt(v1) THEN RAISE Syntax(TL()) END;
      IF NOT GetInt(v2) THEN RAISE Syntax(TL()) END;

      IF NOT GetExact(RB) THEN RAISE Syntax(TL()) END;
      RETURN TRUE
    END GetVersion;

  PROCEDURE GetNetlist(VAR t : T) : BOOLEAN 
    RAISES ANY =
    VAR
      nm : Token;
    BEGIN
      IF NOT GetExact(N.NETLISTkw) THEN RETURN FALSE END;

      <*ASSERT t = NIL*>
      t := NEW(T);
      t.cellTbl := NEW(AtomCellTbl.Default).init();
      t.longNames := Subcell.NewLongNames();

      IF NOT GetIdent(nm) THEN RETURN FALSE END;
      
      LOOP
        IF GetExact(RB) THEN
          RETURN TRUE
        ELSIF GetExact(LB) THEN
          IF    GetVersion() THEN
          ELSIF GetCell() THEN
          ELSE RAISE Syntax(TL())
          END
        ELSE
          RAISE Syntax(TL())
        END
      END
    END GetNetlist;
    
  PROCEDURE ParseTop(VAR t : T) : BOOLEAN 
    RAISES ANY =
    BEGIN
      IF NOT GetExact(LB) THEN RETURN FALSE END;

      TRY
        IF GetNetlist(t) THEN
          RETURN TRUE
        ELSE
          RETURN FALSE
        END
      EXCEPT
        Rd.EndOfFile => RAISE Syntax(TL())
      END

    END ParseTop;

    (* complete set of exceptions is

    RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, Syntax } 

    *)

  VAR
    t : T := NIL;
  BEGIN
    TRY
      (* Trivial() *)
      
      IF NOT ParseTop(t) THEN
        RAISE Syntax(TL())
      END;
      PutPos();
      IO.Put("\n")
    EXCEPT
      Rd.EndOfFile => (* skip *)
    |
      Rd.Failure(x) => RAISE Rd.Failure(x)
    |
      Thread.Alerted => RAISE Thread.Alerted
    |
      Syntax(line) =>
      Debug.Error(F("?Syntax error on line %s of input (%s:%s)",
                    Int(lineno),
                    Compiler.ThisFile(),
                    Int(line)))
    ELSE
      Debug.Error("Unexpected exception\n" & ExceptionInfo.Fmt(Compiler.ThisException()));
      <*ASSERT FALSE*>
    END;
    
    Debug.Out(F("Read %s bytes", Int(totBytes)));
    RETURN t
  END Parse;

TYPE Token = RECORD s, n : CARDINAL END;

EXCEPTION Syntax(CARDINAL);

PROCEDURE InitCellTblAux(tbl : AtomCellTbl.T; to : CARDINAL) =
  VAR
    iter := tbl.iterate();
    a : Atom.T;
    c : CellRec.T;
  BEGIN
    WHILE iter.next(a, c) DO
      c.aux := to
    END
  END InitCellTblAux;
          
BEGIN END BraceParse.
