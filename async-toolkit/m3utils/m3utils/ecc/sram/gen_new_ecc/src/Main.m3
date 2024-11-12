MODULE Main;

(* 
   build ECC logic for SRAMS.

   Author: Mika Nystrom <mika@alum.mit.edu>
   
   May, 2013
 *)

IMPORT Word, IO;
IMPORT IntSet, IntSetDef;
IMPORT TextSet, TextSetDef;
IMPORT RefList;
IMPORT Fmt;
IMPORT RefSeq, RefArraySort, Text;
IMPORT IntArraySort;
IMPORT Debug;
IMPORT ParseParams;
IMPORT Pathname;
IMPORT Stdio;
IMPORT Thread;
IMPORT TextWr, FileWr, Wr, OSError, AL;

<*FATAL Thread.Alerted*>

VAR Bits       : CARDINAL;               (* := 64;           *)
VAR DataBits   : CARDINAL;              
VAR ParityBits : CARDINAL;               (* :=  6;           *)
VAR MaxBitPos  : CARDINAL;               (* := ParityBits-1; *)
(* 
   "parity bits" run from -1 to ParityBits-1 
   (that is, there are ParityBits + 1 of them)
   bit -1 is the overall parity
   the other bits are the Hamming code
*)


PROCEDURE WriteDefs(defFn : Pathname.T) 
  RAISES { OSError.E, Wr.Failure } =
  BEGIN
    WITH wr = FileWr.Open(defFn) DO
      Wr.PutText(wr, "`ifndef ECCDEFS\n");
      Wr.PutText(wr, "`define ECCDEFS 1\n");
      Wr.PutText(wr, "localparam DBITS = " & II(DataBits) & ";\n");
      Wr.PutText(wr, "localparam CBITS = " & II(ParityBits+1) & ";\n");
      Wr.PutText(wr, "localparam TBITS = " & II(Bits) & ";\n");
      Wr.PutText(wr, "`endif\n");
      Wr.Close  (wr)
    END
  END WriteDefs;

PROCEDURE SeekSize(dataBits                        : CARDINAL;
                   VAR bits, parityBits, maxBitPos : CARDINAL) 
  RAISES { Wr.Failure, OSError.E } =
  BEGIN
    FOR p := 2 TO LAST(CARDINAL) DO
      WITH d = Word.LeftShift(1, p) - p - 1 DO
        IF d >= dataBits THEN
          (* success *)
          bits := dataBits + p + 1;
          parityBits := p;
          maxBitPos := parityBits - 1;
          Debug.Out(F("dataBits %s parityBits %s bits %s",
                      II(dataBits), II(parityBits), II(bits)));
          IF defFn # NIL THEN WriteDefs(defFn) END;
          RETURN
        END
      END
    END
  END SeekSize;

CONST TE = Text.Equal;

TYPE ABitPos = [-1..LAST(CARDINAL)];


(* the order in which things are built is very very tricky! *)

TYPE Stream = { Prolog, Declare, Assign, Instance, Epilog };

VAR streams : ARRAY Stream OF TextWr.T;

PROCEDURE InitStreams() = 
  BEGIN 
    FOR i := FIRST(streams) TO LAST(streams) DO streams[i] := TextWr.New() END 
  END InitStreams;

PROCEDURE DumpStreams(fn : Pathname.T;) 
  RAISES { Wr.Failure, OSError.E } =
  BEGIN
    IF NOT quiet THEN IO.Put(fn & "\n") END;
    WITH wr = FileWr.Open(fn) DO
      FOR i := FIRST(streams) TO LAST(streams) DO
        WITH txt = TextWr.ToText(streams[i]) DO
          Wr.PutText(wr, txt);
          streams[i] := NIL
        END
      END;
      Wr.Close(wr)
    END
  END DumpStreams;

PROCEDURE Emit(stream : Stream;
               t      : TEXT) =
  <*FATAL Wr.Failure*>
  BEGIN
    Debug.Out(t);
    Wr.PutText(streams[stream], t);
    Wr.PutChar(streams[stream], '\n')
  END Emit;

PROCEDURE Declare(v : TEXT) =
  BEGIN
    Emit(Stream.Declare, F("logic  %s;",v))
  END Declare;

PROCEDURE DeclareArray(v : TEXT; lo, hi : INTEGER) =
  BEGIN
    Emit(Stream.Declare, F("logic[%s:%s] %s;", II(hi), II(lo), v))
  END DeclareArray;

PROCEDURE IsBitSet(in  : CARDINAL;
                   bit : ABitPos) : BOOLEAN =
  BEGIN
    (* by convention, bit -1 is set for all *)
    IF bit = -1 THEN 
      RETURN TRUE
    ELSE
      RETURN Word.And(1, Word.RightShift(in, bit)) = 1
    END
  END IsBitSet;

PROCEDURE NumBitsSet(in : CARDINAL) : CARDINAL =
  VAR
    b := 0;
  BEGIN
    WHILE in # 0 DO
      IF Word.And(in, 1) = 1 THEN INC(b) END;
      in := Word.RightShift(in, 1)
    END;
    RETURN b
  END NumBitsSet;

VAR inputGates : RefList.T := NIL;

TYPE 
  Gate = BRANDED OBJECT
    inputs  : TextSet.T;  (* direct inputs *)
    output  : TEXT;       (* name of output node *)
  END;

  XorGate = Gate BRANDED OBJECT
    sources : IntSet.T;   (* ultimate inputs *)
  END;

  NandGate = Gate BRANDED OBJECT END;

VAR gc := 0;

PROCEDURE Commatize(s : TextSet.T) : TEXT =
  BEGIN RETURN Infixize(s, ",") END Commatize;

PROCEDURE Infixize(s : TextSet.T; sep : TEXT) : TEXT =
  VAR 
    iter := s.iterate();
    t : TEXT;
    res, h := "";
  BEGIN
    WHILE iter.next(t) DO
      res := res & h & t;
      h := sep
    END;
    RETURN res
  END Infixize;

CONST
  F = Fmt.F;
  I = FmtInt;
  II = Fmt.Int;

PROCEDURE FmtInt(i : INTEGER) : TEXT =
  BEGIN
    RETURN F("%04s", Fmt.Int(i))
  END FmtInt;

PROCEDURE Arrayize(nam : TEXT; s : IntSet.T) : TextSet.T =
  VAR
    t := NEW(TextSetDef.T).init();
    i : INTEGER;
  BEGIN
    WITH iter = s.iterate() DO
      WHILE iter.next(i) DO
        EVAL t.insert(F("%s[%s]", nam, I(i)))
      END
    END;
    RETURN t
  END Arrayize;

PROCEDURE TextSingleton(s : TextSet.T) : TEXT =
  VAR t : TEXT;
  BEGIN
    <*ASSERT s.size() = 1 *>
    WITH iter = s.iterate() DO
      EVAL iter.next(t); RETURN t
    END
  END TextSingleton;

PROCEDURE EmitGateSyntactic(ct      : CT; 
                            inputs  : TextSet.T;
                            output  : TEXT;
                            comment : TEXT := "") =
  CONST
    Op = ARRAY CT OF TEXT { "^", "&", "|", NIL };
    Px = ARRAY CT OF TEXT { "",  "!", "!", "!" };
  BEGIN
    IF NOT TE(comment, "") THEN comment := "// " & comment END;
    Emit(Stream.Assign,
         F("assign %s = %s(%s); %s",
           output, Px[ct], Infixize(inputs, Op[ct]), comment));
    INC(gc)
  END EmitGateSyntactic;
                            

PROCEDURE Alphabetize(s : TextSet.T) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();
    iter := s.iterate();
    l : [ 'A'..'Z' ] := 'A';
    t : TEXT;
  BEGIN
    WHILE iter.next(t) DO
      EVAL res.insert( F(".%s(%s)",
                         Text.FromChar(l),
                         t) );
      INC(l)
    END;
    RETURN res
  END Alphabetize;

PROCEDURE EmitGateStructural(ct      : CT; 
                             inputs  : TextSet.T; 
                             output  : TEXT;
                             comment : TEXT := "") =
  BEGIN
    IF NOT TE(comment, "") THEN comment := "// " & comment END;
    
    IF ct = CT.Xor AND inputs.size() = 5 THEN
      (* our library doesnt contain xor5 so use xor6 instead... *)
      WITH set = inputs.union(Set("'0")) DO
        EmitGateStructural(ct, set, output, comment);
        RETURN
      END
    END;
    
    WITH type = CellType(ct, inputs.size()) DO
      Emit(Stream.Instance,
           F("%s u_%s_%s (.Q(%s), %s);",
           type, type, I(gc), output, Commatize(Alphabetize(inputs)))
      & comment
      );
      INC(gc)
    END
  END EmitGateStructural;

PROCEDURE EmitComment(txt : TEXT) =
  BEGIN
    IF    EmitGate = EmitGateStructural THEN
      Emit(Stream.Instance, "// " & txt)
    ELSIF EmitGate = EmitGateSyntactic THEN
      Emit(Stream.Assign, "// " & txt)
    ELSE
      <*ASSERT FALSE*>
    END
  END EmitComment;

TYPE CT = { Xor, Nand, Nor, Inv };
CONST CTNames = ARRAY CT OF TEXT { "xor", "nand", "nor", "inv" };

VAR EmitGate : PROCEDURE (ct      : CT; 
                          inputs  : TextSet.T; 
                          output  : TEXT;
                          comment : TEXT := "") 
  := EmitGateStructural;

PROCEDURE CellType(ct : CT; fanin : CARDINAL) : TEXT =
  BEGIN
    IF (ct = CT.Nand OR ct = CT.Nor) AND fanin = 1 THEN ct := CT.Inv END;

    CASE ct OF 
      CT.Inv =>
      <*ASSERT fanin=1*>
      RETURN F("a28_%s_a1", CTNames[ct])
    ELSE
      RETURN F("a28_%s%s_a1", CTNames[ct], II(fanin))
    END
  END CellType;

PROCEDURE EmitXorGate(sources : IntSet.T; inputs : TextSet.T; comment := "") =
  BEGIN
    <* ASSERT inputs.size() > 0 AND inputs.size() <= MaxFanin *>

    WITH output = "xor" & I(gc) DO
      Declare(output);
      IF inputs.size() = 1 THEN
        Emit(Stream.Assign,
             F("assign %s = %s;",
               output, TextSingleton(inputs)));
        INC(gc);
      ELSE
        EmitGate(CT.Xor, inputs, output, comment);
      END;
      inputGates := RefList.Cons(NEW(XorGate, 
                                     sources := sources.copy(), 
                                     output  := output,
                                     inputs  := inputs),
                                 inputGates)
    END
  END EmitXorGate;

PROCEDURE EmitInputXorGate(bits : IntSet.T) =
  BEGIN
    EmitXorGate(bits, Arrayize("input_data", bits), "EmitInputXorGate")
  END EmitInputXorGate;

PROCEDURE EmitFirst(VAR bits : IntSet.T; bit : CARDINAL) =
  BEGIN
    EVAL bits.insert(bit);
    
    IF bits.size() = MaxFanin THEN
      EmitPush(bits)
    END
  END EmitFirst;

PROCEDURE EmitPush(VAR bits : IntSet.T) =
  BEGIN
    IF bits.size() > 0 THEN
      EmitInputXorGate(bits);
      bits := NEW(IntSetDef.T).init();
    END
  END EmitPush;

(**********************************************************************)

(* introduce a special type because the original code was written with
   a statically sized array from -1 to an upper limit, and that is not
   possible with standard dynamically sized arrays in Modula-3. *)
TYPE
  XorGateArray = OBJECT
    seq : RefSeq.T;
  METHODS
    init() : XorGateArray               := XGAInit;
    get(at : ABitPos) : XorGate         := XGAGet;
    set(at : ABitPos; gate : XorGate)   := XGASet;
    first() : ABitPos                   := XGAFirst;
    last() : ABitPos                    := XGALast;
  END;

PROCEDURE XGAFirst(<*UNUSED*>xga : XorGateArray) : ABitPos =
  BEGIN RETURN FIRST(ABitPos) END XGAFirst;

PROCEDURE XGALast(xga : XorGateArray) : ABitPos =
  VAR lastidx := xga.seq.size() - 1;
  BEGIN
    RETURN lastidx + FIRST(ABitPos)
  END XGALast;

PROCEDURE XGAInit(xga : XorGateArray) : XorGateArray =
  BEGIN
    xga.seq := NEW(RefSeq.T).init(); RETURN xga
  END XGAInit;

PROCEDURE XGAGet(xga : XorGateArray; at : ABitPos) : XorGate =
  BEGIN
    RETURN xga.seq.get(at - FIRST(ABitPos))
  END XGAGet;

PROCEDURE XGASet(xga : XorGateArray; at : ABitPos; gate : XorGate) =
  VAR
    idx := at - FIRST(ABitPos);
  BEGIN
    WHILE idx > xga.seq.size()-1 DO xga.seq.addhi(NIL) END;
    xga.seq.put(idx, gate)
  END XGASet;

(**********************************************************************)

PROCEDURE BuildXorTrees(mode : Mode) : XorGateArray =
  CONST
    FirstLevels = 3;
  VAR
    res := NEW(XorGateArray).init();
    bits : IntSet.T := NEW(IntSetDef.T).init();
  BEGIN
    EmitComment("BuildXorTrees");
    FOR f := -1 TO FirstLevels - 1 DO
      EmitComment("BuildXorTrees, FirstLevel, f = " & II(f));
      FOR i := 0 TO Bits-1 DO
        IF IsBitSet(i, f) THEN EmitFirst(bits, i) END
      END;
      EmitPush(bits)
    END;
    
    FOR i := 0 TO ParityBits-1 DO
      EmitComment("BuildXorTrees, ParityBits, i = " & II(i));
      WITH tree = BuildParityTree(i) DO
        Debug.Out(F("setting res[%s] to %s", II(i), tree.output));
        res.set(i,tree)
      END
    END;
    
    EmitComment("BuildXorTrees, BuildCheckBit");
    res.set(-1,BuildCheckBit(mode)); (* bit "zero" *)
    <*ASSERT bits.size() = 0*>
    RETURN res
  END BuildXorTrees;

PROCEDURE MakeXorFrom(seq : RefSeq.T; start, limit : CARDINAL) =
  VAR
    inputs := NEW(TextSetDef.T).init();
    sources := NEW(IntSetDef.T).init();
  BEGIN
    FOR i := start TO limit-1 DO
      WITH g = NARROW(seq.get(i), XorGate) DO
        sources := sources.union(g.sources);
        EVAL inputs.insert(g.output)
      END
    END;
    EmitXorGate(sources, inputs, "MakeXorFrom")
  END MakeXorFrom;

PROCEDURE SortSeq(VAR seq : RefSeq.T) =
  VAR
    a := NEW(REF ARRAY OF REFANY, seq.size());
  BEGIN
    FOR i := 0 TO seq.size()-1 DO
      a[i] := seq.get(i)
    END;
    RefArraySort.Sort(a^, CompareGateIndex);
    FOR i := 0 TO seq.size()-1 DO
      seq.put(i, a[i])
    END;
  END SortSeq;

PROCEDURE CompareGateIndex(a, b : REFANY) : [-1..1] =
  BEGIN
    WITH ag = NARROW(a, XorGate), bg = NARROW(b, XorGate) DO
      RETURN Text.Compare(ag.output, bg.output)
    END
  END CompareGateIndex;

PROCEDURE FormatSet(s : IntSet.T) : TEXT =
  <*FATAL Wr.Failure*>
  VAR wr := TextWr.New();
      iter := s.iterate();
      a := NEW(REF ARRAY OF INTEGER, s.size());
      p := 0;
      i : INTEGER;
  BEGIN
    WHILE iter.next(i) DO a[p] := i; INC(p) END;
    IntArraySort.Sort(a^);
    FOR i := FIRST(a^) TO LAST(a^) DO
      Wr.PutText(wr, II(a[i]));
      Wr.PutChar(wr, ' ');
    END;
    RETURN TextWr.ToText(wr)
  END FormatSet;

PROCEDURE BuildFromWanted(wanted : IntSet.T;
                          okToUseInputs : BOOLEAN) : XorGate =

  PROCEDURE ScanWanted() =
    BEGIN
      wanted := sources.copy();
      buildfrom := NEW(RefSeq.T).init();

      (* search for existing xors *)
      VAR
        p := inputGates;
      BEGIN
        WHILE p # NIL DO
          WITH xor = NARROW(p.head,XorGate) DO
            IF xor.sources.subset(wanted) THEN
              Debug.Out(F("tree want input %s",
                       xor.output));
              
              wanted := wanted.diff(xor.sources);
              buildfrom.addhi(xor)
            END
          END;
          p := p.tail
        END
      END;
    END ScanWanted;

  PROCEDURE FillInGaps() =
    VAR
      bits : IntSet.T := NEW(IntSetDef.T).init();
    BEGIN
      <*ASSERT bits.size() = 0*>
      FOR i := 0 TO Bits-1 DO
        IF wanted.member(i) THEN EmitFirst(bits, i) END
      END;
      EmitPush(bits)
    END FillInGaps;

  VAR
    buildfrom : RefSeq.T;
    sources: IntSet.T;

  BEGIN
    Debug.Out("BuildFromWanted: " & FormatSet(wanted));

    sources := wanted.copy();

    REPEAT
      ScanWanted();

      IF okToUseInputs AND wanted.size() # 0 THEN
        FillInGaps();
        ScanWanted()
      END;
      <*ASSERT wanted.size() = 0*>

      Debug.Out("buildfrom.size()=" & II(buildfrom.size()));

      IF buildfrom.size() > 1 THEN
        SortSeq(buildfrom);
        FOR i := 0 TO buildfrom.size()-1 BY MaxFanin DO
          MakeXorFrom(buildfrom, i, MIN(buildfrom.size(), i+MaxFanin))
        END
      END;

    UNTIL buildfrom.size() = 1;
    RETURN buildfrom.get(0)
  END BuildFromWanted;

PROCEDURE BuildParityTree(pow : CARDINAL) : XorGate =
  VAR
    wanted := NEW(IntSetDef.T).init();
  BEGIN
    Debug.Out("***** building parity tree at level " & II(pow));
    FOR i := 0 TO Bits-1 DO
      IF IsBitSet(i, pow) THEN EVAL wanted.insert(i) END
    END;
    RETURN BuildFromWanted(wanted,TRUE)
  END BuildParityTree;

PROCEDURE BuildCheckBit(mode : Mode) : XorGate =
  VAR
    wanted := NEW(IntSetDef.T).init();
  BEGIN
    Debug.Out("***** building inverse max-parity tree");
    EmitComment("BuildCheckBit: building inverse max-parity tree");
    FOR i := 0 TO Bits-1 DO
      IF NOT IsBitSet(i, ParityBits-1) THEN EVAL wanted.insert(i) END
    END;
    EVAL BuildFromWanted(wanted,TRUE);

    Debug.Out("***** building check-bit tree");
    EmitComment("BuildCheckBit: building check-bit tree");
    wanted := NEW(IntSetDef.T).init();
    FOR i := 0 TO Bits-1 DO
      CASE mode OF
        Mode.Read  =>    EVAL wanted.insert(i)
      |
        Mode.Write =>
        (* this is a bit tricky:
       
           in the encoder, the parity check-bits are included in the result.
           therefore they need to be accounted for when inserting bits.
           We do this by counting how many times a bit appears in the parity
           check bits.  It appears as many times as there are bits set in its
           index, union itself (parity-check bits appear precisely once but are
           zero on input... so the formula still works out by checking for bits
           that have an even number of bits set (except bit zero)).
        *)
        IF i # 0 AND NumBitsSet(i) MOD 2 = 0 THEN
          EVAL wanted.insert(i)
        END
      END
    END;
    RETURN BuildFromWanted(wanted,TRUE);
  END BuildCheckBit;

PROCEDURE Set(t0, t1, t2, t3, t4, t5, t6 : TEXT := NIL) : TextSet.T = 
  (* make a fresh text set from a limited number of TEXTs *)
  BEGIN
    WITH s = NEW(TextSetDef.T).init() DO

      PROCEDURE P(t : TEXT) = BEGIN IF t # NIL THEN EVAL s.insert(t) END END P;

      BEGIN
        P(t0); P(t1); P(t2); P(t3); P(t4); P(t5); P(t6)
      END;
      RETURN s
    END
  END Set;

PROCEDURE BuildSyndrome() =
  BEGIN
    FOR i := 0 TO MaxBitPos DO
      WITH nname = F("syn_%s_n", I(i)),
           pname = F("syn_%s", I(i)) DO
        Declare(nname); Declare(pname);
        IF bufferSyndrome THEN
          EmitGate(CT.Inv, Set(F("ecc_parity[%s]", II(i))), nname);
          EmitGate(CT.Inv, Set(nname), pname)
        ELSE
          Emit(Stream.Assign, F("assign %s = ecc_parity[%s];", pname, II(i)));
          EmitGate(CT.Inv, Set(pname), nname)
        END
      END
    END
  END BuildSyndrome;

VAR nandList : RefList.T := NIL;

PROCEDURE GenerateNands(start, width : CARDINAL) =
  VAR
    ctr := NEW(REF ARRAY OF CARDINAL, width);

  PROCEDURE Increment() : BOOLEAN = 
    BEGIN
      FOR i := FIRST(ctr^) TO LAST(ctr^) DO
        IF ctr[i] = 1 THEN 
          ctr[i] := 0;
          IF i = LAST(ctr^) THEN RETURN TRUE END
        ELSE
          ctr[i] := 1;
          RETURN FALSE
        END
      END;
      <*ASSERT FALSE*>
    END Increment;

  PROCEDURE PrintNand() =
    VAR 
      s := NEW(TextSetDef.T).init();
      t : TEXT;
    BEGIN
      FOR i := 0 TO width-1 DO
        t := "syn_" & I(i + start);
        IF ctr[i] = 0 THEN t := t & "_n" END;
        EVAL s.insert(t)
      END;
      WITH output = "nand" & I(gc) DO
        
        Declare(output);
        EmitGate(CT.Nand, s, output);
        nandList := RefList.Cons(NEW(NandGate, 
                                     inputs := s.copy(), 
                                     output := output),
                                 nandList)
      END
    END PrintNand;

  BEGIN
    FOR i := FIRST(ctr^) TO LAST(ctr^) DO ctr[i] := 0 END;

    REPEAT PrintNand() UNTIL Increment()
  END GenerateNands;

PROCEDURE BuildNandArray() =
  BEGIN
    FOR i := 0 TO MaxBitPos BY NandWidth DO
      GenerateNands(i, MIN(NandWidth,MaxBitPos-i+1))
    END
  END BuildNandArray;

PROCEDURE BuildNor(b : CARDINAL) =
  VAR
    s := NEW(TextSetDef.T).init();
    t : TEXT;
    r := NEW(TextSetDef.T).init();
    p := nandList;
  BEGIN
    FOR i := 0 TO MaxBitPos DO
      t := "syn_" & I(i);
      IF NOT IsBitSet(b, i) THEN
        t := t & "_n"
      END;
      EVAL s.insert(t)
    END;

    WHILE p # NIL DO
      WITH nand = NARROW(p.head, NandGate) DO
        IF nand.inputs.subset(s) THEN
          EVAL r.insert(nand.output);
          s := s.diff(nand.inputs)
        END
      END;
      p := p.tail
    END;

    <*ASSERT s.size() = 0 *>
    WITH output = "nor" & I(gc),
         inv  = F("invert%s", I(b)) DO
      Declare(output);
      Declare(inv);
      EmitGate(CT.Nor, r, output);

      Emit(Stream.Assign,
           F("assign %s = %s;", inv, output));
    END
  END BuildNor;

PROCEDURE BuildNorArray() =
  BEGIN
    FOR i := 0 TO Bits-1 DO BuildNor(i) END
  END BuildNorArray;

PROCEDURE BuildOutputCorrector() =
  BEGIN
    FOR i := 0 TO Bits-1 DO
      EmitGate(
          CT.Xor,
          Set(F("input_data[%s]",II(i)),F("invert%s",I(i))),
          F("corrected[%s]",II(i)));
    END
  END BuildOutputCorrector;

PROCEDURE BuildErrBitLogic() =
  BEGIN
    Emit(Stream.Assign,
         F("assign o_err_detect = |(ecc_parity[%s-1:-1]);", II(ParityBits)));
    Emit(Stream.Assign,
         "assign o_err_multpl = o_err_detect & !ecc_parity[-1];");
  END BuildErrBitLogic;

PROCEDURE BuildAssignReadIOs() =
  VAR
    cc, dc := 0;
  BEGIN
    DeclareArray("input_data" , 0, Bits-1);
    DeclareArray("corrected"  , 0, Bits-1);

    FOR i := 0 TO Bits-1 DO
      IF NumBitsSet(i) <= 1 THEN
        Emit(Stream.Assign,
             F("assign input_data[%s] = i_chk    [%s];", II(i), II(cc)));
        INC(cc)
      ELSE
        Emit(Stream.Assign,
             F("assign input_data[%s] = i_data   [%s];", II(i), II(dc)));
        Emit(Stream.Assign,
             F("assign o_data    [%s] = corrected[%s];", II(dc), II(i)));
        INC(dc)
      END
    END;

  END BuildAssignReadIOs;

PROCEDURE BuildAssignWriteIOs() =
  VAR
    cc, dc := 0;
  BEGIN
    DeclareArray("input_data" , 0, Bits-1);

    FOR i := 0 TO Bits-1 DO
      IF NumBitsSet(i) <= 1 THEN
        Emit(Stream.Assign,
             F("assign input_data[%s] = '0;", II(i)));
        Emit(Stream.Assign,
             F("assign o_chk     [%s] = ecc_parity[%s-1];", II(cc), II(cc)));
        INC(cc)
      ELSE
        Emit(Stream.Assign,
             F("assign input_data[%s] = i_data   [%s];", II(i), II(dc)));
        Emit(Stream.Assign,
             F("assign o_data    [%s] = i_data   [%s];", II(dc), II(dc)));
        INC(dc)
      END
    END
  END BuildAssignWriteIOs;

PROCEDURE WriteProlog(mode : Mode) =

  PROCEDURE Put(txt : TEXT) =
    BEGIN Emit(Stream.Prolog, txt) END Put;

  BEGIN
    Put(F("module %s", mn));
    Put(  "  (");
    Put(F("input  logic [%s-1:0] i_data,",II(Bits-ParityBits-1) ));
    IF mode = Mode.Read THEN
      Put(F("input  logic [%s-1:0] i_chk," ,II(ParityBits+1) ));
    END;
    Put(  "");
    IF mode = Mode.Read THEN
      Put(  "output logic          o_err_detect,");
      Put(  "output logic          o_err_multpl,");
    END;
    Put(F("output logic [%s-1:0] o_data",II(Bits-ParityBits-1) ));
    IF mode = Mode.Write THEN
      Put(F(",output logic [%s-1:0] o_chk"  ,II(ParityBits+1) ));
    END;
    Put(  "  );");
  END WriteProlog;

PROCEDURE WriteEpilog() =
  BEGIN Emit(Stream.Epilog, "endmodule") END WriteEpilog;

TYPE
  Mode = { Read, Write };

PROCEDURE InitData() =
  BEGIN
    (* if I used proper software engineering practice, this would not be
       necessary, but since this started life as a quick and dirty program... *)
    inputGates := NIL;
    gc         := 0;
    nandList   := NIL
  END InitData;

PROCEDURE BuildOne() RAISES { OSError.E, Wr.Failure } =
  BEGIN
    InitStreams();
    InitData();

    WITH pb = BuildXorTrees(mode) (* used for reads and writes *) DO
      
      DeclareArray("ecc_parity", pb.first(), pb.last());
      FOR i := pb.first() TO pb.last() DO
        Emit(Stream.Assign,
             F("assign ecc_parity[%s] = %s;", II(i), pb.get(i).output))
      END
    END;
    
    CASE mode OF 
      Mode.Read  => BuildAssignReadIOs()
    |
      Mode.Write => BuildAssignWriteIOs()
    END;
    
    IF mode = Mode.Read THEN
      BuildSyndrome();
      BuildNandArray();
      BuildNorArray();
      BuildOutputCorrector();
      BuildErrBitLogic();
    END;
    
    WriteProlog(mode);
    WriteEpilog();
    
    DumpStreams(ofn)
  END BuildOne;

PROCEDURE WriteSelectorFiles(dir    : Pathname.T;
                             pfx    : ARRAY Mode OF TEXT;
                             fb, tb : CARDINAL) 
  RAISES { Wr.Failure, OSError.E } =
  PROCEDURE P(txt : TEXT) =
    BEGIN Emit(Stream.Prolog, txt) END P;

  PROCEDURE E(txt : TEXT) =
    BEGIN Emit(Stream.Epilog, txt) END E;

  PROCEDURE N(txt : TEXT) =
    BEGIN Emit(Stream.Instance, txt) END N;

  PROCEDURE WriteRead() RAISES { Wr.Failure, OSError.E } =
    VAR
      m := Mode.Read;
    BEGIN
      InitStreams();
      P(F("module %s #(", pfx[m]));
      P(  "  WDTH = 34,");
      P(  "  CBTS =  7");
      P(  ")  (");
      P(  "input  logic [WDTH-1:0] i_data,");
      P(  "input  logic [CBTS-1:0] i_chk,");
      P(  "output logic            o_err_detect,");
      P(  "output logic            o_err_multpl,");
      P(  "output logic [WDTH-1:0] o_data");
      P(  ");");
      N(  "generate");

      FOR w := fb TO tb DO
        VAR 
          e : TEXT;
        BEGIN
          IF w = fb THEN e := "" ELSE e := "else " END;
          WITH mn = pfx[m] & "_" & II(w) DO
            N(F("  %sif (WDTH == %s)", e, II(w)));
            N(F("    %s u_%s", mn, mn));
            N(  "    (.i_data(i_data), .i_chk(i_chk),");
            N(  "     .o_err_detect(o_err_detect), .o_err_multpl(o_err_multpl), ");
            N(  "     .o_data(o_data));")
          END
        END
      END;
      N(  " else __I_am_a_gObBlEdYgo0k_SynTaX_err0R__ u_erR0r();");

      N(  "endgenerate");
      E(  "endmodule");

      WITH fn = dir & "/" & pfx[m] & ".sv" DO
        DumpStreams(fn)
      END
    END WriteRead;

  PROCEDURE WriteWrite() RAISES { Wr.Failure, OSError.E } =
    VAR
      m := Mode.Write;
    BEGIN
      InitStreams();
      P(F("module %s #(", pfx[m]));
      P(  "  WDTH = 34,");
      P(  "  CBTS =  7");
      P(  ")  (");
      P(  "input  logic [WDTH-1:0] i_data,");
      P(  "output logic [WDTH-1:0] o_data,");
      P(  "output logic [CBTS-1:0] o_chk");
      P(  ");");
      N(  "generate");

      FOR w := fb TO tb DO
        VAR 
          e : TEXT;
        BEGIN
          IF w = fb THEN e := "" ELSE e := "else " END;
          WITH mn = pfx[m] & "_" & II(w) DO
            N(F("  %sif (WDTH == %s)", e, II(w)));
            N(F("    %s u_%s", mn, mn));
            N(  "    (.i_data(i_data),");
            N(  "     .o_data(o_data),");
            N(  "     .o_chk(o_chk));")
          END
        END
      END;
      N(  " else __I_am_a_gObBlEdYgo0k_SynTaX_err0R__ u_erR0r();");
      N(  "endgenerate");
      E(  "endmodule");

      WITH fn = dir & "/" & pfx[m] & ".sv" DO
        DumpStreams(fn)
      END
    END WriteWrite;

  BEGIN
    WriteRead();
    WriteWrite()
  END WriteSelectorFiles;

VAR
  pp                         := NEW(ParseParams.T).init(Stdio.stderr);
  mn : TEXT;
  ofn : Pathname.T           := NIL;
  mode                       := Mode.Read;

  MaxFanin                   :=  4;
  NandWidth                  :=  3;
  sizeFound                  := FALSE;
  defFn : Pathname.T         := NIL;
  bufferSyndrome             := FALSE;
  range                      := FALSE;
  fromBits, toBits : CARDINAL;
  targetDir : Pathname.T;
  quiet                      := FALSE;
BEGIN
  TRY
    bufferSyndrome := pp.keywordPresent("-buffersyndrome");

    IF pp.keywordPresent("-structural") THEN
      EmitGate := EmitGateStructural
    END;

    IF pp.keywordPresent("-syntactic") THEN
      EmitGate := EmitGateSyntactic
    END;

    IF pp.keywordPresent("-r") OR pp.keywordPresent("-read") THEN
      mode := Mode.Read
    END;
    IF pp.keywordPresent("-w") OR pp.keywordPresent("-write") THEN
      mode := Mode.Write
    END;
    IF pp.keywordPresent("-defs") THEN
      defFn := pp.getNext()
    END;
    IF pp.keywordPresent("-m") THEN
      mn := pp.getNext();
      ofn := mn & ".sv"
    END;

    IF pp.keywordPresent("-maxfanin") THEN
      MaxFanin := pp.getNextInt()
    END;

    IF pp.keywordPresent("-nandwidth") THEN
      NandWidth := pp.getNextInt()
    END;

    IF pp.keywordPresent("-databits") OR pp.keywordPresent("-d") THEN
      DataBits := pp.getNextInt();
      SeekSize(DataBits, Bits, ParityBits, MaxBitPos);
      sizeFound := TRUE
    END;

    IF pp.keywordPresent("-range") THEN
      range := TRUE;
      fromBits  := pp.getNextInt();
      toBits    := pp.getNextInt();
      targetDir := pp.getNext()
    END;

    quiet := pp.keywordPresent("-q") OR pp.keywordPresent("-quiet");

    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF range THEN
    CONST
      Pfx = ARRAY Mode OF TEXT { "read_ecc", "write_ecc" };
    BEGIN
      FOR d := fromBits TO toBits DO
        FOR m := FIRST(Mode) TO LAST(Mode) DO
          mn := Pfx[m] & "_" & II(d);
          ofn := targetDir & "/" & mn & ".sv";
          DataBits := d;
          SeekSize(DataBits, Bits, ParityBits, MaxBitPos);
          mode := m;
          BuildOne()
        END
      END;

      WriteSelectorFiles(targetDir, Pfx, fromBits, toBits);
    END

  ELSE
    IF NOT sizeFound THEN Debug.Error("Must state data bits -d") END;

    IF ofn = NIL THEN Debug.Error("Must state module name -m") END;
    BuildOne()
  END;

END Main.
