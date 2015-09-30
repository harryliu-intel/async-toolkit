MODULE Main;

(* 
   build ECC logic for SRAMS.

   Author: Mika Nystrom <mika@alum.mit.edu>
   
   May, 2013
 *)

IMPORT Word;
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
IMPORT TextWr, FileWr, Wr, OSError, AL;

CONST Bits        = 64;
CONST ParityBits  =  6;
CONST MaxFanin    =  4;
CONST NandWidth   =  3;

CONST TE = Text.Equal;

TYPE BitPos = [-1..ParityBits-1];

(* the order in which things are built is very very tricky! *)

TYPE Stream = { Prolog, Declare, Assign, Instance, Epilog };

VAR streams : ARRAY Stream OF TextWr.T;

PROCEDURE InitStreams() = 
  BEGIN 
    FOR i := FIRST(streams) TO LAST(streams) DO streams[i] := TextWr.New() END 
  END InitStreams;

PROCEDURE DumpStreams() =
  BEGIN
    WITH wr = FileWr.Open(ofn) DO
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
                   bit : BitPos) : BOOLEAN =
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
  VAR 
    iter := s.iterate();
    t : TEXT;
    res, h := "";
  BEGIN
    WHILE iter.next(t) DO
      res := res & h & t;
      h := ", "
    END;
    RETURN res
  END Commatize;

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

PROCEDURE EmitGate(ct      : CT; 
                   inputs  : TextSet.T; 
                   output  : TEXT;
                   comment : TEXT := "") =
  BEGIN
    IF NOT TE(comment, "") THEN comment := "// " & comment END;
    WITH type = CellType(ct, inputs.size()) DO
      Emit(Stream.Instance,
           F("%s u_%s_%s (%s, %s);",
           type, type, I(gc), output, Commatize(inputs))
      & comment
      );
      INC(gc)
    END
  END EmitGate;

TYPE CT = { Xor, Nand, Nor, Inv };
CONST CTNames = ARRAY CT OF TEXT { "xor", "nand", "nor", "inv" };

PROCEDURE CellType(ct : CT; fanin : CARDINAL) : TEXT =
  BEGIN
    CASE ct OF 
      CT.Inv =>
      <*ASSERT fanin=1*>
      RETURN F("a28_%s_a1", CTNames[ct])
    ELSE
      RETURN F("a28_%s%s_a1", CTNames[ct], II(fanin))
    END
  END CellType;

PROCEDURE EmitXorGate(sources : IntSet.T; inputs : TextSet.T) =
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
        EmitGate(CT.Xor, inputs, output);
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
    EmitXorGate(bits, Arrayize("input_data", bits))
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

PROCEDURE BuildXorTrees(mode : Mode) : ARRAY BitPos OF XorGate = 
  CONST
    FirstLevels = 3;
  VAR
    res : ARRAY BitPos OF XorGate;
    bits : IntSet.T := NEW(IntSetDef.T).init();
  BEGIN
    FOR f := -1 TO FirstLevels - 1 DO
      FOR i := 0 TO Bits-1 DO
        IF IsBitSet(i, f) THEN EmitFirst(bits, i) END
      END;
      EmitPush(bits)
    END;
    
    FOR i := 0 TO ParityBits-1 DO
      res[i] := BuildParityTree(i)
    END;
    
    res[-1] := BuildCheckBit(mode); (* bit "zero" *)
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
    EmitXorGate(sources, inputs)
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
    RETURN inputGates.head
  END BuildFromWanted;

PROCEDURE BuildParityTree(pow : [0..ParityBits-1]) : XorGate =
  VAR
    wanted := NEW(IntSetDef.T).init();
  BEGIN
    Debug.Out("***** building parity tree at level " & II(pow));
    FOR i := 0 TO Bits-1 DO
      IF IsBitSet(i, pow) THEN EVAL wanted.insert(i) END
    END;
    RETURN BuildFromWanted(wanted,FALSE)
  END BuildParityTree;

PROCEDURE BuildCheckBit(mode : Mode) : XorGate =
  VAR
    wanted := NEW(IntSetDef.T).init();
  BEGIN
    Debug.Out("***** building inverse max-parity tree");
    FOR i := 0 TO Bits-1 DO
      IF NOT IsBitSet(i, ParityBits-1) THEN EVAL wanted.insert(i) END
    END;
    EVAL BuildFromWanted(wanted,FALSE);

    Debug.Out("***** building check-bit tree");
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
    FOR i := 0 TO LAST(BitPos) DO
      WITH nname = F("syn_%s_n", I(i)),
           pname = F("syn_%s", I(i)) DO
        Declare(nname); Declare(pname);
        EmitGate(CT.Inv, Set(F("ecc_parity[%s]", II(i))), nname);
        EmitGate(CT.Inv, Set(nname), pname)
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

    REPEAT
      PrintNand();
    UNTIL Increment()
  END GenerateNands;

PROCEDURE BuildNandArray() =
  BEGIN
    FOR i := 0 TO LAST(BitPos) BY NandWidth DO
      GenerateNands(i, NandWidth)
    END
  END BuildNandArray;

PROCEDURE BuildNor(b : CARDINAL) =
  VAR
    s := NEW(TextSetDef.T).init();
    t : TEXT;
    r := NEW(TextSetDef.T).init();
    p := nandList;
  BEGIN
    FOR i := 0 TO LAST(BitPos) DO
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
    WITH output = "nor" & I(gc) DO
      Declare(output);
      EmitGate(CT.Nor, r, output);
      Emit(Stream.Assign,
           F("assign invert%s = %s;", I(b), output));
    END
  END BuildNor;

PROCEDURE BuildNorArray() =
  BEGIN
    FOR i := 0 TO Bits-1 DO
      BuildNor(i)
    END
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

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  mn : TEXT;
  ofn : Pathname.T := NIL;
  mode := Mode.Read;
BEGIN
  InitStreams();

  TRY
    IF pp.keywordPresent("-w") OR pp.keywordPresent("-write") THEN
      mode := Mode.Write
    END;
    IF pp.keywordPresent("-m") THEN
      mn := pp.getNext();
      ofn := mn & ".sv"
    END;
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF ofn = NIL THEN Debug.Error("Must state module name -m") END;

  WITH pb = BuildXorTrees(mode) (* used for reads and writes *) DO

    DeclareArray("ecc_parity", FIRST(pb), LAST(pb));
    FOR i := FIRST(pb) TO LAST(pb) DO
      Emit(Stream.Assign,
           F("assign ecc_parity[%s] = %s;", II(i), pb[i].output))
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
  
  DumpStreams()
END Main.
