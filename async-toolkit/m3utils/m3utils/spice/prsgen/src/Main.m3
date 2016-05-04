MODULE Main;

(* 
   prsgen program

   waveform generator for PRESTO from
   simple digital file description
*)

(* Author : mika.nystroem@intel.com *)
(*          May 3, 2016             *)

IMPORT FileRd;
IMPORT ParseParams, Stdio;
IMPORT TextLongRealTbl AS TextLRTbl;
FROM Fmt IMPORT Int, F, LongReal;
IMPORT Rd, TextReader, Text, Scan;
IMPORT Thread;
IMPORT TextList;
IMPORT FileWr;
IMPORT Debug;
IMPORT CardSeq;
IMPORT RefSeq;
IMPORT Lex, FloatMode, OSError;
IMPORT AL;
IMPORT Wx;
IMPORT TextUtils;
IMPORT BigInt;
IMPORT CharSeq;
IMPORT IntList;
IMPORT Wr;

IMPORT LongRealPair    AS LRPair;
IMPORT LongRealPairSeq AS LRPairSeq;
IMPORT LongRealSeq     AS LRSeq;

IMPORT Fmt;
IMPORT Pathname;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

EXCEPTION Syntax(TEXT);

TYPE
  Struct = RefSeq.T;

  Array  = OBJECT lo, hi : CARDINAL END;

PROCEDURE Tok(l : TextList.T; n : CARDINAL) : TEXT RAISES { Syntax } =
  BEGIN
    IF n >= TextList.Length(l) THEN
      RAISE Syntax("not enough tokens")
    ELSE
      RETURN TextList.Nth(l, n)
    END
  END Tok;

PROCEDURE RemToks(l : TextList.T; n : CARDINAL) : TEXT RAISES { Syntax } =
  VAR
    res := "";
  BEGIN
    WHILE n > 0 DO
      IF l.tail = NIL THEN
        RAISE Syntax("not enough tokens")
      END;
      l := l.tail;
      DEC(n);
    END;
    WHILE l # NIL DO
      res := res & l.head;
      l := l.tail
    END;
    RETURN res
  END RemToks;

PROCEDURE ParseNames(token : TEXT) : Struct RAISES { Syntax } =
  VAR
    wx := Wx.New();
    p := 0;
    res := NEW(Struct).init();
    dim := 0;
    s, t : [-1..LAST(CARDINAL)];
  BEGIN
    Debug.Out(F("Parsing names \"%s\"", token));
    
    LOOP
      s := Text.FindChar(token, '[', p);
      IF s = -1 THEN
        Wx.PutText(wx, Text.Sub(token, p, LAST(CARDINAL)));
        res.addhi(Wx.ToText(wx));
        Debug.Out(F("ParseNames tmpl \"%s\"",res.get(res.size()-1)));
        RETURN res
      ELSE
        Wx.PutText(wx, Text.Sub(token, p, s-p))
      END;
      t := Text.FindChar(token, ']', s);

      IF t = -1 THEN
        RAISE Syntax("mismatched brackets in \"" & token & "\"")
      END;

      (* found beginning and end of array indexing
         [ is at s
         ] is at t 

         now extract from s+1 to t-1.

         ex.
         [1]
         s=0
         t=2
         begin = s+1 = 1
         lim   = t   = 2
         len   = lim-begin = t-(s+1) = t-s-1
      *)

      WITH b = s+1,
           e = t-s-1,
           aSpec = Text.Sub(token, b, e) DO
        Debug.Out(F("parsing array spec (sub(\"%s\",%s,%s)) \"%s\"",
                    token,
                    Int(b),
                    Int(e),
                    aSpec));
        WITH arr = ParseRange(aSpec) DO
          res.addhi(arr);
          Debug.Out(F("ParseNames [%s:%s]", Int(arr.lo), Int(arr.hi)))
        END
      END;
      
      Wx.PutText(wx, F("[$%s]",Int(dim)));
      INC(dim);
      p := t+1
    END
  END ParseNames;

PROCEDURE ParseRange(txt : TEXT) : Array RAISES { Syntax } =
  BEGIN
    txt := TextUtils.Replace(txt, " ", "");
    txt := TextUtils.Replace(txt, "\t", "");

    TRY
      WITH cp = Text.FindChar(txt, ':') DO
        IF cp = -1 THEN
          WITH z = Scan.Int(txt) DO
            RETURN NEW(Array, lo := z, hi := z)
          END
        END;
        WITH base = Scan.Int(Text.Sub(txt, 0, cp)) DO
          IF Text.GetChar(txt, cp+1) = '+' THEN
            (* 10+:10 syntax *)
            WITH range = Scan.Int(Text.Sub(txt, cp+2)) DO
              RETURN NEW(Array, lo := base, hi := base + range - 1)
            END
          ELSE
            WITH aux = Scan.Int(Text.Sub(txt, cp+1)) DO
              RETURN NEW(Array, lo := MIN(base,aux), hi := MAX(base,aux))
            END
          END
        END
      END
    EXCEPT
      Lex.Error, FloatMode.Trap =>
      RAISE Syntax("dont understand array spec \"" & txt & "\"")
    END
  END ParseRange;

TYPE
  Value = CardSeq.T OBJECT
  METHODS
    msb() : [0..1] := VMsb;
    getBit(bit : CARDINAL) : [0..1] := GetBit;
    initBit(v : [0..1]) : Value := InitBitV;
  END;

  ValSeq = OBJECT
    cycles     : WordSeq;
    codaRepeat : CARDINAL;
  METHODS
    slice(bit : CARDINAL) : ValSeq := SliceVS;
    step(s : CARDINAL) : Value := StepVS;
  END;

PROCEDURE InitBitV(v : Value; b : [0..1]) : Value =
  BEGIN
    EVAL v.init();
    v.addhi(b);
    RETURN v
  END InitBitV;

PROCEDURE StepVS(seq : ValSeq; step : CARDINAL) : Value =
  (* a time slice *)
  VAR
    n := seq.cycles.size();
  BEGIN
    IF    step < n THEN
      RETURN seq.cycles.get(step)
    ELSIF seq.codaRepeat = 0 THEN
      RETURN seq.cycles.get(n-1)
    ELSE
      WITH overrun = step - n,
           phase   = overrun MOD seq.codaRepeat,
           start   = n - seq.codaRepeat,
           idx     = start + phase DO
        RETURN seq.cycles.get(idx)
      END
    END
  END StepVS;
  
PROCEDURE SliceVS(seq : ValSeq; bit : CARDINAL) : ValSeq =
  (* slice out a bit *)
  VAR
    res := NEW(ValSeq,
               cycles := NEW(WordSeq).init(),
               codaRepeat := seq.codaRepeat);
  BEGIN
    FOR i := 0 TO seq.cycles.size()-1 DO
      WITH sv = NARROW(seq.cycles.get(i),Value) DO
        res.cycles.addhi(NEW(Value).initBit(sv.getBit(bit)))
      END
    END;
    RETURN res
  END SliceVS;

PROCEDURE GetBit(v : Value; bit : CARDINAL) : [0..1] =
  BEGIN
    IF bit < v.size() THEN
      RETURN v.get(bit)
    ELSE
      RETURN v.msb()
    END
  END GetBit;

TYPE
  WordSeq = RefSeq.T OBJECT
  OVERRIDES
    addhi := AddhiWS;
  END;

PROCEDURE AddhiWS(ws : WordSeq; READONLY x : REFANY) =
  BEGIN
    Debug.Out("addhi : " & FmtValue(x));
    RefSeq.T.addhi(ws, x)
  END AddhiWS;

PROCEDURE FmtValue(v : Value) : TEXT =
  VAR res := "";
  BEGIN
    FOR i := v.size()-1 TO 0 BY -1 DO
      CASE v.get(i) OF
        0 => res := res & "0"
      |
        1 => res := res & "1"
      ELSE
        <*ASSERT FALSE*>
      END
    END;
    RETURN res
  END FmtValue;

PROCEDURE VMsb(v : Value) : [0..1] =
  BEGIN
    IF v.size() = 0 THEN RETURN 0 ELSE RETURN v.get(v.size()-1) END
  END VMsb;

PROCEDURE ParseVal(token : TEXT) : Value RAISES { Syntax } =
  BEGIN
    RETURN ParseVals(token).step(0)
  END ParseVal;
  
PROCEDURE ParseVals(token : TEXT) : ValSeq RAISES { Syntax } =

  PROCEDURE More() : BOOLEAN =
    BEGIN RETURN p < n END More;

  PROCEDURE GetC() : CHAR =
    BEGIN WITH c = Text.GetChar(token, p) DO INC(p); RETURN c END END GetC;

  PROCEDURE PeekC() : CHAR =
    BEGIN RETURN Text.GetChar(token, p) END PeekC;

  PROCEDURE GetNumber() : Value RAISES { Syntax } =

    PROCEDURE Digit(c : CHAR; xbase := 0) : CARDINAL RAISES { Syntax } =
      VAR
        res : [0..40]; (* give or take on the upper limit *)
      BEGIN
        IF xbase = 0 THEN xbase := base END;
        CASE c OF
          '0'..'9' => res := ORD(c) - ORD('0')
        |
          'a'..'z' => res := ORD(c) - ORD('a') + 10
        |
          'A'..'Z' => res := ORD(c) - ORD('A') + 10
        ELSE
          RAISE Syntax("Cant parse number \"" & token & "\"")
        END;
        IF res >= xbase THEN
          RAISE Syntax("Digit out of base range in \"" & token & "\"")
        END;
        RETURN res
      END Digit;
  
    PROCEDURE SetBase() RAISES { Syntax } =
      BEGIN
        IF baseSet THEN
          RAISE Syntax("base set more than once : " & token)
        END;
        base := 0;
        FOR i := 0 TO soFar.size()-1 DO
          base := 10 * base + Digit(soFar.get(i),10)
        END;
        baseSet := TRUE;
        soFar := soFar.init();
      END SetBase;
      
    VAR
      soFar := NEW(CharSeq.T).init();
      base := 10;
      baseSet := FALSE;
    BEGIN
      WHILE More() DO
        WITH c = PeekC() DO
          IF c = '_' THEN
            EVAL GetC();
            SetBase();
            Debug.Out("Set base to " & Int(base))
          ELSIF c IN SET OF CHAR { '=', '(', ')' } THEN
            EXIT
          ELSE
            soFar.addhi(GetC())
          END
        END
      END;
      (* past any base spec with base set, all digits in soFar *)
      VAR
        bigNum  := BigInt.Zero;
        bigBase := BigInt.New(base);
        bits    := NEW(Value).init();
        bigTwo  := BigInt.New(2);
        bigRem  : BigInt.T;
      BEGIN
        FOR i := 0 TO soFar.size()-1 DO
          bigNum := BigInt.Add(BigInt.Mul(bigBase, bigNum),
                               BigInt.New(Digit(soFar.get(i))))
        END;

        (* number is in bigNum *)

        WHILE NOT BigInt.Equal(bigNum, BigInt.Zero) DO
          BigInt.Divide(bigNum, bigTwo, bigNum, bigRem);
          IF    BigInt.Equal(bigRem,BigInt.Zero) THEN
            bits.addhi(0)
          ELSIF BigInt.Equal(bigRem,BigInt.One) THEN
            bits.addhi(1)
          ELSE
            <*ASSERT FALSE*>
          END
        END;
        RETURN bits
      END
    END GetNumber;
    
  VAR
    res := NEW(ValSeq, cycles := NEW(WordSeq).init(), codaRepeat := 1);
    p := 0;
    n : CARDINAL;
    codaStart := 0;
  BEGIN

    Debug.Out(F("ParseVals(\"%s\")", token));
    token := TextUtils.Replace(token, "=0x", "=16_");
    n := Text.Length(token);
    
    WHILE More() DO
      WITH c = GetC() DO
        Debug.Out("GetC : " & Text.FromChar(c));
        
        IF    c = '=' THEN
          res.cycles.addhi(GetNumber())
        ELSIF c = '(' THEN
          codaStart := res.cycles.size();
          Debug.Out("codaStart = " & Int(codaStart));
        ELSIF c = ')' THEN
          res.codaRepeat := res.cycles.size()-codaStart;
          Debug.Out("codaRepeat = " & Int(res.codaRepeat));
          RETURN res
        ELSIF c = '0' THEN
          res.cycles.addhi(ZeroSeq)
        ELSIF c = '1' THEN
          res.cycles.addhi(OneSeq)
        END
      END
    END;
    RETURN res
  END ParseVals;

VAR ZeroSeq, OneSeq : Value;

PROCEDURE Setup() =
  BEGIN
    ZeroSeq := NEW(Value).init();
    ZeroSeq.addhi(0);
    OneSeq  := NEW(Value).init();
    OneSeq .addhi(1)
  END Setup;

PROCEDURE MakeTheClock(nm : TEXT) RAISES { Syntax, Wr.Failure } =
  VAR
    rise   := GetLongReal("rise");
    fall   := GetLongReal("fall");
    period := GetLongReal("period");
    duty   := GetLongReal("duty");
    length := GetLongReal("length");

    hi     := duty * period;

    (*
         ______
        /  hi  \   lo   /
        --------|------|
       /         \____/     

       sustainhi = hi - rise/2 - fall/2
       sustainlo = lo - rise/2 - fall/2
    *)

  BEGIN
    (* clock starts at zero at t=0 *)
    VAR
      t := rise/2.0d0;
    BEGIN
      WHILE t < length + period DO
        ticks.addhi(t);
        t := t + period
      END
    END;

    VAR
      wf := NEW(Waveform).init();
    BEGIN
      FOR i := 0 TO ticks.size()-1 DO
        WITH t = ticks.get(i) DO
          wf.addhi(LRPair.T { t   -rise/2.0d0, 0.0d0 });
          wf.addhi(LRPair.T { t   +rise/2.0d0, 1.0d0 });
          wf.addhi(LRPair.T { t+hi-fall/2.0d0, 1.0d0 });
          wf.addhi(LRPair.T { t+hi+fall/2.0d0, 0.0d0 })
        END
      END;
      EmitWaveform(nm, wf)
    END
  END MakeTheClock;

VAR ticks := NEW(LRSeq.T).init();

TYPE Waveform = LRPairSeq.T;
  
PROCEDURE ParseInput(rd : Rd.T)
  RAISES { Syntax, Rd.Failure, Wr.Failure } =
  VAR
    lNo := 0;
  BEGIN
    TRY
      LOOP
        INC(lNo);
        VAR
          ln := Rd.GetLine(rd);
          rr : TextReader.T;
          tk : TextList.T;
        BEGIN
          Debug.Out(F("Got line \"%s\"", ln));
          
          rr := NEW(TextReader.T).init(ln);
          tk := rr.shatter(" \t", endDelims := "", skipNulls := TRUE);
          
          IF tk = NIL OR Text.GetChar(tk.head, 0) = '#' THEN
            (* empty line or comment *)
          ELSIF TE(tk.head, "param") THEN
            WITH pn = Tok(tk, 1),
                 pvt = Tok(tk, 2),
                 pv  = GetLongReal(pvt) DO

              Debug.Out(F("Defining param %s <- %s", pn, LongReal(pv)));
              EVAL params.put(pn, pv)
            END
          ELSIF TE(tk.head, "clock") THEN
            WITH cn = Tok(tk, 1) DO
              Debug.Out(F("Defining clock %s", cn));
              MakeTheClock(cn)
            END
          ELSIF TE(tk.head, "drive") THEN
            WITH nms  = ParseNames(Tok(tk,1)),
                 vals = ParseVals(RemToks(tk,2)) DO
              CreateDrivers(nms, vals)
            END
          ELSIF TE(tk.head, "initial") THEN
            WITH nms = ParseNames(Tok(tk,1)),
                 val = ParseVal(Tok(tk,2)) DO
              CreateInitial(nms, val)
            END
          ELSIF TE(tk.head, "probe") THEN
            WITH nms = ParseNames(Tok(tk,1)) DO
              CreateProbes(nms)
            END
          ELSE
            RAISE Syntax("Unknown keyword \"" & tk.head & "\"")
          END
        END
      END;
    EXCEPT
      Syntax(e) =>  RAISE Syntax(e & " on line " & Int(lNo))
    |
      Rd.EndOfFile => (* done *)
    END
  END ParseInput;

(**********************************************************************)

PROCEDURE CreateProbes(struct : Struct;
                       p      : CARDINAL := 0;
                       soFar  : IntList.T := NIL) RAISES { Wr.Failure } =
  BEGIN
    WITH this = struct.get(p) DO
      TYPECASE this OF
        TEXT(txt) =>
        Wr.PutText(wr, F("set_probe -silent -type V %s\n",
                         MakeNodeName(txt,soFar)))
      |
        Array(arr) =>
        FOR i := arr.lo TO arr.hi DO
          CreateProbes(struct, p+1, IntList.Append(soFar, IntList.List1(i)))
        END
      ELSE
        <*ASSERT FALSE*>
      END
    END
  END CreateProbes;

PROCEDURE MakeNodeName(pattern : TEXT; indices : IntList.T) : TEXT =
  VAR
    ip := indices;
    q  := 0;
  BEGIN
    WHILE ip # NIL DO
      pattern := TextUtils.Replace(pattern,
                                   F("[$%s]",Int(q)),
                                   F("[%s]", Int(ip.head)));
      ip := ip.tail;
      INC(q)
    END;
    RETURN pattern
  END MakeNodeName;
  
PROCEDURE CreateDrivers(struct : Struct;
                        vals   : ValSeq;
                        p      : CARDINAL := 0;
                        soFar  : IntList.T := NIL)
  RAISES { Wr.Failure, Syntax } =
  BEGIN
    WITH this = struct.get(p) DO
      TYPECASE this OF
        TEXT(txt) =>
        VAR
          nm := MakeNodeName(txt, soFar);
          wf : Waveform;
        BEGIN
          Debug.Out(F("CreateDrivers emitting waveform for %s", nm));
          wf := MakeWaveform(vals);
          EmitWaveform(nm, wf)
        END
      |
        Array(arr) =>
        Debug.Out(F("CreateDrivers Array %s to %s", Int(arr.lo), Int(arr.hi)));
        FOR i := arr.lo TO arr.hi DO
          Debug.Out(F("CreateDrivers slicing vals at %s", Int(i-arr.lo)));
          CreateDrivers(struct,
                        vals.slice(i-arr.lo),
                        p+1,
                        IntList.Append(soFar, IntList.List1(i)))
        END
      ELSE
        <*ASSERT FALSE*>
      END
    END
  END CreateDrivers;

PROCEDURE MakeWaveform(vals : ValSeq) : Waveform
  RAISES { Syntax } =
  VAR
    res    := NEW(Waveform).init();
    rise   := GetLongReal("rise");
    fall   := GetLongReal("fall");
    off    := GetLongReal("doffset");
    length := GetLongReal("length");
    t      := 0.0d0;
    p      := 0;
    init   := FLOAT(NARROW(vals.step(0),Value).getBit(0),LONGREAL);
    prv    := init;
    tran   : LONGREAL;
  BEGIN
    Debug.Out(F("t %s init %s", LongReal(t), LongReal(init)));
    res.addhi(LRPair.T { t, init } );
    WHILE t < length DO
      WITH ct = ticks.get(p),
           st = ct + off,
           v  = NARROW(vals.step(p),Value).getBit(0),
           nxt = FLOAT(v, LONGREAL) DO
        Debug.Out(F("prv %s nxt %s", LongReal(prv), LongReal(nxt)));
        IF nxt # prv THEN
          IF nxt > prv THEN tran := rise ELSE tran := fall END;
          res.addhi(LRPair.T { st-tran/2.0d0, prv });
          res.addhi(LRPair.T { st+tran/2.0d0, nxt })
        END;
        prv := nxt;
        t := st;
        INC(p)
      END
    END;
    RETURN res
  END MakeWaveform;

PROCEDURE CreateInitial(struct : Struct;
                        val   : Value;
                        p      : CARDINAL := 0;
                        soFar  : IntList.T := NIL)
  RAISES { Wr.Failure, Syntax } =
  BEGIN
    WITH this = struct.get(p) DO
      TYPECASE this OF
        TEXT(txt) =>
        WITH nm    = MakeNodeName(txt,soFar),
             z     = val.getBit(0) DO
          EmitInitial(nm, z)
        END
      |
        Array(arr) =>
        FOR i := arr.lo TO arr.hi DO
          CreateInitial(struct,
                        NEW(Value).initBit(val.getBit(i-arr.lo)),
                        p+1,
                        IntList.Append(soFar, IntList.List1(i)))
        END
      ELSE
        <*ASSERT FALSE*>
      END
    END
  END CreateInitial;

  (**********************************************************************)

VAR srcId := 0;
    
PROCEDURE MakeSourceName(nm : TEXT) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := 0 TO Text.Length(nm)-1 DO
      WITH c = Text.GetChar(nm, i) DO
        CASE c OF
          '{', '}' => (* skip *)
        |
          'a'..'z', 'A'..'Z', '_', '0'..'9' => Wx.PutChar(wx, c)
        ELSE
          Wx.PutChar(wx, '_')
        END
      END
    END;

    WITH res = Wx.ToText(wx) &
         Fmt.Pad(Int(srcId), length := 8, padChar := '0') DO
      INC(srcId);
      RETURN res
    END
      
  END MakeSourceName;
  
PROCEDURE EmitInitial(nm : TEXT; v : [0..1]) RAISES { Wr.Failure } =
  VAR inits : TEXT;
  BEGIN
    CASE v OF
      0 => inits := "0.0"
    |
      1 => inits := "=vcc()"
    END;
    Wr.PutText(wr, F("set_dc_condition -silent -dc %s %s\n",
                     inits, nm))
  END EmitInitial;

PROCEDURE EmitWaveform(nm : TEXT; wf : Waveform) RAISES { Wr.Failure } =
  VAR
    vs : TEXT;
    inits : TEXT;
  BEGIN
    Debug.Out(F("EmitWaveform %s", nm));
    IF wf.get(0).k2 = 0.0d0 THEN
      inits := "0.0"
    ELSIF wf.get(0).k2 = 1.0d0 THEN
      inits := "=vcc()"
    END;
    
    Wr.PutText(wr, F("set_wave -type v -shape pwl -dc %s -pwl {\n",inits));
    FOR i := 0 TO wf.size()-1 DO
      WITH p = wf.get(i) DO
        IF    p.k2 = 0.0d0 THEN
          vs := "0.0"
        ELSIF p.k2 = 1.0d0 THEN
          vs := "=vcc()"
        ELSE
          Debug.Out("p.k2 = " & LongReal(p.k2));
          <*ASSERT FALSE*>
        END;
        Wr.PutText(wr, F("%s %s\n", Fmt.LongReal(p.k1), vs));
      END
    END;

    WITH srcNm = MakeSourceName(nm) DO
      Wr.PutText(wr, F("} -repeat 0.0 -real 0.0 -imaginary 0.0 %s\n",
                       srcNm));
      
      Wr.PutText(wr, F("set_source -silent -stimuli %s -ref_net gnd %s\n",
                       srcNm, nm))
    END;

    IF gnuplot # NIL THEN
      VAR fn := gnuplot & "/" & nm & ".dat";
          wr : Wr.T;
      BEGIN
        TRY
          wr := FileWr.Open(fn);
        
          Wr.PutText(wr, F("%s %s\n", "0.0 ", LongReal(wf.get(0).k2)));
          FOR i := 0 TO wf.size()-1 DO
            WITH r = wf.get(i) DO
              Wr.PutText(wr, F("%s %s\n", LongReal(r.k1), LongReal(r.k2)))
            END
          END;
          Wr.Close(wr)
        EXCEPT
          Wr.Failure(x) => Debug.Error("I/O error while writing \"" &
            fn & "\" : Wr.Failure : " & AL.Format(x))
        |
          OSError.E(x) => Debug.Error("Error while opening/closing \"" &
            fn & "\" : OSError.E : " & AL.Format(x))
        END
      END
    END
  END EmitWaveform;

PROCEDURE EmitHeader() RAISES { Wr.Failure } =
  BEGIN
    Wr.PutText(wr, "set_wave -type v -shape dc -dc =vcc() -real 0.0 -imaginary 0.0 vcc\n");
    Wr.PutText(wr, "set_wave -type v -shape dc -dc 0.0 -real 0.0 -imaginary 0.0 vss\n");
  END EmitHeader;
  
(**********************************************************************)
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  params := NEW(TextLRTbl.Default).init();
  wr := Stdio.stdout;
  rd := Stdio.stdin;
  gnuplot : Pathname.T := NIL;
  
PROCEDURE GetLongReal(txt : TEXT) : LONGREAL RAISES { Syntax } =
  BEGIN
    TRY
      RETURN Scan.LongReal(txt)
    EXCEPT
      Lex.Error, FloatMode.Trap =>
      VAR
        v : LONGREAL;
      BEGIN
        IF params.get(txt, v) THEN
          RETURN v
        ELSE
          RAISE Syntax("not a number : \"" & txt & "\"")
        END
      END
    END
  END GetLongReal;
  
BEGIN
  TRY
    IF pp.keywordPresent("-gnuplot") OR pp.keywordPresent("-gp") THEN
      gnuplot := pp.getNext()
    END;
    
    WHILE pp.keywordPresent("-param") OR pp.keywordPresent("-p") DO
      WITH pn = pp.getNext(),
           pvt = pp.getNext(),
           pv = GetLongReal(pvt) DO
        EVAL params.put(pn, pv)
      END
    END;
    
    IF pp.keywordPresent("-i") THEN
      WITH ifn = pp.getNext() DO
        TRY
          rd := FileRd.Open(ifn)
        EXCEPT
          OSError.E(x) => Debug.Error("Couldnt open input file \"" & ifn &
            "\" : OSError.E : " & AL.Format(x))
        END
      END
    END;

    IF pp.keywordPresent("-o") THEN
      WITH ofn = pp.getNext() DO
        TRY
          wr := FileWr.Open(ofn)
        EXCEPT
          OSError.E(x) => Debug.Error("Couldnt open output file \"" & ofn &
            "\" : OSError.E : " & AL.Format(x))
        END
      END
    END;

    pp.finish()
  EXCEPT
    Syntax, ParseParams.Error =>
    Debug.Error("Couldnt parse command-line parameters")
  END;

  Setup();

  TRY
    EmitHeader();
    ParseInput(rd);
    Rd.Close(rd);
    Wr.Close(wr)
  EXCEPT
    Syntax(e) =>
    Debug.Error("Syntax error while parsing input : " & e)
  |
    Rd.Failure(x) => Debug.Error("I/O error while reading input : Rd.Failure : " & AL.Format(x))
  |
    Wr.Failure(x) => Debug.Error("I/O error while writing output : Wr.Failure : " & AL.Format(x))
  END
  
END Main.
