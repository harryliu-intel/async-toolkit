MODULE SimDumper;

IMPORT NodesList;
IMPORT Debug;
IMPORT Wr;
FROM Fmt IMPORT Int, F, Bool; IMPORT Fmt;
IMPORT TranSeq;
IMPORT Dims;
IMPORT Thread;
IMPORT SpiceLineWriter;
IMPORT Text;
IMPORT Pathname;
IMPORT IO;
IMPORT TextSeq;
IMPORT Sim;
IMPORT SimParams;
IMPORT Nodes;
IMPORT Intf;
IMPORT Src;
IMPORT Rdr;
IMPORT NodeRec, NodeRecSeq;
IMPORT SimModel;
IMPORT Valenv;
IMPORT AssertionList;
IMPORT ProbeMode;
IMPORT TextSetDef;
IMPORT Params;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*> (* sloppy! *)

CONST LR = Fmt.LongReal;

CONST TE = Text.Equal;


PROCEDURE DumpProbes(wr : Wr.T; of : NodeRecSeq.T) =
  BEGIN
    Wr.PutText(wr, "\n");

    FOR i := 0 TO of.size()-1 DO
      WITH rdr = of.get(i) DO
        Wr.PutText(wr, F(".PROBE TRAN v(%s)\n", rdr.nm))
      END
    END
  END DumpProbes;

PROCEDURE MakeDictionaries(srcs, rdrs : NodeRecSeq.T) =

  PROCEDURE DoOne(z : Nodes.T) =

    PROCEDURE Single(READONLY u : Dims.T) =
      VAR
        v := NEW(REF Dims.T, NUMBER(u));
      BEGIN
        v^ := u;
        WITH nm = z.nm & Dims.Format(u),
             sNm = z.sNm & Dims.Format(u),
             rec = NEW(NodeRec.T, nm := nm, sNm := sNm, idx := v, nds := z) DO

          Debug.Out("MakeDictionaries " & nm);
          TYPECASE z.intf OF
            Src.T => srcs.addhi(rec)
          |
            Rdr.T => rdrs.addhi(rec)
          ELSE
            <*ASSERT FALSE*>
          END
        END
      END Single;

    VAR
      q    := Dims.Clone  (z.dims^);
      iter := Dims.Iterate(z.dims^);
    BEGIN
      WHILE iter.next(q^) DO Single(q^) END
    END DoOne;

  VAR 
    p := lst;
  BEGIN
    WHILE p # NIL DO
      DoOne(p.head);
      p := p.tail
    END
  END MakeDictionaries;

PROCEDURE FillInSrc(nrc         : NodeRec.T;
                    VAR data    : ARRAY OF LONGREAL;
                    params      : SimParams.T) =
  BEGIN
    Debug.Out("filling in " & nrc.nm);
    <*ASSERT nrc          # NIL*>
    <*ASSERT nrc.nds      # NIL*>
    <*ASSERT nrc.nds.intf # NIL*>
    WITH src = NARROW(nrc.nds.intf, Src.T) DO
      FOR i := FIRST(data) TO LAST(data) DO
        WITH t = params.step * FLOAT(i,LONGREAL) DO
          data[i] := src.getV(nrc.idx^,t)
        END
      END
    END
  END FillInSrc;


VAR
  lst      : NodesList.T  := NIL;
  theSrcs  : NodeRecSeq.T := NIL;
  theRdrs  : NodeRecSeq.T := NIL;

PROCEDURE DumpIt(wr        : Wr.T; 
                 sp        : SimParams.T; 
                 sim       : Sim.T; 
                 pm        : ProbeMode.T;
                 modelName : TEXT;
                 modelPath : Pathname.T) = 
  (* write the requested output *)

  VAR
    srcData : REF ARRAY OF ARRAY OF LONGREAL;
    last : LONGREAL;

  CONST
    MaxTimeMargin = 5.0d-9;

  BEGIN
    theRdrs := NEW(NodeRecSeq.T).init();
    theSrcs := NEW(NodeRecSeq.T).init();

    MakeDictionaries(theSrcs, theRdrs);
    Debug.Out("Sources : " & Int(theSrcs.size()));
    Debug.Out("Readers : " & Int(theRdrs.size()));

    MakeTransitionSequences(theSrcs);

    last := FindFinalRequestedTransition(theSrcs);
    sp.maxTime := last + MaxTimeMargin;

    IO.Put("Setting max simulation time to " & LR(sp.maxTime) & "\n");

    srcData := NEW(REF ARRAY OF ARRAY OF LONGREAL,
                   theSrcs.size(), 
                   FLOOR(sp.maxTime/sp.step)+1);

    FOR i := 0 TO theSrcs.size()-1 DO
      FillInSrc(theSrcs.get(i), srcData[i], sp)
    END;

    DumpSpiceHeader(wr, sim, pm, modelName, modelPath);

    DumpData(wr, theSrcs, srcData^, sim, sp);

  END DumpIt;

PROCEDURE FinishDump(wr : Wr.T; pm : ProbeMode.T; ass : AssertionList.T) =
  BEGIN
    DumpInstantiation(wr);

    IF pm = ProbeMode.T.IO OR pm = ProbeMode.T.Outputs THEN
      Debug.Out("Dumping rdr probes: " & Int(theRdrs.size()));
      DumpProbes(wr, theRdrs)
    END;

    IF pm = ProbeMode.T.IO THEN
      Debug.Out("Dumping src probes: " & Int(theSrcs.size()));
      DumpProbes(wr, theSrcs)
    END;

    IF pm = ProbeMode.T.Assertions THEN
      (* make synthetic NodeRecs *)
      VAR
        s := NEW(TextSetDef.T).init();
        seq := NEW(NodeRecSeq.T).init();
        p := ass;
      BEGIN
        WHILE p # NIL DO
          WITH nm = p.head.nm DO
            IF NOT s.insert(nm) THEN
              WITH rec = NEW(NodeRec.T, nm := nm) DO
                seq.addhi(rec)
              END
            END
          END;
          p := p.tail
        END;
        DumpProbes(wr, seq)
      END
    END;

    DumpSpiceFooter(wr)
  END FinishDump;

PROCEDURE DumpSpiceHeader(wr        : Wr.T; 
                          sim       : Sim.T; 
                          pm        : ProbeMode.T;
                          modelName : TEXT;
                          modelPath : Pathname.T) =
  PROCEDURE P(txt : TEXT) =
    BEGIN Wr.PutText(wr, txt); Wr.PutChar(wr, '\n') END P;
    
  VAR
    str := "*";

  BEGIN
    FOR i := 0 TO Params.Count-1 DO
      str := str & " " & Params.Get(i)
    END;
    P(str);

    P(".TEMP " & Int(ROUND(Temp)));
    P(F(".PARAM vtrue=%s",LR(Vdd)));
    
  
    P(".OPTION CMIFLAG=1 CMIUSRFLAG=3 ");
    P(".OPTION PDMI=1");

    CASE sim OF
      Sim.T.XA =>
      (* for XA *)
      P("* XA options");
      P(F(".OPTION POST=fsdb"));
      P(".OPTION XA_CMD=\"set_sim_level -level 6\"");
      P(".OPTION XA_CMD=\"set_wildcard_rule -match* all\""); (* match all *)
      P(".OPTION XA_CMD=\"set_message_option -limit 100\"");
      P(".OPTION XA_CMD=\"enable_print_statement 1\"");
      P(".OPTION XA_CMD=\"set_sim_case -case sensitive\"");
    |
      Sim.T.HSPICE =>
      (* for HSPICE *)
      VAR
        probeOpt : TEXT;
      BEGIN
        IF pm = ProbeMode.T.All THEN
          probeOpt := ""
        ELSE
          probeOpt := " PROBE=1"
        END;
        P("* HSPICE options");
        P(".PARAM ceil(x)='x<0 ? int(x) : int(x) < x ? int(x)+1 : int(x)'");
        P(F(".OPTION CSDF%s",probeOpt));
        P(".OPTION runlvl=5"); (* second most accurate *)
        P(".OPTION warnlimit=2000");
        P(".OPTION dcon=1");
      END;
    END;

    FOR i := 0 TO simExtras[sim].size()-1 DO
      P(simExtras[sim].get(i))
    END;

    P(F(".LIB \"%s\" %s", modelPath, modelName));
    P("");
    P(F(".include \"%s\"\n",dutLibFn));
    P("")
  END DumpSpiceHeader;

PROCEDURE DumpSpiceFooter(wr : Wr.T) =
  BEGIN
    Wr.PutText(wr, ".END\n")
  END DumpSpiceFooter;

PROCEDURE MakeTransitionSequences(srcs : NodeRecSeq.T) =
  BEGIN
    FOR i := 0 TO srcs.size()-1 DO
      WITH rec = srcs.get(i) ,
           nds = NARROW(rec.nds.intf,Src.T) DO
        <*UNUSED*>VAR dummy := nds.getV(rec.idx^,0.0d0); BEGIN END
      END
    END
  END MakeTransitionSequences;

PROCEDURE FindFinalRequestedTransition(srcs : NodeRecSeq.T) : LONGREAL =
  VAR
    last := 0.0d0;
    seq : TranSeq.T;
  BEGIN
    FOR i := 0 TO srcs.size()-1 DO
      WITH rec = srcs.get(i) ,
           nds = NARROW(rec.nds.intf,Src.T),
           hdd = nds.trans.get(rec.idx^,seq) DO
        <*ASSERT hdd*>
        IF NOT nds.infinite() THEN
          FOR j := 0 TO seq.size()-1 DO
            WITH sj = seq.get(j) DO
              Debug.Out(F("Sequence %s final trans at %s last=%s",
                          rec.nds.nm & Dims.Format(rec.idx^),
                          LR(sj.t + sj.rf),
                          Fmt.Bool(sj.t + sj.rf > last)));
              last := MAX(last, sj.t + sj.rf)
            END
          END
        END
      END
    END;
    RETURN last
  END FindFinalRequestedTransition;

PROCEDURE DumpData(wr         : Wr.T; 
                   srcs       : NodeRecSeq.T; 
                   READONLY d : ARRAY OF ARRAY OF LONGREAL;
                   sim        : Sim.T;
                   sp         : SimParams.T) =

  PROCEDURE P(txt : TEXT) =
    BEGIN Wr.PutText(wr, txt); Wr.PutChar(wr, '\n') END P;
    
  PROCEDURE Header() =
    BEGIN
      lw.word("TIME");
      FOR i := 0 TO srcs.size()-1 DO lw.word(F("pv%s",Int(i+1))) END;
      lw.eol()
    END Header;

  PROCEDURE Line(i : CARDINAL) =
    BEGIN
      lw.word(LR(FLOAT(i,LONGREAL)*sp.step));
      FOR si := 0 TO srcs.size()-1 DO
        lw.word(LR(d[si,i]))
      END;
      lw.eol()
    END Line;

  VAR 
    lw := NEW(SpiceLineWriter.T).init(wr);
  BEGIN

    CASE sim OF
      Sim.T.HSPICE =>
      P("\n.TRAN DATA=dsrc\n");
      FOR i := 0 TO srcs.size()-1 DO
        WITH rec = srcs.get(i) DO
          P(F("V%s %s 0 PWL(TIME, pv%s)", Int(i+1), rec.sNm, Int(i+1)))
        END
      END;
      P("\n.DATA dsrc");
    
      Header();
      
      FOR i := FIRST(d[0]) TO LAST(d[0]) DO
        Line(i)
      END;

      P(".ENDDATA\n");

    |
      Sim.T.XA =>
      VAR
        seq : TranSeq.T;
      BEGIN
        FOR i := 0 TO srcs.size()-1 DO
          WITH rec = srcs.get(i) ,
               nds = NARROW(rec.nds.intf,Src.T),
               hdd = nds.trans.get(rec.idx^,seq) DO
            Debug.Out("dumping " & rec.nds.nm & Dims.Format(rec.idx^));
            <*ASSERT hdd*>
            lw.word(F("V%s %s 0 PWLZ", Int(i+1), rec.sNm));

            lw.word("(");
            VAR
              v := seq.get(0).v;
            BEGIN
              FOR j := 0 TO seq.size()-1 DO
                WITH sj = seq.get(j) DO
                  lw.word(LR(sj.t));
                  lw.word(LR(v  ));
                  lw.word(LR(sj.t+sj.rf));
                  lw.word(LR(sj.v));
                  v := sj.v;
                  
                  (* clock sequences can stretch beyond maxTime ... *)
                  IF sj.t + sj.rf > sp.maxTime THEN EXIT END;
                END
              END
            END;

            lw.word(")");
            lw.eol()
          END
        END
      END;
      
      P(F("\n.TRAN step=%s stop=%s\n",
          LR(sp.step),
          LR(sp.maxTime)))
    END
    
  END DumpData;

PROCEDURE SetDutName(nm : TEXT) =
  BEGIN
    dutName := nm
  END SetDutName;

(*
PROCEDURE Refize(READONLY a : ARRAY OF BitInteger.T) : REF ARRAY OF BitInteger.T =
  VAR
    res := NEW(REF ARRAY OF BitInteger.T, NUMBER(a));
  BEGIN
    res^ := a;
    RETURN res
  END Refize;
*)

PROCEDURE RefizeT(READONLY a : ARRAY OF TEXT) : REF ARRAY OF TEXT =
  VAR
    res := NEW(REF ARRAY OF TEXT, NUMBER(a));
  BEGIN
    res^ := a;
    RETURN res
  END RefizeT;

VAR
  dutLibFn : Pathname.T;
  dutType  : TEXT;
  dutArgs  : REF ARRAY OF TEXT;

PROCEDURE DeclSequence(libFile       : Pathname.T;
                       type          : TEXT;
                       READONLY args : ARRAY OF TEXT) =
  BEGIN
    dutLibFn := libFile;
    dutType  := type;
    dutArgs  := RefizeT(args)
  END DeclSequence;

PROCEDURE DumpInstantiation(wr : Wr.T) =
  VAR
    lw := NEW(SpiceLineWriter.T).init(wr);
  BEGIN
    lw.word(dutName);
    FOR i := FIRST(dutArgs^) TO LAST(dutArgs^) DO
      DumpArgList(lw,dutArgs[i])
    END;
    lw.word("/");
    lw.word(dutType);
    lw.eol()
  END DumpInstantiation;

PROCEDURE DumpArgList(lw : SpiceLineWriter.T; arg : TEXT) =
  VAR
    p := lst;
    search := dutName & "." & arg;
  BEGIN
    WHILE p # NIL DO
      WITH nds = NARROW(p.head,Nodes.T) DO
        IF TE(search, nds.nm) THEN
          WITH iter = Dims.Iterate(nds.dims^),
               q    = Dims.Clone  (nds.dims^) DO
            WHILE iter.next(q^) DO
              lw.word(arg & Dims.Format(q^))
            END
          END;
          RETURN
        END
      END;
      p := p.tail
    END;
    Debug.Error("No arg named \"" & arg & "\"")
  END DumpArgList;

(**********************************************************************)


PROCEDURE AddNodes(nm            : TEXT;
                   READONLY dims : Dims.T;
                   intf          : Intf.T) =
  BEGIN
    WITH nds = NEW(Nodes.T).init(dutName, nm, dims, intf) DO
      intf.complete(nds);
      lst := NodesList.Cons(nds, lst);
    END
  END AddNodes;

PROCEDURE AddDigitalModel(model   : SimModel.T; 
                          clockNm : TEXT; 
                          aWr     : Wr.T) : AssertionList.T =
  VAR
    clockNds : Nodes.T := NIL;
    clockSeq : TranSeq.T;
    v := LAST(LONGREAL);
    env := NEW(Valenv.T).init(dutName,
                              theSrcs, 
                              Valenv.Lims { -1.0d0      *Vdd, 
                                             1.0d0/4.0d0*Vdd,
                                             3.0d0/4.0d0*Vdd,
                                             2.0d0      *Vdd });
  BEGIN
    (* find clock node *)
    clockNm := dutName & "." & clockNm;

    VAR p := lst; BEGIN
      WHILE p # NIL DO 
        Debug.Out("SimDumper.AddDigitalModel, scanning node " & p.head.nm);
        IF TE(p.head.nm, clockNm) THEN 
          clockNds := p.head
        END;
        p := p.tail
      END
    END;
    <*ASSERT clockNds # NIL*>
    WITH src    = NARROW(clockNds.intf, Src.T),
         hadIt  = src.trans.get(Dims.Scalar, clockSeq) DO
      <*ASSERT hadIt*>
    END;

    FOR i := 0 TO clockSeq.size()-1 DO
      WITH tran = clockSeq.get(i),
           trig = Vdd/2.0d0,
           nv   = tran.v,
           go   = v<trig AND nv>trig DO

        Debug.Out(F("trig %s v %s nv %s go=%s t=%s",
                    LR(trig), LR(v), LR(nv), 
                    Bool(go),
                    LR(tran.t)));

        IF go THEN
          WITH ct = tran.t + (trig-v)/(nv-v)*tran.rf DO
            env.setTime(ct);
            model.simStep(env)
          END
        END;
        v := nv
      END
    END;

    VAR 
      q := env.getAssertions();
      p := q;
    BEGIN
      Debug.Out(Int(AssertionList.Length(p)) & " assertions");

      WHILE p # NIL DO
        WITH a = p.head DO
          Wr.PutText(aWr, F("ASSERTRANGE %s %s %s %s\n", 
                            dutName & "." & a.nm, 
                            LR(a.tm), LR(a.minV), LR(a.maxV)))
        END;
        p := p.tail
      END;
      RETURN q
    END
  END AddDigitalModel;

BEGIN 
  simExtras := ARRAY Sim.T OF TextSeq.T { NEW(TextSeq.T).init(),
                                          NEW(TextSeq.T).init() };

END SimDumper.
