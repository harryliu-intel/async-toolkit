MODULE TcamSimulation;
IMPORT Verb, Math;
FROM TcamSequencer IMPORT Compile;
IMPORT Dims;
IMPORT BitInteger;
FROM Fmt IMPORT F; IMPORT Fmt;
IMPORT CommandSeq;
IMPORT Random;
IMPORT SimDumper;
FROM SimDumper IMPORT AddNodes, Vdd;
IMPORT Tcam;
IMPORT TcamModel;
FROM Dims IMPORT Scalar;
IMPORT SimModel;
IMPORT ParseParams;
IMPORT SimParams;
IMPORT Sim;
IMPORT StandardSettings;
FROM SimSupport IMPORT ClockSrc, IntegerSrc, SeqSrc, Reader;
IMPORT TcamPrograms;

CONST SI = BitInteger.Small;

PROCEDURE Clog2(q : CARDINAL) : CARDINAL =
  BEGIN RETURN CEILING(Math.log(FLOAT(q,LONGREAL))/Math.log(2.0d0)) END Clog2;

VAR
  C := Tcam.T {
    N  :=          512,
    W  :=           40,
    LN := LAST(CARDINAL),
    CN :=           12,
    SS :=           64 (* depth of a slice *),
    SN := LAST(CARDINAL)
  };
    
  I0   := SI( 0);
  I1   := SI( 1);
  Icf9 := SI(16_cf9);
  IN1  := SI(-1);

CONST DutName = "X1";

VAR theModel : SimModel.T;

PROCEDURE Build(pp : ParseParams.T; sp : SimParams.T; modelName : TEXT)
  RAISES { ParseParams.Error }=
  TYPE
    V   = Verb.T;

  PROCEDURE RunTheProgram(pt : TcamPrograms.Type) =
    BEGIN
      TcamPrograms.Progs[pt](prog,C)
    END RunTheProgram;

  VAR    
    Seq : ARRAY Verb.T OF REF ARRAY OF BitInteger.T;
    prog := NEW(CommandSeq.T).init();
    s := StandardSettings.New(pp, sp, NEW(Random.Default).init());
    theClock := NEW(ClockSrc,
                    s := s,
                    spd := s.spd,
                    lo := 0.0d0,
                    hi := Vdd);
    pt := TcamPrograms.ParseFlag(pp);

  BEGIN 
    C.LN := Clog2( C.N*2 );
    C.SN := C.N DIV C.SS;

    RunTheProgram(pt);

    Compile(prog, Seq);

    SimDumper.SetDutName(DutName);
    AddNodes(ClockName  , Scalar        , theClock                        );
    AddNodes("VDD"      , Scalar        , NEW(IntegerSrc, s := s, val := I1     ) );
    AddNodes("VSS"      , Scalar        , NEW(IntegerSrc, s := s, val := I0     ) );
    
    AddNodes("CFG"      , Dims.T { C.CN } , NEW(IntegerSrc, s := s, val := Icf9   ) );

    AddNodes("ADDR"     , Dims.T { C.LN } , NEW(SeqSrc    , s := s, q := Seq[V.Addr]
                                                        , c := theClock ) );
    AddNodes("DATA"     , Dims.T {  C.W } , NEW(SeqSrc    , s := s, q := Seq[V.Data]
                                                        , c := theClock ) );
    AddNodes("KEN"      , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Look]
                                                        , c := theClock ) );
    AddNodes("REN"      , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Read]
                                                        , c := theClock ) );
    AddNodes("WEN"      , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Writ]
                                                        , c := theClock ) );

    AddNodes("READ_DATA", Dims.T {  C.W } , NEW(Reader    , c := theClock ) );
    AddNodes("RHIT"     , Dims.T {  C.N } , NEW(Reader    , c := theClock ) );

    AddNodes("MASK"     , Dims.T {  C.W } , NEW(IntegerSrc, s := s, val := IN1    ) );
    AddNodes("LHIT"     , Dims.T {  C.N } , NEW(IntegerSrc, s := s, val := IN1    ) );

    AddNodes("SLICE_EN" , Dims.T { C.SN } , NEW(IntegerSrc, s := s, val := IN1    ) );
    AddNodes("RESET_N"  , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Rset]
                                                        , c := theClock ) );

    SimDumper.DeclSequence(s.extractPath,
                           "ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl",
                           ARRAY OF TEXT { "ADDR", 
                                           "CFG",
                                           ClockName,
                                           "DATA",
                                           "KEN",
                                           "LHIT",
                                           "MASK",
                                           "READ_DATA",
                                           "REN",
                                           "RESET_N",
                                           "RHIT",
                                           "SLICE_EN",
                                           "VDD",
                                           "VSS",
                                           "WEN" });


  IF s.verboseNodes THEN
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %s* -limit 2\"", SimDumper.Renamer(DutName & "."))); 
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].Xmid[0].* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xinputs.* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].Xchunk[0].Xe[0].* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].Xchunk[0].Xz[0].Xz[0][0].* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].Xchunk[0].* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].Xchunk[0].b[0]* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].Xmid[0].* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].a[0]* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].hit[0]* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].b* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.Xslice[0].Xctrl.* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.addr2* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %sXtcam.ADDR2* -limit 10\"", SimDumper.Renamer(DutName & ".")))
  END;

  theModel := NEW(TcamModel.T).init(C,
                                    s.cycleTime,
                                    s.assertHoldFrac,
                                    s.assertHoldTime,
                                    TcamModel.NamingConvention.Andrew)
    
END Build;

PROCEDURE GetModel() : SimModel.T = 
  BEGIN RETURN theModel END GetModel;

(**********************************************************************)

BEGIN END TcamSimulation.
