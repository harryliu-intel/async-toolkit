
MODULE Main;

(* 
   produce HSPICE/XA setup file for NPG TCAM simulations

   Author : Mika Nystrom <mika.nystroem@intel.com>
   
   September, 2015
*)

IMPORT TcamSimulation AS TheSimulation;
IMPORT SimDumper;
IMPORT Sim;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT FileWr;
IMPORT Wr;
IMPORT Thread;
IMPORT ProbeMode;
IMPORT Text;
IMPORT Env;
IMPORT Pathname;
IMPORT SimParams;
IMPORT Params;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

VAR
  modelPath : Pathname.T;

  pp        := NEW(ParseParams.T).init(Stdio.stderr);
  sim       := Sim.T.HSPICE;
  probeMode := ProbeMode.T.Outputs;
  outPfx    := "out";
  modelName := "tttt";
  simParams := SimParams.T { 1.0d-12, 10000.0d-9 };

  cmdLn     := "";

BEGIN
  FOR i := 0 TO Params.Count-1 DO
    cmdLn := cmdLn & Params.Get(i) & " "
  END;
  Debug.Out("RUNNING " & cmdLn);

  IF pp.keywordPresent("-mp") OR pp.keywordPresent("-modelpath") THEN
    modelPath := pp.getNext()
  END;

  IF pp.keywordPresent("-runlvl") THEN
    SimDumper.runLvl := pp.getNextInt()
  END;
  
  SimDumper.rename := pp.keywordPresent("-rename");

  IF pp.keywordPresent("-step") THEN
    simParams.step := pp.getNextLongReal()
  END;

  IF pp.keywordPresent("-maxtime") THEN
    (* no need to specify this, normally *)
    simParams.maxTime := pp.getNextLongReal() 
  END;

  WITH mr = Env.Get("hspice_model_root"),
       mm = Env.Get("hspice_model") DO
    IF mr = NIL THEN
      Debug.Error("Set hspice_model_root or override model_path with -mp")
    END;
    IF mm = NIL THEN
      Debug.Error("Set hspice_model or override modelPath with -mp")
    END;
    modelPath := mr & "/" & mm
  END;

  IF pp.keywordPresent("-vdd") THEN
    SimDumper.Vdd := pp.getNextLongReal()
  ELSE
    SimDumper.Vdd := 0.95d0
  END;

  IF pp.keywordPresent("-temp") THEN
    SimDumper.Temp := pp.getNextLongReal()
  ELSE
    SimDumper.Temp := 0.0d0
  END;

  TRY

    IF pp.keywordPresent("-f") THEN 
      WITH nm = pp.getNext() DO
        FOR i := FIRST(Sim.T) TO LAST(Sim.T) DO
          IF TE(nm, Sim.Names[i]) THEN sim := i END
        END
      END
    END;

    IF pp.keywordPresent("-o") THEN
      outPfx := pp.getNext()
    END;

    IF pp.keywordPresent("-m") THEN
      modelName := pp.getNext()
    END;

    IF pp.keywordPresent("-probemode") OR pp.keywordPresent("-pm") THEN
      WITH pm = pp.getNext() DO
        FOR i := FIRST(ProbeMode.Names) TO LAST(ProbeMode.Names) DO
          IF TE(ProbeMode.Names[i],pm) THEN
            probeMode := i
          END
        END
      END
    END

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  (* we need to break this up as follows 

     1. pass pp to TheSimulation, for simulation-related flags

     2. break up the SimDumper.DumpIt so that probes can be derived
        from assertions

     Things to control:

     probe mode         - digital y*
     process corner     - digital y
     CFG bits           - digital 
     test program       - digital
     assertion gen mode - digital
     RC modes           - digital ?
     simulator          - digital y
     Vdd                          y
     clock speed                  y
     setup/hold?                  y
     temperature                  y
     simulator timestep(poststep) y
     duty cycle?
     noise?
     thresholds?
  *)

  TheSimulation.Build(pp, simParams);

  pp.finish();

  WITH wr    = FileWr.Open(outPfx & ".spice"),
       model = TheSimulation.GetModel(),
       clk   = TheSimulation.ClockName,
       aWr   = FileWr.Open(outPfx & ".ass"),
       sWr   = FileWr.Open(outPfx & ".simulator") DO
    SimDumper.DumpIt(wr, 
                     simParams, sim, 
                     probeMode, 
                     modelName, modelPath);

    (* must call after SimDumper.DumpIt *)
    WITH ass =    SimDumper.AddDigitalModel(model, clk, aWr) DO
      SimDumper.FinishDump(wr, probeMode, ass)
    END;

    Wr.PutText(sWr, Sim.Names[sim]); Wr.PutChar(sWr, '\n');
    Wr.Close(wr);
    Wr.Close(aWr);
    Wr.Close(sWr)
  END

END Main.
