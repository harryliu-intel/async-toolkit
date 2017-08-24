INTERFACE SimDumper;
IMPORT Wr, SimParams;
IMPORT Dims, Intf;
IMPORT Pathname;
IMPORT Sim, TextSeq;
IMPORT SimModel;
IMPORT ProbeMode;
IMPORT AssertionList;
IMPORT SimMeasurement;

PROCEDURE DumpIt(wr : Wr.T; 
                 VAR sp : SimParams.T; 
                 sim : Sim.T; 
                 pm : ProbeMode.T;
                 modelName : TEXT;
                 modelPath : Pathname.T);

PROCEDURE SetVarModels(varModels : TEXT);

PROCEDURE FinishDump(wr : Wr.T; pm : ProbeMode.T; ass : AssertionList.T; READONLY sp : SimParams.T; sim : Sim.T);

VAR dutName : TEXT;
VAR Vdd     : LONGREAL;
VAR Temp    : LONGREAL;

PROCEDURE AddNodes(nm            : TEXT;
                   READONLY dims : Dims.T;
                   intf          : Intf.T);

PROCEDURE DeclSequence(libFile       : Pathname.T;
                       type          : TEXT;
                       READONLY args : ARRAY OF TEXT);


PROCEDURE SetDutName(nm : TEXT);

PROCEDURE AddDigitalModel(model : SimModel.T; clockNm : TEXT; aWr : Wr.T; VAR sp : SimParams.T) : AssertionList.T;

VAR simExtras : ARRAY Sim.T OF TextSeq.T;

VAR runLvl : [1..6] := 5; (* hspice run level *)
    
VAR rename := FALSE;

PROCEDURE Renamer(txt : TEXT) : TEXT;

PROCEDURE SetInterfaceNodeSequence(seq : TextSeq.T);
  (* set the correct order of interface nodes to override the default
     order *)

TYPE ProbeType = SimMeasurement.ProbeType;

PROCEDURE AddProbes(type : ProbeType; to : TEXT);

PROCEDURE AddMeasurement(m : SimMeasurement.T);
  
END SimDumper.
