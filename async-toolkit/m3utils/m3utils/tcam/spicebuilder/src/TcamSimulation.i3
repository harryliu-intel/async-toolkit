INTERFACE TcamSimulation;
IMPORT SimModel;
IMPORT ParseParams;
IMPORT SimParams;

PROCEDURE Build(pp : ParseParams.T; sp : SimParams.T);

PROCEDURE GetModel() : SimModel.T; (* generate assertions for last "Build" *)

CONST ClockName = "CLK";

END TcamSimulation.
