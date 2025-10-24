INTERFACE TcamSimulation;
IMPORT SimModel;
IMPORT ParseParams;
IMPORT SimParams;

PROCEDURE Build(pp : ParseParams.T; sp : SimParams.T; modelName : TEXT)
  RAISES { ParseParams.Error };

PROCEDURE GetModel() : SimModel.T; (* generate assertions for last "Build" *)

CONST ClockName = "CLK";

END TcamSimulation.
