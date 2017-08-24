INTERFACE TcamSimulationSDG64;
IMPORT SimModel;
IMPORT ParseParams;
IMPORT SimParams;

PROCEDURE Build(pp : ParseParams.T; sp : SimParams.T; modelName : TEXT)
  RAISES { ParseParams.Error };

PROCEDURE GetModel() : SimModel.T; (* generate assertions for last "Build" *)

CONST ClockName = "clk";

PROCEDURE GetVarNames(varScenarioName : TEXT;
                      modelName := "tttt") : REF ARRAY OF TEXT;

END TcamSimulationSDG64.
