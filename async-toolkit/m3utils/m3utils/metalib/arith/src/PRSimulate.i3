INTERFACE PRSimulate;
IMPORT PRS, Name, ArithP, Transition;
IMPORT Scenario;

PROCEDURE Set(set : PRS.PRS; node : Name.T; to : BOOLEAN; at : ArithP.T);

PROCEDURE Get(set : PRS.PRS; node : Name.T) : Transition.T;
(* NIL if no value yet *)

TYPE Result = { Deadlock, EndOfScenario };

PROCEDURE Cycle(set : PRS.PRS; 
                scenario : Scenario.T; 
                minMult : LONGREAL) : Result;
  (* minMult is the multiplicative margin, e.g., 0.9 *)

END PRSimulate.
