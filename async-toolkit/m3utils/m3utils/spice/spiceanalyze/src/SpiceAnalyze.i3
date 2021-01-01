INTERFACE SpiceAnalyze;
IMPORT SpiceCircuit;
IMPORT TextSet;
IMPORT TextCktCellTbl;
IMPORT TextSpiceCircuitTbl;

(* process a single SUBCKT definition *)
PROCEDURE Cell(nm : TEXT;
               (* name of current SUBCKT *)
               
               ckt : SpiceCircuit.T;
               (* the SUBCKT definition *)
               
               power : PowerSets;
               (* the power node names *)
               
               hierTbl : TextCktCellTbl.T;
               (* output: hierarchy *)
               
               subCkts : TextSpiceCircuitTbl.T
               (* (global) map from type names to definitions -- to resolve
                   subcell types *)
  );
 
CONST Brand = "SpiceAnalyze";

TYPE
  Power      = { GND, Vdd };
  PowerSets  = ARRAY Power OF TextSet.T;

CONST
  PowerNames = ARRAY Power OF TEXT { "GND", "Vdd" };

END SpiceAnalyze.
