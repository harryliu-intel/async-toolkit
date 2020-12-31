INTERFACE SpiceAnalyze;
IMPORT SpiceCircuit;
IMPORT TextSet;
IMPORT TextCktCellTbl;
IMPORT TextSpiceCircuitTbl;

(* process a single SUBCKT definition *)
PROCEDURE Cell(nm : TEXT;
               ckt : SpiceCircuit.T;
               power : PowerSets;
               hierTbl : TextCktCellTbl.T;
               subCkts : TextSpiceCircuitTbl.T
  );
 
CONST Brand = "SpiceAnalyze";

TYPE
  Power      = { GND, Vdd };
  PowerSets  = ARRAY Power OF TextSet.T;

CONST
  PowerNames = ARRAY Power OF TEXT { "GND", "Vdd" };

END SpiceAnalyze.
