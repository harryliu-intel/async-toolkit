INTERFACE SpiceAnalyze;
IMPORT SpiceCircuit;
IMPORT TextSet;
IMPORT TextCktCellTbl;
IMPORT TextSpiceCircuitTbl;
IMPORT Pathname;

(* process a single SUBCKT definition *)
PROCEDURE Cell(nm : TEXT;
               (* name of current SUBCKT *)
               
               ckt : SpiceCircuit.T;
               (* the SUBCKT definition *)
               
               power : PowerSets;
               (* the power node names *)
               
               hierTbl : TextCktCellTbl.T;
               (* output: hierarchy *)
               
               subCkts : TextSpiceCircuitTbl.T;
               (* (global) map from type names to definitions -- to resolve
                  subcell types *)

               outDir : Pathname.T
               (* directory whither to put output result files *)
  );
 
CONST Brand = "SpiceAnalyze";

TYPE
  Power      = { GND, Vdd };
  PowerSets  = ARRAY Power OF TextSet.T;

CONST
  PowerNames = ARRAY Power OF TEXT { "GND", "Vdd" };

TYPE
  TransistorType = { N, P, Unknown };

CONST
  TransistorTypeNames = ARRAY TransistorType OF TEXT { "N", "P", "Unknown" };
     
PROCEDURE DecodeTransistorTypeName(tn : TEXT) : TransistorType;

END SpiceAnalyze.
