INTERFACE CspSim;
IMPORT CspCompiledProcess AS Process;
IMPORT CspPortObject;
IMPORT TextFrameTbl;
IMPORT TextPortTbl;

TYPE Builder = PROCEDURE();

CONST Brand = "CspSim";

PROCEDURE RegisterProcess(proc : Process.Frame);

PROCEDURE RegisterClosure(cl : Process.Closure);

PROCEDURE RegisterClosures(READONLY cls : ARRAY OF Process.Closure);

PROCEDURE RegisterEdge(edge : CspPortObject.T);

PROCEDURE GetProcTbl() : TextFrameTbl.T;

PROCEDURE GetPortTbl() : TextPortTbl.T;


END CspSim.
