INTERFACE CspSim;
IMPORT CspCompiledProcess AS Process;
IMPORT CspPortObject;
IMPORT TextFrameTbl;
IMPORT TextPortTbl;
IMPORT CspFrameSeq AS FrameSeq;
IMPORT TextSet;

TYPE Builder = PROCEDURE(restrict : TextSet.T);

CONST Brand = "CspSim";

PROCEDURE RegisterProcess(proc : Process.Frame);

PROCEDURE RegisterClosure(cl : Process.Closure);

PROCEDURE RegisterClosures(READONLY cls : ARRAY OF Process.Closure);

PROCEDURE RegisterEdge(edge : CspPortObject.T);

PROCEDURE GetProcTbl() : TextFrameTbl.T;

PROCEDURE GetProcSeq() : FrameSeq.T;

PROCEDURE GetPortTbl() : TextPortTbl.T;

PROCEDURE GetAllProcNames() : REF ARRAY OF TEXT;

PROCEDURE GetFrame(nm : TEXT) : Process.Frame;

END CspSim.
