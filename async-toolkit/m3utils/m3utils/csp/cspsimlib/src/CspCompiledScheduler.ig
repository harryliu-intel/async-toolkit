GENERIC INTERFACE CspCompiledScheduler();

(* 
   this is GENERIC because the implementation MODULE depends on CspDebug,
   which allows debugging to be controlled at compile time 
*)

IMPORT CspCompiledProcess AS Process;
IMPORT Word;
IMPORT CspPortObject;
IMPORT TextFrameTbl;
IMPORT TextPortTbl;
IMPORT CspChannel;

(* The following invariant must always be maintained: 

   a single CSP process is tied to a specific Scheduler, 

   -- denoted by the "affinity" field in the Process Frame 
 *)

PROCEDURE Schedule(closure : Process.Closure);
  (* schedule a closure in the current CSP process to run *)
  
PROCEDURE ScheduleFork(READONLY closures : ARRAY OF Process.Closure) : CARDINAL;
  (* schedule a list of closures, for a fork, in the current process to run *)

PROCEDURE ScheduleComm(from, toSchedule : Process.Closure);
  (* schedule a block in another CSP process to run owing to a communication *)
  
PROCEDURE ScheduleWait(from, toSchedule : Process.Closure);
  (* schedule a block in another CSP process to run owing to a select *)
  
    
PROCEDURE Run(mt : CARDINAL := 0; greedy := FALSE);

CONST SchedulingLoop = Run;

PROCEDURE GetTime() : Word.T;
  
CONST Release = Schedule;
CONST ReleaseFork = ScheduleFork;

PROCEDURE RegisterProcess(proc : Process.Frame);

PROCEDURE RegisterEdge(edge : CspPortObject.T);

PROCEDURE GetProcTbl() : TextFrameTbl.T;

PROCEDURE GetPortTbl() : TextPortTbl.T;

PROCEDURE ReadDirty(chan : CspChannel.T; cl : Process.Closure);
  (* a channel with a surrogate has been read from *)
  
PROCEDURE WriteDirty(chan : CspChannel.T; cl : Process.Closure);
  (* a channel surrogate has been written to *)
  
END CspCompiledScheduler.
