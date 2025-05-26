GENERIC INTERFACE CspCompiledScheduler();
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
IMPORT CspPortObject;

PROCEDURE Schedule(closure : Process.Closure);
  (* schedule a closure in the current process to run *)
  
PROCEDURE ScheduleFork(READONLY closures : ARRAY OF Process.Closure) : CARDINAL;
  (* schedule a list of closures, for a fork, in the current process to run *)

PROCEDURE ScheduleOther(from, toSchedule : Process.Closure);
  (* schedule a block in another process to run *)
    
PROCEDURE Run();

CONST SchedulingLoop = Run;

PROCEDURE GetTime() : Word.T;
  
CONST Release = Schedule;
CONST ReleaseFork = ScheduleFork;

PROCEDURE RegisterProcess(proc : Process.Frame);

PROCEDURE RegisterEdge(edge : CspPortObject.T);

END CspCompiledScheduler.
