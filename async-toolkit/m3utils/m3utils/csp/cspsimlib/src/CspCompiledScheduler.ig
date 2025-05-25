GENERIC INTERFACE CspCompiledScheduler();
IMPORT CspCompiledProcess AS Process;
IMPORT Word;

PROCEDURE Schedule(closure : Process.Closure);
PROCEDURE ScheduleFork(READONLY closures : ARRAY OF Process.Closure) : CARDINAL;

PROCEDURE Run();

CONST SchedulingLoop = Run;

PROCEDURE GetTime() : Word.T;
  
CONST Release = Schedule;
CONST ReleaseFork = ScheduleFork;

END CspCompiledScheduler.
