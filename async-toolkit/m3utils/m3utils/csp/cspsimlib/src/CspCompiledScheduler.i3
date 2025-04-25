INTERFACE CspCompiledScheduler;
IMPORT CspCompiledProcess AS Process;
IMPORT Word;

PROCEDURE Schedule(closure : Process.Closure);

PROCEDURE Run();

CONST SchedulingLoop = Run;

PROCEDURE GetTime() : Word.T;
  
CONST Release = Schedule;

END CspCompiledScheduler.
