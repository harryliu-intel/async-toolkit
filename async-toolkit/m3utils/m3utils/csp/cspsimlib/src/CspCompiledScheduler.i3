INTERFACE CspCompiledScheduler;
IMPORT CspCompiledProcess AS Process;

PROCEDURE Schedule(closure : Process.Closure);

PROCEDURE Run();

CONST Release = Schedule;

END CspCompiledScheduler.
