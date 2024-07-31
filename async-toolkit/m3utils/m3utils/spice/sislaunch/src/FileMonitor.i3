INTERFACE FileMonitor;
IMPORT Pathname;
IMPORT Time;

(* monitor a file containing an integer for updates *)

TYPE
  ReturnType = CARDINAL;
  
  T <: Public;

  Public = OBJECT METHODS
    init(path     : Pathname.T;
         cb       : Callback := NIL;
         interval : Time.T   := DefInterval;
         defValue : INTEGER  := DefValue) : T;

    registerCallback(cb : Callback);

    poll() : ReturnType;
  END;

  Callback = OBJECT METHODS
    changed(newVal : ReturnType); (* mu is locked during call *)
  END;

CONST DefInterval = 5.0d0;

      DefValue    = MAX(FIRST(ReturnType) - 1, FIRST(INTEGER));

CONST Brand = "FileMonitor";

END FileMonitor.
