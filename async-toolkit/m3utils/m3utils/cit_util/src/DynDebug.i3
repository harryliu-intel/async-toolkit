INTERFACE DynDebug;

(* dynamic debugging *)

PROCEDURE DebugThis(moduleName : TEXT;
                    minLevel   : CARDINAL := 0) : REF BOOLEAN;

END DynDebug.
