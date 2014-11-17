(* $Id$ *)

INTERFACE BDDSystemState;

TYPE SystemState <: REFANY;

PROCEDURE GetSystemState() : SystemState;

PROCEDURE SetSystemState(state : SystemState);

END BDDSystemState.
