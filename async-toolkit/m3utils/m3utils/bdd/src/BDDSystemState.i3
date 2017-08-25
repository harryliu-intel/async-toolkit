(* $Id: BDDSystemState.i3,v 1.2 2014/11/20 10:30:40 mika Exp $ *)

INTERFACE BDDSystemState;

TYPE SystemState <: REFANY;

PROCEDURE GetSystemState() : SystemState;

PROCEDURE SetSystemState(state : SystemState);

PROCEDURE NewDefaultSystemState() : SystemState;

END BDDSystemState.
