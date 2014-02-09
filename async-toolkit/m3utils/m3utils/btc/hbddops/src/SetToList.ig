(* $Id$ *)

GENERIC INTERFACE SetToList(Base, BaseSet);
IMPORT SchemePair;

PROCEDURE SetToList(set : BaseSet.T) : SchemePair.T;

PROCEDURE SetToArr(set : BaseSet.T) : REF ARRAY OF Base.T;

CONST Brand = "SetToList(" & BaseSet.Brand &")";

END SetToList.
