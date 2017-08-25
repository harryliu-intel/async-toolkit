(* $Id: SetToList.ig,v 1.1 2014/02/09 11:16:08 mika Exp $ *)

GENERIC INTERFACE SetToList(Base, BaseSet);
IMPORT SchemePair;

PROCEDURE SetToList(set : BaseSet.T) : SchemePair.T;

PROCEDURE SetToArr(set : BaseSet.T) : REF ARRAY OF Base.T;

CONST Brand = "SetToList(" & BaseSet.Brand &")";

END SetToList.
