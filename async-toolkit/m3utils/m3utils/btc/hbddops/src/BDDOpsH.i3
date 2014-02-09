(* $Id$ *)

INTERFACE BDDOpsH;
IMPORT BDDSet, BDD;
IMPORT Word;
IMPORT SopBDD;

PROCEDURE AccumulateBDD(s  : BDDSet.T; 
                        op : PROCEDURE(a, b : BDD.T) : BDD.T;
                        init : BDD.T) : BDD.T;

PROCEDURE MakeCntrBdd(READONLY a : ARRAY OF BDD.T; cntr : Word.T) : BDD.T;

PROCEDURE XFormat(x : BDD.T; inQuotes := FALSE) : TEXT;

PROCEDURE PfxFormat(x : BDD.T; prefix : TEXT; inQuotes : BOOLEAN; aliasMapper : SopBDD.AliasMapper) : TEXT;

PROCEDURE InfixFormatSet(x : BDDSet.T; prefix : TEXT; inQuotes : BOOLEAN; aliasMapper : SopBDD.AliasMapper; op : TEXT) : TEXT;

PROCEDURE Substitute(in, v, by : BDD.T) : BDD.T;
  (* substitute via Shannon cofactor expansion *)

END BDDOpsH.
