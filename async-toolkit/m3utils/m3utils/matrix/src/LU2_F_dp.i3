(* $Id: LU2_F_dp.i3,v 1.1 2008/02/02 11:20:22 mika Exp $ *)

INTERFACE LU2_F_dp;
IMPORT LRMatrix2 AS M;

PROCEDURE BackSubstitute(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V);

END LU2_F_dp.
