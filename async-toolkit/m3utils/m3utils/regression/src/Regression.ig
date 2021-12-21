(* $Id: Regression.ig,v 1.1 2008/01/22 08:23:52 mika Exp $ *)

GENERIC INTERFACE Regression(M);

(* M.Base is REAL, LONGREAL, or EXTENDED *)

TYPE 
  T = OBJECT
    mean_sq_y, s_sq_dev : M.Base;
    b : REF M.M;
    R_sq, F : M.Base;
  END;

PROCEDURE Run(x, y : REF M.M;
              (* OUT *) VAR yHat : REF M.M;
              debug : BOOLEAN;
              data : T;
              h := FLOAT(0, M.Base));


PROCEDURE RunR(x, y : REF M.M; 
               yHat : REF M.M;
               debug : BOOLEAN;
               data : T;
               VAR r : Recycler;
               h := FLOAT(0, M.Base));
  (* if you use this one, all the data structures will be shared via
     the recycler.  Must be careful to use it only for x and y of the
     same dimensions as the one that was used to allocate.  On the
     first call to RunR with a given x and y dimensions, r should be NIL.

     yHat will be shared via the recycler, as will data.b (so they
     must be copied if they are to be re-used) *)

PROCEDURE RunR1(READONLY x : M.M;
                READONLY y : M.V;
                VAR yHat_c : M.V;
                debug : BOOLEAN;
                data : T;
                VAR recycler : Recycler;
                h := FLOAT(0, M.Base));
  (* same as above, but with Vector *)

TYPE Recycler <: ROOT;

CONST Brand = "Regression(" & M.Brand & ")";

PROCEDURE RidgeRegress(READONLY x : M.M; 

                       h          : M.Base;

                       VAR res    : M.M (* OUT : ((xTx + h^2 I)^-1)(xT) *);

                       indx       : REF ARRAY OF INTEGER;
                       (* scratch, cols of x *)

                       VAR xTx    : M.M;
                       (* size cols of x, square *)
                       
                       debug := FALSE);
  

END Regression.
