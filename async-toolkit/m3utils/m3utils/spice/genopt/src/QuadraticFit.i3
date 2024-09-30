INTERFACE QuadraticFit;
IMPORT LRVector;

(* positive definite quadratic fit *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(dims : CARDINAL; rho : LONGREAL) : T;

    addPoint(p : LRVector.T;         (* coordinate *)
             y : LONGREAL;           (* response *)
             w : LONGREAL := 1.0d0   (* weight *)
    );

    pred(p : LRVector.T) : LONGREAL; (* predicted value at p *)

    getState() : LRVector.T;         (* the full set of params in the order
                                         { x0, L, C } *)

    setState(to : LRVector.T);       (* from prev. call of getState *)  
    
    getMinimum() : LRVector.T;       (* get x0 *)
    
  END;

CONST Brand = "QuadraticFit";

END QuadraticFit.
    
