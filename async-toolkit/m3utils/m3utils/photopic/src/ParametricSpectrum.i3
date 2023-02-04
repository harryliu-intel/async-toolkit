INTERFACE ParametricSpectrum;
IMPORT LRVector;
IMPORT LRFunction;
IMPORT SchemePair;

TYPE
  T <: ROOT;

PROCEDURE New(l0, l1          : LONGREAL; (* lo, hi wavelengths *)

              nparams         : CARDINAL;

              baseSpectrum    : LRFunction.T
              (* the base spectrum to modify *)
              ) : T;

PROCEDURE ZeroP(t : T) : LRVector.T;
  (* get a zero point out of the spectrum *)

PROCEDURE ModifyV(v : LRVector.T; idx : CARDINAL; to : LONGREAL) : LONGREAL;
  
PROCEDURE GetFunc(t : T;
               p : LRVector.T (* parameters *)
              ) : LRFunction.T;
  (* returns a modified spectrum *)

PROCEDURE Scheme2Vec(lst : SchemePair.T) : LRVector.T;

PROCEDURE SetVec(to, from : LRVector.T);
  
END ParametricSpectrum.
               
  
              
    
