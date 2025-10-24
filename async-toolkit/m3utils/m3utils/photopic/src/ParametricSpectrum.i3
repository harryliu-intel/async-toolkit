(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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

PROCEDURE Vec2Scheme(vec : LRVector.T) : SchemePair.T;

PROCEDURE SetVec(to, from : LRVector.T);
  
PROCEDURE Subdivide(a : LRVector.T) : LRVector.T;

PROCEDURE MakeBlackbodyInWavelength(temp, lambda0, lambda1 : LONGREAL) : LRFunction.T;
  (* 
     this is an optimization so we don't have to go through Scheme for
     evaluations ... 

     make a truncated blackbody spectrum in the wavelength space,
     for a given temperature, truncated to [ lambda0 , lambda1 ] 
  *)
  

END ParametricSpectrum.

               
  
              
    
