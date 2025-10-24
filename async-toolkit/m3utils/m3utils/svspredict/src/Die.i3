(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Die;
IMPORT Word;

TYPE
  Value = { Sigma, Power, DeltaV };
  
  T = ARRAY Value OF LONGREAL;

  Array = ARRAY OF T;

  (* sigma is the process sigma of the die *)
  (* p is the power of the die under some spec'd conditions *)
  (* deltaV is the voltage margin of the die under some spec'd conditions *)

CONST Brand = "Die";

TYPE CompRes = [-1..1];
     
PROCEDURE ComparePower(READONLY a, b : T) : CompRes;
  (* = CompareV(Value.Power, a, b) *)
  
PROCEDURE CompareSigma(READONLY a, b : T) : CompRes;
  (* = CompareV(Value.Sigma, a, b) *)
  
PROCEDURE CompareDeltaV(READONLY a, b : T) : CompRes;
  (* = CompareV(Value.DeltaV, a, b) *)
  
PROCEDURE CompareV(value : Value; READONLY a, b : T) : CompRes;

CONST Compare = ComparePower;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE ExtractValues(READONLY arr : Array; val : Value) : REF ARRAY OF LONGREAL;

PROCEDURE CutoffArr(READONLY a : Array;
                    value      : Value;
                    max        : LONGREAL;
                    higher     : BOOLEAN) : REF Array;

END Die.
  
