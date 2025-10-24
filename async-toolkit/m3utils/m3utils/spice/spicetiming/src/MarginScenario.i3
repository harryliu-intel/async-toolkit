(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MarginScenario;
IMPORT CheckDir;
IMPORT Transition;
IMPORT Word;

TYPE
  T = RECORD
    datDir            : CheckDir.T;
    clkDir            : Transition.Dir;
    datNm             : TEXT;
    clkNm             : TEXT;
    tag               : TEXT;
  END;

CONST Brand = "MarginScenario";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;
  
PROCEDURE Format(READONLY a : T) : TEXT;
  
END MarginScenario.
