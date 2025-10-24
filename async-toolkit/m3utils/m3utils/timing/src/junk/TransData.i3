(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TransData;

TYPE
  
  DelayAttr = { BackAnnotated, Fanout1000 };
  Attribute = { dont_touch, dont_use, map_only, size_only, ideal_net };

  T = OBJECT
  END;

  T = RECORD
    label : TEXT;
    paren : TEXT;
    fanout : CARDINAL;
    trans : LONGREAL;
    incr : LONGREAL;
    delayAttrs : SET OF DelayAttr;
    path : LONGREAL;
    attrs : SET OF Attribute;
    voltage : LONGREAL;
  END;

CONST Brand = "TransData";

END TransData.
