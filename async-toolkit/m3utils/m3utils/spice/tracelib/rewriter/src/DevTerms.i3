(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DevTerms;

CONST MaxTerm = 255;

TYPE Index = [ 1 .. MaxTerm ];
      
TYPE
  T = RECORD
    terms : SET OF Index;
    max   : [ FIRST(Index) - 1 .. LAST(Index) ];
  END;

CONST Empty = T { SET OF Index {}, FIRST(Index) - 1 };
  
CONST Brand = "DevTerms";

END DevTerms.
     
