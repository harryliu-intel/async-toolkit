(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ClockSpec;
IMPORT Tyme;

TYPE
  T = RECORD
    f : Tyme.T;
    e : Tyme.T; (* for now, should be Allan diagram? *)

    (* f1588:fi::d:n *)
    d : CARDINAL; 
    n : CARDINAL;
  END;

CONST Brand = "ClockSpec";

END ClockSpec.
