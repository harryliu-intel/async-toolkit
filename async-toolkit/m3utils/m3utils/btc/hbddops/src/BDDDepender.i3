(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: BDDDepender.i3,v 1.2 2015/02/14 03:49:10 mika Exp $ *)

INTERFACE BDDDepender;
IMPORT BDD, BDDSet;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init     () : T;
    depends  (b : BDD.T) : BDDSet.T;
    isPlainOr(b : BDD.T) : BOOLEAN;
    flush    (b : BDD.T);
  END;

PROCEDURE AllocIfNull(VAR dep : T);

CONST Brand = "BDDDepender";

END BDDDepender.
