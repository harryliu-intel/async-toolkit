(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspMaster;
IMPORT CspSim;
IMPORT CspRemote;

TYPE
  T <: Public;

  Public = CspRemote.T OBJECT METHODS
    init(nworkers : CARDINAL;
         cmd      : TEXT;
         bld      : CspSim.Builder;
         mt       : CARDINAL        ) : T;
    run();
  END;

CONST Brand = "CspMaster";

END CspMaster.
