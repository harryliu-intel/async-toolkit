(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspRemote;

(* parent object of CspWorker and CspMaster *)

TYPE
  T <: Public;

  Public = OBJECT
    nworkers : CARDINAL; (* how big is the network *)
    mt       : CARDINAL; (* how many scheduler threads per worker *)
  METHODS
    gid2wid(gid : CARDINAL) : CARDINAL; (* global to worker id *)
    gid2sid(gid : CARDINAL) : CARDINAL; (* global to scheduler id *)
  END;

CONST Brand = "CspRemote";

END CspRemote.
