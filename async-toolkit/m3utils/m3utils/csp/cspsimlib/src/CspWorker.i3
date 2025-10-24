(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspWorker;
IMPORT IP;
IMPORT Thread;
IMPORT CspSim;
IMPORT TextCardTbl;
IMPORT TextRemoteChannelTbl;
IMPORT CspRemote;

TYPE
  T <: Public;

  Public = CspRemote.T OBJECT METHODS
    init(id : CARDINAL; bld : CspSim.Builder) : T;

    getEp() : IP.Endpoint;

    getThread() : Thread.T;

    awaitInitialization();
    (* network is initialized *)

    getProcMap() : TextCardTbl.T;
    (* mapping of processes to scheduler id *)

    getChannelData() : TextRemoteChannelTbl.T;

    getId() : CARDINAL;
  END;

CONST Brand = "CspWorker";

END CspWorker.
