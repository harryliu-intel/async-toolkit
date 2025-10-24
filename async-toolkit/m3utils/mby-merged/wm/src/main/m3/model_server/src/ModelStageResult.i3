(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ModelStageResult;
IMPORT ServerPacket AS Pkt;
IMPORT Metadata;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    push(opkt : Pkt.T; om : Metadata.T);
  END;
  
CONST Brand = "ModelStageResult";

END ModelStageResult.
