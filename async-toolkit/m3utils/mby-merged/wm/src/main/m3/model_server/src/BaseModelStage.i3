(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE BaseModelStage;
IMPORT Metadata;
IMPORT ServerPacket AS Pkt;
IMPORT Coroutine;

TYPE
  T = OBJECT
  METHODS
    poll (VAR out : Pkt.T; VAR meta : Metadata.T) : BOOLEAN;
  END;

CONST Brand = "BaseModelStage";

END BaseModelStage.
