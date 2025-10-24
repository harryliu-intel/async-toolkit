(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE HlpRepStageModel;
IMPORT Coroutine;
IMPORT ServerPacket AS Pkt;
IMPORT hlp_top_map_addr AS TopAddr;
IMPORT Metadata;
IMPORT ModelStageResult;

TYPE
  Indices = RECORD
  END;

PROCEDURE HandlePacket(ipkt    : Pkt.T;
                       h       : TopAddr.H;
                       indices : Indices;
                       imd     : Metadata.T;
                       out     : ModelStageResult.T);

TYPE Meta = Metadata.T;

END HlpRepStageModel.
