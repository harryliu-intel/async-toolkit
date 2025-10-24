(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE HlpStageModelServer = StageModelServer(StageModelServer, HlpModel, hlp_top_map, hlp_top_map_addr)
END HlpStageModelServer.
