(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE MbyStageModelServer = StageModelServer(StageModelServer,
                                              MbyModelC, mby_top_map, mby_top_map_addr, MbyModelStagesC)
END MbyStageModelServer.
