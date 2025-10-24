(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MbyParserToModifierMeta;
IMPORT Metadata;
IMPORT MbyParserInfo;

TYPE
  T = Metadata.T OBJECT
    info : MbyParserInfo.T;
  END;

CONST Brand = "MbyParserToModifierMeta";

END MbyParserToModifierMeta.
