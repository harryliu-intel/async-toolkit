(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE ModelStageResult;
IMPORT ModelStageResultClass;
IMPORT Coroutine;
IMPORT ServerPacket AS Pkt;
IMPORT Metadata;

REVEAL
  T = ModelStageResultClass.Private BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
    push := Push;
  END;

PROCEDURE Init(t : T; co : Coroutine.T) : T =
  BEGIN
    t.opkt := NIL;
    t.co := co;
    RETURN t
  END Init;

PROCEDURE Push(t : T; opkt : Pkt.T; om : Metadata.T) =
  BEGIN
    t.opkt := opkt;
    t.om := om;
    EVAL Coroutine.Call(t.co)
  END Push;

BEGIN END ModelStageResult.
