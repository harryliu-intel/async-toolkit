(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MbyMapperToClassifier;
IMPORT MbyTypes;
IMPORT MbyFfuTypes;

TYPE
  T = RECORD
    paError          :                    BOOLEAN;
    ffuKeys          :                    MbyFfuTypes.Keys;
    ffuActions       :                    MbyFfuTypes.Actions;
    ffuScenario      :                    MbyFfuTypes.Scenario;
    ffuVrid          :                    MbyFfuTypes.Vrid;
    ipOption         : ARRAY [0.. 2-1] OF BOOLEAN;
    priorityProfile  :                    MbyFfuTypes.PriorityProfile;
    noPriEnc         :                    BOOLEAN;
    learnMode        :                    BOOLEAN;
    l2Ivlan1CntIndex :                    MbyTypes.L2IvlanCntIndex;
  END;

CONST Brand = "MbyMapperToClassifier";

END MbyMapperToClassifier.
