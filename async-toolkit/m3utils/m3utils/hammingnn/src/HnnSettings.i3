(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE HnnSettings;
IMPORT Hnn;
IMPORT Word;

REVEAL
  Hnn.T <: Settings;

CONST
  MaxS = 32; (* hmm... *)

TYPE
  Settings = Hnn.Public OBJECT METHODS
    setS(s : [ 1 .. MaxS ]);
  END;

END HnnSettings.

