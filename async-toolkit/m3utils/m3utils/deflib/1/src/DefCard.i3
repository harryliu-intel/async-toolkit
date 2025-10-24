(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DefCard;
IMPORT DefNumbers;

TYPE T = CARDINAL;

CONST Get = DefNumbers.GetCard;
CONST MustBe = DefNumbers.MustBeCard;
CONST MustGet = DefNumbers.MustGetCard;

CONST Brand = "DefCard";

END DefCard.
