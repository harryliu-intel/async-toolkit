(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SpiceValues;
IMPORT CheckDir;
IMPORT CardSeq;
IMPORT TransitionFinder;

PROCEDURE GetValues(clkDirs           : CheckDir.T;
                    clkTrIdx          : CARDINAL;
                    clkNm             : TEXT;
                    thresh            : LONGREAL;
                    READONLY ta, da   : ARRAY OF LONGREAL;
                    tranFinder        : TransitionFinder.T;
                    resetTime         : LONGREAL) : CardSeq.T;


END SpiceValues.
