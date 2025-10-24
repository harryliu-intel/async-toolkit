(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TcamPrograms;
IMPORT CommandSeq, Tcam;
IMPORT ParseParams;

TYPE Type = { Def, Short, Minimal, ReadWrite, AllMiss, Var, SingleHit };
CONST
  Names = ARRAY Type OF TEXT { "def",
                               "short",
                               "minimal",
                               "rw",
                               "allmiss",
                               "var",
                               "singlehit"
  };

TYPE Proc = PROCEDURE(prog : CommandSeq.T; params : Tcam.T);

VAR Progs : ARRAY Type OF Proc;

PROCEDURE ParseFlag(pp : ParseParams.T) : Type RAISES { ParseParams.Error };

PROCEDURE WhichProgram() : Type;
  
END TcamPrograms.
