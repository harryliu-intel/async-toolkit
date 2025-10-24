(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: UseFortran.m3,v 1.1 2010/07/08 08:25:16 mika Exp $ *)

MODULE UseFortran; 
IMPORT Env;

PROCEDURE True() : BOOLEAN = BEGIN RETURN Env.Get("FORTRANMATH") # NIL END True;

BEGIN END UseFortran.
