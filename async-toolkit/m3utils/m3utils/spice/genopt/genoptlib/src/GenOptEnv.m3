(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE GenOptEnv;
IMPORT Env;

BEGIN
  NbPool  := Env.Get("NBPOOL");
  NbQslot := Env.Get("NBQSLOT");
  M3Utils := Env.Get("M3UTILS");
  NbOpts  := Env.Get("NBRUNOPTIONS");
END GenOptEnv.

