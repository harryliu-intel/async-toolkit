(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Rotation;

PROCEDURE Matrix(READONLY dir : v3; theta : Rads; VAR m4 : M4);

CONST Brand = "Rotation";
      
END Rotation.
