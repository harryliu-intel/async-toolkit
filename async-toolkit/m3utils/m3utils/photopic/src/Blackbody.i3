(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Blackbody;

PROCEDURE PlanckRadiance(T, nu : LONGREAL) : LONGREAL;
  (* spectral radiance in watts per square meter per steradian per hertz at 
     given temperature in kelvin and frequency in hertz *)

END Blackbody.
