(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE YieldModel;

PROCEDURE Stapper(A, D0, n, alpha : LONGREAL) : LONGREAL;

PROCEDURE BoseEinstein(A, D0, n : LONGREAL) : LONGREAL;

PROCEDURE Poisson(A, D0, n : LONGREAL) : LONGREAL;

PROCEDURE GammaDistPdf(alpha, beta, x : LONGREAL) : LONGREAL;
  (* p.d.f. that Stapper used for his distribution 

     note that Stapper's beta is 1/beta = theta in Wikipedia
  *)

PROCEDURE GammaDistCdf(alpha, beta, x : LONGREAL) : LONGREAL;
  (* c.d.f. that Stapper used for his distribution 

     note that Stapper's beta is 1/beta = theta in Wikipedia
  *)

CONST Brand = "YieldModel";

END YieldModel.
