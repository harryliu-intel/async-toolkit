(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StdfTypes;
IMPORT StdfU1, StdfU2, StdfU4, StdfB1, StdfI2, StdfI4, StdfDn, StdfCn, StdfN1,
       StdfVn, StdfR4;

TYPE U1 = StdfU1.T;
     U2 = StdfU2.T;
     U4 = StdfU4.T;
     B1 = StdfB1.T;
     I2 = StdfI2.T;
     I4 = StdfI4.T;
     Dn = StdfDn.T;
     Cn = StdfCn.T;
     N1 = StdfN1.T;
     Vn = StfdVn.T;
     R4 = StdfR4.T;

CONST Brand = "StdfTypes";

END StdfTypes.
