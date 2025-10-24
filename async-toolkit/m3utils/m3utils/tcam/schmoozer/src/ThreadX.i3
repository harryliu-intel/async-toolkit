(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ThreadX;
IMPORT Thread;
IMPORT Refany;

TYPE T = Thread.T;
     
CONST Equal = Refany.Equal;
      
CONST Brand = "Thread eXtended for generics";

END ThreadX.
