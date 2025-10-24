(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TcamModel;
IMPORT Tcam;
IMPORT SimModel;

TYPE
  T <: Public;

  Public = SimModel.T OBJECT METHODS
    init(READONLY c : Tcam.T;
         cycleTime, assertHoldFrac, assertHoldTime : LONGREAL;
         namingConvention : NamingConvention) : T;
  END;

  NamingConvention = { Andrew (* upper case etc *),
                       SDG    (* lower case etc *) };

CONST Brand = "TcamModel";

END TcamModel.
    
