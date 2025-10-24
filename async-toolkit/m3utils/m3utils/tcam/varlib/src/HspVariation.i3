(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE HspVariation;
IMPORT Variation;

TYPE
  T <: Public;

  Public = Variation.T OBJECT METHODS
    getVars(fromLib, baseTranTypeName : TEXT) : REF ARRAY OF TEXT;
  END;
  
CONST Brand = "HspVariation";

CONST GeoVars = ARRAY OF TEXT { "M", "Ldrawn", "Wdrawn", "NF", "scale" };

CONST FixedParams = ARRAY OF TEXT { "widtnflag" };
      
END HspVariation.
