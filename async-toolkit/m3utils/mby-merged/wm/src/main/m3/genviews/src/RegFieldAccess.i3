(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegFieldAccess;
(* RDL Spec section 7.4 *)
IMPORT RdlPredefProperty;
IMPORT ParseError;

TYPE
  Master = { SW, HW };

  Op     = { R, W };

  OpSet = SET OF Op;

  N = RECORD nm : TEXT; opSet : OpSet END;

  T = ARRAY Master OF OpSet;

CONST
  Brand = "RegFieldAccess";

  Default = T { RW, RW };

CONST
  NA = OpSet {            };
  R  = OpSet { Op.R       };
  W  = OpSet {       Op.W };
  RW = OpSet { Op.R, Op.W };

CONST
  Named = ARRAY OF N { N { "na", NA },
                       N { "r" , R  },
                       N { "w" , R  },
                       N { "rw", RW } };
CONST 
  MasterProperty =
    ARRAY Master OF RdlPredefProperty.T {    
      RdlPredefProperty.T.sw,
      RdlPredefProperty.T.hw };

PROCEDURE Parse(named : TEXT) : OpSet RAISES { ParseError.E };
  
END RegFieldAccess.
