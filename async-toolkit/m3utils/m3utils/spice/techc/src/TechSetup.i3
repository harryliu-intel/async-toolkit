(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TechSetup;

IMPORT TechConfig;
FROM TechConfig IMPORT Simu;
IMPORT TextTextTbl;

PROCEDURE DoSetup(READONLY c : TechConfig.T);

CONST
  CommonOptions =
    ".OPTION CMIFLAG=1 CMIUSRFLAG=3 PDMI=1\n"  &
    ".OPTION POST=fsdb PROBE=1\n";

  XaOptions =
    CommonOptions & 
    ".OPTION XA_CMD=\"set_sim_level -level 6\"\n" &
    ".OPTION XA_CMD=\"set_wildcard_rule -match* one\"\n" &
    ".OPTION XA_CMD=\"set_message_option -limit 100\"\n" &
    ".OPTION XA_CMD=\"enable_print_statement 1\"\n" &
    ".OPTION XA_CMD=\"set_sim_case -case sensitive\"";

  HspiceOptions = CommonOptions &
    ".OPTION measform=2";

  SimOptions = ARRAY Simu OF TEXT { XaOptions, HspiceOptions };

VAR
  extraMap, overrideMap : TextTextTbl.T;
  
END TechSetup.
