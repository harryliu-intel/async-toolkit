(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TechConvert;
IMPORT Pathname;
IMPORT Watchdog;
IMPORT TextWr;
IMPORT TechConfig;
IMPORT OSError;

CONST (* paths relative to $M3UTILS *)
  CtPath = "spice/ct/AMD64_LINUX/ct";

  NanosimrdPath = "spice/fsdb/src/nanosimrd";

  SpicestreamPath = "spice/spicecompress/spicestream/AMD64_LINUX/spicestream";

PROCEDURE DoConvert(READONLY c : TechConfig.T;
                    traceRoot  : Pathname.T; exitOnError : BOOLEAN) : BOOLEAN;

TYPE
  MyCb <: PublicCb;

  PublicCb = Watchdog.Callback OBJECT
    cmd : TEXT;
    wr  : TextWr.T;
  END;

PROCEDURE FindFsdbInDir(dir : Pathname.T) : Pathname.T RAISES { OSError.E };
 
END TechConvert.
