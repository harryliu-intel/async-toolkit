(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspCompilerDriver;
IMPORT Pathname;
IMPORT TextCspPortSeqTbl;
IMPORT OSError, Rd;
IMPORT TextSeq;

EXCEPTION SyntaxError;
          
TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(procGraphPath : Pathname.T) : T
      RAISES { OSError.E, Rd.Failure, SyntaxError } ;
    (* Phase 0. initialize from a process graph file *)

    getProcTypes() : TextSeq.T;
    (* Phase 1. return list of process types depended on *)

    setProcessPorts(tbl : TextCspPortSeqTbl.T);
    (* Phase 2. pass in a table of the process port lists *)

    genBuilder(named : TEXT; defaultSlack : CARDINAL) : TEXT;
    (* generate a PROCEDURE that will build the process graph as
       required *)
  END;

CONST Brand = "CspCompilerDriver";
      
END CspCompilerDriver.
   
