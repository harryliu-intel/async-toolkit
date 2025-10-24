(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SpiceFlat;
IMPORT Wr;
IMPORT TextRefTbl;
IMPORT TextTextSetTbl;
IMPORT TextSeq;
IMPORT TextTextTbl;
IMPORT SpiceCircuit;
IMPORT SpiceInstance;
IMPORT TextSpiceInstanceSetTbl;
IMPORT TextSpiceCircuitTbl;

PROCEDURE Visit(nm      : TEXT; 
                wr      : Wr.T; 
                ckt     : SpiceCircuit.T;
                subCkts : TextSpiceCircuitTbl.T;
                level   : CARDINAL := 0)
  RAISES { Wr.Failure } ;
  (* print a flattened representation of ckt *)

PROCEDURE DumpOneType(wr   : Wr.T; 
                      tn   : TEXT; 
                      ckt  : SpiceCircuit.T;
                      tbl  : TextRefTbl.T;
                      pTbl : TextTextSetTbl.T)
  RAISES { Wr.Failure };
  
PROCEDURE DumpGprofFormat(wr         : Wr.T;
                          typeCntTbl : TextRefTbl.T;
                          parentTbl  : TextTextSetTbl.T)
  RAISES { Wr.Failure };

PROCEDURE DumpBriefFlat(wr         : Wr.T; 
                        top        : TEXT; 
                        typeCntTbl : TextRefTbl.T;
                        cnt        : CARDINAL := 1;
                        level      := 0)
  RAISES { Wr.Failure };

PROCEDURE VisitCktNodes(pfx    : TEXT;
                        symTab : TextTextSetTbl.T;
                        ckt    : SpiceCircuit.T;
                        pNms   : TextSeq.T;
                        assocs : TextSpiceInstanceSetTbl.T;
                        me     : SpiceInstance.T;
                        subCkts : TextSpiceCircuitTbl.T);
  (* build symbol table for every circuit node in system *)

PROCEDURE DumpSymtab(wr : Wr.T;
                     symTab : TextTextSetTbl.T;
                     (*OUT*)canonTbl : TextTextTbl.T)
  RAISES { Wr.Failure };

PROCEDURE CleanAssocs(tbl      : TextSpiceInstanceSetTbl.T;
                      canonTbl : TextTextTbl.T) : TextSpiceInstanceSetTbl.T;

PROCEDURE Canonicalize(nm        : TEXT;
                       VAR canon : TEXT;
                       canonTbl  : TextTextTbl.T) : BOOLEAN;

PROCEDURE BuildCanonTbl(aliasTbl : TextTextSetTbl.T;
                        canonTbl : TextTextTbl.T);

CONST Brand = "SpiceFlat";

END SpiceFlat.
