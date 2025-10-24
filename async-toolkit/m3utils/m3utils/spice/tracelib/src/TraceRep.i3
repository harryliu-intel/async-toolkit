(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TraceRep;
IMPORT Trace;
IMPORT TextCardTbl;
IMPORT CardTextSeqTbl;
IMPORT ZtraceFile;
IMPORT Pathname;
IMPORT TraceFile;

(* 
   Reveal the internal alias dictionaries of the Trace.T representation 

   Not sure this actually needs to be revealed to any clients---the 
   interface provided by Trace.T ought to be sufficient for any reasonable
   purposes.
*)

REVEAL
  Trace.T <: Private;

TYPE
  Private = Trace.Public OBJECT
    fwdTbl   : TextCardTbl.T;
    revTbl   : CardTextSeqTbl.T;

  METHODS
    getActualFormat(VAR actualPath     : Pathname.T;
                    VAR actualVersion  : TraceFile.Version);

    getMetadata() : ZtraceFile.Metadata; (* only valid for CompressedV1 *)
  END;

  PrivateCompressedV1 = Trace.T OBJECT
    z : ZtraceFile.Metadata;
  END;

  CompressedV1 <: PrivateCompressedV1;
  
END TraceRep.
