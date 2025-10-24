(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SchemaGraph;
IMPORT Pathname;
IMPORT Schema;
IMPORT TextSeq;
IMPORT RefSeq;
IMPORT Scheme;
IMPORT OSError, Rd;

PROCEDURE ReadSchema(spn : Pathname.T) : Schema.T
  RAISES { OSError.E, Rd.Failure };
  
PROCEDURE ReadData(schema : Schema.T; files : TextSeq.T) : RefSeq.T
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile };

PROCEDURE EvalFormulas(scm      : Scheme.T;
                       schema   : Schema.T;
                       data     : RefSeq.T;
                       postEval : Callback := NIL)
  RAISES { Scheme.E };
  (* if postEval is non-NIL, postEval.next() will be called after
     each data line has been processed (and before the next) *)

TYPE
  Callback = OBJECT METHODS
    next();
  END;

PROCEDURE DoSweeps(targDir : Pathname.T; schema : Schema.T; data : RefSeq.T; doLabels : BOOLEAN);

CONST Brand = "SchemaGraph";

END SchemaGraph.
