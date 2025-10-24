(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TraceRewriter;
IMPORT TextSeq;
IMPORT Pathname;
IMPORT TraceOp;
IMPORT Rd;
IMPORT OSError;
IMPORT SpiceCompress;
IMPORT ArithConstants;
IMPORT Wr;
IMPORT TraceFile;
IMPORT Matrix;
IMPORT Trace;
IMPORT Pickle;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(root         : Pathname.T;
         rewriterPath : Pathname.T) : T
    RAISES { OSError.E, Rd.Failure, Rd.EndOfFile, TraceFile.FormatError };
    (* rewriterPath not currently used for anything *)

    addhi(stream          : TEXT;
          norm            : SpiceCompress.Norm;
          code            : ArithConstants.Encoding;
          aliases         : TextSeq.T) : CARDINAL
    RAISES { TraceFile.FormatError, OSError.E, Rd.EndOfFile, Rd.Failure, Wr.Failure };
    (* 
       given the data stream "data", in the same format as that produced
       by spicestream in Filter mode (see spicestream/src/Main.m3), add
       as the highest node id in the file in the appropriate format.

       aliases given in the aliases structure.  Do not include the 
       NAMES alias---this code will add it automatically 
    *)

    flush() RAISES { Wr.Failure, OSError.E, Rd.Failure, Rd.EndOfFile, TraceFile.FormatError  } ;
    (* flush all edits to disk *)

    addhiOp(op           : TraceOp.T;
            aliases      : TextSeq.T;
            relPrec      : LONGREAL;
            encoding     : ArithConstants.Encoding) : CARDINAL
    RAISES { TraceFile.FormatError, OSError.E, Rd.EndOfFile, Rd.Failure, Wr.Failure, Matrix.Singular, Pickle.Error };

    shareTrace() : Trace.T;
    (* pull out the internal Trace.T so we can share it (read-only please)
       -- call after .init(...) *)
  END;

CONST
  Brand = "TraceRewriter";

END TraceRewriter.
