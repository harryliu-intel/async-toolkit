(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE AplotDirectory;

(* note that as standard in aplot, "numnodes" does NOT include the TIME
   pseudo-node.  Also in alignment with fsdb.

   This means that if numnodes = K, we have
   pseudo-node TIME with id 0
   K nodes from 1 .. K [NOT K-1]
*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    getNodeOffset(idx : CARDINAL) : CARDINAL;
    
    write(to : Wr.T);

    read(from : Rd.T);
  END;

  Ragged <: T OBJECT METHODS
    new(numnodes : CARDINAL) : T;
        
    recordNodeOffset(idx : CARDINAL; at : CARDINAL);
  END;

  Null <: T OBJECT METHODS
    new(format : AplotHeader.Format; numnodes, steps : CARDINAL) : T;
  END;

CONST Brand = "AplotDirectory";

END AplotDirectory.

    
