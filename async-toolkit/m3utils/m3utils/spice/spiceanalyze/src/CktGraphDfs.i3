(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CktGraphDfs;
IMPORT CktGraph AS G;
IMPORT CktNodeList AS NodeList;

PROCEDURE Node(n : G.Node; v : NodeVisitor);

TYPE
  NodeVisitor = OBJECT METHODS
    visit(path : NodeList.T; via : G.Element; this : G.Node) : BOOLEAN;
    (* return whether you want to continue searching 

       path is the list back to the starting point in reverse order *)
  END;

CONST Brand = "CktGraphDfs";
      
END CktGraphDfs.
