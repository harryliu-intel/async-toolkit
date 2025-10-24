(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TechTemplate;
IMPORT Pathname, OSError, TextSeq, TextTextTbl;
IMPORT Rd, Wr;

TYPE T = TextSeq.T;
     
PROCEDURE LoadTemplate(path : Pathname.T) : T
  RAISES { OSError.E, Rd.Failure };
  
PROCEDURE ModifyTemplate(template : T; map : TextTextTbl.T);
  
PROCEDURE WriteTemplate(template : T; path : Pathname.T)
  RAISES { OSError.E, Wr.Failure };

END TechTemplate.
