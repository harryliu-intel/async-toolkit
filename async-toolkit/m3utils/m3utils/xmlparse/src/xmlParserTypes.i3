(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE xmlParserTypes;
IMPORT xmlParserContext;
FROM Ctypes IMPORT const_char_star;
TYPE Context = xmlParserContext.T;

TYPE
  StartProc    = PROCEDURE (c : Context; s : REFANY; el : const_char_star);
  (* called on start from expat *)
  
  AttrProc     = PROCEDURE (c : Context; s : REFANY; tag, attr : const_char_star);
  (* called on attr from expat *)
  
  EndProc      = PROCEDURE (c : Context; s : REFANY);
  (* called on end from expat *)
  
  CharDataProc = PROCEDURE (c : Context; s : REFANY; len : CARDINAL; data : const_char_star);
  (* called on chardata from expat *)

END xmlParserTypes.
