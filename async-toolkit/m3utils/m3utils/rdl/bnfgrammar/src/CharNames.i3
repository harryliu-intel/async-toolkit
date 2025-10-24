(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CharNames;

(* 
   special characters that appear in the BNF grammars, these are enough
   to cover csrspec (and probably many other languages) 
*)

TYPE T = RECORD c : CHAR; nm : TEXT END;

CONST
  Mappings = ARRAY OF T {
  T { '.', "Per" },
  T { '&', "Amp" },
  T { '*', "Ast" },
  T { '%', "Pct" },
  T { '(', "Lpa" },
  T { ')', "Rpa" },
  T { '=', "Equ" },
  T { '~', "Til" },
  T { '|', "Pip" },
  T { '[', "Lsq" },
  T { ']', "Rsq" },
  T { '{', "Lcu" },
  T { '}', "Rcu" },
  T { '-', "Min" },
  T { '$', "Dol" },
  T { '<', "Lth" },
  T { '>', "Gth" },
  T { ',', "Com" },
  T { '+', "Plu" },
  T { '@', "Ats" },
  T { '^', "Car" },
  T { '?', "Que" },
  T { '!', "Exc" },
  T { '/', "Fsl" },
  T { ';', "Sco" },
  T { ':', "Col" }
  };

CONST Brand = "CharNames";

PROCEDURE Map(c : CHAR; VAR to : TEXT) : BOOLEAN;
  
END CharNames.

  
