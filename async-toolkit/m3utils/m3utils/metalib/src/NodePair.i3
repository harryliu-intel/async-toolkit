(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE NodePair;
IMPORT Name, Dsim, Word;

TYPE T = RECORD fanout, fanin : Name.T; outDir, inDir : Dsim.Sense END;
     
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Format(t : T) : TEXT;

CONST Brand = "NodePair";

END NodePair.
