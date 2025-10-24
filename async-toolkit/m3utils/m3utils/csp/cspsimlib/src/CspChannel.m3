(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CspChannel;
IMPORT CspChannelRep;
IMPORT CspCompiledProcess AS Process;
IMPORT Debug;

REVEAL
  T = CspChannelRep.Private BRANDED Brand OBJECT
  END;

CONST doDebug = TRUE;
      
PROCEDURE CheckSurrogate(c : T; fr : Process.Frame) : T =
  BEGIN
    (* 
       see if I am the write end and the read end is on a different
       scheduler.  If so, replace c with the surrogate 
    *)
    IF NOT c.isSurrogate() AND
      fr = c.writer AND
      c.writer.affinity # c.reader.affinity THEN

      IF doDebug THEN
        Debug.Out("CspChannel.CheckSurrogate : splitting out surrogate for " &
          c.nm)
      END;
      
      c := c.makeSurrogate()
      
    END;
    RETURN c
  END CheckSurrogate;


  
BEGIN END CspChannel.
  
