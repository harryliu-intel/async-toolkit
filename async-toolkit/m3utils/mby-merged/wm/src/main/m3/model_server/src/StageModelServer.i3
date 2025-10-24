(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StageModelServer;
IMPORT ModelServer;
IMPORT Pathname;
IMPORT ModelServerSuper;
IMPORT UpdaterFactory;
IMPORT Byte;

(********************************************************************** 
 *
 *

   White Model Model Server -- for single-stage model servers 

   Main module, largely follows model_server.c from IES system in terms
   of interface.

   Author : Mika Nystrom <mika.nystroem@intel.com>
   January, 2019
                                                                      *
                                                                      *
 **********************************************************************)


CONST DefInfoFileName = ModelServer.DefInfoFileName;

TYPE
  T <: Public;

  Public = ModelServerSuper.T OBJECT
    topMapName : TEXT;
    stageName  : TEXT;
  METHODS
    init(stageName    : TEXT;
         factory      : UpdaterFactory.T;
         infoPath     : Pathname.T       := ".";
         quitOnLastClientExit            := FALSE;
         infoFileName : Pathname.T       := DefInfoFileName) : T;
    (* initialize object.  infoPath is a directory path where
       the host:port file is created with the filename given below
       by InfoFileName *)

    (****** abstract methods, implement in child type: ******)
    runStage(READONLY in     : ARRAY OF Byte.T;
             VAR      out    : REF ARRAY OF Byte.T;
             READONLY rxData : ARRAY OF Byte.T;
             VAR      txData : REF ARRAY OF Byte.T);
    
  END;

CONST Brand = "StageModelServer";

END StageModelServer.
