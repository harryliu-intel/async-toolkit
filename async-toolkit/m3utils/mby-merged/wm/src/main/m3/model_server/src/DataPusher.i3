(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DataPusher;
IMPORT ServerPacket;
IMPORT IP, Thread;
IMPORT Word, Refany;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(hostname : TEXT; tcpPort : IP.Port) : T
      RAISES { IP.Error, Thread.Alerted } ;
    push(packet : ServerPacket.T);
    forceExit();
    exitCallback();
    getPort() : IP.Port;
    getHostname() : TEXT;
  END;

CONST Brand = "DataPusher";

PROCEDURE Hash(t : T) : Word.T; (* for generics *)

CONST Equal = Refany.Equal;
      
END DataPusher.
