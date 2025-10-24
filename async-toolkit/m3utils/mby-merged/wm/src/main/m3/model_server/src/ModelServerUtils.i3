(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ModelServerUtils;
FROM ModelServerSuper IMPORT Instance, MsgHandler;
IMPORT FmModelMessageHdr;
IMPORT NetContext;
IMPORT Rd, Wr;
IMPORT ServerPacket AS Pkt;

PROCEDURE HandleMsgCommandQuit(m      : MsgHandler;
                               READONLY hdr     : FmModelMessageHdr.T;
                               VAR cx : NetContext.T;
                               inst   : Instance);

  
PROCEDURE HandleMsgVersionInfo(<*UNUSED*>m  : MsgHandler;
                               READONLY hdr : FmModelMessageHdr.T;
                               VAR cx       : NetContext.T;
                               inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure, Wr.Failure };

PROCEDURE GetStringC(rd      : Rd.T;
                     bufflen : CARDINAL;
                     VAR cx  : NetContext.T) : TEXT
  RAISES { Rd.EndOfFile, Rd.Failure };

PROCEDURE PutStringS(sp : Pkt.T; end : Pkt.End; str : TEXT);

CONST Brand = "ModelServerUtils";

END ModelServerUtils.
