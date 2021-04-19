(*-------------------------------------------------------------------------
 *
 * pqcomm.h--
 *    Parameters for the communication module
 *
 *
 * Copyright (c) 1994, Regents of the University of California
 * $Id$
 * Modula-3 Translation Copyright (c) 1996 Critical Mass, Inc.
 * Updated for PostgreSQL v6.0 (970201) by Ernesto Rico-Schmidt 6.2.1997
 *
 * NOTES
 *    Some of this should move to libpq.h
 *
 *-------------------------------------------------------------------------
*)

INTERFACE PQcomm;

FROM PQtypes IMPORT int, char;
FROM PostgreSQL IMPORT NAMEDATALEN;

(* startup msg parameters: path length, argument string length *)
CONST PATH_SIZE	= 64;
CONST ARGV_SIZE	= 64;

(* The various kinds of startup messages are for the various kinds of user
  authentication systems.  In the beginning, there was only STARTUP_MSG and
  all connections were unauthenticated.  Now, there are several choices of
  authentication method (the client picks one, but the server needn't
  necessarily accept it).  So now, the STARTUP_MSG message means to start
  either an unauthenticated or a host-based authenticated connection,
  depending on what the server prefers.  This is possible because the
  protocol between server and client is the same in both cases (basically,
  no negotiation is required at all).  *)

TYPE MsgType = {
  ACK_MSG,            (* acknowledge a message *)
  ERROR_MSG,          (* error response to client from server *)
  RESET_MSG,          (* client must reset connection *)
  PRINT_MSG,          (* tuples for client from server *)
  NET_ERROR,          (* error in net system call *)
  FUNCTION_MSG,       (* fastpath call (unused) *)
  QUERY_MSG,          (* client query to server *)
  STARTUP_MSG,        (* initialize a connection with a backend *)
  DUPLICATE_MSG,      (* duplicate msg arrived (errors msg only) *)
  INVALID_MSG,        (* for some control functions *)
  STARTUP_KRB4_MSG,   (* krb4 session follows startup packet *)
  STARTUP_KRB5_MSG,   (* krb5 session follows startup packet *)
  STARTUP_HBA_MSG,    (* use host-based authentication *)
  STARTUP_UNAUTH_MSG  (* use unauthenticated connection *)
  (* insert new values here -- DO NOT REORDER OR DELETE ENTRIES *)
};

TYPE Addr = ADDRESS;

TYPE PacketLen = int;     (* packet length *)

TYPE  StartupInfo = RECORD
  database : ARRAY [0..PATH_SIZE-1] OF char; (* database name *)
  user : ARRAY [0..NAMEDATALEN-1] OF char;   (* user name *)
  options : ARRAY [0..ARGV_SIZE-1] OF char;  (* possible additional args *)
  execFile : ARRAY [0..ARGV_SIZE-1] OF char; (* possible backend to use *)
  tty : ARRAY [0..PATH_SIZE-1] OF char;      (* possible tty for debug output*)
END;

TYPE StartupInfo_star = UNTRACED REF StartupInfo;

(* amount of available data in a packet buffer *)
CONST MESSAGE_SIZE = BYTESIZE(StartupInfo) + 5;

(* I/O can be blocking or non-blocking *)
CONST BLOCKING = 0;
CONST NON_BLOCKING = 1;

(* a PacketBuf gets shipped from client to server so be careful of
   differences in representation.  Be sure to use htonl() and ntohl() on the
   len and msgtype fields! *)

TYPE PacketBuf = RECORD
  len : int;
  msgtype : MsgType;
  data : ARRAY [0..MESSAGE_SIZE-1] OF char;
END;

TYPE PacketBuf_star = UNTRACED REF PacketBuf;

(* update the conversion routines StartupInfo2PacketBuf() and
   PacketBuf2StartupInfo() (decl. below) if StartupInfo or PacketBuf structs
   ever change *)

(* socket descriptor port we need addresses of both sides to do
   authentication calls *)

TYPE Port = RECORD
  sock : int;        (* file descriptor *)
  mask : int;        (* select mask *)
  nBytes : int;      (* nBytes read in so far *)
  laddr : ADDRESS;   (* local addr (us) sockaddr_in *)
  raddr : ADDRESS;   (* remote addr (them) sockaddr_in *)
(* id : PacketBufId; (* id of packet buf currently in use *) *)
  buf: PacketBuf;    (* stream implementation (curr pack buf) *)
END;

TYPE Port_star = UNTRACED REF Port;

(* invalid socket descriptor *)
CONST INVALID_SOCK = (-1);

CONST INVALID_ID = (-1);
CONST MAX_CONNECTIONS = 10;
CONST N_PACK_BUFS = 20;

(* no multi-packet messages yet *)
CONST MAX_PACKET_BACKLOG = 1;

CONST DEFAULT_STRING = "";

<*EXTERNAL*> VAR Pfout, Pfin : FILE;
<*EXTERNAL*> VAR PQAsyncNotifyWaiting : int;

TYPE FILE = ADDRESS;

(* prototypes for functions in pqpacket.c *)

<*EXTERNAL*> PROCEDURE PacketReceive (port : Port_star;
                                      buf : PacketBuf_star;
                                      nonBlocking : char) : int;

<*EXTERNAL*> PROCEDURE PacketSend (port : Port_star;
                                   buf : PacketBuf_star;
                                   len : PacketLen;
                                   nonBlocking : char) : int;

(* <*EXTERNAL*> PROCEDURE StartupInfo2PacketBuf (startup : StartupInfo_star)
: PacketBuf_star; *)

(* <*EXTERNAL*> PROCEDURE PacketBuf2StartupInfo (buf : PacketBuf_star) :
StartupInfo_star; *)

END PQcomm.
