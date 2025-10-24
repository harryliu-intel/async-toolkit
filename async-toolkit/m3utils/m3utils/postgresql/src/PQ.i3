(*-------------------------------------------------------------------------
 *
 * libpq-fe.h--
 *    This file contains definitions for structures and
 *    externs for functions used by frontend postgres applications.
 *
 * Copyright (c) 1994, Regents of the University of California
 * Modula-3 Translation Copyright (c) 1996 Critical Mass, Inc.
 * Updated for PostgreSQL v6.0 (970201) by Ernesto Rico-Schmidt 6.2.1997
 *
 * $Id$
 *
 *-------------------------------------------------------------------------
 *)

UNSAFE INTERFACE PQ;

(* ----------------
 *	include stuff common to fe and be
 * ----------------
 *)
FROM PQtypes IMPORT char, char_star, int, int_star, short;


IMPORT PQcomm;

FROM PostgreSQL IMPORT NAMEDATALEN, Oid;

FROM PGRES IMPORT PGRES_T;

TYPE PGRES = PGRES_T;

TYPE CONNECTION = { OK, BAD };
TYPE ConnStatusType = CONNECTION;

CONST CONNECTION_OK: ConnStatusType = CONNECTION.OK;
CONST CONNECTION_BAD: ConnStatusType = CONNECTION.BAD;


TYPE ExecStatusType = PGRES;

CONST PGRES_EMPTY_QUERY : PGRES = PGRES.EMPTY_QUERY;
CONST PGRES_COMMAND_OK : PGRES = PGRES.COMMAND_OK;
CONST PGRES_TUPLES_OK : PGRES = PGRES.TUPLES_OK;
CONST PGRES_COPY_OUT : PGRES = PGRES.COPY_OUT;
CONST PGRES_COPY_IN : PGRES = PGRES.COPY_IN;
CONST PGRES_BAD_RESPONSE : PGRES = PGRES.BAD_RESPONSE;
CONST PGRES_NONFATAL_ERROR : PGRES = PGRES.NONFATAL_ERROR;
CONST PGRES_FATAL_ERROR : PGRES = PGRES.FATAL_ERROR;

(* string descriptions of the ExecStatusTypes *)
<*EXTERNAL*> VAR pgresStatus: ARRAY PGRES OF char_star; 

(* POSTGRES backend dependent Constants.  *)

(* ERROR_MSG_LENGTH should really be the same as ELOG_MAXLEN in utils/elog.h*)
CONST ERROR_MSG_LENGTH = 4096;
CONST COMMAND_LENGTH = 20;
CONST REMARK_LENGTH = 80;
CONST PORTAL_NAME_LENGTH = 16;

(* ----------------
 * PQArgBlock --
 *	Information (pointer to array of this structure) required
 *	for the PQfn() call.
 * ----------------
 *)

TYPE PQArgBlock = RECORD
  len : int;
  isint : int;
  u : int;
END;

TYPE PQArgBlock_star = UNTRACED REF PQArgBlock;

TYPE PGresAttDesc = RECORD
  name : char_star; (* type name *)
  adtid : Oid;      (* type id *)
  adtsize : short;  (* type size *)
END;

TYPE PGresAttDesc_star = UNTRACED REF PGresAttDesc;

(* use char* for Attribute values, ASCII tuples are guaranteed to be
   null-terminated For binary tuples, the first four bytes of the value is
   the size, and the bytes afterwards are the value.  The binary value is
   not guaranteed to be null-terminated.  In fact, it can have embedded
   nulls*)

CONST NULL_LEN = (-1);     (* pg_result len for NULL value *)

TYPE PGresAttValue = RECORD
  len : int;          (* length in bytes of the value *)
  value : char_star;  (* actual value *)
END;

TYPE PGresAttValue_star = UNTRACED REF PGresAttValue;

TYPE PGnotify = RECORD
  relname : ARRAY [0..NAMEDATALEN-1] OF char;
                      (* name of relation containing data *)
  be_pid : int;       (* process id of backend *)
END;

TYPE PGnotify_star = UNTRACED REF PGnotify;

TYPE PGlobjfuncs = RECORD
    fn_lo_open : Oid;   (* OID of backend function lo_open *)
    fn_lo_close : Oid;  (* OID of backend function lo_close *)
    fn_lo_creat : Oid;  (* OID of backend function lo_creat *)
    fn_lo_unlink:  Oid; (* OID of backend function lo_unlink *)
    fn_lo_lseek : Oid;  (* OID of backend function lo_lseek *)
    fn_lo_tell : Oid;   (* OID of backend function lo_tell *)
    fn_lo_read : Oid;   (* OID of backend function LOread *)
    fn_lo_write : Oid;  (* OID of backend function LOwrite *)
END;

(* PGconn encapsulates a connection to the backend *)
TYPE PGconn = RECORD
  pghost : char_star;      (* the machine on which the server is running *)
  pgtty : char_star;       (* tty on which the backend messages is displayed *)
  pgport : char_star;      (* the communication port with the backend *)
  pgoptions : char_star;   (* options to start the backend with *)
  dbName : char_star;      (* database name *)
  status : ConnStatusType;
  errorMessage : ARRAY [0..ERROR_MSG_LENGTH-1] OF char;
  (* pipes for be/fe communication *)
  Pfin : FILE;
  Pfout : FILE;
  Pfdebug : FILE;
  port : Port;             (* really a Port* *)
  asyncNotifyWaiting: int;
  notifyList : Dllist;
  pguser : char_star;      (* Postgres username of user who is connected *)
  lobjfuncs : PGlobjfuncs; (* Backend function OID's for large object access *)
END;

TYPE PGconn_star = UNTRACED REF PGconn;

CONST CMDSTATUS_LEN = 40;

(* PGresult encapsulates the result of a query *)
(* unlike the old libpq, we assume that queries only return in one group *)
TYPE PGresult = RECORD
  ntups : int;
  numAttributes : int;
  attDescs : PGresAttDesc_star;
  tuples : PGresAttValue_star;
                     (* each PGresTuple is an array of PGresAttValue's *)
  tupArrSize : int;  (* size of tuples array allocated *)
  resultStatus : ExecStatusType;
  cmdStatus : ARRAY [0..CMDSTATUS_LEN-1] OF char;
                     (* cmd status from the last insert query*)
  binary : int;      (* binary tuple values if binary == 1, otherwise ASCII *)
  conn : PGconn;
END;

TYPE PGresult_star = UNTRACED REF PGresult;

TYPE pqbool = char;
  (* We can't use the conventional "bool", because we are designed to be
     included in a user's program, and user may already have that type
     defined.  Pqbool, on the other hand, is unlikely to be used.  *)

TYPE PQprintOpt = RECORD
    header : pqbool;       (* print output field headings and row count *)
    align : pqbool;        (* fill align the fields *)
    standard : pqbool;      (* old brain dead format *)
    html3 : pqbool;        (* output html tables *)
    expanded : pqbool;     (* expand tables *)
    pager : pqbool;        (* use pager for output if needed *)
    fieldSep : char_star;  (* field separator *)
    tableOpt :char_star;   (* insert to HTML <table ...> *)
    caption : char_star;   (* HTML <caption> *)
    fieldName : char_star;
                       (* null terminated array of repalcement field names *)
END;

TYPE PQprintOpt_star = UNTRACED REF PQprintOpt;

(* ----------------
 * Structure for the conninfo parameter definitions of PQconnectdb()
 * ----------------
 *)
TYPE PQconninfoOption = RECORD
  keyword : char_star;  (* The keyword of the option *)
  environ : char_star;  (* Fallback environment variable name *)
  compiled : char_star; (* Fallback compiled in default value *)
  val : char_star;      (* Options value *)
  label : char_star;    (* Label for field in connect dialog *)
  dispchar : char_star; (* Character to display for this field *)
                        (* in a connect dialog. Values are: *)
                        (*    ""   Display entered value as is *)
                        (*    "*"  Password field - hide value *)
                        (*    "D"  Debug options - don't *)
                        (*         create a field by default *)
  dispsize: int;        (* Field size in characters for dialog *)
END;

(* ===  in fe-connect.c === *)

(* make a new client connection to the backend *)
<*EXTERNAL*> PROCEDURE PQconnectdb (conninfo : char_star): PGconn_star;

(* nonblocking version of the above *)
<*EXTERNAL*> PROCEDURE PQconnectStart (conninfo : char_star): PGconn_star;
<*EXTERNAL*> PROCEDURE PQconnectPoll (conn : PGconn_star): PostgresPollingStatusType;

TYPE PostgresPollingStatusType = { POLLING_FAILED, POLLING_READING, POLLING_WRITING, POLLING_OK, POLLING_ACTIVE };

<*EXTERNAL*> PROCEDURE PQconndefaults (): PQconninfoOption;

<*EXTERNAL*> PROCEDURE PQsetdbLogin (pghost : char_star;
                                pgport : char_star;
                                pgoptions : char_star;
                                pgtty : char_star;
                                dbName : char_star;
                                login : char_star;
                                pwd : char_star): PGconn_star;

(* close the current connection and free the PGconn data structure *)
<*EXTERNAL*> PROCEDURE PQfinish (conn : PGconn_star);

(* close the current connection and restablish a new one with the same
   parameters *)
<*EXTERNAL*> PROCEDURE PQreset (conn : PGconn_star);

<*EXTERNAL*> PROCEDURE PQdb (conn : PGconn_star) : char_star;

<*EXTERNAL*> PROCEDURE PQuser (conn : PGconn_star) : char_star;

<*EXTERNAL*> PROCEDURE PQhost (conn : PGconn_star) : char_star;

<*EXTERNAL*> PROCEDURE PQoptions (conn : PGconn_star) : char_star;

<*EXTERNAL*> PROCEDURE PQport (conn : PGconn_star) : char_star;

<*EXTERNAL*> PROCEDURE PQtty (conn : PGconn_star) : char_star;

<*EXTERNAL*> PROCEDURE PQstatus (conn : PGconn_star) : ConnStatusType;

<*EXTERNAL*> PROCEDURE PQerrorMessage (conn : PGconn_star) : char_star;

<*EXTERNAL*> PROCEDURE PQtrace (conn : PGconn; debug_port: FILE);

<*EXTERNAL*> PROCEDURE PQuntrace (conn : PGconn);

(* === in fe-exec.c === *)

<*OBSOLETE*>
<*EXTERNAL*> PROCEDURE PQexec (conn : PGconn_star;
                              query : char_star): PGresult_star;
(* WARNING: PQexec blocks---will block ALL M3 threads *)

(* new async stuff follows ---*)

<*EXTERNAL*> PROCEDURE PQsendQuery(conn : PGconn_star;
                                   query : char_star) : int;

<*EXTERNAL*> PROCEDURE PQgetResult(conn : PGconn_star) : PGresult_star;

<*EXTERNAL*> PROCEDURE PQisBusy(conn : PGconn_star) : int;

<*EXTERNAL*> PROCEDURE PQsocket(conn : PGconn_star) : int;

<*EXTERNAL*> PROCEDURE PQconsumeInput(conn : PGconn_star) : int;

(*--- end async stuff *)

<*EXTERNAL*> PROCEDURE PQgetline (conn : PGconn_star;
                                  string : char_star;
                                  length : int): int;

<*EXTERNAL*> PROCEDURE PQendcopy (conn : PGconn_star): int;

<*EXTERNAL*> PROCEDURE PQsetnonblocking(conn : PGconn_star;
                                        arg : int) : int;

<*EXTERNAL*> PROCEDURE PQisnonblocking(conn : PGconn_star) : int;

<*EXTERNAL*> PROCEDURE PQflush(conn : PGconn_star) : int;

<*EXTERNAL*> PROCEDURE PQputline (conn : PGconn_star; string : char_star);

<*EXTERNAL*> PROCEDURE PQresultStatus (res : PGresult_star): ExecStatusType;

<*EXTERNAL*> PROCEDURE PQntuples (res : PGresult_star): int;

<*EXTERNAL*> PROCEDURE PQnfields (res : PGresult_star): int;

<*EXTERNAL*> PROCEDURE PQfname (res : PGresult_star;
                                field_num : int): char_star;

<*EXTERNAL*> PROCEDURE PQfnumber (res : PGresult_star;
                                  field_name: char_star): int;

<*EXTERNAL*> PROCEDURE PQftype (res : PGresult_star; field_num : int): Oid;

<*EXTERNAL*> PROCEDURE PQfsize (res : PGresult_star; field_num : int): short;

<*EXTERNAL*> PROCEDURE PQcmdStatus (res : PGresult_star): char_star;

<*EXTERNAL*> PROCEDURE PQoidStatus (res : PGresult_star): char_star;

<*EXTERNAL*> PROCEDURE PQgetvalue (res : PGresult_star;
                                   tup_num : int;
                                   field_num : int): char_star;

<*EXTERNAL*> PROCEDURE PQgetlength (res: PGresult_star;
                                    tup_num : int;
                                    field_num : int): int;

<*EXTERNAL*> PROCEDURE PQgetisnull (res : PGresult_star;
                                    tup_num : int;
                                    field_num : int): int;

<*EXTERNAL*> PROCEDURE PQclear (res : PGresult_star);

(* PQdisplayTuples() is a better version of PQprintTuples() *)
<*EXTERNAL*> PROCEDURE PQdisplayTuples (res : PGresult_star;
                            fp : FILE;         (* where to send the output *)
			    fillAlign : int;   (* pad the fields with spaces *)
			    fieldSep : char;   (* field separator *)
			    printHeader : int; (* display headers? *)
			    quiet : int);

<*EXTERNAL*> PROCEDURE PQprintTuples (res : PGresult_star; 
			  fout : FILE;        (* output stream *)
			  printAttName : int; (* print attribute names or not*)
			  terseOutput : int;  (* delimiter bars or not?*)
			  width : int);       (* width of column, 
					         if 0, use variable width *)

<*EXTERNAL*> PROCEDURE PQprint (fout : FILE;  (* output stream *)
                                res : PGresult_star;
                                ps : PQprintOpt_star);   (* option structure *)

<*EXTERNAL*> PROCEDURE PQnotifies (conn : PGconn): PGnotify_star;

<*EXTERNAL*> PROCEDURE PQfn (conn : PGconn_star;
                             fnid : int; 
                             result_buf: int_star; 
                             result_len: int_star;
                             result_is_int : int;
                             args : PQArgBlock_star; 
                             nargs : int): PGresult_star;

(* === in fe-auth.c === *)

(* removed from PGSQL 8? *)
(*
<*EXTERNAL*> PROCEDURE fe_getauthsvc (PQerrormsg : char_star): PQcomm.MsgType;

<*EXTERNAL*> PROCEDURE fe_setauthsvc (name : char_star; PQerrormsg: char_star);

<*EXTERNAL*> PROCEDURE fe_getauthname (PQerrormsg : char_star): char_star;
*)

(* === in fe-misc.c === *)

(* pqGets and pqPuts gets and sends strings to the file stream returns 0 if
   successful if debug is non-null, debugging output is sent to that stream
   *)

<*EXTERNAL*> PROCEDURE pqGets (s : char_star;
                               maxlen : int;
                               stream : FILE;
                               debug : FILE): int;

<*EXTERNAL*> PROCEDURE pqGetnchar (s : char_star;
                                   maxlen : int;
                                   stream : FILE;
                                   debug : FILE): int;

<*EXTERNAL*> PROCEDURE pqPutnchar (s : char_star;
                                   maxlen : int;
                                   stream : FILE;
                                   debug : FILE): int;

<*EXTERNAL*> PROCEDURE pqPuts (s : char_star;
                               stream : FILE;
                               debug : FILE): int;

<*EXTERNAL*> PROCEDURE pqGetc (stream : FILE;
                               debug : FILE): int;

(* get a n-byte integer from the stream into result *)
(* returns 0 if successful *)

<*EXTERNAL*> PROCEDURE pqGetInt(result : int_star;
                                bytes : int;
                                stream : FILE;
                                debug : FILE): int;

(* put a n-byte integer into the stream *)
(* returns 0 if successful *)

<*EXTERNAL*> PROCEDURE pqPutInt(n : int;
                                bytes : int;
                                stream : FILE;
                                debug : FILE): int;

<*EXTERNAL*> PROCEDURE pqFlush(stream : FILE; debug : FILE);

(* === in fe-lobj.c === *)

<*EXTERNAL*> PROCEDURE lo_open (conn : PGconn;
                                lobjId : Oid;
                                mode : int): int;

<*EXTERNAL*> PROCEDURE lo_close (con : PGconn; fd : int): int;
                                 
<*EXTERNAL*> PROCEDURE lo_read (conn : PGconn;
                                fd : int;
                                buf : char_star;
                                len : int): int;

<*EXTERNAL*> PROCEDURE lo_write (conn : PGconn;
                                 fd : int;
                                 buf : char_star;
                                 len : int): int;

<*EXTERNAL*> PROCEDURE lo_lseek (conn : PGconn;
                                 fd : int;
                                 offset : int;
                                 whence : int): int;

<*EXTERNAL*> PROCEDURE lo_creat (conn : PGconn; mode : int): Oid;

<*EXTERNAL*> PROCEDURE lo_tell (conn : PGconn; fd : int): int;

<*EXTERNAL*> PROCEDURE lo_unlink (conn : PGconn; lobjId : Oid): int;

<*EXTERNAL*> PROCEDURE lo_import (conn : PGconn; filename : char_star): Oid;

<*EXTERNAL*> PROCEDURE lo_export (conn : PGconn;
                                  lobjId : Oid;
                                  filename : char_star): int;

(* max length of message to send  *)
CONST MAX_MESSAGE_LEN = 8193;

(* maximum number of fields in a tuple *)
CONST BYTELEN = 8;
CONST MAX_FIELDS = 512;

(* fall back options if they are not specified by arguments or defined by
   environment variables *)
CONST DefaultHost = "localhost";
CONST DefaultTty = "";
CONST DefaultOption = "";

TYPE TUPLE = ADDRESS;
TYPE FILE = ADDRESS;
TYPE Dllist = ADDRESS;
TYPE Port = ADDRESS;

END PQ.
