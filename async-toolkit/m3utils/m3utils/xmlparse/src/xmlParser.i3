(* $Id: xmlParser.i3,v 1.3 2011/01/28 03:12:03 mika Exp $ *)

INTERFACE xmlParser;
IMPORT xmlParserContext;
FROM Ctypes IMPORT const_char_star;
FROM xmlParserTypes IMPORT StartProc, AttrProc, EndProc, CharDataProc;

TYPE Context = xmlParserContext.T;

<*EXTERNAL*>
PROCEDURE xmlParserMain(path : const_char_star;
                        stuff : REFANY;
                        s : StartProc; a : AttrProc; e : EndProc;
                        c : CharDataProc) : INTEGER;
  (* returns 0 if OK, -1 if error *)

<*EXTERNAL*>
PROCEDURE xmlParserString(string : const_char_star;
                          stuff : REFANY;
                          s : StartProc; a : AttrProc; e : EndProc;
                          c : CharDataProc) : INTEGER;
  (* returns 0 if OK, -1 if error *)

(* use the below stuff if you want to parse in small chunks ... *)
  
<*EXTERNAL*>
PROCEDURE xmlParserInit(path : const_char_star;
                        stuff : REFANY;
                        s : StartProc; a : AttrProc; e : EndProc;
                        c : CharDataProc) : xmlParserContext.T;
  (* returns NULL on failure *)

<*EXTERNAL*>
PROCEDURE xmlParseContextIsNull(context : Context) : INTEGER;
  (* returns 0 if OK, something else otherwise *)

<*EXTERNAL*>
PROCEDURE xmlParseChunk(context : Context) : INTEGER;
  (* returns -1 if error, 0 if done, 1 if more to do *)

<*EXTERNAL*>
PROCEDURE xmlParseStopParser(context : Context; resumable : INTEGER) : INTEGER;

<*EXTERNAL*>
PROCEDURE xmlParseResumeParser(context : Context) : INTEGER;

<*EXTERNAL*>
PROCEDURE xmlStatusError() : INTEGER;

<*EXTERNAL*>
PROCEDURE xmlStatusOK() : INTEGER;

<*EXTERNAL*>
PROCEDURE xmlStatusSuspended() : INTEGER;

VAR
  XML_STATUS_ERROR, XML_STATUS_OK, XML_STATUS_SUSPENDED : INTEGER;

<*EXTERNAL*>
PROCEDURE xmlParserIsSuspended(context : Context) : INTEGER;

<*EXTERNAL*>
PROCEDURE xmlNullCopy(s : const_char_star) : const_char_star;

<*EXTERNAL*>
PROCEDURE xmlLenCopy(s : const_char_star; len : CARDINAL) : const_char_star;

<*EXTERNAL*>
PROCEDURE xmlCopyFree();
  
END xmlParser.
