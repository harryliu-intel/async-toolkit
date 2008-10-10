(* $Id$ *)

INTERFACE RemoteFileRd;
IMPORT Pathname, Rd, OSError;

PROCEDURE Open(p : Pathname.T) : Rd.T RAISES { OSError.E };
(* open file in one of following formats
   <localfile>
   <remotehost>:<remotefile>
   <remoteuser>@<remotehost>:<remotefile>

   <remotefile> may contain colons, <localfile> may not *)

END RemoteFileRd.
