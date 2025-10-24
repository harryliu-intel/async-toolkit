(* $Id$ *)

INTERFACE GCSite;
IMPORT DBerr;

PROCEDURE InitDB();
  (* call this right after initializing database *)

PROCEDURE InitPW(recordHostName, dbName : TEXT) RAISES { DBerr.Error };
  (* call this to initialize the pricewatcher connection, if desired.
     recordHostName is the name of the host where the pricewatch is running
     dbName is the name of the current trading DB (e.g., tradingreal...) *)

PROCEDURE SigninURL() : TEXT;

END GCSite.
