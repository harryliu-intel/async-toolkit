(* $Id$ *)

INTERFACE Pickles;
IMPORT Database;
IMPORT Fingerprint;
IMPORT Rd, Wr, Pathname, ProcUtils;

PROCEDURE UnStuff(bytea : Database.ByteA) : REFANY RAISES { ProcUtils.ErrorExit };

PROCEDURE UnStuffOC(rd : Rd.T;
                        tmpNam1, tmpNam2 : Pathname.T) : REFANY;
  (* out of core unstuffer *)

PROCEDURE Stuff(ref : REFANY) : Database.ByteA; 

PROCEDURE StuffAndFingerprint(ref : REFANY; 
                               VAR fingerprint : Fingerprint.T) : Database.ByteA;

PROCEDURE StuffAndFingerprintWr(ref : REFANY; 
                                VAR fingerprint : Fingerprint.T;
                                wr : Wr.T;
                                tmpNam1, tmpNam2 : Pathname.T);
  (* out of core stuffer.  leaves wr unclosed (may be used with TextWr.T) *)

PROCEDURE StuffAndFingerprintAsTextWr(ref : REFANY; 
                                VAR fingerprint : Fingerprint.T;
                                wr : Wr.T;
                                tmpNam1 : Pathname.T);
  (* out of core stuffer.  leaves wr unclosed (may be used with TextWr.T).
     Does not turn data into legal bytea for postgres. *)

END Pickles.
