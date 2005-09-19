(* $Id$ *)

INTERFACE UnixFilter;
IMPORT Rd, Wr, Thread, Pathname;

PROCEDURE RR(cmd : TEXT; source : Rd.T; wd0 : Pathname.T := NIL) : Rd.T RAISES { Rd.Failure, 
                                                        Thread.Alerted };
  (* read filter *)

PROCEDURE WW(cmd : TEXT; target : Wr.T; wd0 : Pathname.T := NIL) : Wr.T RAISES { Wr.Failure };
  (* write filter *)

PROCEDURE RW(cmd : TEXT; source :Rd.T; target :Wr.T; wd0 : Pathname.T := NIL) RAISES { Rd.Failure,
                                                              Wr.Failure,
                                                              Thread.Alerted };
  (* from rd to wr until Rd.EndOfFile *)

END UnixFilter.
