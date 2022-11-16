INTERFACE MarginDump;

(* take a database of margins and dump it to disk in human-readable forms *)

IMPORT MarginMeasurementSeq;

PROCEDURE Do(seq    : MarginMeasurementSeq.T;
             Nworst : CARDINAL) : MarginMeasurementSeq.T;
  (* the sequence that is returned is the Nworst of each type *)

END MarginDump.
