INTERFACE MarginDump;

(* take a database of margins and dump it to disk in human-readable forms *)

IMPORT MarginMeasurementSeq;

PROCEDURE Do(seq : MarginMeasurementSeq.T);

END MarginDump.
