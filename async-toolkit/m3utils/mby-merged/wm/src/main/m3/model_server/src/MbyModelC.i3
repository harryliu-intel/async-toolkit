INTERFACE MbyModelC;
IMPORT UpdaterFactory;

PROCEDURE GetUpdaterFactory() : UpdaterFactory.T;

VAR rp, wp : UNTRACED REF ADDRESS;
    (* hack for now -- this is the r-map and w-map of the sim *)
    (* shouldnt really be globals should they *)
  
END MbyModelC.
