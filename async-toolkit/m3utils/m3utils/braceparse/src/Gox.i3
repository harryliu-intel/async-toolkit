INTERFACE Gox;
IMPORT TextList;
IMPORT BraceParse;

(* 
   this is the entry point for producing a GOX report to solve
   Chen Benuness's problem of knowing the amount of transistor gate
   of every type and length in the design 
*)

PROCEDURE ProduceReports(roots               : TextList.T;
                         (* the root cell types to report on *)
                         
                         parsed              : BraceParse.T;
                         (* the parsed data from BraceParse *)
                         
                         levels              : CARDINAL;
                         (* how many levels deep to drill the report *)
                         
                         transistorReportSfx : TEXT
                         (* the suffix to use for the report's filename 
                            the full filename is constructed as follows:

                            <root type>.<suffix>.rpt --- human readable

                            <root type>.<suffix>.csv --- CSV, drilled by 
                                                         length and MOS type

                            <root type>.<suffix>_consolidated.csv 
                                                     --- CSV, by MOS type

                         *)
  
  );

CONST Brand = "Gox";

END Gox.
