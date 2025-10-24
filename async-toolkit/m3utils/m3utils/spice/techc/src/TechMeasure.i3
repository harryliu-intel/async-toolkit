INTERFACE TechMeasure;
IMPORT Pathname;
IMPORT TechConfig;

PROCEDURE DoMeasure(READONLY c : TechConfig.T;
                    traceRoot, outName, workDir : Pathname.T;
                    exitOnError := TRUE) : BOOLEAN;
  (* returns TRUE iff we measure a cycle time *)

END TechMeasure.
