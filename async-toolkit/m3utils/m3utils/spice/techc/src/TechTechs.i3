INTERFACE TechTechs;
IMPORT TechProcess;
FROM TechConfig IMPORT Tech;
IMPORT N5TechProcess, N3TechProcess, N3ETechProcess;
IMPORT P1276p4TechProcess, P1278p3TechProcess;

CONST Techs = ARRAY Tech OF TechProcess.T { N5TechProcess.P,
                                        P1276p4TechProcess.P,
                                        N3TechProcess.P,
                                        N3ETechProcess.P,
                                        P1278p3TechProcess.P
  };

END TechTechs.
