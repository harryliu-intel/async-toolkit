INTERFACE TechTechs;
IMPORT TechProcess;
FROM TechConfig IMPORT Tech;
IMPORT N5TechProcess, N3TechProcess, N3ETechProcess;
IMPORT P1276p4TechProcess, P1276p4_g1m_TechProcess, P1278p3TechProcess;
IMPORT P1276p4_aml1_TechProcess;
IMPORT P1276p4_aml2_TechProcess;
       
CONST Techs = ARRAY Tech OF TechProcess.T { N5TechProcess.P,
                                        P1276p4TechProcess.P,
                                        P1276p4_g1m_TechProcess.P,
                                        P1276p4_aml1_TechProcess.P,
                                        P1276p4_aml2_TechProcess.P,
                                        N3TechProcess.P,
                                        N3ETechProcess.P,
                                        P1278p3TechProcess.P
  };

END TechTechs.
