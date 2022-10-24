INTERFACE TechSimulate;

IMPORT TechConfig;

TYPE Config = TechConfig.T;

PROCEDURE DoSimulate(READONLY c : Config);

VAR ProcDeadline : LONGREAL;
    
END TechSimulate.
