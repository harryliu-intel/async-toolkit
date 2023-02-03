UNSAFE INTERFACE COBYLA;

<*EXTERNAL cobyla_*>
PROCEDURE Call(N, M           : ADDRESS;
               X              : ADDRESS;
               RHOBEG, RHOEND : ADDRESS;
               IPRINT         : ADDRESS;
               MAXFUN         : ADDRESS;
               W              : ADDRESS;
               IACT           : ADDRESS);

END COBYLA.
