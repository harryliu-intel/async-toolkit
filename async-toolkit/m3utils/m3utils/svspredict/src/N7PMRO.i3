INTERFACE N7PMRO;
IMPORT N7Tech;
IMPORT PMRO;

TYPE
  T = ARRAY N7Tech.Transistor OF PMRO.T;

  (* from PMRO data sheet *)
  
CONST
  V = T {
  PMRO.T { 394, 450, 512 },
  PMRO.T { 310, 367, 430 },
  PMRO.T { 242, 292, 348 }
  };

END N7PMRO.
