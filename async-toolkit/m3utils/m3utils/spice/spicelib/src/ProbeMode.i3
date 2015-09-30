INTERFACE ProbeMode;

TYPE T = {  Assertions ,  Outputs ,  IO ,  All  };
     

CONST Names = ARRAY T OF TEXT 
         { "assertions", "outputs", "io", "all" };

CONST Brand = "ProbeMode";

END ProbeMode.
