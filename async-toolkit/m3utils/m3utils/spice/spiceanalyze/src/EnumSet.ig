GENERIC INTERFACE EnumSet(Enum);

TYPE P = Enum.T;
     
TYPE T = SET OF P;

CONST Brand = "EnumSet(" & Enum.Brand & ")";

CONST
  Empty        = T {};
  
END EnumSet.
