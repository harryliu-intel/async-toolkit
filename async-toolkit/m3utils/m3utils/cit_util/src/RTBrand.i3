INTERFACE RTBrand;

EXCEPTION NotBranded;

PROCEDURE Get(x : REFANY) : TEXT RAISES { NotBranded } ;

END RTBrand.
