INTERFACE RefanyF;
IMPORT Refany;
TYPE
  T = Refany.T;
CONST
  Equal = Refany.Equal;
PROCEDURE Hash(ref: T): INTEGER;
PROCEDURE Format(ref: T): TEXT;
END RefanyF.
