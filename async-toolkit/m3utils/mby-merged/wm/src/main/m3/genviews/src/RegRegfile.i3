INTERFACE RegRegfile;
IMPORT RegContainer;

TYPE
  T = RegContainer.T BRANDED Brand OBJECT END;

CONST
  Brand = "RegRegfile";

END RegRegfile.
