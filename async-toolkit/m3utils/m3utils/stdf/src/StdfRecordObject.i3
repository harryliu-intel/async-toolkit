INTERFACE StdfRecordObject;
IMPORT StdfRecordHeader;

TYPE
  T = BRANDED Brand OBJECT
    hdr : StdfRecordHeader.T;
  END;

CONST Brand = "StdfRecordObject";

END StdfRecordObject.
