INTERFACE StdfRecordObject;
IMPORT StdfRecordHeader;

TYPE
  Public = BRANDED Brand OBJECT
    hdr : StdfRecordHeader.T;
  END;

  T <: Public;
  
CONST Brand = "StdfRecordObject";

END StdfRecordObject.
