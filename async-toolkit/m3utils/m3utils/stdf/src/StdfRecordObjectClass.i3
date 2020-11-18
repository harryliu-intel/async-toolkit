INTERFACE StdfRecordObjectClass;
IMPORT StdfRecordObject;
IMPORT StdfRecordTypes;

TYPE
  Class = StdfRecordObject.Public OBJECT
    tag : StdfRecordTypes.Enum;
  END;

REVEAL
  StdfRecordObject.T = Class BRANDED OBJECT END;
  
CONST Brand = "StdfRecordObjectClass";

END StdfRecordObjectClass.
