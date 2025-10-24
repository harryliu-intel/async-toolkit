INTERFACE StdfTypeName;

CONST NotAFixedLength = -1;
      
PROCEDURE GetByteLength(nm : TEXT) : [ NotAFixedLength..LAST(CARDINAL) ];

CONST Brand = "StdfTypeName";

END StdfTypeName.
