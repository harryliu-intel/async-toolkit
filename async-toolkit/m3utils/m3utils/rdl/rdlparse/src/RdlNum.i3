INTERFACE RdlNum;
IMPORT BigInt;
EXCEPTION
  ParseError (TEXT);
  
TYPE
  T <: Public;

  Public = OBJECT
    x          : BigInt.T;
    parsedType : ParsedType;
    str        : TEXT; (* parsed string that gave rise to this number *)
    width      : CARDINAL := UnspecifiedWidth;
  METHODS
    init(str : TEXT; accepted := AllTypes) : T RAISES { ParseError };
    (* parse the TEXT str as a (unsigned) number per the RDL spec.  Accept 
       only the formats specified in accepted.  Raise ParseError exception
       on a malformed number or if the number is not of one of the accepted
       formats *)
  END;

  ParsedType = { PlainDec, VerilogDec, HexHex, VerilogHex, VerilogBin, VerilogOct };

CONST
  ParsedBase = ARRAY ParsedType OF [1..16] { 10, 10, 16, 16, 2, 8 };
  ParsedTypeNames = ARRAY ParsedType OF TEXT { "PlainDec", "VerilogDec", "HexHex", "VerilogHex", "VerilogBin", "VerilogOct" };

CONST Brand = "RdlNum";
      
      UnspecifiedWidth = LAST(CARDINAL);

      AllTypes = SET OF ParsedType { FIRST(ParsedType)..LAST(ParsedType) };

END RdlNum.
