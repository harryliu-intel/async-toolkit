INTERFACE NetTypes;
IMPORT Word;

TYPE
  U8  = [ 0..      16_ff ];
  U16 = [ 0..    16_ffff ];
  U32 = [ 0..16_ffffffff ];
  U64 = Word.T;

TYPE ByteOrder = { BE, LE };

PROCEDURE FormatU8(u : U8) : TEXT;

PROCEDURE FormatU16(u : U16) : TEXT;

PROCEDURE FormatU32(u : U32) : TEXT;

PROCEDURE FormatU64(u : U64) : TEXT;
  
END NetTypes.
