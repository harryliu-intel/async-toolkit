INTERFACE CspIntrinsics;
IMPORT CspString;
FROM CspCompiledProcess IMPORT Frame;
IMPORT NativeInt, DynamicInt;

PROCEDURE print(frame : Frame; str : CspString.T) : BOOLEAN;

PROCEDURE string_native(frame  : Frame;
                        num    : NativeInt.T;
                        base   : INTEGER) : TEXT;

PROCEDURE string_dynamic(frame : Frame;
                         num   : DynamicInt.T;
                         base  : INTEGER) : TEXT;

PROCEDURE walltime(frame : Frame) : NativeInt.T;

PROCEDURE simtime(frame : Frame) : NativeInt.T;

PROCEDURE assert(x : BOOLEAN; text : TEXT) : NativeInt.T;

PROCEDURE random_native(bits : NativeInt.T) : NativeInt.T;

PROCEDURE random_dynamic(x : DynamicInt.T; bits : NativeInt.T) : DynamicInt.T;

CONST random_wide = random_dynamic;

TYPE
  Putter = OBJECT
  METHODS
    put(str : CspString.T);
  END;
  
PROCEDURE GetPutter() : Putter;
  
PROCEDURE SetPutter(putter : Putter);
  
END CspIntrinsics.
