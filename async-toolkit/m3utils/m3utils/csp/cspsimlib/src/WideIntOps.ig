GENERIC INTERFACE WideIntOps(Type);
IMPORT DynamicInt, NativeInt;

TYPE T = Type.T;

PROCEDURE unpack_dynamic(VAR t : T; x, scratch : DynamicInt.T) : DynamicInt.T;

PROCEDURE unpack_native(VAR t : T; x : NativeInt.T) : NativeInt.T;

PROCEDURE pack_dynamic(x, scratch : DynamicInt.T; READONLY t : T) : DynamicInt.T;

PROCEDURE pack_native(x : NativeInt.T; READONLY t : T) : NativeInt.T;

END WideIntOps.
