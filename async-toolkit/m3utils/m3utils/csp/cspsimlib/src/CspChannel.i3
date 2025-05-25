INTERFACE CspChannel;
IMPORT CspPortObject;
IMPORT CspCompiledProcess AS Process;

TYPE
  T <: Public;

  Public = CspPortObject.T  OBJECT
    slack          : (*CONST*) CARDINAL;
    writer, reader : (*CONST*) Process.Frame;
  END;

CONST Brand = "CspChannel";

END CspChannel.
