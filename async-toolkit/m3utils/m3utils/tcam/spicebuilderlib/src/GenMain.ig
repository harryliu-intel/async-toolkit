GENERIC INTERFACE GenMain();

IMPORT ParseParams, Wr, OSError;

PROCEDURE DoIt(pp : ParseParams.T) RAISES { ParseParams.Error, Wr.Failure, OSError.E };

END GenMain.
