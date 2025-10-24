INTERFACE ModelCWrite;
IMPORT Word;

<*EXTERNAL set_model_c2m3callback*>
PROCEDURE SetCallback(cb : CbProc);

TYPE CbProc = PROCEDURE(addr : ADDRESS; val : Word.T);

END ModelCWrite.
