INTERFACE WmUtils;
IMPORT Word;

PROCEDURE GetUnnamedField(rvalue     : Word.T;
                          start, len : CARDINAL) : Word.T;

PROCEDURE ModfyUnnamedField(rvalue : Word.T;
                            start, len : CARDINAL;
                            value : Word.T) : Word.T;

END WmUtils.
