MODULE WmUtils;
IMPORT Word;

PROCEDURE GetUnnamedField(rvalue     : Word.T;
                          start, len : CARDINAL) : Word.T =
  BEGIN
    RETURN Word.Extract(rvalue, start, len)
  END GetUnnamedField;

PROCEDURE ModfyUnnamedField(rvalue : Word.T;
                            start, len : CARDINAL;
                            value : Word.T) : Word.T =
  BEGIN
    RETURN Word.Insert(rvalue, value, start, len)
  END ModfyUnnamedField;

BEGIN END WmUtils.
