INTERFACE AreaSpec;
IMPORT Entry;

TYPE
  T = RECORD
    col    : Entry.CsvCols;
    colVal : TEXT;
    area   : LONGREAL;
  END;

CONST Brand = "AreaSpec";

END AreaSpec.
