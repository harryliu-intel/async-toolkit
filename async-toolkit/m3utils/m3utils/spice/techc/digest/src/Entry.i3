INTERFACE Entry;

TYPE
  CsvCols = {

  (* independent variables : *)
  Tech, Corn, Tran, Cell, Mode,
  Simu, Cels, Fano, Volt, Temp,
  Sigm, Lcap,
  
  (* dependent variables : *)
  Cycl, Curr, Icur, Path,

  (* independent variables : *)
  MoNm

  };
  
  
  Entry = ARRAY CsvCols OF TEXT;
  T = REF Entry;

CONST Brand = "Entry";

PROCEDURE Compare(a, b : T) : [ -1 .. 1 ];
  (* sort by Volt *)

PROCEDURE CompareLR(a, b : T; col : CsvCols) : [-1 .. 1];
  
CONST
  CsvColNames = ARRAY CsvCols OF TEXT 
  { "Tech", "Corn", "Tran", "Cell", "Mode",
    "Simu", "Cels", "Fano", "Volt", "Temp",
    "Sigm", "Lcap",
    "Cycl", "Curr", "Icur", "Path",
    "MoNm"

  };


PROCEDURE Format(t : T) : TEXT;
  
END Entry.
