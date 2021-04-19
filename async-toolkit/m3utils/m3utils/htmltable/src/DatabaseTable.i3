(* $Id$ *)

INTERFACE DatabaseTable;
IMPORT DBerr, HTMLTable;
IMPORT HTMLFormatting;
IMPORT Lex, FloatMode;
IMPORT Thread, Wr;

EXCEPTION IsNull;

TYPE 
  T <: Public;

  Vector = REF ARRAY OF TEXT; (* can be either row or col *)
  Matrix = REF ARRAY OF ARRAY OF TEXT;

  (* the Result object has to be implemented by each type of database 
     engine adapter *)
  Result = OBJECT METHODS
    initTable(table : T) : T;
  END;

  Public = OBJECT
  METHODS
    init(from : Result) : T;
    getColsByNames(READONLY colNames : ARRAY OF TEXT) : Matrix RAISES { DBerr.Error };
    getColByName(named : TEXT) : Vector RAISES { DBerr.Error };
    getColNames() : Vector;

    getUniqueEntry(colName : TEXT) : TEXT RAISES { DBerr.Error };

    getIsNull(colName : TEXT) : BOOLEAN RAISES { DBerr.Error };
    getIsNullI(colNum : CARDINAL) : BOOLEAN RAISES { DBerr.Error };

    get(colName : TEXT) : TEXT RAISES { DBerr.Error };  (* same as above *)
    getI(colNum : CARDINAL) : TEXT RAISES { DBerr.Error };

    getInt(colName : TEXT) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };
    getLR(colName : TEXT) : LONGREAL
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };
    getIntI(colNum : CARDINAL) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };
    getIntOrEmpty(colName : TEXT; emptyValue : INTEGER) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };
    getIntOrZero(colName : TEXT) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };
    getLROrEmpty(colName : TEXT; emptyValue : LONGREAL) : LONGREAL
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };
    getLROrZero(colName : TEXT) : LONGREAL
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };
    getIntOrEmptyI(colNum : CARDINAL; emptyValue : INTEGER) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };
    getLRI(colNum : CARDINAL) : LONGREAL
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap };

    (****************************************)

    getN(colName : TEXT) : TEXT RAISES { IsNull, DBerr.Error };  (* same as above *)
    getIN(colNum : CARDINAL) : TEXT RAISES { IsNull, DBerr.Error };

    getIntN(colName : TEXT) : INTEGER 
      RAISES { IsNull, DBerr.Error, Lex.Error, FloatMode.Trap };
    getLRN(colName : TEXT) : LONGREAL
      RAISES { IsNull, DBerr.Error, Lex.Error, FloatMode.Trap };
    getIntIN(colNum : CARDINAL) : INTEGER 
      RAISES { IsNull, DBerr.Error, Lex.Error, FloatMode.Trap };
    getLRIN(colNum : CARDINAL) : LONGREAL
      RAISES { IsNull, DBerr.Error, Lex.Error, FloatMode.Trap };



    getNumRows() : CARDINAL;
    rows() : CARDINAL; (* same as above *)

    getNumCols() : CARDINAL;
    cols() : CARDINAL; (* same as above *)

    getRow(index : CARDINAL) : T; (* get a single-row table ... *)

    allColsToHTML(formatting, rowFormatting : Formatting := NIL) : HTMLTable.T;
    colsToHTML(READONLY colNames : ARRAY OF TEXT;
               formatting, rowFormatting : Formatting := NIL) : HTMLTable.T RAISES { DBerr.Error };

    putAllColsAsTxtWithHeadings(wr : Wr.T;
                    READONLY headings : ARRAY OF TEXT)
      RAISES { Wr.Failure,Thread.Alerted,DBerr.Error }; 

    putAllColsAsTxt(wr:Wr.T) RAISES { Wr.Failure,Thread.Alerted,DBerr.Error };
      (* this one just uses the default headings *)
  END;

TYPE Formatting = HTMLFormatting.T;

END DatabaseTable.
