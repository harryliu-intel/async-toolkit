(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

MODULE Database;
IMPORT Text;
IMPORT Debug;
IMPORT HTMLTable;
IMPORT DBerr;
FROM DatabaseClass IMPORT PrivateTable;
FROM DatabaseTable IMPORT Formatting, IsNull;
IMPORT Scan, Lex, FloatMode;
IMPORT Wr, Fmt, Thread, TextWr;

REVEAL 
  Table = PrivateTable BRANDED "SQL Table" OBJECT
  OVERRIDES
    init := InitTable;
    getColsByNames := GetTableColsByNames;
    getColByName := GetColByName;
    getColNames := GetTableColNames;
    getNumRows := GetNumRows;
    getNumCols := GetNumCols;
    rows := GetNumRows;
    cols := GetNumCols;

    getRow := GetRow;

    (*******************************)

    getUniqueEntry := GetUniqueEntry;

    (*******************************)

    getIsNull := GetIsNull;
    getIsNullI := GetIsNullI;

    get := GetUniqueEntry;
    getI := GetI;
    getInt := GetInt;
    getLR := GetLR;
    getIntI := GetIntI;
    getIntOrEmptyI := GetIntOrEmptyI;
    getIntOrZero := GetIntOrZero;
    getIntOrEmpty := GetIntOrEmpty;
    getLROrZero := GetLROrZero;
    getLROrEmpty := GetLROrEmpty;
    getLRI := GetLRI;

    getN := GetUniqueEntryN;
    getIN := GetIN;
    getIntN := GetIntN;
    getLRN := GetLRN;
    getIntIN := GetIntIN;
    getLRIN := GetLRIN;

    (*******************************)

    allColsToHTML := AllColsToHTML;
    colsToHTML := ColsToHTML;

    putAllColsAsTxtWithHeadings := PutAllColsAsTxtWithHeadings;
    putAllColsAsTxt := PutAllColsAsTxt;
  END;

(* really should prevent this from being re-entrant and also *)
(* the other functions to be called in an uninitialized state *)
(* use a boolean flag for this... *)

PROCEDURE InitTable(tab : Table; from : Result) : Table =
  BEGIN
    RETURN from.initTable(tab)
  END InitTable;

PROCEDURE AllColsToHTML(self : Table; 
                        formatting, rowFormatting : Formatting) : HTMLTable.T =
  <* FATAL DBerr.Error *>
  VAR
    colNames := self.getColNames();
  BEGIN 
    Debug.Out("AllColsToHTML called.");
    RETURN self.colsToHTML(colNames^, formatting, rowFormatting)
  END AllColsToHTML;

PROCEDURE GetRow(self : Table; idx : CARDINAL) : Table = 
  BEGIN
    WITH res = NEW(Table,
                   fieldNames := self.fieldNames,
                   data := NEW(Matrix, 1, NUMBER(self.data[0]))) DO
      res.data[0] := self.data[idx];
      
      RETURN res
    END
  END GetRow;

PROCEDURE ColsToHTML(self : Table; 
                     READONLY colNames : ARRAY OF TEXT;
                     formatting, rowFormatting : Formatting) : HTMLTable.T 
  RAISES { DBerr.Error } =
  VAR
    matrix := self.getColsByNames(colNames);
    namesA := NEW(REF ARRAY OF TEXT, NUMBER(colNames));
    res := NEW(HTMLTable.T).init(matrix, 
                                 colNames := namesA, 
                                 formatting := formatting,
                                 rowFormatting := rowFormatting);
    nameP := NEW(Vector, NUMBER(colNames));
  BEGIN 
    namesA^ := colNames;

    nameP^ := colNames;
    res.addRow(nameP, 0, HTMLTable.Whence.Top);
    RETURN res 
  END ColsToHTML;

PROCEDURE GetNumRows(self : Table) : CARDINAL =
  BEGIN RETURN NUMBER(self.data^) END GetNumRows;

PROCEDURE GetNumCols(self : Table) : CARDINAL =
  BEGIN RETURN NUMBER(self.fieldNames^) END GetNumCols;

PROCEDURE GetColIndex(table : Table; 
                      colName : TEXT) : CARDINAL RAISES { DBerr.Error } =
  VAR
    checkName := ToLower(colName);
  BEGIN
    FOR i := FIRST(table.fieldNames^) TO LAST(table.fieldNames^) DO
      IF Text.Equal(ToLower(table.fieldNames[i]), checkName) THEN
        RETURN i
      END
    END;
    RAISE DBerr.Error("No such field " & colName)
  END GetColIndex;

PROCEDURE GetTableColNames(self : Table) : Vector =
  BEGIN RETURN self.fieldNames END GetTableColNames;

PROCEDURE GetIsNull(self : Table; 
                         colName : TEXT) : BOOLEAN RAISES { DBerr.Error } =
  VAR
    matrix := self.data;
    colIndex := GetColIndex(self,colName);
  BEGIN
    IF NUMBER(matrix^) > 1 THEN RAISE DBerr.Error("GetUniqueEntry called on non-unique entry " & colName) END;
    IF NUMBER(matrix^) = 0 THEN RAISE DBerr.Error("GetUniqueEntry (" & colName & 
      ") called on empty matrix") 
    END;
    RETURN matrix[0,colIndex] = NIL
  END GetIsNull;

PROCEDURE GetIsNullI(self : Table; colIdx : CARDINAL) : BOOLEAN RAISES { DBerr.Error } =
  VAR
    matrix := self.data;
    colName := self.fieldNames[colIdx];
  BEGIN
    IF NUMBER(matrix^) > 1 THEN RAISE DBerr.Error("GetUniqueEntry called on non-unique entry " & colName) END;
    IF NUMBER(matrix^) = 0 THEN RAISE DBerr.Error("GetUniqueEntry (" & colName & 
      ") called on empty matrix") 
    END;
    RETURN matrix[0,colIdx] = NIL
  END GetIsNullI;

(**********************************************************************)

PROCEDURE GetUniqueEntry(self : Table; 
                         colName : TEXT) : TEXT RAISES { DBerr.Error } =
  VAR
    matrix := self.data;
    colIndex := GetColIndex(self,colName);
  BEGIN
    IF NUMBER(matrix^) > 1 THEN RAISE DBerr.Error("GetUniqueEntry called on non-unique entry " & colName) END;
    IF NUMBER(matrix^) = 0 THEN RAISE DBerr.Error("GetUniqueEntry (" & colName & 
      ") called on empty matrix") 
    END;
    WITH r = matrix[0,colIndex] DO
      IF r = NIL THEN RETURN "" ELSE RETURN r END
    END
  END GetUniqueEntry;

PROCEDURE GetI(self : Table; colIdx : CARDINAL) : TEXT RAISES { DBerr.Error } =
  VAR
    matrix := self.data;
    colName := self.fieldNames[colIdx];
  BEGIN
    IF NUMBER(matrix^) > 1 THEN RAISE DBerr.Error("GetUniqueEntry called on non-unique entry " & colName) END;
    IF NUMBER(matrix^) = 0 THEN RAISE DBerr.Error("GetUniqueEntry (" & colName & 
      ") called on empty matrix") 
    END;
    WITH r = matrix[0,colIdx] DO
      IF r = NIL THEN RETURN "" ELSE RETURN r END
    END
  END GetI;

(**********************************************************************)
PROCEDURE    GetInt(self : Table; colName : TEXT) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap } =
  BEGIN RETURN Scan.Int(self.get(colName)) END GetInt;

PROCEDURE     GetLR(self : Table; colName : TEXT) : LONGREAL 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap } =
  BEGIN RETURN Scan.LongReal(self.get(colName)) END GetLR;

PROCEDURE     GetIntI(self : Table; colNum : CARDINAL) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap }=
  BEGIN RETURN Scan.Int(self.getI(colNum)) END GetIntI;

PROCEDURE     GetIntOrEmptyI(self : Table; 
                             colNum : CARDINAL;
                             emptyVal : INTEGER) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap }=
  BEGIN 
    WITH txt = self.getI(colNum) DO
      IF Text.Equal(txt,"") THEN RETURN emptyVal ELSE RETURN Scan.Int(txt) END
    END
  END GetIntOrEmptyI;

PROCEDURE     GetIntOrEmpty(self : Table; 
                            colName : TEXT;
                            emptyVal : INTEGER) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap }=
  BEGIN 
    WITH txt = self.get(colName) DO
      IF Text.Equal(txt,"") THEN RETURN emptyVal ELSE RETURN Scan.Int(txt) END
    END
  END GetIntOrEmpty;

PROCEDURE     GetIntOrZero(self : Table; 
                            colName : TEXT) : INTEGER 
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap }=
  BEGIN 
    RETURN self.getIntOrEmpty(colName,0)
  END GetIntOrZero;

PROCEDURE     GetLROrZero(self : Table; 
                            colName : TEXT) : LONGREAL
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap }=
  BEGIN 
    RETURN self.getLROrEmpty(colName,0.0d0)
  END GetLROrZero;

PROCEDURE     GetLROrEmpty(self : Table; 
                            colName : TEXT;
                            emptyVal : LONGREAL) : LONGREAL
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap }=
  BEGIN 
    WITH txt = self.get(colName) DO
      IF Text.Equal(txt,"") THEN RETURN emptyVal ELSE RETURN Scan.LongReal(txt) END
    END
  END GetLROrEmpty;

PROCEDURE     GetLRI(self : Table; colNum : CARDINAL) : LONGREAL
      RAISES { DBerr.Error, Lex.Error, FloatMode.Trap } =
  BEGIN RETURN Scan.LongReal(self.getI(colNum)) END GetLRI;
  

(**********************************************************************)

PROCEDURE GetUniqueEntryN(self : Table; 
                         colName : TEXT) : TEXT RAISES { IsNull, 
                                                         DBerr.Error } =
  VAR
    matrix := self.data;
    colIndex := GetColIndex(self,colName);
  BEGIN
    IF GetIsNullI(self,colIndex) THEN RAISE IsNull END;

    IF NUMBER(matrix^) > 1 THEN RAISE DBerr.Error("GetUniqueEntry called on non-unique entry " & colName) END;
    IF NUMBER(matrix^) = 0 THEN RAISE DBerr.Error("GetUniqueEntry (" & colName & 
      ") called on empty matrix") 
    END;
    WITH r = matrix[0,colIndex] DO
      IF r = NIL THEN RETURN "" ELSE RETURN r END
    END
  END GetUniqueEntryN;

PROCEDURE GetIN(self : Table; colIdx : CARDINAL) : TEXT RAISES { IsNull, 
                                                                 DBerr.Error } =
  VAR
    matrix := self.data;
    colName := self.fieldNames[colIdx];
  BEGIN
    IF GetIsNullI(self,colIdx) THEN RAISE IsNull END;
    IF NUMBER(matrix^) > 1 THEN RAISE DBerr.Error("GetUniqueEntry called on non-unique entry " & colName) END;
    IF NUMBER(matrix^) = 0 THEN RAISE DBerr.Error("GetUniqueEntry (" & colName & 
      ") called on empty matrix") 
    END;
    WITH r = matrix[0,colIdx] DO
      IF r = NIL THEN RETURN "" ELSE RETURN r END
    END
  END GetIN;

(**********************************************************************)
PROCEDURE    GetIntN(self : Table; colName : TEXT) : INTEGER 
      RAISES { IsNull, DBerr.Error, Lex.Error, FloatMode.Trap } =
  BEGIN RETURN Scan.Int(self.getN(colName)) END GetIntN;

PROCEDURE     GetLRN(self : Table; colName : TEXT) : LONGREAL 
      RAISES { IsNull, DBerr.Error, Lex.Error, FloatMode.Trap } =
  BEGIN RETURN Scan.LongReal(self.getN(colName)) END GetLRN;

PROCEDURE     GetIntIN(self : Table; colNum : CARDINAL) : INTEGER 
      RAISES { IsNull, DBerr.Error, Lex.Error, FloatMode.Trap }=
  BEGIN RETURN Scan.Int(self.getIN(colNum)) END GetIntIN;

PROCEDURE     GetLRIN(self : Table; colNum : CARDINAL) : LONGREAL
      RAISES { IsNull, DBerr.Error, Lex.Error, FloatMode.Trap } =
  BEGIN RETURN Scan.LongReal(self.getIN(colNum)) END GetLRIN;
  

(**********************************************************************)

PROCEDURE GetTableColsByNames (self : Table; 
                               READONLY colNames : ARRAY OF TEXT) : Matrix 
  RAISES { DBerr.Error } =
  VAR
    res := NEW(Matrix, NUMBER(self.data^), NUMBER(colNames));
  BEGIN
    FOR col := 0 TO LAST(colNames) DO
      VAR 
        colIndex := GetColIndex(self,colNames[col]); 
      BEGIN
       (* Wr.PutText(Stdio.stdout, "Getting col: " & colNames[col] & ": " & Fmt.Int(colIndex) & "\n"); *)
        FOR row := 0 TO LAST(res^) DO
          WITH d = self.data[row,colIndex] DO
            IF d = NIL THEN res[row,col] := "" ELSE res[row,col] := d END
          END
        END
      END
    END;
    RETURN res
  END GetTableColsByNames;

PROCEDURE GetColByName (self : Table; 
                               name :  TEXT) : Vector
  RAISES { DBerr.Error } =
  VAR
    res := NEW(Vector, NUMBER(self.data^));
    colIndex := GetColIndex(self,name);
  BEGIN
    FOR row := 0 TO LAST(res^) DO
      res[row] := self.data[row,colIndex]
    END;
    RETURN res
  END GetColByName;

PROCEDURE ToLower ( mixed : TEXT ) : TEXT =
  VAR 
    lower := NEW(REF ARRAY OF CHAR, Text.Length(mixed));
  BEGIN
    Text.SetChars(lower^,mixed);
    FOR i := 0 TO LAST(lower^) DO WITH l = lower[i] DO
      IF l >= 'A' AND l <= 'Z' THEN
        l := VAL( ORD(l) - ORD('A') + ORD('a') , CHAR )
      END
    END END;
    RETURN Text.FromChars(lower^)
  END ToLower;

PROCEDURE PutAllColsAsTxtWithHeadings(tab : Table;
                           wr : Wr.T;
                           READONLY headings : ARRAY OF TEXT;
                           ) RAISES { Wr.Failure, DBerr.Error,
                                      Thread.Alerted }=

  PROCEDURE UnNil(VAR a : ARRAY OF TEXT) =
    BEGIN
      FOR i := FIRST(a) TO LAST(a) DO
        IF a[i] = NIL THEN a[i] := "" END
      END
    END UnNil;

  CONST
    Spaces = " ";
    Dashes = "=";
    Dash   = '=';
  VAR
    widths := NEW(REF ARRAY OF CARDINAL, NUMBER(headings));
    sqlnames := tab.getColNames();
    col : Vector;
  BEGIN
    FOR i := FIRST(widths^) TO LAST(widths^) DO
      widths[i] := Text.Length(headings[i]);
      col := tab.getColByName(sqlnames[i]);

      UnNil(col^);

      FOR j := FIRST(col^) TO LAST(col^) DO
        widths[i] := MAX(widths[i],Text.Length(col[j]))
      END;
    END;
    FOR i := FIRST(widths^) TO LAST(widths^) DO
      Wr.PutText(wr, Spaces);
      Wr.PutText(wr, Fmt.Pad(headings[i],widths[i]));
      Wr.PutText(wr, Spaces)
    END;
    Wr.PutChar(wr,'\n');
    FOR i := FIRST(widths^) TO LAST(widths^) DO
      Wr.PutText(wr, Dashes);
      Wr.PutText(wr, Fmt.Pad("",widths[i],padChar:=Dash));
      Wr.PutText(wr, Dashes)
    END;
    Wr.PutChar(wr,'\n');
    FOR r := 0 TO tab.getNumRows() - 1 DO
      FOR i := FIRST(widths^) TO LAST(widths^) DO
        col := tab.getColByName(sqlnames[i]);

        UnNil(col^);

        Wr.PutText(wr, Spaces);
        Wr.PutText(wr, Fmt.Pad(col[r],widths[i]));
        Wr.PutText(wr, Spaces)
      END;
      Wr.PutChar(wr,'\n');
    END;
  END PutAllColsAsTxtWithHeadings;

PROCEDURE PutAllColsAsTxt(tab : Table;
                           wr : Wr.T) RAISES { Wr.Failure, DBerr.Error,
                                               Thread.Alerted } =
  BEGIN
    tab.putAllColsAsTxtWithHeadings(wr,tab.fieldNames^)
  END PutAllColsAsTxt;

PROCEDURE Escape(t : TEXT; quoted : BOOLEAN) : TEXT =
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    IF quoted THEN Wr.PutChar(wr, '\'') END;

    FOR i := 0 TO Text.Length(t)-1 DO
      WITH c = Text.GetChar(t,i) DO
        IF c = '\'' THEN
          Wr.PutText(wr, "\\'")
        ELSE
          Wr.PutChar(wr, c)
        END
      END
    END;

    IF quoted THEN Wr.PutChar(wr, '\'') END;

    RETURN TextWr.ToText(wr)
  END Escape;

PROCEDURE EscapeQ(t : TEXT) : TEXT =
  BEGIN RETURN Escape(t,quoted := TRUE) END EscapeQ;

PROCEDURE Unescape(t : TEXT) : TEXT =
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    FOR i := 0 TO Text.Length(t)-1 DO
      WITH c = Text.GetChar(t,i) DO
        IF c = '\\' THEN
          (* skip *)
        ELSE
          Wr.PutChar(wr, c)
        END
      END
    END;
    RETURN TextWr.ToText(wr)
  END Unescape;

BEGIN END Database.


