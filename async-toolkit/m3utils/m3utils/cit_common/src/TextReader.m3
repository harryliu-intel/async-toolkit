(* $Id$ *)
MODULE TextReader;
IMPORT Text, TextList;
(*IMPORT Debug,Fmt;*)
IMPORT Rd, Wr, Thread, RdCopy, TextWr;

EXCEPTION IncompatibleDelimiters;

REVEAL 
  T = Public BRANDED "TextReader" OBJECT

    (* for efficiency, we can advance in the text without reallocating *)
    (* the line contains valid data from character 0 to Text.Length(line) *)
    line : TEXT;
    start : CARDINAL := 0;
  OVERRIDES
    next := Next;
    nextE := NextE;
    init := Init;
    initFromRd := InitFromRd;
    isEmpty := IsEmpty;
    shatter := Shatter;
  END;

PROCEDURE NextE(self : T; 
                delims : TEXT; skipNulls : BOOLEAN) : TEXT RAISES { NoMore } = 
  VAR res : TEXT; BEGIN 
    IF self.next(delims,res,skipNulls) THEN RETURN res
    ELSE RAISE NoMore
    END
  END NextE;

PROCEDURE IsEmpty(self : T) : BOOLEAN = 
  BEGIN RETURN Text.Length(self.line) <= self.start END IsEmpty;

PROCEDURE Next(self : T; 
               delims : TEXT; VAR res : TEXT; skipNulls : BOOLEAN) : BOOLEAN =
  VAR
    darr := NEW(REF ARRAY OF CHAR, Text.Length(delims));
    min := Text.Length(self.line);
  BEGIN
    Text.SetChars(darr^,delims);
    FOR i := 0 TO LAST(darr^) DO
      VAR
        where := Text.FindChar(self.line,darr[i], self.start);
      BEGIN
        IF where >= 0 AND where < min THEN min := where END;
      END;
    END;
    res := Text.Sub(self.line,self.start,min);

    (* this can be implemented simply by increasing self.start *)
    self.line := Text.Sub(self.line,min+1,LAST(CARDINAL)); (* save rest *)
    IF Text.Length(res) = 0 AND Text.Length(self.line) <= self.start THEN 
      RETURN FALSE 
    END;
    
    IF Text.Length(res) = 0 AND skipNulls THEN
      RETURN Next(self,delims,res,skipNulls)
    END;

    RETURN TRUE
  END Next;

PROCEDURE NextS(self : T; 
                delims : SET OF CHAR; 
                VAR res : TEXT; 
                skipNulls : BOOLEAN) : BOOLEAN =
  BEGIN
    FOR i := self.start TO Text.Length(self.line) DO
      IF Text.GetChar(self.line, i) IN delims THEN
        res := Text.Sub(self.line, self.start, i);
        self.line := Text.Sub(self.line, i+1, LAST(CARDINAL));

        IF i # self.start OR NOT skipNulls THEN 
          RETURN TRUE 
        ELSE
          RETURN NextS(self,delims,res,skipNulls)
        END
      END
    END;

    (* fall-through case: return everything since we can't find a 
       delimiter ... *)

    res := self.line;
    RETURN FALSE
  END NextS;

PROCEDURE Init(self: T; line : TEXT) : T =
  BEGIN self.line := line; RETURN self END Init;

PROCEDURE InitFromRd(self : T; rd : Rd.T) : T RAISES { Rd.Failure, Thread.Alerted } =
  <* FATAL Wr.Failure *> (* cant happen *)
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    EVAL RdCopy.ToWriter(rd, wr);
    RETURN self.init(TextWr.ToText(wr))
  END InitFromRd;

PROCEDURE Shatter(self : T; listDelims : TEXT; 
            endDelims : TEXT; skipNulls := FALSE) : TextList.T =

  <* FATAL IncompatibleDelimiters *> (* force a program abort *)

  PROCEDURE CheckOverlap() =
    BEGIN
      FOR i := 0 TO Text.Length(listDelims) - 1 DO
        IF Text.FindChar(endDelims, Text.GetChar(listDelims, i)) # -1 THEN
          RAISE IncompatibleDelimiters
        END
      END
    END CheckOverlap;

  VAR
    res : TextList.T := NIL;
  BEGIN
    CheckOverlap();
    TRY
      VAR 
        reader := NEW(T).init(self.nextE(endDelims)); 
        word : TEXT; 
      BEGIN
        WHILE Next(reader, listDelims, word, skipNulls) DO
          res := TextList.Cons(word, res)
        END;
        res := TextList.ReverseD(res)
      END
    EXCEPT NoMore => <* ASSERT res = NIL *> (* skip *)
    END;
    RETURN res
  END Shatter;

BEGIN END TextReader.
