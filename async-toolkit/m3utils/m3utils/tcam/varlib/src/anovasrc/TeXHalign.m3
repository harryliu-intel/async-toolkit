(* $Id: TeXHalign.m3,v 1.4 2006/02/27 23:35:49 mika Exp $ *)

MODULE TeXHalign;
IMPORT RefList, CardSet;
IMPORT Wr, Thread, TextWr, Fmt;

TYPE NW = { N, W };
TYPE BSet = SET OF NW;
CONST EmptyBSet = BSet {};

REVEAL
  T = Public BRANDED Brand OBJECT
    data : REF ARRAY OF ARRAY OF TEXT;
    boundaries : REF ARRAY OF ARRAY OF BSet;
    justification : REF ARRAY OF Justification;
    gangs : RefList.T := NIL; (* list of CardSet.Ts *)
  OVERRIDES
    init := Init;
    put := Put;
    addRule := AddRule;
    makeColumnGang := MakeColumnGang;
    makeTeX := MakeTeX;
    setJustification := SetJustification;
  END;

PROCEDURE Init(t : T; h, w : CARDINAL) : T = 
  BEGIN
    t.data := NEW(REF ARRAY OF ARRAY OF TEXT, h, w);
    t.boundaries := NEW(REF ARRAY OF ARRAY OF BSet, h+1,w+1);
    t.justification := NEW(REF ARRAY OF Justification, w);
    
    FOR i := FIRST(t.justification^) TO LAST(t.justification^) DO
      t.justification[i] := Justification.Right
    END;

    FOR i := FIRST(t.data^) TO LAST(t.data^) DO
      FOR j := FIRST(t.data[0]) TO LAST(t.data[0]) DO
        t.data[i,j] := ""
      END
    END;

    FOR i := FIRST(t.boundaries^) TO LAST(t.boundaries^) DO
      FOR j := FIRST(t.boundaries[0]) TO LAST(t.boundaries[0]) DO
        t.boundaries[i,j] := EmptyBSet
      END
    END;

    RETURN t
  END Init;

PROCEDURE SetJustification(t : T; col : CARDINAL; j : Justification) =
  BEGIN t.justification[col] := j END SetJustification;

PROCEDURE Put(t : T; p : Point; what : TEXT) = 
  BEGIN 
    t.data[p.row, p.col] := what
  END Put;

PROCEDURE AddRule(t : T; b1, b2 : Point) =
  BEGIN
    <* ASSERT (b1.row = b2.row AND ABS(b1.col-b2.col) = 1) OR 
              (b1.col = b2.col AND ABS(b1.row-b2.row) = 1) *>
    WITH bs = t.boundaries[b2.row, b2.col] DO
      IF b1.row = b2.row THEN

        <* ASSERT b1.row # LAST(t.boundaries^) *> 
        (* no sideways dividers below table *)

        bs := bs + BSet { NW.W }
      ELSE
        <* ASSERT b1.col # LAST(t.boundaries[0]) *>
        (* no vert dividers to right of table *)

        bs := bs + BSet { NW.N }
      END
    END
  END AddRule;

PROCEDURE MakeColumnGang(t : T; cols : CardSet.T) =
  BEGIN
    t.gangs := RefList.Cons(cols, t.gangs)
  END MakeColumnGang;

PROCEDURE MakeTeX(t : T) : TEXT =
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN 
    PutTeX(t,wr);
    RETURN TextWr.ToText(wr)
  END MakeTeX;

PROCEDURE PutTeX(t : T; wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Wr.PutText(wr, "\\halign{\n");
    PutTeXPreamble(t, wr);
    FOR i := FIRST(t.data^) TO LAST(t.data^) DO
      (* top rules, if any *)
      PutTeXRowTop(t, wr, i);
      
      (* actual text in row *)
      PutTeXRowBody(t, wr, i)
    END;
    
    (* bottom rules, if any---are top rules of "LAST+1'th row" *)
    PutTeXRowTop(t, wr, LAST(t.data^)+1); 

    Wr.PutText(wr, "}\n")
  END PutTeX;

(* generally speaking, 
   kth text entry is in position 2k+1 (starting from 0) 
   divider between (k-1)th and kth text entries is in position 2k
 *)

PROCEDURE PutTeXPreamble(t : T; wr : Wr.T) 
  RAISES 
    { Wr.Failure, Thread.Alerted } =
  CONST 
    space = "\\quad";
  BEGIN
    Wr.PutText(wr, "%%% PREAMBLE (" & Fmt.Int(NUMBER(t.data[0])) & 
      " columns) %%%\n");
    FOR i := FIRST(t.data[0]) TO LAST(t.data[0]) DO
      Wr.PutText(wr, "\\vrule#&");

      CASE t.justification[i] OF
        Justification.Left =>   Wr.PutText(wr, space & "#" & space & "\\hfil")
      |
        Justification.Center => Wr.PutText(wr, "\\hfil"&space&"#"&space&"\\hfil")
      |
        Justification.Right =>  Wr.PutText(wr, "\\hfil"&space&"#"&space)
      END;
      Wr.PutText(wr, "&")
    END;
    Wr.PutText(wr, "\\vrule#\\cr\n")
    
  END PutTeXPreamble;

PROCEDURE PutTeXRowTop(t : T; wr : Wr.T; row : CARDINAL) 
  RAISES 
    { Wr.Failure, Thread.Alerted } =
  BEGIN
    (* dividing line *)
    WITH b = t.boundaries[row] DO
      FOR i := FIRST(b) TO LAST(b) DO
        IF b[i] # EmptyBSet OR 
           i > 0 AND NW.N IN b[i-1] THEN
          (* fill in divider square if 
             (1) we are either N or W
             (2) cell to W is N
             (3) (SKIP THIS ONE) cell to N is W
          *)
          Wr.PutText(wr, "height0.4pt")
        ELSE
          Wr.PutText(wr, "\\omit\\vbox to 0.4pt{}")
        END;
        IF i # LAST(b) THEN
          IF NW.N IN b[i] THEN
            (* fill in top line *)
            Wr.PutText(wr, "&\\omit\\hrulefill&")
          ELSE
            Wr.PutText(wr, "&\\omit&")
          END
        ELSE
          Wr.PutText(wr, "\\cr\n")
        END
      END (* FOR i *);

      (* top spacer *)
      PutTeXVerticalSpacer(t, wr, row)
    END
  END PutTeXRowTop;

PROCEDURE PutTeXVerticalSpacer(t : T; wr : Wr.T; row : CARDINAL)  
  RAISES 
    { Wr.Failure, Thread.Alerted } =
  BEGIN
    WITH b = t.boundaries[row] DO
      FOR i := FIRST(b) TO LAST(b) DO
        IF NW.W IN b[i] THEN
          Wr.PutText(wr, "height2pt")
        ELSE
          Wr.PutText(wr, "\\omit\\vbox to 2pt{}")
        END;
        IF i = LAST(b) THEN
          Wr.PutText(wr, "\\cr\n")
        ELSE
          Wr.PutText(wr, "&\\omit&")
        END
      END (* FOR i *);
    END
  END PutTeXVerticalSpacer;

PROCEDURE PutTeXRowBody(t : T; wr : Wr.T; row : CARDINAL) 
  RAISES 
    { Wr.Failure, Thread.Alerted } =
  BEGIN
    (* line data *)
    WITH b = t.boundaries[row],
         d = t.data[row] DO
      <* ASSERT NUMBER(b) = NUMBER(d) + 1 *>
      FOR i := FIRST(d) TO LAST(d) DO
        IF NW.W IN b[i] THEN
          Wr.PutText(wr, "&" & d[i] & "&")
        ELSE
          Wr.PutText(wr, "\\omit&"& d[i] & "&")
        END
      END;
      IF NW.W IN b[LAST(b)] THEN
        Wr.PutText(wr, "\\cr\n")
      ELSE
        Wr.PutText(wr, "\\omit\\cr\n")
      END
    END;
    
    (* bottom spacer *)
    PutTeXVerticalSpacer(t,wr,row)
  END PutTeXRowBody;

BEGIN END TeXHalign.


