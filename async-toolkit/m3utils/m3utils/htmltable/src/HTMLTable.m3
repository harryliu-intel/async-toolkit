(* $Id$ *)

MODULE HTMLTable;
IMPORT HTML;
IMPORT Fmt, Debug;
IMPORT HTMLFormatting;
IMPORT Text;
IMPORT RefSeq;
IMPORT TextWr, Wr, Thread;
IMPORT DBerr;

<* FATAL Thread.Alerted *>

REVEAL
  T = Public BRANDED "Table" OBJECT
    fields : HTML.Matrix;
    useBorders := FALSE;
    borderWidth := 0;
    colNames : REF ARRAY OF TEXT;
    formatting : HTMLFormatting.T;
    rowFormatting : HTMLFormatting.RowFormat;
    firstIsHead : BOOLEAN;
  OVERRIDES
    init := Init;
    format := Format;
    addRow := AddRow;
  END;

PROCEDURE AddRow(self : T; 
                 stuff: AVector; row : CARDINAL; 
                 whence : Whence) =
  VAR
    replace : CARDINAL;
    newFields : HTML.Matrix;
  BEGIN
    (* cant grow matrix in width... really a bug *)

    (* need to fix this bug!!!! *)

    IF self.fields = NIL OR NUMBER(self.fields^) = 0 THEN 
      self.fields := NEW(HTML.Matrix,0,NUMBER(HTML.WrapVector(stuff)^)) 
    END;
    IF NUMBER(self.fields^) > 0 THEN
      newFields := NEW(HTML.Matrix, 
                       NUMBER(self.fields^) + 1, NUMBER(self.fields[0]))
    ELSE
      newFields := NEW(HTML.Matrix, 
                       1, 
                       NUMBER(HTML.WrapVector(stuff)^))
    END;

    CASE whence OF
    | Whence.Top =>    replace := row
    | Whence.Bottom => replace := NUMBER(self.fields^) - row
    END;

    Debug.Out("NUMBER(newFields^): " & Fmt.Int(NUMBER(newFields^)));
    Debug.Out("NUMBER(self.fields^): " & Fmt.Int(NUMBER(self.fields^)));

    IF NUMBER(newFields^) >0  THEN
      Debug.Out("NUMBER(newFields[0]): " & Fmt.Int(NUMBER(newFields[0])))
    END;

    IF NUMBER(self.fields^) >0  THEN
      Debug.Out("NUMBER(self.fields[0]): " & Fmt.Int(NUMBER(self.fields[0])))
    END;

    Debug.Out("replace = " & Fmt.Int(replace));

    SUBARRAY(newFields^, 0 , replace) := SUBARRAY(self.fields^, 0, replace);
    newFields[replace] := HTML.WrapVector(stuff)^;

    SUBARRAY(newFields^, replace + 1, NUMBER(self.fields^) - replace) :=
        SUBARRAY(self.fields^, replace , NUMBER(self.fields^) - replace);
    
    self.fields := newFields
  END AddRow;

PROCEDURE Init(self : T; stuff : REFANY; 
               useBorders : BOOLEAN; borderWidth : CARDINAL;
               colNames : REF ARRAY OF TEXT;
               formatting : HTMLFormatting.T;
               rowFormatting : HTMLFormatting.RowFormat;
               firstIsHead : BOOLEAN) : T =
  BEGIN
    self.fields := HTML.WrapMatrix(stuff);
    self.useBorders := useBorders; 
    self.borderWidth := borderWidth;
    self.formatting := formatting;
    self.colNames := colNames;
    self.rowFormatting := rowFormatting;
    self.firstIsHead := firstIsHead;
    RETURN self;
  END Init;

PROCEDURE Format(self : T) : TEXT =

  PROCEDURE Put(t1, t2, t3, t4 : TEXT := NIL) =
    <* FATAL Wr.Failure *>
    BEGIN
      WITH t = ARRAY [0..3] OF TEXT { t1, t2, t3, t4 } DO
        FOR i := FIRST(t) TO LAST(t) DO
          IF t[i] # NIL THEN Wr.PutText(tWr, t[i]) END
        END
      END
    END Put;

  VAR
    tWr := NEW(TextWr.T).init();
  BEGIN
    IF self.fields = NIL THEN self.fields := NEW(HTML.Matrix,0,0) END;
    Put("<table ");
    IF self.useBorders THEN
      Put("border"); 
      IF self.borderWidth > 0 THEN
        Put("=",Fmt.Int(self.borderWidth))
      END
    END;
    Put(">\n");

    IF self.colNames # NIL THEN
      Put("<colgroup>\n");
      FOR i := FIRST(self.colNames^) TO LAST(self.colNames^) DO
        Put("  <col");
        VAR p := self.formatting; BEGIN
          WHILE p # NIL DO
            IF p.matchesByName(self.colNames[i]) THEN
              Put(" "); Put(p.colAttrs())
            END;
            p := p.next
          END
        END;
        Put(">\n");
      END;
      Put("<thead>\n")
    END;

    FOR row := FIRST(self.fields^) TO LAST(self.fields^) DO
      IF NOT self.firstIsHead AND row = FIRST(self.fields^) THEN
        Put("<tbody>\n")
      ELSIF self.firstIsHead AND row = FIRST(self.fields^)+1 THEN
        Put("<tbody>\n")
      END;

      Put("<tr");
      VAR
        p := self.rowFormatting;
      BEGIN
        WHILE p # NIL DO
          WITH r = NARROW(p,HTMLFormatting.RowFormat),
               bgcolor = r.bgcolor(), 
               align = r.align(),
               valign = r.valign() DO
            IF bgcolor # NIL THEN
              Put(" bgcolor=\"",bgcolor,"\"")
            END;
            IF align # NIL THEN
              Put(" align=\"",align,"\"")
            END;
            IF valign # NIL THEN
              Put(" valign=\"",valign,"\"")
            END
          END;
          p := p.next
        END
      END;
      Put(">\n");
      FOR col := FIRST(self.fields[row]) TO LAST (self.fields[row]) DO
        VAR
          entryTxt := NARROW(self.fields[row,col],HTML.T).format();
          printTD := TRUE;
          tags := "";
        BEGIN
          IF entryTxt = NIL THEN entryTxt := "(NIL)" END;
          IF self.formatting # NIL AND self.colNames # NIL THEN
            WITH colName = self.colNames[col] DO
              VAR
                p := self.formatting;
              BEGIN
                WHILE p # NIL DO
                  IF p.matchesByName(colName) THEN
                    tags := tags & " " & p.tdAttrs()
                  END;

                  TYPECASE p OF
                    HTMLFormatting.NamedColumnFormat(f) =>
                      IF Text.Equal(colName, f.column) THEN
                        IF ISTYPE(p,HTMLFormatting.DynamicTD) THEN
                          printTD := FALSE
                        END;
                        TRY
                          WITH formatted = f.format(entryTxt) DO
                            TYPECASE formatted OF
                              TEXT => entryTxt := formatted
                            |
                              HTML.T(html) => entryTxt := html.format()
                            ELSE
                              HTML.Error("HTMLTable.Format: Weird type for formatter!")
                            END
                          END(*WITH*)
                        EXCEPT
                          DBerr.Error(txt) =>
                            HTML.Error("HTMLTable.Format: caught DBerr.Error \"" & txt & "\"")
                        END
                      END
                  ELSE
                    (* NOP *)
                  END;
                  p := p.next
                END
              END
            END
          END;
          IF printTD THEN
            Put("<td"&tags&">",entryTxt,"</td>\n")
          ELSE
            Put(entryTxt,"\n")
          END
        END
      END;
      Put("</tr>\n")
    END;
    Put("</table>\n");
    RETURN TextWr.ToText(tWr)
  END Format;

PROCEDURE Horiz(c0, c1, c2, c3, c4, c5, c6 : HTML.Stuff;
                formatting : HTMLFormatting.T) : T =

  PROCEDURE Add(s : HTML.Stuff) =
    BEGIN IF s # NIL THEN stuff.addhi(s) END END Add;

  VAR
    stuff := NEW(RefSeq.T).init();
  BEGIN
    Add(c0); Add(c1); Add(c2); Add(c3); Add(c4); Add(c5); Add(c6);
    
    WITH arr = NEW(REF ARRAY OF ARRAY OF HTML.Stuff, 1, stuff.size()) DO
      FOR i := FIRST(arr[0]) TO LAST(arr[0]) DO
        arr[0,i] := stuff.get(i);
        <* ASSERT arr[0,i] # NIL *>
      END;
      
      RETURN NEW(T).init(arr, useBorders := FALSE,
                         formatting := formatting,
                         colNames := MakeTextCounter(stuff.size()))
    END
  END Horiz;

PROCEDURE Vert(c0, c1, c2, c3, c4, c5, c6 : HTML.Stuff;
               formatting : HTMLFormatting.T) : T =

  PROCEDURE Add(s : HTML.Stuff) =
    BEGIN IF s # NIL THEN stuff.addhi(s) END END Add;

  VAR
    stuff := NEW(RefSeq.T).init();
  BEGIN
    Add(c0); Add(c1); Add(c2); Add(c3); Add(c4); Add(c5); Add(c6);
    
    WITH arr = NEW(REF ARRAY OF ARRAY OF HTML.Stuff, stuff.size(), 1) DO
      FOR i := FIRST(arr^) TO LAST(arr^) DO
        arr[i,0] := stuff.get(i);
        <* ASSERT arr[i,0] # NIL *>
      END;
      
      RETURN NEW(T).init(arr, useBorders := FALSE,
                         colNames := MakeTextCounter(1),
                         formatting := formatting)
    END
  END Vert;

PROCEDURE MakeTextCounter(cnt : CARDINAL) : REF ARRAY OF TEXT =
  VAR
    res := NEW(REF ARRAY OF TEXT, cnt);
  BEGIN
    FOR i := 0 TO cnt-1 DO
      res[i] := Fmt.Int(i) 
    END;
    RETURN res
  END MakeTextCounter;

BEGIN
END HTMLTable.
