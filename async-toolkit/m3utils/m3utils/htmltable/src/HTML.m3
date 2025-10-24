(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE HTML;
IMPORT HTMLPage, HTMLOutput, HTMLText;
IMPORT Debug, Session;

EXCEPTION
  Abort;

REVEAL
  T = Public BRANDED "HTML Element" OBJECT
    session : Session.T;
    up : T;
  OVERRIDES
    init := Init;
    getSession := GetSession;
  END;

PROCEDURE GetSession(self : T) : Session.T =
  BEGIN RETURN self.session END GetSession;

PROCEDURE Init(self : T; session : Session.T; up : T) : T =
  BEGIN self.session := session; self.up := up; RETURN self END Init;

PROCEDURE Error(reason : TEXT; doQuit : BOOLEAN := FALSE) =
  <* FATAL Abort *>
  VAR 
    page := NEW(HTMLPage.T).setHead("<title>M3HTML Error</title>").setBody("<h1>M3HTML ERROR</h1><p>" & reason);
  BEGIN
    Debug.Out("Error: " & reason);
    HTMLOutput.Ship(page);
    IF doQuit THEN RAISE Abort END;
  END Error;

(* automagically wrap TEXT objects *)

PROCEDURE Wrap(stuff : Stuff) : T =
  BEGIN
    TYPECASE stuff OF
    | TEXT => RETURN NEW(HTMLText.T).init(stuff)
    ELSE RETURN stuff (* implicit NARROW *)
    END
  END Wrap;

(* because of the polymorphism of wrap, we can (must) repeat the *)
(* exact same code twice for WrapVector and WrapMatrix *)

PROCEDURE WrapVector(stuff : REFANY) : Vector =
  BEGIN
    TYPECASE stuff OF
    | REF ARRAY OF TEXT (a) => 
      VAR
        new := NEW(Vector,NUMBER(a^));
      BEGIN
        FOR i := FIRST(new^) TO LAST(new^) DO
          new[i] := Wrap(a[i])
        END;
        RETURN new
      END
    | REF ARRAY OF Stuff (a) => 
      VAR
        new := NEW(Vector,NUMBER(a^));
      BEGIN
        FOR i := FIRST(new^) TO LAST(new^) DO
          new[i] := Wrap(a[i])
        END;
        RETURN new
      END
    | Vector => RETURN stuff
    ELSE <* ASSERT FALSE *> (* wrong type *)
    END
  END WrapVector;

PROCEDURE WrapMatrix(stuff : REFANY) : Matrix =
  BEGIN
    TYPECASE stuff OF
    | REF ARRAY OF ARRAY OF TEXT (a) =>     
      IF NUMBER(a^) > 0 THEN
        VAR
          new := NEW(Matrix, NUMBER(a^), NUMBER(a[0]));
        BEGIN
          FOR row := FIRST(new^) TO LAST(new^) DO
            FOR col := FIRST(new[0]) TO LAST(new[0]) DO
              new[row,col] := Wrap(a[row,col])
            END
          END;
          RETURN new
        END
      ELSE
        RETURN NEW(Matrix, NUMBER(a^), 0)
      END
    | REF ARRAY OF ARRAY OF Stuff (a) =>
      IF NUMBER(a^) > 0 THEN
        VAR
          new := NEW(Matrix, NUMBER(a^), NUMBER(a[0]));
        BEGIN
          FOR row := FIRST(new^) TO LAST(new^) DO
            FOR col := FIRST(new[0]) TO LAST(new[0]) DO
              new[row,col] := Wrap(a[row,col])
            END
          END;
          RETURN new
        END
      ELSE
        RETURN NEW(Matrix, NUMBER(a^), 0)
      END
    | Matrix => RETURN stuff
    ELSE <* ASSERT FALSE *>
    END
  END WrapMatrix;

BEGIN END HTML.
