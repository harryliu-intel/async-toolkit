(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

MODULE HTMLFormatting;
IMPORT HTML, Text;

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT 
  OVERRIDES
    colAttrs := NullAttrs;
    tdAttrs := NullAttrs;
    matchesByName := MatchesAll;
  END;

  Alignment = PubAlignment BRANDED Brand & " Alignment" OBJECT
  OVERRIDES
    format := DummyFormat;
    colAttrs := AlignmentColAttrs;
    tdAttrs := AlignmentColAttrs;
  END;

  NamedColumnFormat = PubNamedColumnFormat BRANDED Brand & " NamedColumnFormat" OBJECT
  OVERRIDES
    matchesByName := NamedMatchesByName 
  END;

PROCEDURE NullAttrs(<*UNUSED*>t : T) : TEXT = BEGIN RETURN "" END NullAttrs;

PROCEDURE MatchesAll(<*UNUSED*>t : T; <*UNUSED*> colName : TEXT) : BOOLEAN = 
  BEGIN RETURN TRUE END MatchesAll;

PROCEDURE NamedMatchesByName(t : NamedColumnFormat; colName : TEXT) : BOOLEAN = 
  BEGIN RETURN TE(colName, t.column) END NamedMatchesByName;

PROCEDURE AlignmentColAttrs(t : Alignment) : TEXT =
  CONST
    ACA = ARRAY AlignmentType OF TEXT { 
      "left", "center", "right", "justify", "char" 
    };
  BEGIN
    RETURN "align=\"" & ACA[t.type] & "\""
  END AlignmentColAttrs;

PROCEDURE DummyFormat(<*UNUSED*>t : T;  txt : HTML.Stuff) : HTML.Stuff =
  BEGIN RETURN txt END DummyFormat;

REVEAL
  EveryRowFormat = RowFormat BRANDED Brand & " EveryRowFormat" OBJECT
  OVERRIDES
    bgcolor := NILDummy;
    align := NILDummy;
    valign := NILDummy;
  END;

PROCEDURE NILDummy(<*UNUSED*>erf : EveryRowFormat) : TEXT =
  BEGIN RETURN NIL END NILDummy;

TYPE
  DefRowFormat = EveryRowFormat OBJECT
    myBgcolor, myAlign, myValign : TEXT := NIL;
  OVERRIDES
    bgcolor := DefBgcolor;
    align := DefAlign;
    valign := DefValign;
  END;

PROCEDURE DefBgcolor(d : DefRowFormat) : TEXT =
  BEGIN RETURN d.myBgcolor END DefBgcolor;

PROCEDURE DefAlign(d : DefRowFormat) : TEXT =
  BEGIN RETURN d.myAlign END DefAlign;

PROCEDURE DefValign(d : DefRowFormat) : TEXT =
  BEGIN RETURN d.myValign END DefValign;

PROCEDURE MakeRowFormat(bgcolor, align, valign := NIL) : RowFormat =
  BEGIN
    RETURN NEW(DefRowFormat, 
               myBgcolor := bgcolor, myAlign := align, myValign := valign) 
  END MakeRowFormat;

BEGIN END HTMLFormatting.
