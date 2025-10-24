(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE RegProperty;
IMPORT RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueKeyword;
IMPORT RdlPropertyRvalueConstant;
IMPORT BigInt;
IMPORT Text, ParseError;

PROCEDURE GetKw(q : RdlPropertyAssignRhs.Const) : RdlPropertyRvalueKeyword.T =
  BEGIN
    TYPECASE q.const OF
      RdlPropertyRvalueConstant.Keyword(kw) => RETURN kw.kw
    ELSE
      <*ASSERT FALSE*>
    END
  END GetKw;

PROCEDURE GetNumeric(q : RdlPropertyAssignRhs.Const) : INTEGER =
  BEGIN
    TYPECASE q.const OF
      RdlPropertyRvalueConstant.Num(n) =>
      RETURN BigInt.ToInteger(n.num.x)
    ELSE
      <*ASSERT FALSE*>
    END
  END GetNumeric;

PROCEDURE Unquote(str : TEXT) : TEXT RAISES { ParseError.E } =
  CONST
    DQ = '"';
  VAR
    len := Text.Length(str);
  BEGIN
    IF len < 2 OR Text.GetChar(str,0) # DQ OR Text.GetChar(str,len-1) # DQ THEN
      RAISE ParseError.E("Not properly quoted : str")
    END;
    RETURN Text.Sub(str, 1, len - 2)
  END Unquote;

BEGIN END RegProperty.
