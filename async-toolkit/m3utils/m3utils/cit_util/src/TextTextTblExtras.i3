INTERFACE TextTextTblExtras;
IMPORT TextTextTbl;

TYPE
  T = TextTextTbl.T;

PROCEDURE Scan(src: TEXT): T;

(* Input format:
| targ1 [[=] targ2 [[=] ...]] [=] value
separated by commas and/or newlines
*)

END TextTextTblExtras.
