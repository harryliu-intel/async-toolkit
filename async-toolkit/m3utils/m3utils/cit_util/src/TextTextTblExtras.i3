INTERFACE TextTextTblExtras;
IMPORT TextTextTbl;

TYPE
  T = TextTextTbl.T;

PROCEDURE Scan(src: TEXT; sizeHint: CARDINAL := 0): T;
PROCEDURE ScanMore(src: TEXT; dest: T);

(* Input format:
| targ1 [[=] targ2 [[=] ...]] [=] value
separated by commas and/or newlines
*)

END TextTextTblExtras.
