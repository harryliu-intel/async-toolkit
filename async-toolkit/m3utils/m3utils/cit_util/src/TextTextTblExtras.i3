INTERFACE TextTextTblExtras;
IMPORT TextTextTbl;

TYPE
  T = TextTextTbl.T;

PROCEDURE Scan(src: TEXT): T;
PROCEDURE ScanMore(src: TEXT; dest: T);

(* Input format:
| targ1 [[=] targ2 [[=] ...]] [=] value
separated by commas and/or newlines
*)


PROCEDURE Reverse(tbl: T): T;
PROCEDURE ReverseMore(tbl: T; dest: T);

(* map each value to the last-stored key referring to that value *)

END TextTextTblExtras.
