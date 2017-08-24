MODULE SpiceFormat;
IMPORT Rd;
IMPORT Debug;
IMPORT Text;
IMPORT SpiceCircuitList, TextSpiceCircuitTbl;
IMPORT TextSeq;
IMPORT SpiceObjectSeq;
IMPORT SpiceCircuit;
IMPORT SpiceObjectParse;
FROM SpiceFileFormat IMPORT White;
IMPORT Thread;

VAR Verbose := FALSE;

PROCEDURE GetSingleLine(rd : Rd.T; 
                        VAR p : CARDINAL; 
                        VAR buff : REF ARRAY OF CHAR;
                        VAR lNo : CARDINAL ) : CARDINAL
  RAISES { Rd.Failure, Thread.Alerted } =
  VAR
    len : CARDINAL;
  BEGIN
    IF buff = NIL OR NUMBER(buff^) = 0 THEN
      buff := NEW(REF ARRAY OF CHAR, 1)
    END;
    LOOP
      len := Rd.GetSubLine(rd, SUBARRAY(buff^, p, NUMBER(buff^)-p));
      IF len = 0 THEN
        (* no read now *)
        IF Verbose THEN Debug.Out("GetSingleLine: no read now") END;
        RETURN p
      ELSIF buff[p+len-1] = '\n' THEN
        IF Verbose THEN Debug.Out("GetSingleLine: success") END;
        (*success*)
        INC(lNo);
        DEC(len);
        IF p+len # 0 AND buff[p+len-1] = '\r' THEN
          IF Verbose THEN Debug.Out("GetSingleLine: strip CR") END;
          DEC(len) (* strip carriage return *)
        END;
        RETURN p+len
      ELSE
        (*failure to finish the line*)
        IF Verbose THEN Debug.Out("GetSingleLine: failure to finish") END;
        WITH new = NEW(REF ARRAY OF CHAR, NUMBER(buff^)*2) DO
          SUBARRAY(new^, 0, NUMBER(buff^)) := buff^;
          p := NUMBER(buff^);
          buff := new
        END
      END
    END
  END GetSingleLine;

PROCEDURE GetLine(rd : Rd.T;
                  VAR buff : REF ARRAY OF CHAR;
                  VAR lNo : CARDINAL) : [-1..LAST(CARDINAL)] RAISES { Rd.Failure, Thread.Alerted } =
  VAR 
    p : CARDINAL := 0;
    <*FATAL Rd.EndOfFile*>
  BEGIN
    LOOP
      p := GetSingleLine(rd, p, buff, lNo);
      IF Rd.EOF(rd) THEN RETURN -1 END;
      LOOP
        (* eat the next live character.
           if a + keep getting stuff, else return here *)
        WITH c = Rd.GetChar(rd) DO
          CASE c OF
            '+' => EXIT
          |
            ' ', '\t' => AddToBuff(buff, p, c)(* skip *)
          ELSE
            Rd.UnGetChar(rd); RETURN p
          END
        END
      END
    END
  END GetLine;

PROCEDURE AddToBuff(VAR buff : REF ARRAY OF CHAR;
                    VAR p : CARDINAL;
                    c : CHAR) =
  BEGIN
    IF p = NUMBER(buff^) THEN
      WITH new = NEW(REF ARRAY OF CHAR, 2*NUMBER(buff^)) DO
        SUBARRAY(new^, 0, NUMBER(buff^)) := buff^;
        buff := new
      END
    END;
    buff[p] := c;
    INC(p)
  END AddToBuff;


TYPE
  Private = T OBJECT
    circuit : SpiceCircuitList.T; (* currently parsing *)
  END;

PROCEDURE ParseSpice(rd : Rd.T) : T RAISES { Error } =
  VAR
    buff := NEW(REF ARRAY OF CHAR, 1);
    lNo : CARDINAL := 0;
    p : CARDINAL;

    res := NEW(Private, subCkts := NEW(TextSpiceCircuitTbl.Default).init());
    
  BEGIN 
    res.circuit := NIL;       

    WITH root = NEW(SpiceCircuit.T, 
                    name := NIL, 
                    params := NEW(TextSeq.T).init(),
                    elements := NEW(SpiceObjectSeq.T).init()) DO
      res.circuit := SpiceCircuitList.Cons(root, res.circuit)
    END;

    LOOP
      WITH len = GetLine(rd, buff, lNo) DO
        IF len = -1 THEN 
          IF Verbose THEN Debug.Out("ParseSpice: EOF") END;
          RETURN res
        END;
        IF Verbose THEN Debug.Out("ParseSpice: " & Text.FromChars(SUBARRAY(buff^, 0, len))) END;

        p := 0;
        WHILE p < len AND buff[p] IN White DO
          INC(p)
        END;

        WITH line = SUBARRAY(buff^, p, len-p) DO
          SpiceObjectParse.ParseLine(res.circuit, res.subCkts, line, lNo)
        END
      END
    END
  END ParseSpice;

BEGIN
END SpiceFormat.
