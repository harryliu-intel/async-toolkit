MODULE WebAppUtils;
IMPORT TextTable, FloatMode, Lex;
IMPORT Env, Wr, Stdio;
IMPORT Process;
IMPORT Fmt;
IMPORT URL;
IMPORT Debug;
IMPORT Text;
IMPORT CitTextUtils AS TextUtils;

VAR myID : INTEGER;

PROCEDURE ParseEnv(debug : BOOLEAN) : TextTable.T =
  VAR
    count := Env.Count;
    res :=  NEW(TextTable.T).init();
  BEGIN
    IF VerboseDebug THEN
      myID := Process.GetMyID();
      <*FATAL Wr.Failure*>
      BEGIN
        Wr.PutText(Stdio.stderr, Fmt.Int(myID) & " STARTPAGE:\n")
      END
    END;
    FOR i := 0 TO count - 1 DO
      VAR 
        key, value : TEXT;
      BEGIN
        Env.GetNth(i,key,value);
        IF value = NIL THEN value := "" END;
        EVAL res.put(key,value);
        IF debug THEN
          <*FATAL Wr.Failure*>
          BEGIN
            Wr.PutText(Stdio.stderr,
                       Fmt.Int(myID) & " PAGEDATAKEY " & key & " PAGEDATAVALUE " & value & " ENDPAGEDATA;\n")
          END
        END
      END
    END;
    RETURN res
  END ParseEnv;

PROCEDURE ParseQuestionMarkData(data : TEXT; debug : BOOLEAN) : TextTable.T 
  RAISES { FloatMode.Trap, Lex.Error } =
  VAR
    res := NEW(TextTable.T).init();
  BEGIN
    IF data # NIL THEN
      LOOP
        VAR 
          this, name, value, newdata : TEXT;
        BEGIN
          
          TextUtils.SplitText(data, '&', this, newdata);
          data := newdata;
          this := URL.PlusToSpace(this);
          this := URL.Unescape(this);
          TextUtils.SplitText(this,'=', name, value);

          VAR
            res := "";
          BEGIN
            IF debug THEN Debug.Out("value:") END;
            IF value = NIL THEN value := "" 
            ELSE
              FOR i := 0 TO Text.Length(value) - 1 DO
                res := res & Fmt.Int(ORD(Text.GetChar(value,i)))& ", ";
              END;
            END;
            IF debug THEN Debug.Out(res) END
          END;

          EVAL res.put(name, value); (* must return false? *)
          IF data = NIL THEN EXIT END
        END
      END
    END;
    RETURN res
  END ParseQuestionMarkData;

BEGIN END WebAppUtils.
