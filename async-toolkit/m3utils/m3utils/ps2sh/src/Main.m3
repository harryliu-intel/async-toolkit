MODULE Main;

(* take output of 

   ps wwe 

   and turn into a shell script *)

(* wont work if there are =s in the command itself ... *)

IMPORT Rd, Wr, Stdio;
IMPORT TextList, TextSeq, RegEx;
IMPORT Debug;
IMPORT TextReader;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT CharSeq;
IMPORT Text;

PROCEDURE PushEnv(envL : TextSeq.T; tl : TextList.T) =
  VAR
    e : TEXT;
  BEGIN
    Reformat(tl, e);
    envL.addhi(e)
  END PushEnv;

VAR
  pat := RegEx.Compile("[A-Z0-9_][A-Z0-9_]*=.*");
  
PROCEDURE IsEnvDef(t : TEXT) : BOOLEAN =
  BEGIN RETURN RegEx.Execute(pat, t) # -1 END IsEnvDef;

PROCEDURE Reformat(p : TextList.T; VAR txt : TEXT) =
  BEGIN
    txt := "";
    WHILE p # NIL DO
      txt := txt & p.head;
      IF p.tail # NIL THEN txt := txt & " " END;
      p := p.tail
    END
  END Reformat;

TYPE
  Command = RECORD
    envL : TextSeq.T;
    cmd  : TEXT;
  END;
  
PROCEDURE Input() : Command =
  VAR
    cts := Rd.GetText(Stdio.stdin, LAST(CARDINAL));
    lst := NEW(TextReader.T).init(cts).shatter(" ", "", skipNulls := TRUE);
    cmdL : TextList.T := NIL;
    envL := NEW(TextSeq.T).init();
    
    p := lst;
    
    cEnvL : REFANY := NIL;
    cmd : TEXT;
  BEGIN
    WHILE NOT IsEnvDef(p.head) DO
      cmdL := TextList.Append(cmdL, TextList.List1(p.head));
      p := p.tail
    END;
    
    Reformat(cmdL, cmd);

    Debug.Out(F("Got command:\n%s",cmd));
    
    WHILE p # NIL DO
      Debug.Out(F("next word (IsEnvDef=%s):\n%s",
                  Fmt.Bool(IsEnvDef(p.head)),
                  p.head));
      IF IsEnvDef(p.head) THEN
        IF cEnvL # NIL THEN
          PushEnv(envL, cEnvL)
        END;
        cEnvL := TextList.List1(p.head)
      ELSE
        cEnvL := TextList.Append(cEnvL, TextList.List1(p.head))
      END;
      p := p.tail
    END;
    IF cEnvL # NIL THEN PushEnv(envL, cEnvL) END;

    RETURN Command { envL, cmd }
  END Input;

CONST DQ = '"'; (* " *)
      LF = SET OF CHAR { '\n', '\r' };
      
PROCEDURE QuoteTarget(ea : TEXT) : TEXT =
  VAR
    seq := NEW(CharSeq.T).init();
    haveEquals := FALSE;
  BEGIN
    FOR i := 0 TO Text.Length(ea)-1 DO
      WITH c = Text.GetChar(ea, i) DO
        IF c = '=' THEN
          seq.addhi('=');
          IF NOT haveEquals THEN seq.addhi(DQ) END;
          haveEquals := TRUE;
        ELSE
          IF NOT c IN LF THEN seq.addhi(c) END
        END
      END
    END;
    <*ASSERT haveEquals*>
    seq.addhi(DQ);

    VAR
      a := NEW(REF ARRAY OF CHAR, seq.size());
    BEGIN
      FOR i := 0 TO seq.size()-1 DO
        a[i] := seq.get(i)
      END;
      RETURN Text.FromChars(a^)
    END
  END QuoteTarget;
  
PROCEDURE Output(cmd : Command) =
  VAR
    wr := Stdio.stdout;
  BEGIN
    Wr.PutText(wr, "#!/bin/sh -x\n");
    Wr.PutChar(wr, '\n');
    FOR i := 0 TO cmd.envL.size()-1 DO
      Wr.PutText(wr, F("export %s\n", QuoteTarget(cmd.envL.get(i))))
    END;
    Wr.PutChar(wr, '\n');
    Wr.PutText(wr, F("%s\n", cmd.cmd));
    Wr.Close(wr)
  END Output;
  
BEGIN
  WITH command = Input() DO
    Output(command)
  END
END Main.
