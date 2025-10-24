MODULE PerlFE EXPORTS Main;
IMPORT Rd, Wr, Stdio, UnsafeRd;

TYPE
  PerlState = { Off, Copy, Var };

CONST
  Start = ARRAY OF CHAR { '<', '%',
                                    '=' }; (* what about dollar sign?? *)
  Stop  = ARRAY OF CHAR { '%', '>' };

  SQ = '\'';

  DQ = '"'; (* " *)

  BS = '\\';

PROCEDURE RecoverMatch() =
  BEGIN
    FOR i := 0 TO p-1 DO
      CASE state OF
        PerlState.Off => Push(Start[i])
      |
        PerlState.Var, PerlState.Copy => Push(Stop[i])
      END
    END
  END RecoverMatch;

PROCEDURE Push(c : CHAR) =
  BEGIN
    IF state = PerlState.Off AND c = SQ THEN
      Wr.PutChar(wr, BS)
    END;
    Wr.PutChar(wr, c)
  END Push;

PROCEDURE PushT(t : TEXT) =
  BEGIN
    Wr.PutText(wr, t)
  END PushT;

PROCEDURE SwitchState(from, to : PerlState) =
  BEGIN
    <*ASSERT from # to *>
    <*ASSERT from = state *>
    state := to;
    IF from = PerlState.Off THEN
      Push(SQ);
      Push(';');
      Push('\n');
    END;
    IF from = PerlState.Var THEN
      Push(';');
      Push('\n');
    END;
    IF to = PerlState.Off THEN
      PushT("\nprint '");
    END;
    IF to = PerlState.Var THEN
      PushT("\nprint ");
    END;
  END SwitchState;
  
VAR
  state := PerlState.Copy;
  rd := Stdio.stdin;
  p := 0;
  doRecover, keep : BOOLEAN;
  wr := Stdio.stdout;
  c : CHAR;
  newState : PerlState;
BEGIN
  SwitchState(PerlState.Copy, PerlState.Off);
  TRY
    LOOP
      c := UnsafeRd.FastGetChar(rd);
      doRecover := FALSE;
      keep := FALSE;
      newState := state;
      CASE state OF
        PerlState.Off =>
        IF c = Start[p] THEN
          IF p = LAST(Start) THEN
            newState := PerlState.Var
          ELSE
            INC(p)
          END
        ELSIF p = 2 THEN
          keep := TRUE;
          newState := PerlState.Copy
        ELSE
          doRecover := TRUE; keep := TRUE
        END
      |
        PerlState.Copy, PerlState.Var =>
        IF c = Stop[p] THEN
          IF p = LAST(Stop) THEN
            newState := PerlState.Off
          ELSE
            INC(p)
          END
        ELSE
          doRecover := TRUE; keep := TRUE
        END
      END;

      (* situation here:
         state is state up to before this character
         newState is state after this character
         doRecover is true if we partially matched 

         note special case for PerlState.Copy, this means that the current
         character did NOT match and needs to be recovered 
      *)
      
      IF doRecover THEN
        RecoverMatch();
        p := 0
      END;

      IF newState # state THEN
        IF newState = PerlState.Copy THEN
          Push(c)
        END;
        p := 0;
        SwitchState(state, newState)
      END;

      IF keep THEN
        Push(c)
      END
    END
  EXCEPT
    Rd.EndOfFile =>
    (* we have to clear out the buffer...! *)
    RecoverMatch();
    IF state # PerlState.Copy THEN
      SwitchState(state, PerlState.Copy)
    END
  END
END PerlFE.
