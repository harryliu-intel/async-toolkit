MODULE CommandLoop;

REVEAL
  T = Public BRANDED OBJECT
    prompt: TEXT;
    commands: TextCommandTbl.T;
    prefixes: TextTextTbl.T;
    prev: TextList.T;           (* previously executed commands *)
    term: Term.T;               (* terminal used in "run"       *)
    inputStack: RdList.T;       (* sourcefile readers           *)
  OVERRIDES
    init                := Init;
    putCommand          := PutCommand;
    run                 := Run;
  END;

TYPE
  BuiltInCommand = Command OBJECT loop: T; END;
  QuitCommand = Command BRANDED OBJECT END;


PROCEDURE Init(self: T;
               prompt := "> ";
               help := "help ?";
               quit := "quit ^D";
               load := "load input source";
               save := "save") : T =
  BEGIN
    self.inputStack := NIL;
    self.prompt := prompt;
    self.prev := NIL;
    self.commands := NEW(TextCommandTbl.Default).init();
    self.prefixes := NEW(TextCommandListTbl.Default).init();

    self.putCommand(help, NEW(BuiltInCommand,
                              loop := self,
                              simpleHelp := "-- display list of commands",
                              execute := DoHelp));

    self.putCommand(save,NEW(BuiltInCommand,
               loop := self,
               simpleHelp:="<filename> -- save previously-executed commands",
               execute := DoSave));

    self.putCommand(load, NEW(BuiltInCommand,
               loop := self,
               simpleHelp := "<filename> -- execute commands from a file",
               execute := DoLoad));

    self.putCommand(quit, NEW(QuitCommand,
                              simpleHelp := "-- exit the command loop"));

    RETURN self;
  END Init;

PROCEDURE PreRegister(prefixes: TextCommandListTbl.T; prefix, name: TEXT) =
  VAR
    l: CommandList.T := NIL;
  BEGIN
    EVAL prefixes.get(prefix, l);
    EVAL prefixes.put(prefix, CommandList.Cons(cmd,l));
    IF Text.Length(prefix)#0 THEN
      PreRegister(prefixes, Text.Sub(prefix,0,Text.Length(prefix)-1), name);
    END;
  END PreRegister;

PROCEDURE PutCommand(self: T; names: TEXT; cmd: Command) =
  VAR
    cur := TextUtils.Shatter(names);
  BEGIN
    WHILE cur # NIL DO
      EVAL self.commands.put(cur.head, cmd);
      PreRegister(self.prefixes, cur.head, cur.head);
      cur := cur.tail;
    END;
  END PutCommand;


(*****************************************************************************
 *                                                                           *
 *               "Execute" methods for Built-in Commands                     *
 *                                                                           *
 *****************************************************************************)

PROCEDURE DoHelp(cmd: BuiltInCommand; args: TextList.T; term: Term.T)
  RAISES {Error} =
  VAR
    self := cmd.loop;
    name: TEXT;
    cmd: Command;
  BEGIN
    IF args.tail = NIL THEN
      WITH iter = self.commands.iterate() DO
        WHILE iter.next(name, cmd) DO
          term.wr(cmd.help(TextList.List1(name)), TRUE);
        END;
      END;
    ELSE
      name := args.tail.head;
      CASE Lookup(self, name, cmd) OF
      | LURes.WasPrefix, LURes.Found =>
        cmd.help(TextList.Cons(cmd, args.tail.tail));
      | LURes.NotFound => RAISE Error("command not found");
      | LURes.Ambiguous => RAISE Error("command ambiguous");
      END;
    END;
  END DoHelp;

PROCEDURE DoSave(cmd: BuiltInCommand; args: TextList.T; term: Term.T)
  RAISES {Error} =
  BEGIN
    IF List.Length(args) # 2 THEN
      RETURN "single argument expected."
    ELSE
      TRY
        VAR
          fn := args.tail.head;
          wr := FileWr.Open(fn);
          cur := cmd.loop.prev;
        BEGIN
          WHILE cur # NIL DO
            Wr.PutText(wr, cur.head);
            Wr.PutChar(wr, '\n');
            cur := cur.tail;
          END;
          Wr.Close(fn);
        END;
      EXCEPT OSError.E, Wr.Failure, Thread.Alerted =>
        RAISE Error("bad output file");
      END;
    END;
  END DoSave;

PROCEDURE DoLoad(cmd: BuiltInCommand; args: TextList.T; term: Term.T)
  RAISES {Error} =
  BEGIN
    IF args.tail = NIL THEN RAISE Error("filename missing."); END;
    TRY
      cmd.loop.inputStack := RdList.Cons(FileRd.Open(args.tail.head),
                                         cmd.loop.inputStack);
    EXCEPT OSError.E =>
      RAISE Error("cannot open `"&args.tail.head&"'.");
    END;
  END DoLoad;


(*****************************************************************************
 *                                                                           *
 *                        Command-Table Utilities                            *
 *                                                                           *
 *****************************************************************************)


(* Lookup a command *)

TYPE
  LURes = {Found, WasPrefix, NotFound, Ambiguous};

PROCEDURE Lookup(self: T; VAR name: TEXT; VAR cmd: T): LURes =
  VAR
    l: CommandList.T;
  BEGIN
    IF self.commands.get(name, cmd) THEN
      RETURN LURes.Found;
    ELSIF self.prefixes.get(name, l) THEN
      name := l.head;
      IF NOT self.commands.get(name, cmd) THEN
        <* ASSERT FALSE *>
        (* how did the command end up in the prefix table? *)
      END;
      IF l.tail = NIL THEN
        RETURN LURes.WasPrefix;
      ELSE
        RETURN LURes.Ambiguous;
      END;
    ELSE
      RETURN LURes.NotFound;
    END;
  END Lookup;


(* Complete a command *)

TYPE
  StdCompleter = Cooker.Completer OBJECT
    loop: T;
  OVERRIDES
    do := Complete;
  END;

PROCEDURE Complete(comp: StdCompleter; VAR t: TEXT) =
  VAR
    p:=Text.FindChar(t, ' ');
    name := t;
    q: Command.T;
    term := comp.loop.term;
  BEGIN
    IF p#-1 THEN
      name := Text.Sub(t,0,p);
    END;
    CASE Lookup(comp.loop, name, q) OF
    | LURes.WasPrefix, LURes.Found =>
      IF p=-1 THEN
        t := name & " ";
      ELSE
        TRY
          q.complete(t);
        EXCEPT Error(e) =>
          term.wr("\n" & e & "\n");
        END;
      END;
    | LURes.NotFound => term.wr("\ncommand not found\n");
    | LURes.Ambiguous => term.wr("\ncommand ambiguous\n");
    END;
  END Complete;



(*****************************************************************************
 *                                                                           *
 *                            Command Loop Main                              *
 *                                                                           *
 *****************************************************************************)


PROCEDURE Run(self: T; source: Pathname.T := NIL; term: Term.T := NIL) =
  CONST
    Comment = SET OF CHAR{'%','#'};
  VAR
    completer := NEW(StdCompleter, loop:=self);
    line: TEXT;
  BEGIN
    IF term = NIL THEN
      self.term := Term.Default();
    ELSE
      self.term := term;
    END;
    LOOP
      TRY
        IF source # NIL THEN
          DoLoad(self.load, TextList.List2("",source));
          source := NIL;
        ELSE
          IF self.inputStack = NIL THEN
            line := Cooker.Input(self.prompt, completer, self.prev,
                                 "", FALSE, TRUE);
          ELSE
            TRY
              line := Rd.GetLine(self.inputStack.head);
              IF Text.Equal(line, "") OR Text.GetChar(line,0) IN Comment THEN
                line := "";
              ELSE
                self.term.wr(Prompt & line, TRUE, TRUE);
              END;
            EXCEPT Rd.EndOfFile =>
              self.inputStack := self.inputStack.tail;
              line := "";
            END;
          END;
          
          IF Text.Equal(line,"\003") THEN
            self.term.wr("quit", TRUE, TRUE);
          ELS
          ELSIF Text.Equal(line,"\032") THEN
            self.term.wr("\nSuspended\njust kidding - I don't know how to do that.\n");
          ELSIF Text.Equal(line,"") THEN
          ELSE
            IF Text.Equal(line,"\004") THEN
              line := "^D";
            END;
            VAR
              tr := NEW(TextReader.T).init(line);
              portion: TEXT;
              args: TextList.T;
            BEGIN
              WHILE tr.next(";",portion,TRUE) DO
                args := NEW(TextReader.T).init(portion).shatter(": ","",TRUE);
                IF args # NIL THEN
                  CASE Quommand.Lookup(args.head, q) OF
                  | LURes.WasPrefix, LURes.Found => q.execute(args, self.term);
                  | LURes.NotFound =>
                    RAISE Error("command not found; try `help'.");
                  | LURes.Ambiguous =
                    RAISE Error("command ambiguous; try `help'.");
                  END;
                END;
              END;
            END;
            prev := TextList.Cons(line, prev);
          END;
        END;
      EXCEPT Error(e) =>
        IF self.inputStack # NIL THEN
          self.inputStack := self.inputStack.tail;
          self.term.wr("error: " & e, TRUE, TRUE);
        END;
      END;
    END;
  END Run;




BEGIN
END CommandLoop.
