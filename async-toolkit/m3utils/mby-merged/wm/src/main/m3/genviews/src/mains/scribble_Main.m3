MODULE Main;
IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT IO;
IMPORT CompAddr;
IMPORT Fmt;
IMPORT Debug;
IMPORT RTName;
IMPORT Random;
IMPORT Word;
IMPORT CsrOp;
IMPORT Time;

VAR
  map  := NEW(MapAddr.H).init(CompAddr.T { 0, 0 });
  rand := NEW(Random.Default).init();

  time0, time1 : Time.T;
CONST
  NumWrites = 1000 * 1000;

BEGIN
  IO.Put(Fmt.Int(CompAddr.initCount) & " fields have been address initialized.\n");
  Debug.Out("Scribbling");

  time0 := Time.Now();
  FOR i := 1 TO NumWrites DO
    WITH startW = rand.integer(map.a.min.word, map.a.max.word),
         startB = rand.integer(0, BITSIZE(Word.T)-1),
         wid    = rand.integer(0, BITSIZE(Word.T)-1),
         val    = rand.integer(FIRST(Word.T), LAST(Word.T)),
         op     = CsrOp.MakeWrite(CompAddr.T { startW, startB },
                                  wid,
                                  val) DO
      EVAL map.csrOp(op)
    END
  END;
  time1 := Time.Now();

  Debug.Out(Fmt.F("%s narrow writes per second", Fmt.LongReal(FLOAT(NumWrites,LONGREAL)/(time1-time0), prec := 6)));

END Main.
