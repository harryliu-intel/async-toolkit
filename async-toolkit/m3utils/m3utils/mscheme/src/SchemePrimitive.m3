(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemePrimitive;
IMPORT Debug;
IMPORT SchemeEnvironment, SchemeProcedureClass;
IMPORT Scheme, SchemeClass;

FROM Scheme IMPORT Object, Symbol, String, Vector, E;

FROM SchemeUtils IMPORT Length, First, Second, Third,
                        Stringify, StringifyQ, Error, Warn, Equal, Eqv,
                        Rest, PedanticFirst, PedanticRest, Cons, 
                        SetFirst, SetRest, Reverse, Str, List2, ListToString,
                        Vec, InPort, OutPort, List1, ListToVector, ListStar,
                        VectorToList, Write;

IMPORT SchemeInputPort, SchemeContinuation, SchemeMacro, SchemeString;
FROM SchemeBoolean IMPORT Truth, False, True, TruthO;
FROM SchemeProcedure IMPORT Proc; IMPORT SchemeProcedure;
FROM SchemeLongReal IMPORT FromLR, FromO, Zero, One;
FROM SchemeChar IMPORT Character, Char, IChr, LowerCase, UpperCase, Digits,
                       White, Upcase, Downcase;
IMPORT SchemeChar;
IMPORT SchemeSymbol;
IMPORT SchemeLongReal;

IMPORT Fmt, Text, Wx, Wr;
IMPORT Math, Scan, Lex, FloatMode;
IMPORT Process;
IMPORT OSError, FileWr, FileRd, AL;
IMPORT Thread;
IMPORT SchemePair;

TYPE Pair = SchemePair.T;

<* FATAL Thread.Alerted *>

TYPE Procedure = SchemeProcedure.T;

REVEAL
  T = Public BRANDED Brand OBJECT
    minArgs, maxArgs : CARDINAL;
    idNumber : INTEGER;
  OVERRIDES
    init  := Init;
    apply := Apply;
  END;

PROCEDURE Init(t : T; id : INTEGER; minArgs, maxArgs : CARDINAL) : T =
  BEGIN 
    t.minArgs := minArgs; t.maxArgs := maxArgs; t.idNumber := id; RETURN t
  END Init;

TYPE
  P = { Eq, Lt, Gt, Ge, Le,
        Abs, EofObject, EqQ, EqualQ, Force,
        Car, Floor,  Ceiling, Cons, 
        Divide, Length, List, ListQ, Apply,
        Max, Min, Minus, Newline, 
        Not, NullQ, NumberQ, PairQ, Plus, 
        ProcedureQ, Read, Cdr, Round, Second, 
        SymbolQ, Times, Truncate, Write, Append,
        BooleanQ, Sqrt, Expt, Reverse, Assoc, 
        AssQ, AssV, Member, MemQ, MemV, EqvQ,
        ListRef, ListTail, StringQ, MakeString, String,
        StringLength, StringRef, StringSet, Substring, 
        StringAppend, StringToList, ListToString, 
        SymbolToString, StringToSymbol, Exp, Log, Sin,
        Cos, Tan, Acos, Asin, Atan, 
        NumberToString, StringToNumber, CharQ,
        CharAlphabeticQ, CharNumericQ, CharWhitespaceQ,
        CharUppercaseQ, CharLowercaseQ, CharToInteger,
        IntegerToChar, CharUpcase, CharDowncase,
        VectorQ, MakeVector, Vector, VectorLength,
        VectorRef, VectorSet, ListToVector, Map, 
        Foreach, CallCC, VectorToList, Load, Display,
        InputPortQ, CurrentInputPort, OpenInputFile, 
        CloseInputPort, OutputportQ, CurrentOutputPort,
        OpenOutputFile, CloseOutputPort, ReadChar,
        PeekChar, Eval, Quotient, Remainder,
        Modulo, Third, EofObjectQ, Gcd, Lcm, 
        Cxr, OddQ, EvenQ, ZeroQ, PositiveQ,
        NegativeQ, 

        CharCmpEq, CharCmpLt, CharCmpGt, CharCmpGe, CharCmpLe, 

        CharCiCmpEq, CharCiCmpLt, CharCiCmpGt, CharCiCmpGe, CharCiCmpLe,

        StringCmpEq, StringCmpLt, StringCmpGt, StringCmpGe, StringCmpLe, 

        StringCiCmpEq, StringCiCmpLt, StringCiCmpGt, StringCiCmpGe, StringCiCmpLe,

        ExactQ, InexactQ, IntegerQ,
        CallWithInputFile, CallWithOutputFile,
        Tanh,

        (* extensions follow *)

        New, Class, Method, Exit,
        SetCar, SetCdr, TimeCall, MacroExpand,
        Error, ListStar,
	
	JailBreak,
  M3Op};

PROCEDURE InstallPrimitives(env : SchemeEnvironment.T) : SchemeEnvironment.T =
  CONST n = LAST(CARDINAL);
  BEGIN
    EVAL env
     .defPrim("*",       	ORD(P.Times),     0, n)
     .defPrim("+",       	ORD(P.Plus),      0, n)
     .defPrim("-",       	ORD(P.Minus),     1, n)
     .defPrim("/",       	ORD(P.Divide),    1, n)
     .defPrim("<",       	ORD(P.Lt),        2, n)
     .defPrim("<=",      	ORD(P.Le),        2, n)
     .defPrim("=",       	ORD(P.Eq),        2, n)
     .defPrim(">",       	ORD(P.Gt),        2, n)
     .defPrim(">=",      	ORD(P.Ge),        2, n)
     .defPrim("abs",     	ORD(P.Abs),       1)
     .defPrim("acos",    	ORD(P.Acos),      1)
     .defPrim("append",         ORD(P.Append),    0, n)
     .defPrim("apply",   	ORD(P.Apply),     2, n)
     .defPrim("asin",    	ORD(P.Asin),      1)
     .defPrim("assoc",   	ORD(P.Assoc),     2)
     .defPrim("assq",    	ORD(P.AssQ),      2)
     .defPrim("assv",    	ORD(P.AssV),      2)
     .defPrim("atan",    	ORD(P.Atan),      1)
     .defPrim("boolean?",	ORD(P.BooleanQ),  1)
     .defPrim("caaaar",         ORD(P.Cxr),       1)
     .defPrim("caaadr",         ORD(P.Cxr),       1)
     .defPrim("caaar",          ORD(P.Cxr),       1)
     .defPrim("caadar",         ORD(P.Cxr),       1)
     .defPrim("caaddr",         ORD(P.Cxr),       1)
     .defPrim("caadr",          ORD(P.Cxr),       1)
     .defPrim("caar",           ORD(P.Cxr),       1)
     .defPrim("cadaar",         ORD(P.Cxr),       1)
     .defPrim("cadadr",         ORD(P.Cxr),       1)
     .defPrim("cadar",          ORD(P.Cxr),       1)
     .defPrim("caddar",         ORD(P.Cxr),       1)
     .defPrim("cadddr",         ORD(P.Cxr),       1)
     .defPrim("caddr",     	ORD(P.Third),     1)
     .defPrim("cadr",  	        ORD(P.Second),    1)
     .defPrim("call-with-current-continuation",        ORD(P.CallCC),    1)
     .defPrim("call-with-input-file", ORD(P.CallWithInputFile), 2)
     .defPrim("call-with-output-file", ORD(P.CallWithOutputFile), 2)
     .defPrim("car",     	ORD(P.Car),       1)
     .defPrim("cdaaar",         ORD(P.Cxr),       1)
     .defPrim("cdaadr",         ORD(P.Cxr),       1)
     .defPrim("cdaar",          ORD(P.Cxr),       1)
     .defPrim("cdadar",         ORD(P.Cxr),       1)
     .defPrim("cdaddr",         ORD(P.Cxr),       1)
     .defPrim("cdadr",          ORD(P.Cxr),       1)
     .defPrim("cdar",           ORD(P.Cxr),       1)
     .defPrim("cddaar",         ORD(P.Cxr),       1)
     .defPrim("cddadr",         ORD(P.Cxr),       1)
     .defPrim("cddar",          ORD(P.Cxr),       1)
     .defPrim("cdddar",         ORD(P.Cxr),       1)
     .defPrim("cddddr",         ORD(P.Cxr),       1)
     .defPrim("cdddr",          ORD(P.Cxr),       1)
     .defPrim("cddr",           ORD(P.Cxr),       1)
     .defPrim("cdr",     	ORD(P.Cdr),       1)
     .defPrim("char->integer",  ORD(P.CharToInteger),      1)
     .defPrim("char-alphabetic?",ORD(P.CharAlphabeticQ),      1)
     .defPrim("char-ci<=?",     ORD(P.CharCiCmpLe), 2)
     .defPrim("char-ci<?" ,     ORD(P.CharCiCmpLt), 2)
     .defPrim("char-ci=?" ,     ORD(P.CharCiCmpEq), 2)
     .defPrim("char-ci>=?",     ORD(P.CharCiCmpGe), 2)
     .defPrim("char-ci>?" ,     ORD(P.CharCiCmpGt), 2)
     .defPrim("char-downcase",  ORD(P.CharDowncase),      1)
     .defPrim("char-lower-case?",ORD(P.CharLowercaseQ),      1)
     .defPrim("char-numeric?",  ORD(P.CharNumericQ),      1)
     .defPrim("char-upcase",    ORD(P.CharUpcase),      1)
     .defPrim("char-upper-case?",ORD(P.CharUppercaseQ),      1)
     .defPrim("char-whitespace?",ORD(P.CharWhitespaceQ),      1)
     .defPrim("char<=?",        ORD(P.CharCmpLe), 2)
     .defPrim("char<?",         ORD(P.CharCmpLt), 2)
     .defPrim("char=?",         ORD(P.CharCmpEq), 2)
     .defPrim("char>=?",        ORD(P.CharCmpGe), 2)
     .defPrim("char>?",         ORD(P.CharCmpGt), 2)
     .defPrim("char?",   	ORD(P.CharQ),     1)
     .defPrim("close-input-port", ORD(P.CloseInputPort), 1)
     .defPrim("close-output-port", ORD(P.CloseOutputPort), 1)
     .defPrim("complex?", 	ORD(P.NumberQ),   1)
     .defPrim("cons",    	ORD(P.Cons),      2)
     .defPrim("cos",     	ORD(P.Cos),       1)
     .defPrim("current-input-port", ORD(P.CurrentInputPort), 0)
     .defPrim("current-output-port", ORD(P.CurrentOutputPort), 0)
     .defPrim("display",        ORD(P.Display),   1, 2)
     .defPrim("eof-object?",    ORD(P.EofObjectQ), 1)
     .defPrim("eq?",     	ORD(P.EqQ),       2)
     .defPrim("equal?",  	ORD(P.EqualQ),    2)
     .defPrim("eqv?",    	ORD(P.EqvQ),      2)
     .defPrim("eval",           ORD(P.Eval),      1, 2)
     .defPrim("even?",          ORD(P.EvenQ),     1)
     .defPrim("exact?",         ORD(P.IntegerQ),  1)
     .defPrim("exp",     	ORD(P.Exp),       1)
     .defPrim("expt",    	ORD(P.Expt),      2)
     .defPrim("force",          ORD(P.Force),     1)
     .defPrim("for-each",       ORD(P.Foreach),   1, n)
     .defPrim("gcd",            ORD(P.Gcd),       0, n)
     .defPrim("inexact?",       ORD(P.InexactQ),  1)
     .defPrim("input-port?",    ORD(P.InputPortQ), 1)
     .defPrim("integer->char",  ORD(P.IntegerToChar),      1)
     .defPrim("integer?",       ORD(P.IntegerQ),  1)
     .defPrim("lcm",            ORD(P.Lcm),       0, n)
     .defPrim("length",  	ORD(P.Length),    1)
     .defPrim("list",    	ORD(P.List),      0, n)
     .defPrim("list->string", 	ORD(P.ListToString), 1)
     .defPrim("list->vector",   ORD(P.ListToVector),      1)
     .defPrim("list-ref", 	ORD(P.ListRef),   2)
     .defPrim("list-tail", 	ORD(P.ListTail),  2)
     .defPrim("list?",          ORD(P.ListQ),     1)
     .defPrim("load",           ORD(P.Load),      1)
     .defPrim("log",     	ORD(P.Log),       1)
     .defPrim("macro-expand",   ORD(P.MacroExpand),1)
     .defPrim("make-string", 	ORD(P.MakeString),1, 2)
     .defPrim("make-vector",    ORD(P.MakeVector),1, 2)
     .defPrim("map",            ORD(P.Map),       1, n)
     .defPrim("max",     	ORD(P.Max),       1, n)
     .defPrim("member",  	ORD(P.Member),    2)
     .defPrim("memq",    	ORD(P.MemQ),      2)
     .defPrim("memv",    	ORD(P.MemV),      2)
     .defPrim("min",     	ORD(P.Min),       1, n)
     .defPrim("modulo",         ORD(P.Modulo),    2)
     .defPrim("negative?",      ORD(P.NegativeQ), 1)
     .defPrim("newline", 	ORD(P.Newline),   0, 1)
     .defPrim("not",     	ORD(P.Not),       1)
     .defPrim("null?",   	ORD(P.NullQ),     1)
     .defPrim("number->string", ORD(P.NumberToString),   1, 2)
     .defPrim("number?", 	ORD(P.NumberQ),   1)
     .defPrim("odd?",           ORD(P.OddQ),      1)
     .defPrim("open-input-file",ORD(P.OpenInputFile), 1)
     .defPrim("open-output-file", ORD(P.OpenOutputFile), 1)
     .defPrim("output-port?",   ORD(P.OutputportQ), 1)
     .defPrim("pair?",   	ORD(P.PairQ),     1)
     .defPrim("peek-char",      ORD(P.PeekChar),  0, 1)
     .defPrim("positive?",      ORD(P.PositiveQ), 1)
     .defPrim("procedure?", 	ORD(P.ProcedureQ),1)
     .defPrim("quotient",       ORD(P.Quotient),  2)
     .defPrim("rational?",      ORD(P.IntegerQ), 1)
     .defPrim("read",    	ORD(P.Read),      0, 1)
     .defPrim("read-char",      ORD(P.ReadChar),  0, 1)
     .defPrim("real?", 	        ORD(P.NumberQ),   1)
     .defPrim("remainder",      ORD(P.Remainder), 2)
     .defPrim("reverse", 	ORD(P.Reverse),   1)
     .defPrim("round",  	ORD(P.Round),     1)
     .defPrim("set-car!",	ORD(P.SetCar),    2)
     .defPrim("set-cdr!",	ORD(P.SetCdr),    2)
     .defPrim("sin",     	ORD(P.Sin),       1)
     .defPrim("sqrt",    	ORD(P.Sqrt),      1)
     .defPrim("string", 	ORD(P.String),    0, n)
     .defPrim("string->list", 	ORD(P.StringToList), 1)
     .defPrim("string->number", ORD(P.StringToNumber),   1, 2)
     .defPrim("string->symbol", ORD(P.StringToSymbol),   1)
     .defPrim("string-append",  ORD(P.StringAppend), 0, n)
     .defPrim("string-ci<=?",   ORD(P.StringCiCmpLe), 2)
     .defPrim("string-ci<?" ,   ORD(P.StringCiCmpLt), 2)
     .defPrim("string-ci=?" ,   ORD(P.StringCiCmpEq), 2)
     .defPrim("string-ci>=?",   ORD(P.StringCiCmpGe), 2)
     .defPrim("string-ci>?" ,   ORD(P.StringCiCmpGt), 2)
     .defPrim("string-length",  ORD(P.StringLength),   1)
     .defPrim("string-ref", 	ORD(P.StringRef), 2)
     .defPrim("string-set!", 	ORD(P.StringSet), 3)
     .defPrim("string<=?",      ORD(P.StringCmpLe), 2)
     .defPrim("string<?",       ORD(P.StringCmpLt), 2)
     .defPrim("string=?",       ORD(P.StringCmpEq), 2)
     .defPrim("string>=?",      ORD(P.StringCmpGe), 2)
     .defPrim("string>?",       ORD(P.StringCmpGt), 2)
     .defPrim("string?", 	ORD(P.StringQ),   1)
     .defPrim("substring", 	ORD(P.Substring), 3)
     .defPrim("symbol->string", ORD(P.SymbolToString),   1)
     .defPrim("symbol?", 	ORD(P.SymbolQ),   1)
     .defPrim("tan",     	ORD(P.Tan),       1)
     .defPrim("tanh",     	ORD(P.Tanh),      1)
     .defPrim("vector",    	ORD(P.Vector),    0, n)
     .defPrim("vector->list",   ORD(P.VectorToList), 1)
     .defPrim("vector-length",  ORD(P.VectorLength), 1)
     .defPrim("vector-ref",     ORD(P.VectorRef), 2)
     .defPrim("vector-set!",    ORD(P.VectorSet), 3)
     .defPrim("vector?",    	ORD(P.VectorQ),   1)
     .defPrim("write",   	ORD(P.Write),     1, 2)
     .defPrim("write-char",   	ORD(P.Display),   1, 2)
     .defPrim("zero?",          ORD(P.ZeroQ),     1)
	      
     (*///////////// Extensions ////////////////*)

     .defPrim("new",     	    ORD(P.New),       1)
     .defPrim("class",   	    ORD(P.Class),     1)
     .defPrim("method",  	    ORD(P.Method),    2, n)
     .defPrim("exit",    	    ORD(P.Exit),      0, 1)
     .defPrim("error",    	    ORD(P.Error),     0, n)
     .defPrim("time-call",          ORD(P.TimeCall),  1, 2)
     .defPrim("_list*",             ORD(P.ListStar),  0, n)
     .defPrim("jailbreak",          ORD(P.JailBreak),  1, 1)
     .defPrim("modula-3-op",  ORD(P.M3Op), 3, 3)
       ;

    RETURN env;

  END InstallPrimitives;

PROCEDURE Apply(t : T; interp : Scheme.T; args : Object) : Object 
  RAISES { E } =
  VAR z : Object;
  BEGIN
    WITH nArgs = Length(args) DO
      IF    nArgs < t.minArgs THEN
        RETURN Error("too few args, " & Fmt.Int(nArgs) & 
               ", for " & t.name & ": " & Stringify(args))
      ELSIF nArgs > t.maxArgs THEN
        RETURN Error("too many args, " & Fmt.Int(nArgs) & 
               ", for " & t.name & ": " & Stringify(args))
      END
    END;

    WITH x = First(args),
         y = Second(args) DO
      CASE VAL(t.idNumber,P) OF
          P.Eq => RETURN NumCompare(args, '=')
        |
          P.Lt => RETURN NumCompare(args, '<')
        |
          P.Gt => RETURN NumCompare(args, '>')
        |
          P.Ge => RETURN NumCompare(args, 'G')
        | 
          P.Le => RETURN NumCompare(args, 'L')
        |
          P.Abs =>  RETURN FromLR(ABS(FromO(x)))
        |
          P.EofObject =>  RETURN Truth(SchemeInputPort.IsEOF(x))
        |
          P.EqQ => RETURN Truth(x = y)
        |
          P.EqualQ => RETURN Truth(Equal(x,y))
        |
          P.Force => 
          IF x = NIL OR NOT ISTYPE(x,Procedure) THEN RETURN x
          ELSE RETURN Proc(x).apply(interp,NIL)
          END
        |
          
          P.Car => RETURN PedanticFirst(x)
        |
          P.Floor => RETURN FromLR(FLOAT(FLOOR(FromO(x)),LONGREAL))
        |
          P.Ceiling => RETURN FromLR(FLOAT(CEILING(FromO(x)),LONGREAL))
        |
          P.Cons => RETURN Cons(x,y)
        |
          
          P.Divide => RETURN NumCompute(Rest(args), '/', FromO(x))
        |
          P.Length => RETURN FromLR(FLOAT(Length(x),LONGREAL))
        |
          P.List => RETURN args
        |
          P.ListQ => RETURN Truth(IsList(x))
        |
          P.Apply => RETURN Proc(x).apply(interp,ListStar(Rest(args)))
        |
          
          P.Max => RETURN NumCompute(args, 'X', FromO(x))
        |
          P.Min => RETURN NumCompute(args, 'N', FromO(x))
        |
          P.Minus => RETURN NumCompute(Rest(args), '-', FromO(x))
        |
          P.Newline => 
          TRY
            Wr.PutChar(OutPort(x,interp), '\n');
            Wr.Flush(OutPort(x,interp));
            RETURN True()
          EXCEPT
            Wr.Failure(err) => RAISE E("newline: Wr.Failure: " & AL.Format(err))
          END
        |
          
          P.Not => RETURN Truth(x = False())
        |
          P.NullQ => RETURN Truth(x = NIL)
        |
          P.NumberQ => RETURN Truth(x # NIL AND ISTYPE(x, SchemeLongReal.T))
        |
          P.PairQ => RETURN Truth(x # NIL AND ISTYPE(x,Pair))
        |
          P.Plus => RETURN NumCompute(args, '+', 0.0d0)
        |
          
          P.ProcedureQ => RETURN Truth(x # NIL AND ISTYPE(x,Procedure))
        |
          P.Read => RETURN InPort(x, interp).read()
        |
          P.Cdr => RETURN PedanticRest(x)
        |
          P.Round => RETURN FromLR(FLOAT(ROUND(FromO(x)),LONGREAL))
        |
          P.Second => RETURN Second(x)
        |
          
          P.SymbolQ => RETURN Truth(x # NIL AND ISTYPE(x,Symbol))
        |
          P.Times => RETURN NumCompute(args, '*', 1.0d0)
        |
          P.Truncate => RETURN FromLR(FLOAT(TRUNC(FromO(x)),LONGREAL))
        |
          P.Write => RETURN Write(x, OutPort(y, interp), TRUE)
        |
          P.Append => 
          IF args = NIL THEN RETURN NIL
          ELSE RETURN Append(args)
          END
        |
          
          P.BooleanQ => RETURN Truth(x = True() OR x = False())
        |
          P.Sqrt => RETURN FromLR(Math.sqrt(FromO(x)))
        |
          P.Expt => RETURN FromLR(Math.pow(FromO(x),FromO(y)))
        |
          P.Reverse => RETURN Reverse(x)
        |
          P.Assoc => RETURN MemberAssoc(x, y, 'a', ' ')
        |
          P.AssQ => RETURN MemberAssoc(x, y, 'a', 'q')
        |
          P.AssV => RETURN MemberAssoc(x, y, 'a', 'v')
        |
          P.Member =>RETURN MemberAssoc(x, y, 'm', ' ')
        |
          P.MemQ => RETURN MemberAssoc(x, y, 'm', 'q')
        |
          P.MemV => RETURN MemberAssoc(x, y, 'm', 'v')
        |
          P.EqvQ => RETURN Truth(Eqv(x,y))
        |
          
          P.ListRef =>  
          VAR p := x; BEGIN
            FOR k := TRUNC(FromO(y)) TO 1 BY -1 DO p:= Rest(p) END;
            RETURN First(p)
          END
        |
          P.ListTail =>           
          VAR p := x; BEGIN
            FOR k := TRUNC(FromO(y)) TO 1 BY -1 DO p:= Rest(p) END;
            RETURN p
          END
        |
          P.StringQ => RETURN Truth(x # NIL AND ISTYPE(x,String))
        |
          P.MakeString =>
          VAR
            str := NEW(String, TRUNC(FromO(x)));
          BEGIN
            IF y # NIL THEN
              WITH c = Char(y) DO
                FOR i := FIRST(str^) TO LAST(str^) DO
                  str[i] := c
                END
              END
            END;
            RETURN str
          END
        |
          P.String => RETURN ListToString(args)
        |
          P.StringLength => RETURN FromLR(FLOAT(NUMBER(Str(x)^),LONGREAL))
        |
          P.StringRef => 
          WITH str = Str(x)^, yi = TRUNC(FromO(y)) DO
            IF yi < FIRST(str) OR yi > LAST(str) THEN
              RETURN Error("string index out of bounds")
            ELSE
              RETURN Character(str[yi])
            END
          END
        |
          P.StringSet =>
          WITH z = Third(args), str = Str(x)^, yi = TRUNC(FromO(y)) DO
            IF yi < FIRST(str) OR yi > LAST(str) THEN
              RETURN Error("string index out of bounds")
            ELSE
              str[yi] := Char(z);
              RETURN z
            END
          END
        |
          P.Substring =>
          VAR 
            str := Str(x);
            start := TRUNC(FromO(y));
            end := TRUNC(FromO(Third(args)));
          BEGIN
            (* crimp pointers *)
            start := MAX(start, 0);
            start := MIN(start, LAST(str^));

            end := MAX(end, LAST(str^));
            end := MAX(end,start);

            WITH res = NEW(String, end-start) DO
              res^ := SUBARRAY(str^, start, end-start);
              RETURN res
            END
          END
        |
          
          P.StringAppend => RETURN StringAppend(args)
        |
          P.StringToList =>
          VAR
            result : Pair := NIL;
            str := Str(x);
          BEGIN
            FOR i := LAST(str^) TO FIRST(str^) BY -1 DO
              result := Cons(Character(str[i]),result)
            END;
            RETURN result
          END
        |
          P.ListToString => RETURN ListToString(x)
        |
          P.SymbolToString => 
          IF x = NIL OR NOT ISTYPE(x, Symbol) THEN
            RETURN Error("Not a symbol") 
          END;
          RETURN SchemeString.FromText(SchemeSymbol.ToText(x))
        |
          P.StringToSymbol => 
            RETURN SchemeSymbol.Symbol(Text.FromChars(Str(x)^))
        |
          P.Exp => RETURN FromLR(Math.exp(FromO(x)))
        |
          P.Log => RETURN FromLR(Math.log(FromO(x)))
        |
          P.Sin => RETURN FromLR(Math.sin(FromO(x)))
        |
          P.Cos => RETURN FromLR(Math.cos(FromO(x)))
        |
          P.Tan => RETURN FromLR(Math.tan(FromO(x)))
        |
          P.Acos => RETURN FromLR(Math.acos(FromO(x)))
        |
          P.Asin => RETURN FromLR(Math.asin(FromO(x)))
        |
          P.Atan => RETURN FromLR(Math.atan(FromO(x)))
        |
          
          P.NumberToString => RETURN NumberToString(x,y)
        |
          P.StringToNumber => RETURN StringToNumber(x,y)
        |
          P.CharQ => RETURN Truth(x # NIL AND ISTYPE(x, SchemeChar.T))
        |
          
          P.CharAlphabeticQ => RETURN Truth(Char(x) IN LowerCase + UpperCase)
        |
          P.CharNumericQ => RETURN Truth(Char(x) IN Digits)
        |
          P.CharWhitespaceQ => RETURN Truth(Char(x) IN White)
        |
          
          P.CharUppercaseQ => RETURN Truth(Char(x) IN UpperCase)
        |
          P.CharLowercaseQ => RETURN Truth(Char(x) IN LowerCase)
        |
          P.CharToInteger => RETURN FromLR(FLOAT(ORD(Char(x)),LONGREAL))
        |
          
          P.IntegerToChar => RETURN IChr(TRUNC(FromO(x)))
        |
          P.CharUpcase => RETURN Character(Upcase(Char(x)))
        |
          P.CharDowncase => RETURN Character(Downcase(Char(x)))
        |
          
          P.VectorQ => RETURN Truth(x # NIL AND ISTYPE(x, Vector))
        |
          P.MakeVector =>
          WITH num = TRUNC(FromO(x)) DO
            IF num < 0 THEN
              RETURN Error("Can't make vector of size " & Fmt.Int(num))
            END;
            WITH vec = NEW(Vector, num) DO
              IF y # NIL THEN
                FOR i := 0 TO num-1 DO
                  vec[i] := y
                END
              END;
              RETURN vec
            END
          END
        |
          P.Vector => RETURN ListToVector(args)
        |
          P.VectorLength => RETURN FromLR(FLOAT(NUMBER(Vec(x)^),LONGREAL))
        |
          
          P.VectorRef => RETURN Vec(x)[TRUNC(FromO(y))]
        |
          P.VectorSet => 
          WITH v = Third(args) DO
            Vec(x)[TRUNC(FromO(y))] := v;
            RETURN v 
          END
        |
          P.ListToVector => RETURN ListToVector(x)
        |
          P.Map => RETURN Map(Proc(x), Rest(args), interp, List1(NIL))
        |
          
          P.Foreach =>RETURN Map(Proc(x), Rest(args), interp, NIL)
        |
          P.CallCC =>
          (* make a new arbitrary text *)
          WITH txt = "CallCC" & Fmt.Int(123),
               proc =  NEW(SchemeContinuation.T).init(txt) DO
            TRY RETURN Proc(x).apply(interp, List1(proc))
            EXCEPT E(e) => IF e = txt THEN RETURN proc.value ELSE RAISE E(e) END
            END
          END
        
        |
          P.VectorToList => RETURN VectorToList(x)
        |
          P.Load => RETURN interp.loadFile(x)
        |
          P.Display => RETURN Write(x, OutPort(y, interp), FALSE)
        |
          
          P.InputPortQ => RETURN Truth(x # NIL AND ISTYPE(x,SchemeInputPort.T))
        |
          P.CurrentInputPort => RETURN interp.input
        |
          P.OpenInputFile => RETURN OpenInputFile(x)
        |
          
          P.CloseInputPort => RETURN InPort(x, interp).close()
        |
          P.OutputportQ => RETURN Truth(x # NIL AND ISTYPE(x,Wr.T))
        |
          P.CurrentOutputPort => RETURN interp.output
        |
          
          P.OpenOutputFile => RETURN OpenOutputFile(x)
        |
          P.CloseOutputPort => 
          TRY
            Wr.Close(OutPort(x, interp)); RETURN True()
          EXCEPT
            Wr.Failure(err) => RAISE E("close-output-port: Wr.Failure: " & AL.Format(err))
          END

        |
          P.ReadChar => RETURN InPort(x, interp).readChar()
        |
          P.PeekChar => RETURN InPort(x, interp).peekChar()
        |
          P.Eval => RETURN interp.evalInGlobalEnv(x)
        |
          P.Quotient =>
          VAR d := FromO(x) / FromO(y); BEGIN
            IF d > 0.0d0 THEN
              RETURN FromLR(FLOAT(FLOOR(d),LONGREAL))
            ELSE
              RETURN FromLR(FLOAT(CEILING(d),LONGREAL))
            END
          END
        |
          P.Remainder => RETURN FromLR(FLOAT(TRUNC(FromO(x)) MOD TRUNC(FromO(y)), LONGREAL))
                         (* this must be wrong for negative y *)
        |
          P.Modulo => RETURN FromLR(FLOAT(TRUNC(FromO(x)) MOD TRUNC(FromO(y)), LONGREAL))
        |
          P.Third => RETURN Third(x)
        |
          P.EofObjectQ => RETURN Truth(x = SchemeInputPort.EOF)
        |
          P.Gcd =>
          IF args = NIL THEN RETURN Zero ELSE RETURN Gcd(args) END
        |
          P.Lcm =>
          IF args = NIL THEN RETURN One ELSE RETURN Lcm(args) END
        |
          P.Cxr =>
          VAR p := x; BEGIN
            FOR i := Text.Length(t.name)-2 TO 1 BY -1 DO
              IF Text.GetChar(t.name,i) = 'a' THEN
                p := PedanticFirst(p)
              ELSE
                p := PedanticRest(p)
              END
            END;
            RETURN p
          END
        |
          P.OddQ => RETURN Truth(ABS(TRUNC(FromO(x)) MOD 2) # 0)
        |
          P.EvenQ => RETURN Truth(ABS(TRUNC(FromO(x)) MOD 2) = 0)
        |
          P.ZeroQ => RETURN Truth(FromO(x) = 0.0d0)
        |
          P.PositiveQ => RETURN Truth(FromO(x) > 0.0d0)
        |
          P.NegativeQ => RETURN Truth(FromO(x) < 0.0d0)
        |
          P.CharCmpEq => RETURN Truth(CharCompare(x, y, FALSE) =  0)
        |
          P.CharCmpLt => RETURN Truth(CharCompare(x, y, FALSE) <  0)
        |
          P.CharCmpGt =>RETURN Truth(CharCompare(x, y, FALSE) >  0)
        |
          P.CharCmpGe => RETURN Truth(CharCompare(x, y, FALSE) >=  0)
        |
          P.CharCmpLe =>RETURN Truth(CharCompare(x, y, FALSE) <=  0)
        |
          P.CharCiCmpEq =>RETURN Truth(CharCompare(x, y, TRUE) =  0)
        |
          P.CharCiCmpLt =>RETURN Truth(CharCompare(x, y, TRUE) <  0)
        |
          P.CharCiCmpGt =>RETURN Truth(CharCompare(x, y, TRUE) >  0)
        |
          P.CharCiCmpGe =>RETURN Truth(CharCompare(x, y, TRUE) >=  0)
        |
          P.CharCiCmpLe =>RETURN Truth(CharCompare(x, y, TRUE) <=  0)
        |
          P.StringCmpEq => RETURN Truth(StringCompare(x, y, FALSE) =  0)
        |
          P.StringCmpLt => RETURN Truth(StringCompare(x, y, FALSE) <  0)
        |
          P.StringCmpGt => RETURN Truth(StringCompare(x, y, FALSE) >  0)
        |
          P.StringCmpGe => RETURN Truth(StringCompare(x, y, FALSE) >= 0)
        |
          P.StringCmpLe => RETURN Truth(StringCompare(x, y, FALSE) <= 0)
        |
          P.StringCiCmpEq => RETURN Truth(StringCompare(x, y, TRUE) =  0)
        |
          P.StringCiCmpLt => RETURN Truth(StringCompare(x, y, TRUE) <  0)
        |
          P.StringCiCmpGt => RETURN Truth(StringCompare(x, y, TRUE) >  0)
        |
          P.StringCiCmpGe => RETURN Truth(StringCompare(x, y, TRUE) >= 0)
        |
          P.StringCiCmpLe => RETURN Truth(StringCompare(x, y, TRUE) <= 0)
        |
          P.InexactQ => RETURN Truth(NOT IsExact(x))
        |
          P.ExactQ, P.IntegerQ => RETURN Truth(IsExact(x))
        |
          P.CallWithInputFile =>
          VAR p : SchemeInputPort.T := NIL;
          BEGIN
            TRY p := OpenInputFile(x);
                z := Proc(y).apply(interp, List1(p)) 
            FINALLY
                IF p # NIL THEN EVAL p.close() END
            END;
            RETURN z
          END
        |
          P.CallWithOutputFile => 
          VAR p : Wr.T := NIL;
          BEGIN
            TRY p := OpenOutputFile(x);
                z := Proc(y).apply(interp, List1(p))
            FINALLY
                IF p # NIL THEN 
                  TRY
                    Wr.Close(p) 
                  EXCEPT
                    Wr.Failure(err) => RAISE E("call-with-output-file: on close, Wr.Failure: " & AL.Format(err))

                  END
                END
            END;
            RETURN z
          END
        |
          P.Tanh => RETURN FromLR(Math.tanh(FromO(x)))
        |
          P.New => RETURN False() (* not impl *)
        |
          P.Class => RETURN False() (* not impl *)
        |
          P.Method => RETURN False() (* not impl *)
        |
          P.Exit => 
          IF x = NIL THEN Process.Exit(0) 
          ELSE Process.Exit(TRUNC(FromO(x)))
          END;
          <* ASSERT FALSE *>
        |
          P.SetCar => RETURN SetFirst(x,y)
        |
          P.SetCdr => RETURN SetRest(x,y)
        |
          P.TimeCall => RETURN False() (* not impl *)
        |
          P.MacroExpand => RETURN SchemeMacro.MacroExpand(interp,x)
        |
          P.Error => RETURN Error(Stringify(args))
        |
          P.ListStar => RETURN ListStar(args)
        |
          P.JailBreak =>
          IF interp.jailBreak = NIL THEN
            RETURN Error("No jailbreak defined")
          ELSE
            RETURN interp.jailBreak.apply(args)
          END
        |
          P.M3Op =>
          IF interp.m3TableOps = NIL THEN
            RETURN Error("No table ops defined")
          ELSE
            RETURN interp.m3TableOps.apply(args)
          END
        END
    END
  END Apply;

PROCEDURE IsList(x : Object) : BOOLEAN =
  VAR
    slow, fast := x;
  BEGIN
    LOOP
      IF fast = NIL THEN RETURN TRUE END;
      IF slow = Rest(fast) OR NOT ISTYPE(fast, Pair) OR 
         slow = NIL OR NOT ISTYPE(slow, Pair) THEN
        RETURN FALSE
      END;
      slow := Rest(slow);
      fast := Rest(fast);
      IF fast = NIL THEN RETURN TRUE END;
      IF NOT ISTYPE(fast, Pair) THEN RETURN FALSE END;
      fast := Rest(fast)
    END
  END IsList;

PROCEDURE Append(args : Object) : Object =
  BEGIN
    IF Rest(args) = NIL THEN RETURN First(args) 
    ELSE RETURN Append2(First(args), Append(Rest(args)))
    END
  END Append;

PROCEDURE Append2(x, y : Object) : Object =
  BEGIN
    IF x # NIL AND ISTYPE(x,Pair) THEN RETURN Cons(First(x),
                                                   Append2(Rest(x),y))
    ELSE RETURN y
    END
  END Append2;

PROCEDURE IsExact(x : Object) : BOOLEAN RAISES { E } =
  BEGIN
    IF x = NIL OR NOT ISTYPE(x, SchemeLongReal.T) THEN RETURN FALSE END;
    WITH d = FromO(x) DO
      RETURN d = FLOAT(ROUND(d),LONGREAL) AND 
             ABS(d) < 102962884861573423.0d0 (* ??? *)
    END
  END IsExact;

PROCEDURE MemberAssoc(obj, list : Object; m, eq : CHAR) : Object =
  BEGIN
    WHILE list # NIL AND ISTYPE(list, Pair) DO
      VAR target : Object; 
          found : BOOLEAN;
      BEGIN
        IF m = 'm' THEN target := First(list) ELSE 
          target := First(First(list)) 
        END;

        CASE eq OF
          'q' => found := target = obj
        |
          'v' => found := Eqv(target,obj)
        |
          ' ' => found := Equal(target,obj)
        ELSE
          EVAL Warn("Bad option to memberAssoc:" & Text.FromChar(eq)); 
          RETURN False()
        END;
        
        IF found THEN
          IF m = 'm' THEN RETURN list ELSE RETURN First(list) END
        END;

        list := Rest(list)
      END
    END;
    RETURN False()
  END MemberAssoc;

PROCEDURE NumCompare(args : Object; op : CHAR) : Object RAISES { E } =
  BEGIN
    WHILE Rest(args) # NIL AND ISTYPE(Rest(args), Pair) DO
      VAR
        x := FromO(First(args));
        y : LONGREAL;
      BEGIN
        args := Rest(args);
        y := FromO(First(args));

        CASE op OF
          '>' => IF NOT x >  y THEN RETURN False() END
        |
          '<' => IF NOT x <  y THEN RETURN False() END
        |
          '=' => IF NOT x =  y THEN RETURN False() END
        |
          'L' => IF NOT x <= y THEN RETURN False() END
        |
          'G' => IF NOT x >= y THEN RETURN False() END
        ELSE
          <* ASSERT FALSE *>
        END
      END
    END;
    RETURN True()
  END NumCompare;
      
PROCEDURE NumCompute(args : Object; 
                     op : CHAR; 
                     READONLY start : LONGREAL) : Object 
  RAISES { E } =
  VAR 
    result := start;
  BEGIN
    IF args = NIL THEN
      CASE op OF
        '-' => RETURN FromLR(0.0d0 - result)
      |
        '/' => RETURN FromLR(1.0d0 / result)
      ELSE
        RETURN FromLR(result)
      END
    ELSE
      WHILE args # NIL AND ISTYPE(args, Pair) DO
        WITH x = FromO(First(args)) DO
          IF TruthO(False()) THEN
            (* force a register spill, work around a compiler bug... *)
            Debug.Out(Fmt.LongReal(result) & " " & Fmt.LongReal(x))
          END;
          CASE op OF 
            'X' => IF x > result THEN result := x END
          |
            'N' => IF x < result THEN result := x END
          |
            '+' => result := result + x
          |
            '-' => result := result - x
          |
            '*' => result := result * x
          |
            '/' => result := result / x
          ELSE
            <* ASSERT FALSE *>
          END
        END;
        args := Rest(args)
      END;
      RETURN FromLR(result)
    END
  END NumCompute;

PROCEDURE NumberToString(x, y : Object) : Object RAISES { E } =
  VAR
    base : INTEGER;
  BEGIN
    IF y # NIL AND ISTYPE(y, SchemeLongReal.T) THEN
      base := ROUND(FromO(y))
    ELSE
      base := 10
    END;

    IF base < 2 THEN base := 2 ELSIF base > 16 THEN base := 16 END;

    IF base # 10 OR FromO(x) = FLOAT(ROUND(FromO(x)),LONGREAL) THEN
      RETURN SchemeString.FromText(Fmt.Int(ROUND(FromO(x)), base := base))
    ELSE
      RETURN SchemeString.FromText(Fmt.LongReal(FromO(x)))
    END
  END NumberToString;

PROCEDURE StringToNumber(x, y : Object) : Object RAISES { E } = 
  VAR base : INTEGER;
  BEGIN
    IF y # NIL AND ISTYPE(y, SchemeLongReal.T) THEN
      base := ROUND(FromO(y))
    ELSE
      base := 10
    END;

    IF base < 2 THEN base := 2 ELSIF base > 16 THEN base := 16 END;

    TRY
      IF base = 10 THEN
        RETURN FromLR(Scan.LongReal(StringifyQ(x,FALSE)))
      ELSE
        RETURN FromLR(FLOAT(Scan.Int(StringifyQ(x,FALSE), defaultBase := base),
                            LONGREAL))
      END
    EXCEPT
      FloatMode.Trap, Lex.Error => RETURN False()
    END
  END StringToNumber;

PROCEDURE Gcd(args : Object) : Object RAISES { E } =
  VAR
    gcd := 0;

  BEGIN
    WHILE args # NIL AND ISTYPE(args, Pair) DO
      gcd := Gcd2(ROUND(ABS(FromO(First(args)))), gcd);
      args := Rest(args)
    END;
    RETURN FromLR(FLOAT(gcd,LONGREAL))
  END Gcd;

PROCEDURE Gcd2(a, b : INTEGER) : INTEGER =
  BEGIN 
    IF b = 0 THEN RETURN a 
    ELSE RETURN Gcd2(b, a MOD b)
    END
  END Gcd2;

PROCEDURE Lcm(args : Object) : Object RAISES { E } =
  VAR
    L, g := 1;
  BEGIN
    WHILE args # NIL AND ISTYPE(args, Pair) DO
      WITH n = ABS(ROUND(FromO(First(args)))) DO
        g := Gcd2(n, L);
        IF g = 0 THEN
          L := g 
        ELSE
          L := (n DIV g) * L
        END;
        args := Rest(args)
      END
    END;
    RETURN FromLR(FLOAT(L,LONGREAL))
  END Lcm;

PROCEDURE CharCompare(x, y : Object; ci : BOOLEAN) : INTEGER RAISES { E } =
  BEGIN
    IF ci THEN RETURN ORD(Downcase(Char(x))) - ORD(Downcase(Char(y)))
    ELSE       RETURN ORD(Char(x)) - ORD(Char(y))
    END         
  END CharCompare;

PROCEDURE StringCompare(x, y : Object; ci : BOOLEAN) : INTEGER RAISES { E } =
  BEGIN
    IF x # NIL AND y # NIL AND ISTYPE(x, String) AND ISTYPE(y, String) THEN
      WITH xc = NARROW(x,String), yc = NARROW(y, String) DO
        FOR i := 0 TO MIN(LAST(xc^),LAST(yc^)) DO
          VAR diff : INTEGER; BEGIN
            IF ci THEN
              diff := ORD(Upcase(xc[i])) - ORD(Upcase(yc[i]))
            ELSE
              diff := ORD(xc[i]) - ORD(yc[i])
            END;
            IF diff # 0 THEN RETURN diff END
          END
        END;
        
        RETURN NUMBER(xc^) - NUMBER(yc^)
      END
    ELSE
      EVAL Error("expected two strings, got: " & Stringify(List2(x, y)));
      RETURN 0
    END
  END StringCompare;

PROCEDURE StringAppend(args : Object) : String  RAISES { E } =
  VAR res := Wx.New();
  BEGIN
    WHILE args # NIL AND ISTYPE(args,Pair) DO
      Wx.PutText(res,StringifyQ(First(args),FALSE));
      args := Rest(args)
    END;
    RETURN Str(Wx.ToText(res))
  END StringAppend;

PROCEDURE OpenOutputFile(filename : Object) : Wr.T RAISES { E } =
  BEGIN
    TRY
      RETURN FileWr.Open(StringifyQ(filename, FALSE)) 
    EXCEPT
      OSError.E(err) => RETURN Error("Error opening " & Stringify(filename) & " : "&
                                     AL.Format(err))
    END
  END OpenOutputFile;

PROCEDURE OpenInputFile(filename : Object) : SchemeInputPort.T RAISES { E } =
  BEGIN
    TRY
      WITH rd = FileRd.Open(StringifyQ(filename, FALSE)) DO
        RETURN NEW(SchemeInputPort.T).init(rd)
      END
    EXCEPT
      OSError.E(err) => RETURN Error("Error opening " & Stringify(filename) & " : "&
                                     AL.Format(err))
    END 
  END OpenInputFile;

PROCEDURE Map(proc : SchemeProcedure.T;
              args : Object;
              interp : Scheme.T;
              result : Pair) : Pair RAISES { E } =
  VAR
    accum := result;
  BEGIN
    IF Rest(args) = NIL THEN
      args := First(args);
      WHILE args # NIL AND ISTYPE(args, Pair) DO
        WITH x = proc.apply(interp, List1(First(args))) DO
          IF accum # NIL THEN 
            accum.rest := List1(x);
            accum := accum.rest;
          END;
          args := Rest(args)
        END
      END
    ELSE
      WITH car = Proc(interp.evalInGlobalEnv(SchemeSymbol.Symbol("car"))),
           cdr = Proc(interp.evalInGlobalEnv(SchemeSymbol.Symbol("cdr"))) DO
        WHILE First(args) # NIL AND ISTYPE(First(args),Pair) DO
          WITH x = proc.apply(interp, Map(car, List1(args), interp, List1(NIL))) DO
            IF accum # NIL THEN 
              accum.rest := List1(x);
              accum := accum.rest;
            END
          END;
          args := Map(cdr, List1(args), interp, List1(NIL))
        END
      END
    END;
    RETURN Rest(result)
  END Map;
       
BEGIN END SchemePrimitive.
