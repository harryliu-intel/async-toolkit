(* $Id$ *)

MODULE SchemePrimitive;
IMPORT SchemeEnvironment;

REVEAL
  T = Public BRANDED Brand OBJECT
    minArgs, maxArgs : CARDINAL;
    idNumber : INTEGER;
  OVERRIDES
    init := Init;
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
        InputportQ, CurrentInputPort, OpenInputFile, 
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
        Error, ListStar };

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
     .defPrim("input-port?",    ORD(P.InputportQ), 1)
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
       ;

    RETURN env;

  END InstallPrimitives;

BEGIN END SchemePrimitive.
