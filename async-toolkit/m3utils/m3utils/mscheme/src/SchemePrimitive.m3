(* $Id$ *)

MODULE SchemePrimitive;

TYPE
  P = { Eq, Lt, Gt, Ge, Le,
        Abs, EofObject, EqQ, EqualQ, Force,
        Car, Floor,  Ceiling, Cons, 
        Divid, Length, List, ListQ, Apply,
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
        IntegerToChar, CharUpcase, CharDowncase, StringQ,
        VectorQ, MakeVector, Vector, VectorLength,
        VectorRef, VectorSet, ListToVector, Map, 
        Foreach, CallCC, VectorToList, Load, Display,
        InputportQ, CurrentInputPort, OpenInputFile, 
        CloseInputPort, OutputportQ, Currentoutputport,
        OpenOutputFile, CloseOutputPort, ReadChar,
        PeekChar, Eval, Quotient, Remainder,
        Modulo, Third, EofObjectQ, Gcd, Lcm, 
        Cxr, OddQ, EvenQ, ZeroQ, PositiveQ,
        NegativeQ, 
        CharCmp, CharCiCmp,
        StringCmp, StringCiCmp,
        ExactQ, InexactQ, IntegerQ,
        CallWithInputFile, CallWithOutputFile,
        Tanh,

        (* extensions follow *)

        New, Class, Method, Exit,
        SetCar, SetCdr, TimeCall, MacroExpand,
        Error, ListStar };

PROCEDURE InstallPrimitives(env : SchemeEnvironment.T) : SchemeEnvironment.T =
  BEGIN
    int n = Integer.MAX_VALUE;

    env
     .defPrim("*",       	P.Times,     0, n)
     .defPrim("+",       	P.Plus,      0, n)
     .defPrim("-",       	P.Minus,     1, n)
     .defPrim("/",       	P.Divide,    1, n)
     .defPrim("<",       	P.Lt,        2, n)
     .defPrim("<=",      	P.Le,        2, n)
     .defPrim("=",       	P.Eq,        2, n)
     .defPrim(">",       	P.Gt,        2, n)
     .defPrim(">=",      	P.Ge,        2, n)
     .defPrim("abs",     	P.Abs,       1)
     .defPrim("acos",    	P.Acos,      1)
     .defPrim("append",         P.Append,    0, n)
     .defPrim("apply",   	P.Apply,     2, n)
     .defPrim("asin",    	P.Asin,      1)
     .defPrim("assoc",   	P.Assoc,     2)
     .defPrim("assq",    	P.AssQ,      2)
     .defPrim("assv",    	P.Assv,      2)
     .defPrim("atan",    	P.Atan,      1)
     .defPrim("boolean?",	P.BooleanQ,  1)
     .defPrim("caaaar",         P.Cxr,       1)
     .defPrim("caaadr",         P.Cxr,       1)
     .defPrim("caaar",          P.Cxr,       1)
     .defPrim("caadar",         P.Cxr,       1)
     .defPrim("caaddr",         P.Cxr,       1)
     .defPrim("caadr",          P.Cxr,       1)
     .defPrim("caar",           P.Cxr,       1)
     .defPrim("cadaar",         P.Cxr,       1)
     .defPrim("cadadr",         P.Cxr,       1)
     .defPrim("cadar",          P.Cxr,       1)
     .defPrim("caddar",         P.Cxr,       1)
     .defPrim("cadddr",         P.Cxr,       1)
     .defPrim("caddr",     	P.Third,     1)
     .defPrim("cadr",  	        P.Second,    1)
     .defPrim("call-with-current-continuation",        P.Callcc,    1)
      /*
     .defPrim("call-with-input-file", P.Callwithinputfile, 2)
     .defPrim("call-with-output-file", P.Callwithoutputfile, 2)
     */
     .defPrim("car",     	P.Car,       1)
     .defPrim("cdaaar",         P.Cxr,       1)
     .defPrim("cdaadr",         P.Cxr,       1)
     .defPrim("cdaar",          P.Cxr,       1)
     .defPrim("cdadar",         P.Cxr,       1)
     .defPrim("cdaddr",         P.Cxr,       1)
     .defPrim("cdadr",          P.Cxr,       1)
     .defPrim("cdar",           P.Cxr,       1)
     .defPrim("cddaar",         P.Cxr,       1)
     .defPrim("cddadr",         P.Cxr,       1)
     .defPrim("cddar",          P.Cxr,       1)
     .defPrim("cdddar",         P.Cxr,       1)
     .defPrim("cddddr",         P.Cxr,       1)
     .defPrim("cdddr",          P.Cxr,       1)
     .defPrim("cddr",           P.Cxr,       1)
     .defPrim("cdr",     	P.Cdr,       1)
     .defPrim("char->integer",  P.Chartointeger,      1)
     .defPrim("char-alphabetic?",P.CharalphabeticQ,      1)
     .defPrim("char-ci<=?",     P.Charcicmp+LE, 2)
     .defPrim("char-ci<?" ,     P.Charcicmp+LT, 2)
     .defPrim("char-ci=?" ,     P.Charcicmp+EQ, 2)
     .defPrim("char-ci>=?",     P.Charcicmp+GE, 2)
     .defPrim("char-ci>?" ,     P.Charcicmp+GT, 2)
     .defPrim("char-downcase",  P.Chardowncase,      1)
     .defPrim("char-lower-case?",P.CharlowercaseQ,      1)
     .defPrim("char-numeric?",  P.CharnumericQ,      1)
     .defPrim("char-upcase",    P.Charupcase,      1)
     .defPrim("char-upper-case?",P.CharuppercaseQ,      1)
     .defPrim("char-whitespace?",P.CharwhitespaceQ,      1)
     .defPrim("char<=?",        P.Charcmp+LE, 2)
     .defPrim("char<?",         P.Charcmp+LT, 2)
     .defPrim("char=?",         P.Charcmp+EQ, 2)
     .defPrim("char>=?",        P.Charcmp+GE, 2)
     .defPrim("char>?",         P.Charcmp+GT, 2)
     .defPrim("char?",   	P.CharQ,     1)
     .defPrim("close-input-port", P.Closeinputport, 1)
     .defPrim("close-output-port", P.Closeoutputport, 1)
     .defPrim("complex?", 	P.NumberQ,   1)
     .defPrim("cons",    	P.Cons,      2)
     .defPrim("cos",     	P.Cos,       1)
     .defPrim("current-input-port", P.Currentinputport, 0)
     .defPrim("current-output-port", P.Currentoutputport, 0)
     .defPrim("display",        P.Display,   1, 2)
     .defPrim("eof-object?",    P.EofobjectQ, 1)
     .defPrim("eq?",     	P.EqQ,       2)
     .defPrim("equal?",  	P.EqualQ,    2)
     .defPrim("eqv?",    	P.EqvQ,      2)
     .defPrim("eval",           P.Eval,      1, 2)
     .defPrim("even?",          P.EvenQ,     1)
     .defPrim("exact?",         P.IntegerQ,  1)
     .defPrim("exp",     	P.Exp,       1)
     .defPrim("expt",    	P.Expt,      2)
     .defPrim("force",          P.Force,     1)
     .defPrim("for-each",       P.Foreach,   1, n)
     .defPrim("gcd",            P.Gcd,       0, n)
     .defPrim("inexact?",       P.InexactQ,  1)
     .defPrim("input-port?",    P.InputportQ, 1)
     .defPrim("integer->char",  P.Integertochar,      1)
     .defPrim("integer?",       P.IntegerQ,  1)
     .defPrim("lcm",            P.Lcm,       0, n)
     .defPrim("length",  	P.Length,    1)
     .defPrim("list",    	P.List,      0, n)
     .defPrim("list->string", 	P.Listtostring, 1)
     .defPrim("list->vector",   P.Listtovector,      1)
     .defPrim("list-ref", 	P.Listref,   2)
     .defPrim("list-tail", 	P.Listtail,  2)
     .defPrim("list?",          P.ListQ,     1)
     .defPrim("load",           P.Load,      1)
     .defPrim("log",     	P.Log,       1)
     .defPrim("macro-expand",   P.Macroexpand,1)
     .defPrim("make-string", 	P.Makestring,1, 2)
     .defPrim("make-vector",    P.Makevector,1, 2)
     .defPrim("map",            P.Map,       1, n)
     .defPrim("max",     	P.Max,       1, n)
     .defPrim("member",  	P.Member,    2)
     .defPrim("memq",    	P.MemQ,      2)
     .defPrim("memv",    	P.Memv,      2)
     .defPrim("min",     	P.Min,       1, n)
     .defPrim("modulo",         P.Modulo,    2)
     .defPrim("negative?",      P.NegativeQ, 1)
     .defPrim("newline", 	P.Newline,   0, 1)
     .defPrim("not",     	P.Not,       1)
     .defPrim("null?",   	P.NullQ,     1)
     .defPrim("number->string", P.Numbertostring,   1, 2)
     .defPrim("number?", 	P.NumberQ,   1)
     .defPrim("odd?",           P.OddQ,      1)
      /*
     .defPrim("open-input-file",P.Openinputfile, 1)
     .defPrim("open-output-file", P.Openoutputfile, 1)
     */
     .defPrim("output-port?",   P.OutputportQ, 1)
     .defPrim("pair?",   	P.PairQ,     1)
     .defPrim("peek-char",      P.Peekchar,  0, 1)
     .defPrim("positive?",      P.PositiveQ, 1)
     .defPrim("procedure?", 	P.ProcedureQ,1)
     .defPrim("quotient",       P.Quotient,  2)
     .defPrim("rational?",      P.IntegerQ, 1)
     .defPrim("read",    	P.Read,      0, 1)
     .defPrim("read-char",      P.Readchar,  0, 1)
     .defPrim("real?", 	        P.NumberQ,   1)
     .defPrim("remainder",      P.Remainder, 2)
     .defPrim("reverse", 	P.Reverse,   1)
     .defPrim("round",  	P.Round,     1)
     .defPrim("set-car!",	P.Setcar,    2)
     .defPrim("set-cdr!",	P.Setcdr,    2)
     .defPrim("sin",     	P.Sin,       1)
     .defPrim("sqrt",    	P.Sqrt,      1)
     .defPrim("string", 	P.String,    0, n)
     .defPrim("string->list", 	P.Stringtolist, 1)
     .defPrim("string->number", P.Stringtonumber,   1, 2)
     .defPrim("string->symbol", P.Stringtosymbol,   1)
     .defPrim("string-append",  P.Stringappend, 0, n)
     .defPrim("string-ci<=?",   P.Stringcicmp+LE, 2)
     .defPrim("string-ci<?" ,   P.Stringcicmp+LT, 2)
     .defPrim("string-ci=?" ,   P.Stringcicmp+EQ, 2)
     .defPrim("string-ci>=?",   P.Stringcicmp+GE, 2)
     .defPrim("string-ci>?" ,   P.Stringcicmp+GT, 2)
     .defPrim("string-length",  P.Stringlength,   1)
     .defPrim("string-ref", 	P.Stringref, 2)
     .defPrim("string-set!", 	P.Stringset, 3)
     .defPrim("string<=?",      P.Stringcmp+LE, 2)
     .defPrim("string<?",       P.Stringcmp+LT, 2)
     .defPrim("string=?",       P.Stringcmp+EQ, 2)
     .defPrim("string>=?",      P.Stringcmp+GE, 2)
     .defPrim("string>?",       P.Stringcmp+GT, 2)
     .defPrim("string?", 	P.StringQ,   1)
     .defPrim("substring", 	P.Substring, 3)
     .defPrim("symbol->string", P.Symboltostring,   1)
     .defPrim("symbol?", 	P.SymbolQ,   1)
     .defPrim("tan",     	P.Tan,       1)
     .defPrim("tanh",     	P.Tanh,      1)
     .defPrim("vector",    	P.Vector,    0, n)
     .defPrim("vector->list",   P.Vectortolist, 1)
     .defPrim("vector-length",  P.Vectorlength, 1)
     .defPrim("vector-ref",     P.Vectorref, 2)
     .defPrim("vector-set!",    P.Vectorset, 3)
     .defPrim("vector?",    	P.VectorQ,   1)
     .defPrim("write",   	P.Write,     1, 2)
     .defPrim("write-char",   	P.Display,   1, 2)
     .defPrim("zero?",          P.ZeroQ,     1)
	      
     ///////////// Extensions ////////////////

     .defPrim("new",     	    P.New,       1)
     .defPrim("class",   	    P.Class,     1)
     .defPrim("method",  	    P.Method,    2, n)
     .defPrim("exit",    	    P.Exit,      0, 1)
     .defPrim("error",    	    P.Error,     0, n)
     .defPrim("time-call",          P.Timecall,  1, 2)
     .defPrim("_list*",             P.Liststar,  0, n)
       ;

    RETURN env;

  END InstallPrimitives;

BEGIN END SchemePrimitive.
