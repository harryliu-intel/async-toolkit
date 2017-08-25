%source cmd.t cmd.l
%import cmdTok cmdLex

%module {
IMPORT Scan;
IMPORT Lex, FloatMode;
<* FATAL Lex.Error, FloatMode.Trap *>
}

T_LITERAL:       {val: TEXT}
T_FLOAT:           {val: LONGREAL}

T_LITERAL        {$R T_LITERAL{$$ := $}}
T_FLOAT            {$R T_FLOAT{$$ := Scan.LongReal($)}}
