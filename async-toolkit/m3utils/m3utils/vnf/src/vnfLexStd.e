%source vnf.t vnf.l
%import vnfTok vnfLex

%module {
IMPORT Scan;
IMPORT Lex, FloatMode;
<*FATAL Lex.Error, FloatMode.Trap*>
}

IDENT: { val : TEXT }
bare_IDENT { $R IDENT{$$ := $}}
INT: { val : INTEGER }
INT {$R INT{$$ := Scan.Int($)}}