%source gate.t gate.l
%import gateTok gateLex

%module {
}

IDENT:	       {val: TEXT}

bare_IDENT   {$R IDENT{$$ := $}}
