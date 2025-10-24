%source glitch.t glitch.l
%import glitchTok glitchLex

%module {
}

IDENT:	       {val: TEXT}

bare_IDENT   {$R IDENT{$$ := $}}

