%source vnf.t vnf.l
%import vnfTok vnfLex

%module {
}

IDENT: { val : TEXT }
bare_IDENT { $R IDENT{$$ := $}}
