%source liberty.t liberty.l
%import libertyTok libertyLex

%module {
IMPORT Debug;
}

IDENT:	       {val: TEXT}

bare_IDENT   {Debug.Out("got ident " & $); $R IDENT{$$ := $}}
