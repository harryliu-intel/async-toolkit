%source liberty.t liberty.l
%import libertyTok libertyLex

%interface {
IMPORT LibertyNumber;
}

%module {
IMPORT Debug;
IMPORT LibertyNumber;
IMPORT Scan;
IMPORT Text;
}

IDENT:	   { val : TEXT }
STRING:    { val : TEXT }
NUM:       { val : LibertyNumber.T }


bare_IDENT   {Debug.Out("got ident " & $); $R IDENT{$$ := $}}

STRING { $R STRING { $$ := $ }}


NUM { IF Text.FindChar($, '.') = -1 THEN
        $R NUM{$$ := NEW(LibertyNumber.Integer, val := Scan.Int($))}
      ELSE
        $R NUM{$$ := NEW(LibertyNumber.Floating, val := Scan.LongReal($))}
      END
    }