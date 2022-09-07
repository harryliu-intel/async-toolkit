%source liberty.t liberty.l
%import libertyTok libertyLex

%interface {
IMPORT LibertyNumber;
}

%module {
IMPORT Debug;
IMPORT LibertyNumber;
IMPORT SpecialScan AS Scan;
IMPORT Text;
}

IDENT:	   { val : TEXT }
STRING:    { val : TEXT }
NUM:       { val : LibertyNumber.T }


bare_IDENT   {Debug.Out("got ident " & $); $R IDENT{$$ := $}}

STRING {
  Debug.Out("got string " & $);
  WITH len = Text.Length($),
       sub = Text.Sub($, 1, len - 2),
       str = Scan.String(sub) DO
    $R STRING { $$ := str }
  END
}


NUM { IF Text.FindChar($, '.') = -1 THEN
        $R NUM{$$ := NEW(LibertyNumber.Integer, val := Scan.Int($))}
      ELSE
        $R NUM{$$ := NEW(LibertyNumber.Floating, val := Scan.LongReal($))}
      END
    }