%source source_text.t source_text.y
%import source_text_lexExt source_textParse 
%module {
IMPORT SourceTextSTRINGList;
IMPORT source_textTok;
}

%interface {
IMPORT SourceTextSTRINGList;
IMPORT source_textTok;
}

list1of_STRING: { val : SourceTextSTRINGList.T }
  STRING { $$ := SourceTextSTRINGList.List1($1) }
  dis1   { $$ := SourceTextSTRINGList.Cons($1, $2) }

opt_field_reference_0: { val : SourceTextSTRINGList.T }
  dis0 { $$ := $1 }
  dis1 { $$ := NIL }

boolean_constant: { val : ARRAY [0..0] OF source_textTok.ConstTokenCode }
  true { $$ := ARRAY [0..0] OF source_textTok.ConstTokenCode { source_textTok.T_true } }
  false { $$ := ARRAY [0..0] OF source_textTok.ConstTokenCode { source_textTok.T_false } }