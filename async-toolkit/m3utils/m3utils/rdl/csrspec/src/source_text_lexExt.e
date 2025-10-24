%source source_text.t source_text.l
%import source_textTok source_textLex

IDENTIFIER: { val : TEXT }
IDENTIFIER { RETURN NEW(IDENTIFIER, val := $) }

NUM: { val : TEXT }
NUM { RETURN NEW(NUM, val := $) }

STRING: { val : TEXT }
STRING { RETURN NEW(STRING, val := $) }
