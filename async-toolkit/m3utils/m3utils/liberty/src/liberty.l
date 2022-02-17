%macro {
D                       [0-9]
L                       [a-zA-Z_]
Bare                    {L}((\[{D}*\])|[$@]|{D}|{L}|\.|\/)*
}

DEFINE define
DEFINE_GROUP define_group
TTRUE true
TFALSE false

NUM       [0-9]*
char      {%char}
skip      (\#.*|[ \t\n]|("//"[^\n]*"\n"))
IDENT     Bare
bare_IDENT {Bare}
STRING    "[^\"]*"
