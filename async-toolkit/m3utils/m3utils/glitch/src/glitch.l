%macro {
D                       [0-9]
L                       [a-zA-Z_]
Bare                    {L}((\[{D}*\])|[$@]|{D}|{L}|\.|\/)*
}

OUTPUT    output
ASYNC     async
GATE      gate
EQ        "="

INT       [0-9]*
char      {%char}
skip      (\#.*|[ \t\n]|("//"[^\n]*"\n"))
IDENT     Bare
bare_IDENT {Bare}
