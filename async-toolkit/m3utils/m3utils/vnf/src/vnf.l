%macro {
D                       [0-9]
L                       [a-zA-Z_]
Bare                    (\\[^ \t\n]*)|({L}((\[{D}*\])|[$@]|{D}|{L}|\.)*)
}

OUTPUT    output
INPUT     input
WIRE      wire
MDL       module
ENDMDL    endmodule

IDENT     Bare
bare_IDENT {Bare}

INT       [0-9]*
DOT       "\."
char      {%char}
skip      (\#.*|[ \t\n]|("//"[^\n]*"\n")|(\/\*([^\*]|\*+[^\/])*\*\/))
