%macro {
D                       [0-9]
L                       [a-zA-Z_]
C                       [0-9:]
Bare                    (\\[^ \t\n]*)|({L}((\[{C}*\])|[$@]|{D}|{L}|\.)*)
}

OUTPUT    output
INPUT     input
WIRE      wire
MDL       module
ENDMDL    endmodule
ASSIGN    assign

IDENT     Bare
bare_IDENT {Bare}

INT       \-?[0-9]*
DOT       "\."
char      {%char}
skip      (\#.*|[ \t\n]|("//"[^\n]*"\n")|(\/\*([^\*]|\*+[^\/])*\*\/))


