%macro {
D                       [0-9]
L                       [a-zA-Z_]
X                       [eE]
S                       [\+\-]
}

T_GE		">="
T_LE		"<="
T_EQ		"="
T_NEQ		"#"
T_LT		"<"
T_GT		">"
T_LOR        	"OR"
T_LAND		"AND"
T_DIV        "DIV"
T_MOD        "MOD"
T_LITERAL    {L}({D}|{L}|\.)*
T_FLOAT      {D}{D}*|{D}{D}*\.{D}*|{D}*\.{D}{D}*|{D}*\.{D}{X}{D}{D}*|{D}{D}*{X}{D}{D}*|{D}*\.{D}*{X}{S}{D}{D}*|{D}{D}*{X}{S}{D}{D}*

char       {%char}
skip       ([ \t\n]|("//"[^\n]*"\n"))*
ERROR      [^]

