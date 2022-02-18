%macro {
D                       [0-9]
L                       [a-zA-Z_]
LB                      \[
RB                      \]
ArrayRange              \[{D}{D}*\:{D}{D}*\]
ArrayIndex              \[{D}{D}*\]
Bare                    {L}(((\[{D}*\])|{D}|{L}|\.|\/)*(({ArrayRange}|{ArrayIndex})*))
SC                      \/\*
EC                      \*+\/
Comment                 {SC}(([^\*]|("*"+[^/\*]))*){EC}
}

LBRACK \{
RBRACK (\}\;)|(\})
DEFINE define
DEFINE_GROUP define_group
TTRUE true
TFALSE false

NUM       (([0-9][0-9]*)|([0-9][0-9]*)\.([0-9][0-9]*))
                            
char      {%char}
skip      ({Comment}|[ \t\n]|\\\n)
IDENT     Bare
bare_IDENT {Bare}
STRING    \"[^\"]*\"

