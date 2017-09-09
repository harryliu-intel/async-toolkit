%source rdl.t rdl.y
%import rdlLexExt rdlParse

%module {
IMPORT rdlLexExt;
IMPORT Debug;
VAR doDebug := Debug.DebugThis("rdlParseExt");
}

property_definition:
  initial               {
    IF doDebug THEN
      Debug.Out("registering user defined property " & $1)
    END;
    rdlLexExt.RegisterUserdefProperty($1)
}         