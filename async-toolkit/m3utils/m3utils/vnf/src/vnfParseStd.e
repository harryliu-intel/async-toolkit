%source vnf.t vnf.y
%import vnfLexStd vnfParse

%module {
IMPORT Debug;
}

%interface {
}

%public {
}
module:
  module { Debug.Out("module " & $1); }

decl:
  instance  { Debug.Out("instance " & $2); }
  wire  { Debug.Out("wire " & $2); }
  input  { Debug.Out("input " & $2); }
  output  { Debug.Out("output " & $2); }