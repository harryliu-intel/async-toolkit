%source vnf.t vnf.y
%import vnfLexStd vnfParse

%interface {
IMPORT VnfBundleType;
IMPORT VnfBundle;
IMPORT VnfDecl;
IMPORT VnfModuleList;
IMPORT VnfModule;
}

%module {
IMPORT Debug;
IMPORT VnfBundleType;
IMPORT VnfBundle;
IMPORT VnfDecl;
IMPORT VnfInstance;
IMPORT VnfModuleList;
}

%public {
  tree : VnfModuleList.T := NIL;
}

cmdList:
  cons     { self.tree := VnfModuleList.Cons($2, self.tree); }

module: { val : VnfModule.T }
  module {
    Debug.Out("returning module " & $1);
  }

decl: { val : VnfDecl.T }
  instance  {
    Debug.Out("instance " & $2);
    $$ := NEW(VnfDecl.Instance,
              i := NEW(VnfInstance.T, name := $2));
  }
  wire    {
    Debug.Out("wire " & $2);
    $$ := NEW(VnfDecl.Bundle,
              b := VnfBundle.T { nm   := $2,
                                 type := $1,
                                 usage := VnfBundle.Usage.Wire })
  }        
  input   {
    Debug.Out("input " & $2);
    $$ := NEW(VnfDecl.Bundle,
              b := VnfBundle.T { nm   := $2,
                                 type := $1,
                                 usage := VnfBundle.Usage.Input })
  }
  output  {
    Debug.Out("output " & $2);
    $$ := NEW(VnfDecl.Bundle,
              b := VnfBundle.T { nm   := $2,
                                 type := $1,
                                 usage := VnfBundle.Usage.Output })

  }

opt_arrayspec: { val : VnfBundleType.T }
  empty        { $$ := VnfBundleType.Empty }
  range        { $$ := $1 }

arrayrange: { val : VnfBundleType.T }
  range        { $$ := VnfBundleType.T { $1, $2 } }