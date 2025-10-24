(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PrsImpl;
IMPORT TextSet;
IMPORT FlatRule;
IMPORT PrsNode;
IMPORT FlatRuleSet;
IMPORT Dsim;
IMPORT Name;
IMPORT NameSet;

REVEAL
  PrsNode.T = BRANDED PrsNode.Brand OBJECT
    id      : CARDINAL;
    aliases : NameSet.T;
    fanins  : FlatRuleSet.T;
    fanouts : FlatRuleSet.T;
  END;

  FlatRule.T = BRANDED FlatRule.Brand OBJECT
    id       : CARDINAL;
    foNm     : Name.T;  (* name of fanout within parent cell *)
    rule     : Dsim.Rule;
    inType   : Dsim.Define;
    parent   : Name.T;  (* hierarchical name of parent cell *)
  END;

END PrsImpl.
