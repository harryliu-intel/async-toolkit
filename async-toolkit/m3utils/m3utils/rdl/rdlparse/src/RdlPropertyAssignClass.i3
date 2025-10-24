(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlPropertyAssignClass;
FROM RdlPropertyAssign IMPORT T;
IMPORT RdlExplicitPropertyAssign;
IMPORT RdlPostPropertyAssign;

TYPE
  Explicit = T BRANDED OBJECT
    x : RdlExplicitPropertyAssign.T;
  END;

  Post = T BRANDED OBJECT
    x : RdlPostPropertyAssign.T;
  END;

END RdlPropertyAssignClass.
