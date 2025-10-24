(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

MODULE Request;
IMPORT TextTable, HTML, HTMLForm, HTMLInput;

REVEAL
  T = Public BRANDED Brand OBJECT
    envVars, postVars, getVars : TextTable.T;
  OVERRIDES
    init := Init;
    getEnvVar := GetEnvVar;
    getPostVar := GetPostVar;
    getGetVar := GetGetVar;
    getEnvVars := GetEnvVars;
    getPostVars := GetPostVars;
    getGetVars := GetGetVars;
    addPostVarsAsHidden := AddPostVarsAsHidden;
  END;

PROCEDURE Init(t : T; envVars, postVars, getVars : TextTable.T) : T =
  BEGIN
    t.envVars := envVars;
    t.postVars := postVars;
    t.getVars := getVars;
    RETURN t
  END Init;

PROCEDURE GetEnvVar(t : T; named : TEXT; VAR value : TEXT) : BOOLEAN =
  BEGIN RETURN t.envVars.get(named,value) END GetEnvVar;

PROCEDURE GetPostVar(t : T; named : TEXT; VAR value : TEXT) : BOOLEAN =
  BEGIN RETURN t.postVars.get(named,value) END GetPostVar;

PROCEDURE GetGetVar(t : T; named : TEXT; VAR value : TEXT) : BOOLEAN =
  BEGIN RETURN t.getVars.get(named,value) END GetGetVar;

PROCEDURE GetEnvVars(t : T) : TextTable.T =
  BEGIN RETURN t.envVars END GetEnvVars;

PROCEDURE GetPostVars(t : T) : TextTable.T =
  BEGIN RETURN t.postVars END GetPostVars;

PROCEDURE GetGetVars(t : T) : TextTable.T =
  BEGIN RETURN t.getVars END GetGetVars;

PROCEDURE AddPostVarsAsHidden(t : T; to : HTML.T) =
  VAR
    nam, val : TEXT;
  BEGIN
    WITH iter = t.postVars.iterate() DO
      WHILE iter.next(nam,val) DO
        WITH input = NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                           name := nam,
                                           value := val) DO
          NARROW(to,HTMLForm.T).add(input)
        END
      END
    END
  END AddPostVarsAsHidden;

BEGIN END Request.
