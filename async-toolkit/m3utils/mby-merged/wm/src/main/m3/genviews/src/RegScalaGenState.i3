(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegScalaGenState;
IMPORT RegGenState;
IMPORT Wx;

TYPE
  Section = { Import, Components, Maintype, Trailer };

  Wxs = ARRAY Section OF Wx.T;

TYPE
  T = RegGenState.T OBJECT
    wx       : Wxs;
  METHODS
    put(sec : Section; txt : TEXT);
  END;

CONST Brand = "RegScalaGenState";

END RegScalaGenState.
 

