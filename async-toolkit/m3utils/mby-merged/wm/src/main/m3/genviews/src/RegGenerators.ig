(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE RegGenerators();
IMPORT RegAddrmap, RegRegfile, RegReg;
IMPORT RegGenState;
IMPORT OSError, Thread, Wr;

PROCEDURE GenAddrmap(a : RegAddrmap.T; state : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure };

PROCEDURE GenRegfile(rf : RegRegfile.T; state : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure };

PROCEDURE GenReg(r : RegReg.T; state : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure };

END RegGenerators.
