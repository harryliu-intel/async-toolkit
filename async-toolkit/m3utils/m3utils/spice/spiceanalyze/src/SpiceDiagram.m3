(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SpiceDiagram;
IMPORT SpiceGateSeq;
IMPORT SpiceGate;
IMPORT Canvas;
IMPORT PicArray;
IMPORT PicComponent;
IMPORT PicPoint;
IMPORT PicExtent;
IMPORT Debug;

CONST doDebug = TRUE;

REVEAL
  T = Public BRANDED Brand OBJECT
    gates : SpiceGateSeq.T;
  OVERRIDES
    init    := Init;
    addGate := AddGate;
    render  := Render;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.gates := NEW(SpiceGateSeq.T).init();
    RETURN t
  END Init;

PROCEDURE AddGate(t : T; gate : SpiceGate.T) =
  BEGIN
    t.gates.addhi(gate)
  END AddGate;

PROCEDURE Render(t : T; to : Canvas.T) =
  CONST
    InterGateGap = 20.0d0;
  VAR
    offset := 0.0d0;
  BEGIN
    (* we will just render the gates left to right to begin with *)
    FOR i := 0 TO t.gates.size() - 1 DO
      offset := RenderGate(to, t.gates.get(i), offset) + InterGateGap
    END
  END Render;

PROCEDURE RenderGate(canvas : Canvas.T;
                     g      : SpiceGate.T;
                     offset : LONGREAL) : LONGREAL =
  (* returns urx of just-rendered gate *)
  VAR
    array := NEW(PicArray.T).init();
    j := 0;
  BEGIN

    (* we start from the bottom (power supply) of the N stack *)
    WITH nstack = g[SpiceGate.Pull.Down] DO
      FOR nr := nstack.nrows() - 1 TO 0 BY -1 DO
        WITH row = nstack.getRow(nr) DO
          FOR i := 0 TO row.size() - 1 DO
            array.put(i, j, NEW(PicComponent.T).init(row.get(i)))
          END
        END;
        INC(j)
      END
    END;

    (* we start from the bottom (output) of the P stack *)
    WITH pstack = g[SpiceGate.Pull.Up] DO
      FOR pr := 0 TO pstack.nrows() - 1 DO
        WITH row = pstack.getRow(pr) DO
          FOR i := 0 TO row.size() - 1 DO
            array.put(i, j, NEW(PicComponent.T).init(row.get(i)))
          END
        END;
        INC(j)
      END
    END;

    WITH extent = array.minExtent() DO
      IF doDebug THEN
        Debug.Out("SpiceDiagram.RenderGate: extent=" & PicExtent.Format(extent))
      END;
      array.setExtent(extent);
      array.render(PicPoint.T { offset, 0.0d0 }, canvas);
      RETURN extent.ur.x - extent.ll.x
    END
  END RenderGate;
  
BEGIN END SpiceDiagram.
