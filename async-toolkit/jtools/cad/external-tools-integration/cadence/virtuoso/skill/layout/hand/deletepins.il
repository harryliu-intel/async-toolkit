; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun DeletePinsDeletePinFromShape ( ShapeObj )
  (let (
        ( PinObj ( getq ShapeObj pin ) ) )
    (when PinObj
      (let (
            ( TerminalObj ( getq PinObj term ) ) )
        (let (
              ( TerminalName ( getq TerminalObj name ) ) )
          ( dbDeleteObject PinObj )
          (unless ( getq TerminalObj pins )
            ( dbDeleteObject TerminalObj ) ) ) ) ) ) )
