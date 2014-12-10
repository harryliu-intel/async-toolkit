; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun MathRoundToNearest ( Value Step )
  ( times Step ( round ( quotient Value Step ) ) ) )
  
