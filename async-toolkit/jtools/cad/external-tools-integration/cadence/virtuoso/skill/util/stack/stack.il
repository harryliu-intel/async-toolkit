; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun StackCreateNew ( Stack )
  ( putprop Stack ( list ) `value ) )

(defun StackPush ( Stack Element )
  ( putprop Stack ( cons Element ( get Stack `value ) ) `value ) )

(defun StackPop ( Stack )
  (let (
        ( Element ( car ( symeval Stack ) ) ) )
    ( putprop Stack ( cdr ( get Stack `value ) ) `value  )
    Element ) )
