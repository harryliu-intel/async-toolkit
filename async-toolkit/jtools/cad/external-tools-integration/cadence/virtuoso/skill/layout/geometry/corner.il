; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun CornerIsNoEntry ( Corner )
  ( null ( car Corner ) )
)

(defun CornerIsNoExit ( Corner )
  ( null ( caddr Corner ) ) 
)

(defun CornerIsStraightLine ( Corner ) 
  ( or ( and ( eqv ( car ( car Corner ) ) ( car ( cadr Corner ) ) )
	     ( eqv ( car ( car Corner ) ) ( car ( caddr Corner ) ) )
       )
       ( and ( eqv ( cadr ( car Corner ) ) ( cadr ( cadr Corner ) ) )
	     ( eqv ( cadr ( car Corner ) ) ( cadr ( caddr Corner ) ) )
       )              
  )
)

(defun CornerGetEntryLine ( Corner )
  ( list ( car Corner ) ( cadr Corner ) )
)

(defun CornerGetExitLine ( Corner )
  ( cdr Corner )
)

(defun CornerTurnsRelativeLeft ( Corner )
  (if ( LineIsSegmentIncreasing ( CornerGetEntryLine Corner ) )
      (if ( null ( LineIsSegmentIncreasing ( CornerGetExitLine Corner ) ) )
          t )
    (if ( LineIsSegmentIncreasing ( CornerGetExitLine Corner ) )
        t ) ) )

(defun CornerGetCornerPoint ( Corner )
  ( cond (
	  ( CornerIsNoEntry Corner )
	  (  car ( CornerGetExitLine Corner ) )
	 )
	 ( 
	  ( cadr ( CornerGetEntryLine Corner  ) )
	 )
  )	  
)
