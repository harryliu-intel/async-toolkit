; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun SearchFindSmallestStep ( SearchTable
                                SmallerTestFunc
                                SmallerTestFuncArgs )
  (cond (
         ( equal ( arrayref SearchTable "type" ) "binary" )
         ( BinarySearchFindSmallestStep
           SearchTable
           SmallerTestFunc
           SmallerTestFuncArgs ) )
        (
         ( equal ( arrayref SearchTable "type" ) "linear" )
          ( LinearSearchFindSmallestStep
           SearchTable
           SmallerTestFunc
           SmallerTestFuncArgs ) ) ) )

(defun LinearSearchFindSmallestStep ( LinearSearchTable
                                      SmallerTestFunc
                                      SmallerTestFuncArgs )
  (let (
        ( Min ( arrayref LinearSearchTable "min" ) )
        ( Max ( arrayref LinearSearchTable "max" ) )
        ( Trial ( arrayref LinearSearchTable "trial" ) ) )
    ( printf "%d %d %d\n" Min Max Trial )
    (cond ( 
           ;should we go smaller
           ( apply SmallerTestFunc ( cons Trial SmallerTestFuncArgs ) )
           ( setarray LinearSearchTable "max" Trial )
           ( setarray LinearSearchTable "trial" ( difference Trial 1 ) )
           )
          (
           ( setarray LinearSearchTable "trial" ( plus Trial 1 ) )
           ( setarray LinearSearchTable "min" ( plus Trial 1 ) ) ) )
    (let (
          ( Min ( arrayref LinearSearchTable "min" ) )
          ( Max ( arrayref LinearSearchTable "max" ) ) )
      ;if done
      (cond (
             ( geqp Min Max )
             ( setarray LinearSearchTable "trial" Min )
             ( setarray LinearSearchTable "done" t ) ) )
      ( arrayref LinearSearchTable "done" ) ) ) )

(defun LinearSearchInitTable ( LinearSearchTable 
                               Min 
                               Max
                               Trial )
  ( setarray LinearSearchTable "min" Min )
  ( setarray LinearSearchTable "max" Max )
  ( setarray LinearSearchTable "trial" Trial )
  ( setarray LinearSearchTable "type" "linear" )
  (if ( geqp Min Max )
      ( setarray LinearSearchTable "done" t )
    ( setarray LinearSearchTable "done" nil ) ) )
