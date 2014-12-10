; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun BinarySearchFindSmallestStep ( BinarySearchTable 
                                      SmallerTestFunc
                                      SmallerTestFuncArgs
                                      )
  (let (
        ( Min ( arrayref BinarySearchTable "min" ) )
        ( Max ( arrayref BinarySearchTable "max" ) )
        ( Trial ( arrayref BinarySearchTable "trial" ) ) )
    (let (
          ( Success
            ( apply SmallerTestFunc ( cons Trial SmallerTestFuncArgs ) ) ) )
    ( printf "%d %d %d\n" Min Max Trial )
    (cond ( 
           ;should we go smaller
           Success
           (cond (
                  ( equal Min Trial )
                  ( setarray BinarySearchTable "max" Min ) )
                 (
                  t
                  ( setarray BinarySearchTable "max" Trial ) ) ) )
          (
           ;or bigger
           t
           ( setarray BinarySearchTable "min" ( plus Trial 1 ) ) ) )
    (cond (
           ( and 
             ( not Success )
             ( equal ( arrayref BinarySearchTable "iters" ) 0 ) )
           ( setarray BinarySearchTable "trial" Max ) )
          (
           t
           ( setarray BinarySearchTable "trial"
                      ( quotient 
                        ( plus ( arrayref BinarySearchTable "min" )
                               ( arrayref BinarySearchTable "max" ) ) 2 ) ) ) )
    ( setarray 
      BinarySearchTable "iters" 
      ( plus
        ( arrayref BinarySearchTable "iters" )
        1 ) )
    (cond (
           ( geqp ( arrayref BinarySearchTable "min" ) 
                   ( arrayref BinarySearchTable "max" ) )
           ( setarray BinarySearchTable "done" t )
           t )
          ( 
           t
           ( setarray BinarySearchTable "done" nil )
           nil ) ) ) ) )

(defun BinarySearchFindGreatest ( Min 
                                  Max
                                  GreaterTestFunc
                                  GreaterTestFuncArgs
                                  )
  (let ( 
        ( Ret
          ( BinarySearchFindSmallest
            Min
            Max
            ( lambda ( Trial ) 
              ( not ( apply GreaterTestFunc ( cons Trial GreaterTestFuncArgs ) ) ) )
            nil ) ) )
    (cond ( 
           ( equal Ret Min )
           Min )
          (
           ( equal Ret Max )
           Max )
          (
           t
           ( difference Ret 1 ) ) ) ) )
  
  

              


(defun BinarySearchMakeTable ( Min 
                               Max
                               Trial 
                               @key
                               ( MaxTested nil ) )
  (let (
        ( BinarySearchTable ( makeTable `binarysearch nil ) ) )
    ( BinarySearchInitTable BinarySearchTable Min Max Trial )
    (when MaxTested
      ( setarray BinarySearchTable "iters" 1 ) )
    BinarySearchTable ) )

(defun BinarySearchInitTable ( BinarySearchTable Min Max Trial )
  ( setarray BinarySearchTable "min" Min )
  ( setarray BinarySearchTable "max" Max )
  ( setarray BinarySearchTable "trial" Trial )
  ( setarray BinarySearchTable "iters" 0 )
  ( setarray BinarySearchTable "type" "binary" )
  (if ( geqp Min Max )
      ( setarray BinarySearchTable "done" t )
    ( setarray BinarySearchTable "done" nil ) ) )


(defun BinarySearchFindSmallest ( Min 
                                  Max
                                  SmallerTestFunc
                                  SmallerTestFuncArgs
                                  )
  ( BinarySearchFindSmallestWithGuess 
    Min
    Max
    ( quotient ( plus Min Max ) 2 )
    SmallerTestFunc
    SmallerTestFuncArgs ) )

(defun BinarySearchFindSmallestWithGuess ( Min 
                                           Max
                                           Trial
                                           SmallerTestFunc
                                           SmallerTestFuncArgs
                                           )
  (cond (
         ( lessp Max Min )
         Min )
        (
         (let (
               ( BinarySearchTable ( BinarySearchMakeTable Min Max Trial ) ) )
           (while ( not ( BinarySearchFindSmallestStep
                          BinarySearchTable
                          SmallerTestFunc
                          SmallerTestFuncArgs ) )
             t )
           ( arrayref BinarySearchTable "trial" ) ) ) ) )
        
