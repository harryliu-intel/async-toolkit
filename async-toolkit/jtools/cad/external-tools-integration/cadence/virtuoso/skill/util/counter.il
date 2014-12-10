; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun CounterFillList ( Start Final Increment )
  (let (
        ( List nil )
        ( Curr Start ) )
    ( while ( leqp Curr Final )
      ( setq List ( tconc List Curr ) )
      ( setq Curr ( plus Curr Increment ) ) )
    ( car List ) ) )

(defun CounterCountAndAccumulateResult ( StartIndex 
                                         FinalIndex 
                                         Increment
                                         Func
                                         FuncParams
                                         InitialResult )
  (let (
        ( Curr StartIndex )
        ( CurrResult InitialResult )
        ( GoingUp ( lessp StartIndex FinalIndex ) ) )
    
    (when ( or
            ( and
              GoingUp
              ( leqp Increment 0 ) )
            ( and
              ( not GoingUp )
              ( geqp Increment 0 ) ) )
      (error "CounterCountAndAccumulateResult: Invalid Increment." ) )

    (while ( or
             ( and
               GoingUp
               ( geqp FinalIndex Curr ) )
             ( and
               ( not
                 GoingUp )
               ( leqp FinalIndex Curr ) ) )
      (let ()
        ( setq 
          CurrResult
          ( apply
            Func
            ( cons
              Curr
              ( cons
                CurrResult
                FuncParams ) ) ) )
        ( setq Curr ( plus Curr Increment ) ) ) )
    CurrResult ) )


(defun CounterCountAndAccumulateResults ( StartIndex 
                                          FinalIndex 
                                          Increment
                                          Func
                                          FuncParams )
  ( car
    ( CounterCountAndAccumulateResult
      StartIndex
      FinalIndex
      Increment
      (lambda
        ( CurrCount
          CurrResults
          Func
          FuncParams )
        ( tconc
          CurrResults
          ( apply
            Func
            ( cons
              CurrCount
              FuncParams ) ) ) )
      ( list
        Func
        FuncParams ) 
      nil ) ) )
