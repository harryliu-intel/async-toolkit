; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun EscapeStringIsAlpha ( CharStr )
  ( or
    ( and ( geqp ( strcmp CharStr "a" ) 0 ) ( leqp ( strcmp CharStr "z"  ) 0 ) )
    ( and ( geqp ( strcmp CharStr "A" ) 0 ) ( leqp ( strcmp CharStr "Z" ) 0 ) ) ) )

(defun EscapeStringIsDigit ( CharStr )
  ( and ( geqp ( strcmp CurrChar "0" ) 0 ) ( leqp ( strcmp CurrChar "9" ) 0 ) ) )

(defun EscapeStringIsAlphaNum ( CharStr )
  ( or ( EscapeStringIsAlpha CharStr )
       ( EscapeStringIsDigit CharStr ) ) )

(defun EscapeStringEscapeChar ( CharStr )
  (cond
   (
    ( EscapeStringIsAlphaNum CharStr )
    CharStr )
   (
    ( equal "." CharStr )
    "_D_" )
   (
    ( equal "," CharStr )
    "_C_" )
   (
    ( equal "[" CharStr )
    "_l_" )
   (
    ( equal "]" CharStr )
    "_r_" )  
   (
    ( equal "(" CharStr )
    "_L_" )
   (
    ( equal ")" CharStr )
    "_R_" ) 
   (
    ( equal "-" CharStr )
    "_M_" )
   (
    ( equal "_" CharStr )
    "_U_" )
   (
    t
    ( sprintf nil "_%02x_" ( charToInt ( stringToSymbol CharStr ) ) )
    )
   ) )
   
   
(defun EscapeStringImpl ( String EscapeStringEscapeCharFunc )
  (let (
        ( Len ( strlen String ) )
        ( CurrIndex 1 ) 
        ( Result "" ) )
    
    (while ( leqp CurrIndex Len )
      (let (
            ( CurrChar ( substring String CurrIndex 1 ) ) )

        ( setq Result ( strcat Result ( apply EscapeStringEscapeCharFunc ( list CurrChar ) ) ) )
        ( setq CurrIndex ( plus CurrIndex 1 ) ) ) )
    Result ) )



(defun EscapeStringEscapeRex ( String )
  ( EscapeStringImpl String 
                     ( lambda ( Char ) 
                       (cond (
                              ( equal Char "[" )
                              "\\["
                              )
                             (
                              ( equal Char "]" )
                              "\\]"
                              )
                             ( 
                              ( equal Char "*" )
                              "\\*"
                              )
                             (
                              ( equal Char "+" )
                                "\\+"
                                )
                             (
                              ( equal Char "." )
                              "\\."
                              )
                             (
                              Char 
                             ) ) ) ) )


(defun EscapeString ( String )
  ( EscapeStringImpl String `EscapeStringEscapeChar ) )


(defun EscapeQuotes ( String )
  ( EscapeStringImpl 
    String
    (lambda ( Char )
      (cond (
             ( equal Char "\\" )
             "\\\\"            
             )
            (
             ( equal Char "\"" )
             "\\\""
             )
            (
             Char ) ) ) ) )
