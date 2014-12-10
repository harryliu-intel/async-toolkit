; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

ReadLineCache = ( makeTable `readline nil )
(defun StringUtilReadLine ( CID TimeOut )
  (let (
        Ret
        Accum
        Bit )
    
    (while (let ()
             ( setq Bit ( ipcReadProcess CID TimeOut ) )
             (when ( not ( equal "\n" Bit ) )
               ( setq Accum Bit ) )
             ( equal Bit "\n" )
             ) t )
    ReadLineCache[CID] = ( append 
                           ReadLineCache[CID]
                           (cond ( 
                                  ( stringp Accum )
                                  ( parseString Accum "\n" ) ) )
                           )
    Ret = ( car ReadLineCache[CID] )
    ReadLineCache[CID] = ( cdr ReadLineCache[CID] )
      Ret
    ) )
    

;Remove an optional new line character from the end of a string.
(defun StringUtilChompNewline ( String )
  (let (
        ( StringLen ( strlen String ) ) )
    (if ( equal StringLen 0 )
        ""
      (let (
            ( LastChar ( substring String StringLen ) ) )
        (if ( equal LastChar "\n" )
            (when ( greaterp StringLen 1 )
              ( substring String 1 ( difference StringLen 1 ) ) )
          String  ) ) ) ) )

(defun StringUtilStripWhiteSpace ( String )
  (let (
        ( Result "" )
        ( LeftIndex 1 )
        ( GotNonSpace nil )
        ( Len ( strlen String ) ) )
    (while ( not 
             ( or
               GotNonSpace
               ( lessp Len LeftIndex ) ) )
      (let (
            ( CurrChar ( substring String LeftIndex 1 ) ) )
        (if ( StringUtilIsWhiteSpace CurrChar )
            ( setq LeftIndex ( plus LeftIndex 1 ) )
          ( setq GotNonSpace t ) ) ) )
    (let (
          ( RightIndex Len ) )
      ( setq GotNonSpace nil )
      (while ( not
               ( or
                 GotNonSpace
                 ( lessp RightIndex LeftIndex ) ) )
        (let (
              ( CurrChar ( substring String RightIndex 1 ) ) )
          (if ( StringUtilIsWhiteSpace CurrChar )
              ( setq RightIndex ( difference RightIndex 1 ) )
            ( setq GotNonSpace t ) ) ) )
      (if ( greaterp
            ( difference
              ( plus RightIndex 1 )
              LeftIndex )
            0 )
          ( substring
            String
            LeftIndex
            ( difference
              ( plus RightIndex 1 )
              LeftIndex ) )
        "" ) ) ) )
            


(defun StringUtilIsWhiteSpace ( CharStr )
  ( or
    ( equal
      CharStr
      " " )
    ( equal
      CharStr
      "\n" )
    ( equal
      CharStr
      "\t" ) ) )


(defun StringUtilIsCharDigit ( CharStr )
  ( and
    ( geqp ( strcmp CharStr "0" ) 0 )
    ( leqp ( strcmp CharStr "9" ) 0 ) ) )

(defun StringUtilIsCharLetter ( CharStr )
  ( or
    ( and
      ( geqp ( strcmp CharStr "A" ) 0 )
      ( leqp ( strcmp CharStr "Z" ) 0 ) )
    ( and
      ( geqp ( strcmp CharStr "a" ) 0 )
      ( leqp ( strcmp CharStr "z" ) 0 ) ) ) )

(defun StringUtilGetDigitValue ( CharStr )
  (cond
   ( ( equal CharStr "0" ) 0 )
   ( ( equal CharStr "1" ) 1 )
   ( ( equal CharStr "2" ) 2 )
   ( ( equal CharStr "3" ) 3 )
   ( ( equal CharStr "4" ) 4 )
   ( ( equal CharStr "5" ) 5 )
   ( ( equal CharStr "6" ) 6 )
   ( ( equal CharStr "7" ) 7 )
   ( ( equal CharStr "8" ) 8 )
   ( ( equal CharStr "9" ) 9 )
   ( t nil ) ) )


(defun StringUtilInnerParseNumber ( String )
  (let (
        ( CurrValue 0 )
        ( CurrOffset 1 )
        ( CurrDigitMult 1 )
        ( GotNonDigit nil )
        ( Sign 1 )
        ( FirstChar ( substring String 1 1 ) )
        ( Len ( strlen String ) ) )

    (when ( and
            FirstChar
            ( equal FirstChar "-" ) )
      ( setq Sign -1 )
      ( setq CurrOffset ( plus CurrOffset 1 ) ) )

    (when ( and
            FirstChar
            ( equal FirstChar "+" ) )
      ( setq Sign 1 )
      ( setq CurrOffset ( plus CurrOffset 1 ) ) )

    (while ( and 
             ( not ( lessp Len CurrOffset ) ) 
             ( not GotNonDigit ) 
             CurrValue )
      (let (
            ( CurrChar ( substring String CurrOffset 1 ) ) )
        ( setq CurrOffset ( plus CurrOffset 1 ) )
        (if ( StringUtilIsCharDigit CurrChar )
            (let ()
              ( setq 
                CurrValue
                ( plus
                  (if ( equal CurrDigitMult 1 )
                      ( times
                        CurrValue
                        10 )
                    CurrValue )
                  ( times
                    ( float CurrDigitMult )
                    ( float ( StringUtilGetDigitValue CurrChar ) ) ) ) )
              (unless ( equal CurrDigitMult 1 )
                ( setq CurrDigitMult ( quotient ( float CurrDigitMult ) ( float 10 ) ) ) ) )
          (if ( equal CurrChar "." )
              ( setq CurrDigitMult ( quotient ( float CurrDigitMult ) ( float 10 ) ) )
            ( setq GotNonDigit t ) ) ) ) )
    ( list
      ( times CurrValue Sign )
      (if ( greaterp CurrOffset 1 )
          (if ( and
                ( not GotNonDigit )
                ( lessp Len CurrOffset ) )
              "" 
              ( substring
                String
                ( difference CurrOffset 1 )
                Len ) )
        String ) ) ) )

(defun StringUtilParseNumber ( String )
  ( car ( StringUtilInnerParseNumber ( StringUtilStripWhiteSpace String ) ) ) )

(defun StringUtilParseLengthString ( String )
  (let (
        ( ParseResult ( StringUtilInnerParseNumber String ) ) )
    (let (
          ( Num ( car ParseResult ) )
          ( Rest ( StringUtilStripWhiteSpace ( cadr ParseResult ) ) ) )
      (let (
            ( FirstCharOfRest ( substring Rest 1 1 ) ) )
        (cond
         (
          ( or ( equal FirstCharOfRest "n" ) ( equal FirstCharOfRest "m" ) )
          ( quotient ( float Num ) ( float 1000000000 ) ) )
         (
          ( equal FirstCharOfRest "u" )
          ( quotient ( float Num ) ( float 1000000 ) ) )
         (
          ( or
            ( equal FirstCharOfRest "e" )
            ( equal FirstCharOfRest "E" ) )
          (let (
                ( Exponent ( StringUtilParseNumber ( substring Rest 2 ( strlen Rest ) ) ) ) )
            ( times
              Num
              ( exp
                ( times Exponent ( log 10.0 ) ) ) ) ) )
         (
          t
          Num ) ) ) ) ) )

(defun StringUtilGetFirstChar ( String )
  ( substring String 1 1 ) )

(defun StringUtilGetAllButFirstChar ( String )
  ( substring String 2 ( difference ( strlen String ) 1 ) ) )


(defun StringUtilConvertStringListToSpaceSeperatedString ( StringList )
  ( buildString StringList " " ) )

(defun StringUtilPrefix ( Prefix StringList )
  ( strcat Prefix ( buildString StringList ( strcat " " Prefix ) ) ) )

(defun StringStartsWith ( String Prefix )
  ( strncmp String Prefix ( strlen Prefix ) ) == 0 )
