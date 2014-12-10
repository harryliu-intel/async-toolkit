; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


/*DOC

A ComponentInfo is a duplication of the instance database object, with some extras for dealing with components like contact sharing proeprties and special parameters like strength.  The advantage over using instances is that we don't have to worry about dirtying any databases, and it is more efficient.<br>

This module has get,set functions, functions for converting between instances and ComponentInfos, and functions for dealing with connectivity/parameters.<br>

Meaning of some properties:<br>
<dl>
<dt>TopCSInfo<dd>A hierProp that contains the contact sharing properties of of the top part of the component.
<dt>BottomCSInfo<dd>A hierProp that contains the contact sharing properties of of the bottom part of the component.
<dt>Parity<dd>The orientation about the x axis of the component.  nil for R0; t for MX
<dt>HParity<dd>The orientation about the y axis of the component.  nil for R0; t for MY
<dt>Location<dd>Location of the component.
<dt>BBox<dd>Bounding box of the component.
<dt>Height<dd>Height of bounding box.
<dt>Widdh<dd>Widdh of bounding box.
<dt>Multiplicity<dd>The number of transistors to implement a component.
<dt>Strength<dd>The strength of the transistors in a component, corresponding to a transistor width parameter.  Gates have an n and a p strength with a parameter name like NW2,PW4, etc, where the number represents the multiplicity.  Stacks have either an n strength or a p strength with the same naming convention.

</dl>
*/


(defun ComponentInfoCreateConnTable ( Component )
  "Create a connection table for an instance.  Maps component's terminal name to external net name."
  (let (
        ( ConnTable ( makeTable `bla nil ) ) )
    ( foreach InstTerm ( getq Component conns )
              (let ( 
                    ( Term ( getq ( getq InstTerm term ) name ) )
                    ( Net  ( getq ( getq InstTerm net ) name ) ) )
                (when ( and Term Net )
                  ( setarray ConnTable Term Net ) ) ) )
    ConnTable ) )

(defun ComponentInfoCreateComponentInfoFromComponent ( Component )
  "Creates a ComponentInfo from an instance of a component."
  (let (
        ( LibCellView
          ( list ( getq Component libName )
                 ( getq Component cellName )
                 ( getq Component viewName ) ) )
        ( ParameterList ( copy ( PCellGetParameterList Component ) ) )
        ( ConnTable ( ComponentInfoCreateConnTable Component ) ) )
    (let (
          ( BottomCSInfo
            ( PropGetPropValueForPropName
              ( getq ( getq Component master ) prop )
              "csBottom" ) )
          ( TopCSInfo
            ( PropGetPropValueForPropName
              ( getq ( getq Component master ) prop )
              "csTop" ) ) )
      (let (
            ( BottomTerminalNames
              ( list
                ( PropGetPropValueForPropName
                  BottomCSInfo
                  "NTerminal" )
                ( PropGetPropValueForPropName
                  BottomCSInfo
                  "PTerminal" ) ) )
            ( TopTerminalNames
              ( list
                ( PropGetPropValueForPropName
                  TopCSInfo
                  "NTerminal" )
                ( PropGetPropValueForPropName
                  TopCSInfo
                  "PTerminal" ) ) ) )
        (let (
              ( Ret
                ( ComponentInfoCreateComponentInfo 
                  LibCellView
                  ( getq Component name )
                  ParameterList
                  ( mapcar
                    (lambda ( TerminalName )
                      ( arrayref ConnTable TerminalName ) )
                    BottomTerminalNames )
                  ( mapcar
                    (lambda ( TerminalName )
                      ( arrayref ConnTable TerminalName ) )
                    TopTerminalNames )
                  ConnTable
                  ( RectGetHeight ( getq Component bBox ) )
                  ( RectGetWidth ( getq Component bBox ) )
                  ( getq Component xy )
                  ( getq Component bBox )
                  (if  ( or
                         ( equal ( getq Component orient ) "R0" )
                         ( equal ( getq Component orient ) "MX" ) )
                      nil t )
                  ( PropGetPropValueForPropName 
                    ( getq ( getq Component master ) prop )
                    "csBottom" )
                  ( PropGetPropValueForPropName 
                    ( getq ( getq Component master ) prop )
                    "csTop" ) ) ) )

          ( DominoSetParity Ret (if ( or
                                      ( equal ( getq Component orient ) "R0" )
                                      ( equal ( getq Component orient ) "MY" ) )
                                    nil t ) )
          
          Ret     

 ) ) ) ) )
  
(defun ComponentInfoCreateComponentInfo ( LibCellView      
                                          Name
                                          Params
                                          BottomNetNames
                                          TopNetNames
                                          Conns
                                          Height
                                          Width
                                          Position
                                          BBox
                                          HParity
                                          BottomCSInfo
                                          TopCSInfo
                                          )
  "Constructs a ComponentInfo"
  (let (
        ( ComponentInfo ( makeTable `chaininfo ) ) )
    ( setarray ComponentInfo `libcellview LibCellView )   
    ( setarray ComponentInfo `params Params )
    ( setarray ComponentInfo `conns Conns )
    ( setarray ComponentInfo `height Height )
    ( setarray ComponentInfo `width Width )
    ( setarray ComponentInfo `position Position )
    ( setarray ComponentInfo `bBox BBox  )
    ( setarray ComponentInfo `hparity HParity )
    ( setarray ComponentInfo `bottom_csinfo BottomCSInfo )
    ( setarray ComponentInfo `top_csinfo TopCSInfo )

    ( DominoInit
      ComponentInfo
      Name
      BottomNetNames
      TopNetNames )
  ComponentInfo ) )

(defun ComponentInfoCopy ( ComponentInfo )
  ( TableCopy ComponentInfo ) )

(defun ComponentInfoGetLibCellView ( ComponentInfo )
  ( arrayref ComponentInfo `libcellview ) )

(defun ComponentInfoGetName ( ComponentInfo )
  ( arrayref ComponentInfo `name ) )

(defun ComponentInfoGetParameterList ( ComponentInfo ) 
  ( arrayref ComponentInfo `params ) )

(defun ComponentInfoGetParity ( ComponentInfo )
    ( arrayref ComponentInfo `parity ) )

(defun ComponentInfoGetConnTable ( ComponentInfo ) 
  ( arrayref ComponentInfo `conns ) )

(defun ComponentInfoGetHeight ( ComponentInfo )
  ( arrayref ComponentInfo `height ) )

(defun ComponentInfoGetWidth ( ComponentInfo )
  ( arrayref ComponentInfo `width ) )

(defun ComponentInfoGetPosition ( ComponentInfo )
  ( arrayref ComponentInfo `position ) )

(defun ComponentInfoGetBBox ( ComponentInfo )
  ( arrayref ComponentInfo `bBox ) )

(defun ComponentInfoGetHParity ( ComponentInfo )
  ( arrayref ComponentInfo `hparity ) )

(defun ComponentInfoGetBottomCSInfo ( ComponentInfo )
  (if ( ComponentInfoGetParity ComponentInfo )
      ( arrayref ComponentInfo `top_csinfo )
    ( arrayref ComponentInfo `bottom_csinfo ) ) )

(defun ComponentInfoGetTopCSInfo ( ComponentInfo )
  (if ( ComponentInfoGetParity ComponentInfo )
      ( arrayref ComponentInfo `bottom_csinfo )
    ( arrayref ComponentInfo `top_csinfo ) ) )


(defun ComponentInfoGetNStrength ( ComponentInfo )
  ( nth 2
        ( ComponentInfoGetNWidthParameter
          ComponentInfo ) ) )

(defun ComponentInfoGetPStrength ( ComponentInfo )
  ( nth 2
        ( ComponentInfoGetPWidthParameter
          ComponentInfo ) ) )

(defun ComponentInfoGetNStrengthFromComponent ( Component )
  ( nth 2
        ( ComponentInfoGetNWidthParameterFromParameterList 
          ( PCellGetParameterList Component ) ) ) )

(defun ComponentInfoGetPStrengthFromComponent ( Component )
  ( nth 2
        ( ComponentInfoGetPWidthParameterFromParameterList 
          ( PCellGetParameterList Component ) ) ) )

(defun ComponentInfoGetNMultiplicityFromComponent ( Component )
  (let (
        ( ParamName 
          ( car
            ( ComponentInfoGetNWidthParameterFromParameterList 
              ( PCellGetParameterList Component ) ) ) ) )
    (cond (
           ParamName
           ( rexMatchp "NW\\([0-9]*\\)" ParamName )
           ( atoi ( rexSubstitute "\\1" ) ) )
          (
           0 ) ) ) )
        
(defun ComponentInfoGetPMultiplicityFromComponent ( Component )
  (let (
        ( ParamName 
          ( car
            ( ComponentInfoGetPWidthParameterFromParameterList 
              ( PCellGetParameterList Component ) ) ) ) )
    (cond (
           ParamName
           ( rexMatchp "PW\\([0-9]*\\)" ParamName )
           ( atoi ( rexSubstitute "\\1" ) ) )
          (
           0 ) ) ) )

(defun ComponentInfoGetNLengthFromComponent ( Component )
  ( nth 2
        ( ComponentInfoGetNLengthParameterFromParameterList 
          ( PCellGetParameterList Component ) ) ) )

(defun ComponentInfoGetPLengthFromComponent ( Component )
  ( nth 2
        ( ComponentInfoGetPLengthParameterFromParameterList 
          ( PCellGetParameterList Component ) ) ) )


(defun ComponentInfoGetNLengthParameterFromParameterList ( ParameterList )
  ( rexCompile "NL\\([0-9]*\\)" )
  ( car ( exists Parameter
                 ParameterList 
                 ( rexExecute ( car Parameter ) ) ) ) )

(defun ComponentInfoGetPLengthParameterFromParameterList ( ParameterList )
  ( rexCompile "PL\\([0-9]*\\)" )
  ( car ( exists Parameter
                 ParameterList 
                 ( rexExecute ( car Parameter ) ) ) ) )

(defun ComponentInfoGetNWidthParameterFromParameterList ( ParameterList )
  ( rexCompile "NW\\([0-9]*\\)" )
  ( car ( exists Parameter
                 ParameterList 
                 ( rexExecute ( car Parameter ) ) ) ) )

(defun ComponentInfoGetPWidthParameterFromParameterList ( ParameterList )
  ( rexCompile "PW\\([0-9]*\\)" )
  ( car ( exists Parameter
                 ParameterList 
                 ( rexExecute ( car Parameter ) ) ) ) )

(defun ComponentInfoGetNWidthParameter ( ComponentInfo )
  ( ComponentInfoGetNWidthParameterFromParameterList 
    ( ComponentInfoGetParameterList ComponentInfo ) ) )

(defun ComponentInfoGetPWidthParameter ( ComponentInfo )
  ( ComponentInfoGetPWidthParameterFromParameterList 
    ( ComponentInfoGetParameterList ComponentInfo ) ) )
  
(defun ComponentInfoGetNWidthParameterName ( ComponentInfo )
  ( car ( ComponentInfoGetNWidthParameter ComponentInfo ) ) )

(defun ComponentInfoGetPWidthParameterName ( ComponentInfo )
  ( car ( ComponentInfoGetPWidthParameter ComponentInfo ) ) )

(defun ComponentInfoGetWidthParameterNameFromChainInfo ( ChainInfo )
  (cond (
         ( stringp ( ComponentInfoGetNWidthParameterName ChainInfo ) )
         ( ComponentInfoGetNWidthParameterName ChainInfo ) )
        (
         ( stringp ( ComponentInfoGetPWidthParameterName  ChainInfo ) )
         ( ComponentInfoGetPWidthParameterName  ChainInfo ) ) ) )

(defun ComponentInfoGetStrengthFromChainInfo ( ChainInfo )
  (let (
        ( N ( ComponentInfoGetNStrength ChainInfo ) )
        ( P ( ComponentInfoGetPStrength ChainInfo ) ) )
  (cond ( ( numberp N ) N ) ( ( numberp P ) P ) ) ) )

(defun ComponentInfoSetHeight ( ComponentInfo Height )
  ( setarray ComponentInfo `height Height ) )

(defun ComponentInfoSetWidth ( ComponentInfo Width )
  ( setarray ComponentInfo `width Width ) )

(defun ComponentInfoSetParity ( ComponentInfo Parity )
  ( setarray ComponentInfo `parity Parity ) )

(defun ComponentInfoSetPosition ( ComponentInfo Position )
  ( setarray ComponentInfo `position ) )

(defun ComponentInfoSetStrengthForChainInfo ( ChainInfo NewChainStrength )
  (let (
        ( ChainParameters ( ComponentInfoGetParameterList ChainInfo ) )
        ( ChainStrength ( ComponentInfoGetStrengthFromChainInfo ChainInfo ) )
        ( WidthParameterName ( ComponentInfoGetWidthParameterNameFromChainInfo ChainInfo ) ) )
    ( setarray ChainInfo `params
               ( subst  ( list WidthParameterName
                               "float"
                               NewChainStrength )
                        ( list WidthParameterName
                               "float"
                               ChainStrength )
                        ChainParameters ) ) ) )

(defun ComponentInfoCreateComponentFromComponentInfo ( CellView
                                                       ComponentInfo
                                                       Location )
  "Converts a ComponentInfo into an instance at a given Location in given CellView"
  (let ( 
        ( LibCellView ( ComponentInfoGetLibCellView ComponentInfo ) ) )
    (let (
          ( Instance 
            ( dbCreateParamInstByMasterName 
              CellView
              ( car LibCellView )
              ( cadr LibCellView )
              ( caddr LibCellView )
              ( ComponentInfoGetName ComponentInfo )
              Location
              (if ( ComponentInfoGetParity ComponentInfo ) "MX" "R0" )
              1
              ( append 
                ( ComponentInfoGetParameterList ComponentInfo )
                ( append 
                  (if ( ComponentInfoGetNWidthParameterName ComponentInfo  )
                      ( list
                        ( list ( ComponentInfoGetNWidthParameterName ComponentInfo  )
                               "float" 
                               ( ComponentInfoGetNStrength ComponentInfo ) ) ) )
                  (if ( ComponentInfoGetPWidthParameterName ComponentInfo  )
                      ( list 
                        ( list ( ComponentInfoGetPWidthParameterName ComponentInfo  )
                               "float" 
                               ( ComponentInfoGetPStrength ComponentInfo ) ) ) ) ) ) ) ) )
      ( mapcar
        (lambda ( Conn )
          ( dbCreateConnByName 
            ( dbMakeNet CellView ( cadr Conn ) )
            Instance 
            ( car Conn ) ) )
        ( tableToList ( ComponentInfoGetConnTable ComponentInfo ) ) ) ) ) )
