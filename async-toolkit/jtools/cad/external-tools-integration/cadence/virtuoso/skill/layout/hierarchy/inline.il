; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


;If we inline an instance a and it has a net b, first
; look for a connection from the CellView to a.b
; This will be the case if a.b is a port net
; If there is no connection (a.b is local in a)
; then create a new net a.b, and make sure it doesn't 'short' 
; with an existing net.
(defun InlineMakeNewNet ( OldNet ParentNet CellView Instance ConnMap )
  (cond (
         ( arrayref 
           ConnMap 
           OldNet->name
            ) )
           (
            (let (
                  ( NewNetName
                    ( sprintf 
                      nil 
                      "%s%s%s" 
                      ( getq Instance name )
                      HieracrchyDeliminator 
                      ( getq OldNet name ) ) ) )
              (when ( dbFindNetByName CellView NewNetName )
;                (error ( sprintf nil "Net %s already exists in parent cell" NewNetName ) ) )
                (println ( sprintf nil "Net %s already exists in parent cell" NewNetName ) ) )
              ( setarray 
                ConnMap 
                OldNet->name 
                ( dbMakeNet 
                  CellView                
                  NewNetName
                  ParentNet ) ) ) ) ) )

(defun InlineInstancesSimple ( CellView
                              Instances )
  ( InlineInstances 
    CellView
    CellView
    Instances
    "netlist"
    nil
    nil
    nil
    ?Verbose nil 
    ?CopyPins t ) )

(defun InlineInstances ( ConnectivityCellView
                         CellView
                         Instances
                         ConnectivityViewName
                         UpdateConnectivityCellView
                         FoldableLibCellExpressionPairs
                         SuperStackLibCellPairRegExs
                         @key 
                         ( Verbose t )                    
                         ( CopyPins nil )
                         ( CopyFigs t ) 
                          )
  (let (
        ( ConnectivityInstanceMap ( NameMakeInstanceMapFromCellView 
                                    ConnectivityCellView
                                    FoldableLibCellExpressionPairs ) )
        ( LayoutInstanceMap ( NameMakeInstanceMapFromCellView 
                              CellView
                              FoldableLibCellExpressionPairs ) ) )
    (let (
          ( FilterRet ( NameFilterInstances
                        Instances
                        SuperStackLibCellPairRegExs ) ) )
        (let (
              ( SuperStacks ( cadr FilterRet ) )
              ( Others ( car FilterRet ) ) )            
          ( append
            ( mapcan
              (lambda ( SuperStack )
                ( InlineSuperStack 
                  ConnectivityCellView
                  CellView
                  ConnectivityInstanceMap
                  LayoutInstanceMap
                  SuperStack
                  ConnectivityViewName
                  UpdateConnectivityCellView
                  FoldableLibCellExpressionPairs
                  ?Verbose Verbose
                  ?CopyPins CopyPins
                  ?CopyFigs CopyFigs
                  ) )
              SuperStacks )
            ( mapcan
              (lambda ( Instance )
                ( InlineInstance
                  ConnectivityCellView
                  CellView
                  ConnectivityInstanceMap
                  LayoutInstanceMap
                  Instance   
                  ConnectivityViewName
                  UpdateConnectivityCellView
                  FoldableLibCellExpressionPairs                                       
                  ?Verbose Verbose
                  ?CopyPins CopyPins
                  ?CopyFigs CopyFigs
                ) )
              Others ) ) ) ) ) )

    

(defun InlineSuperStack ( ConnectivityCellView
                          CellView
                          ConnectivityInstanceMap
                          LayoutInstanceMap
                          SuperStack
                          ConnectivityViewName
                          UpdateConnectivityCellView
                          FoldableLibCellExpressionPairs
                          @key
                          ( Verbose nil ) 
                          ( CopyFigs t ) 
                          ( CopyPins nil ) )
  (let (
        ( ChainNameList
          ( SuperStackGetComponentNameListFromSuperStackName
            ( PropGetPropValueForPropName 
              ( getq SuperStack prop )
              "name"
              ) ) ) )
    (let ( 
          ( NewFigs ( InlineInstance
                      ConnectivityCellView
                      CellView
                      ConnectivityInstanceMap
                      LayoutInstanceMap
                      SuperStack
                      ConnectivityViewName
                      UpdateConnectivityCellView
                      FoldableLibCellExpressionPairs  
                      ?Verbose Verbose
                      ?CopyFigs CopyFigs
                      ?CopyPins CopyPins ) ) )
      ;reset chain names to what superstack name implies
      ( foreach
        Chain
        ( setof
          Fig
          NewFigs 
          ( equal ( getq Fig objType ) "inst" ) )
        (let (
              ( ChainName
                ( car ( last ( parseString ( getq Chain name ) "." ) ) ) ) )
          ( dbSetq 
            Chain 
            ( nth 
              ( SuperStackGetComponentIndexFromComponentName
                ChainName )
              ChainNameList  )
                   name
                   ) ) ) ) ) )
                              
(defun InlineInstance (
                       ConnectivityCellView
                       CellView
                       ConnectivityInstanceMap
                       LayoutInstanceMap
                       Instance
                       ConnectivityViewName
                       UpdateConnectivityCellView
                       FoldableLibCellExpressionPairs
                       @key 
                       ( Verbose nil ) 
                       ( CopyPins nil )                      
                       ( CopyFigs t )
                       )
  ;recreate and reeconnect sub Instances
 
  (let (
        ( NewFigs nil )
        ( NetTable ( makeTable `bla nil ) )
        ( InstanceLayoutCellView ( getq Instance master ) )
        ( InstanceConnectivityCellView 
          (if ( ddGetObj  ( getq ( getq Instance master ) libName )
                          ( getq ( getq Instance master ) cellName )
                          ConnectivityViewName )
              ( dbOpenCellViewByType
                ( getq ( getq Instance master ) libName )
                ( getq ( getq Instance master ) cellName )
                ConnectivityViewName
                nil
                "r" )
            ( getq Instance master ) ) ) )
    (let (
          ( ConnMap ( makeTable `conn nil ) )
          ( LayoutInstanceInstanceMap ( NameMakeInstanceMapFromCellView  
                                        InstanceLayoutCellView
                                        FoldableLibCellExpressionPairs ) )
          ( ConnectivityInstanceInstanceMap ( NameMakeInstanceMapFromCellView
                                              InstanceConnectivityCellView
                                              FoldableLibCellExpressionPairs ) ) )
          
      ( foreach InstTerm ( getq Instance conns )
                ( setarray ConnMap InstTerm->term->net->name InstTerm->net ) )

      ( foreach 
        ConnectivityCanonicalInstanceName
        ( NameGetCanonicalInstanceNames
          ConnectivityInstanceInstanceMap )
        (if Verbose
            ( printf "Connectivity Instance: %s\n"
                     ConnectivityCanonicalInstanceName ) )
        (let (
              ( ConnectivitySubInstance
                ( car
                  ( NameGetInstancesForCanonicalName
                    ConnectivityInstanceInstanceMap
                    ConnectivityCanonicalInstanceName ) ) )
              ( LayoutSubInstances                       
                ( NameGetInstancesForCanonicalName
                  LayoutInstanceInstanceMap
                  ConnectivityCanonicalInstanceName ) ) )
          (if Verbose 
              ( printf "Layout Instances: %L\n" LayoutSubInstances~>name ) )
          ( foreach LayoutSubInstance LayoutSubInstances 
                    (if ( equal ( getq LayoutSubInstance objType ) "inst"  )
                        (let (
                              ( NewInstance
                                ( InlineAddFigure
                                  LayoutSubInstance
                                  CellView
                                  Instance
                                  ConnMap
                                  CopyPins ) )
                              )
                          ( setq NewFigs ( cons NewInstance NewFigs ) )
                  ;get connectivity 
                          ( foreach InstTerm ( getq ConnectivitySubInstance conns )
                                    (let (
                                          ( NewNet 
                                            ( InlineMakeNewNet
                                              InstTerm->net
                                              nil
                                              CellView
                                              Instance
                                              ConnMap ) ) )
                                   ;make sure there is in fact a terminal
                                      (cond (
                                             ( getq InstTerm term )
                                             (if Verbose
                                                 ( printf "make connection %s.%s = %s\n" 
                                                   NewInstance->name 
                                                   InstTerm->name 
                                                   NewNet->name ) )
                                             ( dbCreateConnByName
                                               NewNet 
                                               NewInstance
                                               ( getq InstTerm name ) ) )
                                            ( 
                                             (if Verbose
                                                 ( printf "Cant make connection %s.%s = %s\n" 
                                                          NewInstance->name
                                                          InstTerm->name
                                                          NewNet->name ) ) ) ) ) ) ) ) ) ) )
    
    (if CopyFigs then  
      ;add shapes             
      ( foreach Shape ( getq ( getq Instance master ) shapes )
              ( setq NewFigs 
                     ( cons 
                       ( InlineAddFigure Shape CellView Instance ConnMap CopyPins )
                       NewFigs ) ) )
    
      ;add mosaics   
      ( foreach Mosaic ( getq ( getq Instance master ) mosaics )            
              ( setq NewFigs 
                     ( cons 
                       ( InlineAddFigure Mosaic CellView Instance ConnMap CopyPins )
                       NewFigs ) ) )
    )
    (cond (
           ( and UpdateConnectivityCellView
                 ( not ( equal ConnectivityCellView CellView ) ) )
           (if Verbose 
               ( printf "Updating Netlist view...\n" ) )
           (let (
                 ( ConnectivityCellViewInstance 
                   ( NameGetInstancesForCanonicalName
                     LayoutInstanceMap
                     ( NameGetCanonicalInstanceNameForInstance 
                       ConnectivityInstanceMap
                       ConnectivitySubInstance ) ) ) )
              ( InlineInstance 
               ConnectivityCellView
               ConnectivityCellView
               ConnectivityInstanceMap
               ConnectivityInstanceMap
               ConnectivityCellViewInstance
               ConnectivityViewName
               UpdateConnectivityCellView
               FoldableLibCellExpressionPairs   
               ?CopyPins CopyPins
               ?Verbose Verbose
               ?CopyFigs CopyFigs                       
               ) ) ) )

   (if Instance
       ( dbDeleteObject Instance  ) )
   
   NewFigs ) ) )
   

(defun InlineMergeNet ( SinkNet SourceNet )
  ( foreach InstTerm ( getq SourceNet allInstTerms )
    ( dbCreateConn SinkNet ( getq InstTerm inst ) ( getq InstTerm term ) )	    
  )
  ( dbDeleteObject SourceNet )
)


(defun InlineAddFigure ( Figure CellView Instance ConnMap CopyPins )
   (cond (    
          ;dont copy if it has a parent - parent will do so
          ( and 
            ( or CopyPins ( not ( getq Figure pin ) ) )
            ( not ( getq Figure parent ) ) )
          (let (
                ( NewFig ( InlineCopyFig Figure CellView Instance ) ) )
            (cond ( 
                   ( getq Figure net )
                   (let (
                         ( NewNet 
                           ( InlineMakeNewNet
                             ( getq Figure net )
                             nil
                             CellView
                             Instance
                             ConnMap ) ) )
                     ( cond ( NewNet 
                              ( dbAddFigToNet NewFig NewNet ) ) ) ) ) )
            NewFig ) ) ) )

(defun InlineCopyFig ( Figure CellView Instance ) 
  (let (
        ( NewFig ( dbCopyFig 
                   Figure 
                   CellView 
                   ( getq Instance transform ) ) ) )
    (cond ( 
           ;if the figure had a name, give new fig a name
           ( getq Figure name )
           ( dbSetq 
             NewFig 
             ( sprintf 
               nil
               "%s%s%s"
               ( getq Instance name )
               HieracrchyDeliminator
               ( getq Figure name ) )
             name ) ) )
    NewFig ) )
