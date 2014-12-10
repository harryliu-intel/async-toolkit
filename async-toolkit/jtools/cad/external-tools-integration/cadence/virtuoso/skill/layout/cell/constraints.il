; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun ConstraintsReplaceNetWireWidth ( Net
                                        WireWidth
                                        )
  ( dbReplaceProp 
    Net
    "wirewidth"
    "float"
    WireWidth ) )

(defun ConstraintsReplaceNetWireSpacing ( Net
                                          WireSpacing
                                          )
  ( dbReplaceProp 
    Net
    "wirespacing"
    "float"
    WireSpacing ) )

(defun ConstraintsReplaceNetWirePriority ( Net
                                           WirePriority
                                          )
  ( dbReplaceProp 
    Net
    "priority"
    "float"
    WirePriority ) )

(defun ConstraintsReplaceGlobalWireSpacing ( CellView
                                             WireSpacing )
  ( dbReplaceProp 
    CellView
    "wirespacing"
    "float"
    WireSpacing ) )

(defun ConstraintsReplaceGlobalWireSpacing ( CellView
                                             WireSpacing )
  ( dbReplaceProp 
    CellView
    "wirespacing"
    "float"
    WireSpacing ) )

(defun ConstraintsGetNetWireWidth ( Net
                                   )
  ( PropGetPropValueForPropName
    ( getq Net prop )
    "wirewidth" ) )

(defun ConstraintsGetNetWireSpacing ( Net
                                   )
  ( PropGetPropValueForPropName
    ( getq Net prop )
    "wirespacing" ) )

(defun ConstraintsGetNetWirePriority ( Net
                                      )
  ( PropGetPropValueForPropName
    ( getq Net prop )
    "priority" ) )

(defun ConstraintsGetGlobalWireSpacing ( CellView )
  ( PropGetPropValueForPropName
    ( getq CellView prop )
    "wirewidth" ) )

(defun ConstraintsGetGlobalWireSpacing ( CellView )
  ( PropGetPropValueForPropName
    ( getq CellView prop )
    "wirespacing" ) )

(defun ConstraintsGetICCNetName ( NetName )
  (cond (
         ( rexMatchp ".*-.*" NetName )
         ( sprintf nil "'%s'" NetName ) )
        (
         NetName ) ) )


(defun ConstraintsPrint ( CellView )
  ( mapcar 
    (lambda ( Net )
      (when ( or 
              ( ConstraintsGetNetWireWidth
                Net )
              ( ConstraintsGetNetWireSpacing
                Net )
              ( ConstraintsGetNetWirePriority
                Net ) )
        ( printf "Net %s\t width: %g\t spacing: %g\t priority: %d\n"
                 Net->name
                 (cond (
                        ( ConstraintsGetNetWireWidth
                          Net ) )
                       ( 0.0 ) )
                 (cond (
                        ( ConstraintsGetNetWireSpacing
                          Net ) )
                       ( 0.0 ) )
                 (cond (
                        ( ConstraintsGetNetWirePriority
                          Net ) )
                       ( 0 ) )
                 ) ) )
    CellView->nets
    )
  t )

(defun ConstraintsGetNetNames ( CellView 
                                NetNamesToAvoid )
  ( mapcar
    `ConstraintsGetICCNetName
    ( ListDifference
      CellView->nets~>name
      NetNamesToAvoid
      `equal
      nil
      )
    ) )

(defun ConstraintsWriteAllNetsClass_ICC ( CellView
                                          NetNamesToAvoid 
                                          Port)
  ( fprintf
    Port
    "define (class allnetclass " )

  ( foreach NetName
            ( ConstraintsGetNetNames 
              CellView 
              NetNamesToAvoid )
            ( fprintf 
              Port
              ( strcat NetName " " ) ) )
  ( fprintf 
    Port
    ")\n" )
)

(defun ConstraintsWriteInternalNetsClass_ICC ( CellView
                                          NetNamesToAvoid 
                                          Port)
  let( ( internalNetNames )
     internalNetNames=setof( name ConstraintsGetNetNames( CellView NetNamesToAvoid ) rexMatchp( InternalNetRegEx name ))
     if( internalNetNames then
        fprintf( Port "define (class internalnetclass " )
        foreach( NetName internalNetNames
          fprintf( Port strcat( NetName " " ) ) )
        fprintf( Port ")\n" )
        foreach( NetName internalNetNames
          fprintf( Port "circuit net %s ( priority 255 )\n" NetName) )
     )
  )
)


(defun ConstraintsGetICCNetClassName ( NetName )
  ( ConstraintsGetICCNetName
    ( sprintf nil "netclass_%s"
              NetName  ) ) )

(defun ConstraintsWriteNetClass_ICC ( Net
                                      Port )
  ( fprintf
    Port
    "define (class %s %s)\n"
    ( ConstraintsGetICCNetClassName
      ( getq Net name ) )
    ( ConstraintsGetICCNetName ( getq Net name ) ) ) )


(defun ConstraintsWriteGlobalWireSpacing_ICC ( CellView
                                               ICCUnitsPerMeter
                                               Port )
    ( fprintf
      Port
      "rule class_class allnetclass allnetclass (clearance %f)\n"
    ( times ICCUnitsPerMeter
            ( ConstraintsGetGlobalWireSpacing
              CellView ) ) ) )

(defun ConstraintsWriteGlobalWireWidth_ICC ( CellView
                                             ICCUnitsPerMeter
                                             Port )
    ( fprintf
      Port
      "rule class_class allnetclass allnetclass (width %f)\n"
    ( times ICCUnitsPerMeter
            ( ConstraintsGetGlobalWireWidth
              CellView ) ) ) )

(defun ConstraintsWriteWireSpacing_ICC ( Net
                                         ICCUnitsPerMeter
                                         Port )
  ( fprintf
    Port
    "rule class_class %s allnetclass (clearance %f)\n"
    ( ConstraintsGetICCNetClassName
      ( getq Net name ) )
    ( times ICCUnitsPerMeter
            ( ConstraintsGetNetWireSpacing
              Net ) )
    ) )


(defun ConstraintsWriteWireWidth_ICC ( Net
                                       ICCUnitsPerMeter
                                       Port )
  ( fprintf
    Port
    "rule net %s (width %f)\n"
    ( ConstraintsGetICCNetName ( getq Net name ) )
    ( times ICCUnitsPerMeter
            ( ConstraintsGetNetWireWidth
              Net ) )
    ) )

(defun ConstraintsDumpICCToFile ( CellView
                                  FileName
                                  ICCUnitsPerMeter
                                  NetNamesToAvoid )
  (if ( not ( isWritable ( PathDirName FileName ) ) )
      ( printf 
        "Error: Can't dump constraints to non-writable file %s" 
        FileName )
    (let (
          ( Port ( outfile FileName ) ) )
      ( ConstraintsDumpICC 
        CellView
        Port
        ICCUnitsPerMeter 
        NetNamesToAvoid )
      ( close Port ) ) ) )

  
(defun ConstraintsDumpICC ( CellView
                            Port
                            ICCUnitsPerMeter 
                            NetNamesToAvoid )
      ( ConstraintsWriteAllNetsClass_ICC 
        CellView
        NetNamesToAvoid
        Port )
      ( ConstraintsWriteInternalNetsClass_ICC 
        CellView
        NetNamesToAvoid
        Port )
      ( foreach Net ( getq CellView nets )
                (when ( ConstraintsGetNetWireSpacing
                        Net )
                   ( ConstraintsWriteNetClass_ICC 
                     Net
                     Port )
                   ( ConstraintsWriteWireSpacing_ICC 
                     Net
                     ICCUnitsPerMeter
                     Port )
                   )
                (when ( ConstraintsGetNetWireWidth
                        Net )
                  ( ConstraintsWriteWireWidth_ICC 
                    Net
                    ICCUnitsPerMeter
                    Port ) ) )
 )


(defun ConstraintsDumpLayers_ICC ( LPPs 
                                   ViaNames
                                   Port )
  ( fprintf Port "select net *\n" )
  ( fprintf Port "circuit selected (use_layer %s)\n" 
            ( StringUtilConvertStringListToSpaceSeperatedString
              ( mapcar
                `car 
                LPPs ) ) )
  ( fprintf Port "circuit selected (use_via %s)\n" 
            ( StringUtilConvertStringListToSpaceSeperatedString
              ViaNames ) )
  ( fprintf Port "unselect net *\n" )
  )
                          

(defun ConstraintsConvertLPPListToViaNameList ( LPPs
                                                ViaLibraryName
                                                ViaNameFormat
                                                )
  ( setof 
    ViaName
    ( maplist
      (lambda ( Rest )
        (let (
              ( ThisLayer ( car ( car Rest ) ) )
              ( ThatLayer ( car ( cadr Rest ) ) ) )
          (when ( and ThisLayer ThatLayer )
            ( AutoPinGetViaMasterName 
              ViaLibraryName
              ViaNameFormat
              ThisLayer
              ThatLayer ) ) ) )
      LPPs )
    ViaName ) )
