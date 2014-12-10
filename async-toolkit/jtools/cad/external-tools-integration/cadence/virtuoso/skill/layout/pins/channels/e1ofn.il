; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun E1ofNInsertEnable ( EnableNetName DataRailNets )
  (let (
        ( NumDataRails ( length DataRailNets ) ) )
    (let (
          ( CurrPos 0 )
          ( RemainingNets DataRailNets )
          ( Result nil )
          ( EnableInsertPos ( ceiling 
                              ( quotient 
                                ( float NumDataRails ) 
                                ( float 2 ) ) ) ) )
      (if DataRailNets
          (while ( leqp CurrPos NumDataRails )
            (if ( equal CurrPos EnableInsertPos )
                ( setq Result ( tconc Result EnableNetName ) )
              (let ()
                ( setq Result ( tconc Result ( car RemainingNets ) ) )
                ( setq RemainingNets ( cdr RemainingNets ) )
                )
              )
            ( setq CurrPos ( plus CurrPos 1 ) )
            )
        ( setq Result ( tconc Result EnableNetName ) )
        )    
      ( car Result ) ) ) )

(defun E1ofNCalculatePinPitchIndexes ( EnableNetName 
                                       DataRailNets
                                       EnablePinPitchIndex 
                                       PitchesBetweenPins 				       
				       ReservedPitchTable
				       PinOffsets				      
				     )

  (let (
	( ChannelNets ( E1ofNInsertEnable EnableNetName DataRailNets ) )
       )  
    (let (
	  ( NetArray ( listToVector ChannelNets ) )
	  ( NetPitchArray ( makeVector ( length ChannelNets ) ) )
	  ( OffsetArray ( listToVector PinOffsets ) )
	  ( EnableIndex ( quotient ( length ChannelNets ) 2 ) )	
	 )

      ( setarray NetPitchArray EnableIndex 
		 ( list EnableNetName ( plus EnablePinPitchIndex ( arrayref OffsetArray EnableIndex ) ) ) )      

      (let (
	     ( CurrPitch EnablePinPitchIndex )
	    )
	( for I 1 EnableIndex 
	      (let ( 
		    ( CurrIndex ( difference EnableIndex I ) )
		    )	
		(let (
		      ( PitchCount 0 )
		     )
		  (while 
		      ( lessp PitchCount PitchesBetweenPins  )
		    ( setq CurrPitch ( difference CurrPitch 1 ) )
		      ( cond (
			      ( AutoPinIsPitchReserved ReservedPitchTable CurrPitch )	 		     
			     )
			     (					      
			      ( setq PitchCount ( plus PitchCount 1 ) )			      			     
			     )
		      )		   
		   )		        
		  ( setarray NetPitchArray CurrIndex 
			     ( list  ( arrayref NetArray CurrIndex ) ( plus CurrPitch ( arrayref OffsetArray CurrIndex ) ) )
		  )
		)
	      )
        )
      )

      (let (
	     ( CurrPitch EnablePinPitchIndex )
	     )
	( for I ( plus EnableIndex 1 ) ( difference ( length NetPitchArray ) 1 ) 
	      (let ( 
		    ( CurrIndex I )
	       )
		(let (
		      ( PitchCount 0 )
		     )
		  (while 
		      ( lessp PitchCount  PitchesBetweenPins  )
		    ( setq CurrPitch ( plus CurrPitch 1 ) )
		      ( cond (
			      ( AutoPinIsPitchReserved ReservedPitchTable CurrPitch )				     
			     )
			     (			      
			      ( setq PitchCount ( plus PitchCount 1 ) )			 
			     )
		      )		     
		   )		        
		  ( setarray NetPitchArray CurrIndex 
			     ( list  ( arrayref NetArray CurrIndex ) ( plus CurrPitch ( arrayref OffsetArray CurrIndex ) ) )
		  )
		)
	      )
        )
      )
          
      ( vectorToList NetPitchArray )
    )
  )
)


(defun E1ofNQueueLeftE1ofNPins ( PinTable
                                 EnableNetName
                                 DataRailNets
                                 EnablePinPitchIndex
                                 PitchesBetweenPins
				 PinOffsets				
                                 LayerPurposePair
                                 PinWidths
                                 PinLength
                                 WireSpacings
				 WirePitch
                                 BoundaryPolygonEdges
                                 BoundaryPinHorizontalSpacing
                                 BoundaryPinVerticalSpacing
                                 ManufacturingGrid
				 FunctionCacheTable	
				 ReservedPitchTable
				 @key ( CreateRectInfo t )
                                      ( Strut nil )
                               )  
  (let (
        ( PinPitches  	  
	  ( E1ofNCalculatePinPitchIndexes
	    EnableNetName
	    DataRailNets
	    EnablePinPitchIndex
	    PitchesBetweenPins
	    ReservedPitchTable
	    (cond ( 
		    ( null PinOffsets )
		    ( vectorToList ( makeVector ( plus ( length DataRailNets ) 1 ) 0 ) )
		    )
		   (
		    PinOffsets
		    )
		   ) ) ) )

    ( mapcar
      (lambda ( PinPitch Width Spacing )
        ( AutoPinQueueLeftPin
          PinTable
          ( cadr PinPitch )
          ( car PinPitch )
          LayerPurposePair
          Width
          PinLength
          Spacing
          WirePitch
          BoundaryPolygonEdges
          BoundaryPinHorizontalSpacing
          BoundaryPinVerticalSpacing
          ManufacturingGrid
          FunctionCacheTable
          ?CreateRectInfo CreateRectInfo
          ?Strut Strut
          ) )
      PinPitches
      PinWidths
      WireSpacings )
    t ) )

(defun E1ofNQueueRightE1ofNPins ( PinTable
                                  EnableNetName
                                  DataRailNets
                                  EnablePinPitchIndex
                                  PitchesBetweenPins
				  PinOffsets				 
                                  LayerPurposePair
                                  PinWidths
                                  PinLength
                                  WireSpacings
				  WirePitch
                                  BoundaryPolygonEdges
                                  BoundaryPinHorizontalSpacing
                                  BoundaryPinVerticalSpacing	
                                  ManufacturingGrid
				  FunctionCacheTable
				  ReservedPitchTable
				  @key ( CreateRectInfo t )
                                       ( Strut nil ) 
                                )
  
  (let (
        ( PinWidths
          (cond (
                 ( numberp PinWidths )
                 ( ListFillList 
                   (lambda ( N ) PinWidths )
                   ( plus 1 ( length DataRailNets ) ) ) )
                ( PinWidths ) ) )
        ( PinPitches  	  
	  ( E1ofNCalculatePinPitchIndexes
	    EnableNetName
	    DataRailNets
	    EnablePinPitchIndex
	    PitchesBetweenPins
	    ReservedPitchTable
	    ( cond ( 
		    ( null PinOffsets )
		    ( vectorToList ( makeVector ( plus ( length DataRailNets ) 1 ) 0  ) )
		    )
		   (
		    PinOffsets
		    )
		   ) ) ) )    

    ( mapcar
      (lambda ( PinPitch Width Spacing )
      ( AutoPinQueueRightPin
        PinTable
        ( cadr PinPitch )
        ( car PinPitch )
        LayerPurposePair
        Width
        PinLength
        Spacing
	WirePitch
        BoundaryPolygonEdges
        BoundaryPinHorizontalSpacing
        BoundaryPinVerticalSpacing
        ManufacturingGrid
	FunctionCacheTable
	?CreateRectInfo CreateRectInfo
        ?Strut Strut
	) )
      PinPitches
      PinWidths
      WireSpacings )
    t ) )

(defun E1ofNQueueLeftRightE1ofNPins ( PinTable
                                      EnableNetName
                                      DataRailNets
                                      EnablePinPitchIndex
                                      PitchesBetweenPins
                                      PinOffsets				
                                      LayerPurposePair
                                      PinWidths
                                      PinLength
                                      WireSpacings
                                      WirePitch
                                      BoundaryPolygonEdges
                                      BoundaryPinHorizontalSpacing
                                      BoundaryPinVerticalSpacing
                                      ManufacturingGrid
                                      FunctionCacheTable	
                                      ReservedPitchTable
                                      @key ( CreateRectInfo t )
                                           ( Strut nil )
                                    )  
  (let (
        ( PinPitches  	  
	  ( E1ofNCalculatePinPitchIndexes
	    EnableNetName
	    DataRailNets
	    EnablePinPitchIndex
	    PitchesBetweenPins
	    ReservedPitchTable
	    (cond ( 
		    ( null PinOffsets )
		    ( vectorToList ( makeVector ( plus ( length DataRailNets ) 1 ) 0 ) )
		    )
		   (
		    PinOffsets
		    )
		   ) ) ) )

    ( mapcar
      (lambda ( PinPitch Width Spacing )
        ( AutoPinQueueLeftRightPin
          PinTable
          ( cadr PinPitch )
          ( car PinPitch )
          LayerPurposePair
          Width
          PinLength
          Spacing
          WirePitch
          BoundaryPolygonEdges
          BoundaryPinHorizontalSpacing
          BoundaryPinVerticalSpacing
          ManufacturingGrid
          FunctionCacheTable
          ReservedPitchTable
          ?CreateRectInfo CreateRectInfo
          ?Strut Strut
          ) )
      PinPitches
      PinWidths
      WireSpacings )
    t ) )
