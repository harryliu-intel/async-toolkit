; ==============================================================================================================
; Descriptions
; --------------------------------------------------------------------------------------------------------------
; BitTemplate = nil            | ChanTemplate = nil                ==> "SAMPLE_CHANNEL"                        |
; BitTemplate = list( 0 15 1 ) | ChanTemplate = nil                ==> "SAMPLE_CHANNEL[0..15]"                 |
; BitTemplate = nil            | ChanTemplate = list( "0" "e" )    ==> "SAMPLE_CHANNEL.0" / "SAMPLE_CHANNEL.e" | 
; BitTemplate = list( 0 15 1 ) | ChanTemplate = list( "0" "e" )    ==> "SAMPLE_CHANNEL[0..15].0/e"             |
; --------------------------------------------------------------------------------------------------------------
;
; ==============================================================================================================

procedure( TR_DrawPins( @key
				( DebugOn	t )
				( LabelPurpose	"drawing" )
				( PinLpp	list( "METAL5" "drawing" ))
				( PinOrigin	list( 0 0 ))
				( PinTemplate	list( 
							list( 0:0    0.36:0.36 )
							list( 0:0.72 0.36:1.08 )
							list( 0:1.44 0.36:1.80 )
							list( 0:2.16 0.36:2.52 )
							list( 0:2.88 0.36:3.24 )
						))
				( BitPitch	list( 0 19.2 ))
				( BitTemplate	list( 0 15 1 ))
				( ChanName	"SAMPLE_CHANNEL" )
				( ChanTemplate	list( "0" "1" "e" "2" "3" ))
				)

	let(( 	d_Pin
		d_Net
		currentPinTemplate
		currentBitTemplate
		currentbBox
		currentX
		currentY
		currentChanName
		tempChanName
		bitCurrent bitEnd bitInc bitCurrent
		LabelLpp 
		)

	; XY Coordinates
	; ==============
	currentX = car( PinOrigin )
	currentY = cadr( PinOrigin )

	LabelLpp = list( car( PinLpp ) LabelPurpose )

	; Set Up the Current Name 
	; =======================
	if( BitTemplate
	then
		ChanName   = sprintf( nil "%s[$$i]" ChanName )
		bitCurrent = car( BitTemplate )
		bitEnd   = cadr( BitTemplate )
		bitInc   = caddr( BitTemplate )		
	else
		bitCurrent = 0
		bitEnd     = 0
		bitInc     = 1
	)

	if( ChanTemplate
	then
		ChanName = sprintf( nil "%s.$$k" ChanName )
	else
		ChanTemplate = list( "nil" )
	)


	; Processing Each Bit
	; ===================
	while( bitCurrent <= bitEnd

		rexCompile( "\\$\\$i" )

		tempChanName = ChanName

		tempChanName = rexReplace( tempChanName sprintf( nil "%n" bitCurrent) 0 )

		for( k 0 length( ChanTemplate )-1

			currentChan = nth( k ChanTemplate )
			rexCompile( "\\$\\$k" )
			currentChanName = tempChanName
			currentChanName = rexReplace( currentChanName currentChan 0 )
			println( currentChanName )
			

			currentPinTemplate = nth( mod( k length( PinTemplate )) PinTemplate )


			currentbBox = list( 	list( 	currentX + caar( currentPinTemplate )
							currentY + cadar( currentPinTemplate )
							)
						list( 	currentX + caadr( currentPinTemplate )
							currentY + cadadr( currentPinTemplate )
							)
						)

			; First, draw the pin
			; ===================
			d_Pin = dbCreateRect(	geGetWindowCellView( ) 
						PinLpp 
						currentbBox
						)

			; Second, Fill up Connectivity
			; ============================
			d_Net = dbMakeNet( geGetWindowCellView() currentChanName )
			dbCreatePin( d_Net d_Pin )


			; Third, Put Labels
			; ==================
			dbCreateLabel(  geGetWindowCellView( )
                                LabelLpp
				list( 	currentX + caar( currentPinTemplate ) + ( caadr( currentPinTemplate ) - caar( currentPinTemplate )) / 2
					currentY + cadar( currentPinTemplate ) + ( cadadr( currentPinTemplate ) - cadar( currentPinTemplate )) / 2
					)
                                currentChanName
                                "centerLeft" 
                                "R0" 
                                "stick" 
                                0.2 )

		); end for

		bitCurrent = bitCurrent + bitInc
		currentX = currentX + car( BitPitch )
		currentY = currentY + cadr( BitPitch )

	); end while

	; returnValue
	; ===========
	currentX:currentY

	); end let

); end procedure


; From CAST
; ==========
; defchan TxFifoManagerChannels ()(
;    // channels used by TX_OUT
;    TxFifoManagerOutPort   +TX_OUT;
;
;    // status IO
;    e1of4     -STATUS;          // 2-bit, FIFO status channel, core -> glue
;    e1of2     -NEW_CALENDAR;    // 1-bit, start of new calendar, core -> glue
;
;    // core states
;    e1of2     -CORE_ERROR;      // 1-bit, device fatal error, core -> glue
;    e1of2     -GOOD_SYNC[0..1]; // 1-bit, is in good sync, core -> glue
;                                // Two signals are needed. Might come at 
;                                // different times. 
;    e1of2     -TRAINING_ACTIVE; // 1-bit, is sending training pattern
;    e1of2     -DIP2_ERROR;      // 1-bit, status bad parity detected
;    e1of2     -STALL;           // 1-bit, stalled; data flow control
;
;    // reset output
;    e1of2     +nRST             // hard reset command
;    ){}
;
; defchan TxFifoManagerOutPort()(    
;    e1of4[4]  +PORT_ADDR;       // port ID channel
;    e1of4[32] +DATA;            // data channel
;    e1of2[3]  +LAST_BYTE;       // number of valid bytes
;    e1of4     +EOPS;            // end-of-packet status channel
;    e1of2     +SOP;             // start-of-packet channel
;    e1of2     +FBP              // force-bad-parity channel
;    ){}
;
; defchan TxFifoManagerNodes ()(
;
;    // link layer configure
;    node[8]  +i_calendar_m;     // calendar multiplicity
;    node[8]  +i_calendar_len;   // calendar length
;    node[1]  +i_tsclk_edge;     // status clock edge
;    node[4]  +i_max_bad_parity; // max number of bad parities to declare 
;    node[4]  +i_min_good_parity;// min number of good parities to declare sync
;    node[8]  +i_alpha;          // training sequence multiplicity
;    node[32] +i_data_max_t;     // DATA_MAX_T [?]
;    node[8]  +i_stall_wm_low;   // FIFO watermark low
;    node[8]  +i_stall_wm_high   // FIFO watermark high
;    ){}

procedure( DrawPins_TOP_TX( )

	let((	e1of4onM5
		e1of2onM5
		X Y XY
		)

	X = 216
	Y = 76.8
	XY = X:Y

	e1of4onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:1.44 0.36:1.80 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.00 0.36:3.36 )
				list( 0:3.72 0.36:4.08 )
				)

	e1of2onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.72 0.36:4.08 )
				)

	ONEof2onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:3.72 0.36:4.08 )
				)

	EonM5 = list( 
				list( 0:2.16 0.36:2.64 )
				)

	e1of1onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:2.16 0.36:2.64 )
				)

	node4onM7 = list( 
				list( 0:1.00 0.36:1.40 )
				list( 0:1.80 0.36:2.20 )
				list( 0:2.60 0.36:3.00 )
				list( 0:3.40 0.36:3.80 )
				)



	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of1onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_s2aerr" 
			?ChanTemplate	list( "0" "e")
			)

;    // status IO
;    e1of4     -STATUS;          // 2-bit, FIFO status channel, core -> glue

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of4onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.STATUS" 
			?ChanTemplate	list( "0" "1" "e" "2" "3" )
			)

;    e1of2     -NEW_CALENDAR;    // 1-bit, start of new calendar, core -> glue

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.NEW_CALENDAR" 
			?ChanTemplate	list( "0" "e" "1" )
			)
;
;    // core states
;    e1of2     -CORE_ERROR;      // 1-bit, device fatal error, core -> glue

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.CORE_ERROR" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     -GOOD_SYNC[0..1]; // 1-bit, is in good sync, core -> glue
;                                // Two signals are needed. Might come at 
;                                // different times. 

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 1 1 )
			?ChanName	"tx_channels.GOOD_SYNC" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     -TRAINING_ACTIVE; // 1-bit, is sending training pattern

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.TRAINING_ACTIVE" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     -DIP2_ERROR;      // 1-bit, status bad parity detected

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.DIP2_ERROR" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     -STALL;           // 1-bit, stalled; data flow control

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.STALL" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    // reset output
;    e1of2     +nRST             // hard reset command

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.nRST" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;checkpoint
;    e1of2[3]  +LAST_BYTE;       // number of valid bytes

	TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	ONEof2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 2 1 )
			?ChanName	"tx_channels.TX_OUT.LAST_BYTE" 
			?ChanTemplate	list( "0" "1" )
			)

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	EonM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"txsigconv._lastbyte01.le" 
			?ChanTemplate	nil
			)

	XY = car(XY):cadr(XY)+9.6

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	EonM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"txsigconv._lastbyte2.le" 
			?ChanTemplate	nil
			)

;    e1of2     +SOP;             // start-of-packet channel

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	ONEof2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.TX_OUT.SOP" 
			?ChanTemplate	list( "0" "1" )
			)

;    e1of2     +FBP              // force-bad-parity channel

	TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	ONEof2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_channels.TX_OUT.FBP" 
			?ChanTemplate	list( "0" "1" )
			)

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	EonM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"txsigconv._sop_force_bad.le" 
			?ChanTemplate	nil
			)


;    e1of4     +EOPS; ==> chantxdata[1] // end-of-packet status channel

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	list( X+0 Y+192 )
			?PinTemplate	e1of4onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"txsigconv.chantxdata[1]" 
			?ChanTemplate	list( "0" "1" "e" "2" "3" )
			)

;    e1of4[32] +DATA;      ==> chantxdata[4..35]         // data channel
;    e1of4[4]  +PORT_ADDR; ==> chantxdata[36..39]       // port ID channel

	TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of4onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 4 39 1 )
			?ChanName	"txsigconv.chantxdata" 
			?ChanTemplate	list( "0" "1" "e" "2" "3" )
			)


	X = 216
	Y = 76.8
	XY = X:Y


; All "tx_nodes" changed to "tx_core"
;    node[8]  +i_calendar_m;     // calendar multiplicity

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 7 1 )
			?ChanName	"tx_core.i_calendar_m" 
			?ChanTemplate	nil
			)

;    node[8]  +i_calendar_len;   // calendar length

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 7 1 )
			?ChanName	"tx_core.i_calendar_len" 
			?ChanTemplate	nil
			)

;    node[1]  +i_tsclk_edge;     // status clock edge

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"tx_core.i_tsclk_edge" 
			?ChanTemplate	nil
			)

;    node[4]  +i_max_bad_parity; // max number of bad parities to declare 

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 3 1 )
			?ChanName	"tx_core.i_max_bad_parity" 
			?ChanTemplate	nil
			)

;    node[4]  +i_min_good_parity;// min number of good parities to declare sync

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 3 1 )
			?ChanName	"tx_core.i_max_good_parity" 
			?ChanTemplate	nil
			)

;    node[8]  +i_alpha;          // training sequence multiplicity

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 7 1 )
			?ChanName	"tx_core.i_alpha" 
			?ChanTemplate	nil
			)

;    node[32] +i_data_max_t;     // DATA_MAX_T [?]

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 4.8 )
			?BitTemplate	list( 0 31 1 )
			?ChanName	"tx_core.i_data_max_t" 
			?ChanTemplate	nil
			)

;    node[8]  +i_stall_wm_low;   // FIFO watermark low

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 7 1 )
			?ChanName	"tx_core.i_stall_wm_low" 
			?ChanTemplate	nil
			)

;    node[8]  +i_stall_wm_high   // FIFO watermark high

	XY=TR_DrawPins(	?PinLpp		list( "METAL7" "pin" )
			?PinOrigin	XY
			?PinTemplate	node4onM7
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 7 1 )
			?ChanName	"tx_core.i_stall_wm_high" 
			?ChanTemplate	nil
			)

	); end let

); end procedure









; ===============================================================================================================

procedure( DrawPins_TOP_RX( )

	let((	e1of4onM5
		e1of2onM5
		X Y XY
		)

	X = 259.2
	Y = 76.8
	XY = X:Y

	e1of4onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:1.44 0.36:1.80 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.00 0.36:3.36 )
				list( 0:3.72 0.36:4.08 )
				)

	e1of3onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:1.44 0.36:1.80 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.00 0.36:3.36 )
				)

	e1of2onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.72 0.36:4.08 )
				)

	ONEof2onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:3.72 0.36:4.08 )
				)

	EonM5 = list( 
				list( 0:2.16 0.36:2.64 )
				)

	e1of1onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:2.16 0.36:2.64 )
				)

	node4onM7 = list( 
				list( 0:1.00 0.36:1.40 )
				list( 0:1.80 0.36:2.20 )
				list( 0:2.60 0.36:3.00 )
				list( 0:3.40 0.36:3.80 )
				)



	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of1onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 3 1 )
			?ChanName	"rx_s2aerr" 
			?ChanTemplate	list( "0" "e")
			)

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of1onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"rx_watchdog_intr" 
			?ChanTemplate	list( "0" "e" )
			)


;  defchan RxFifoManagerChannels ()(
;  
;      e1of2     -WATCHDOG;                    // watchdog (pseudo-clock ticks)
;      e1of2     -DATA_VALID;                  // data inputs
;      e1of4[4]  -ADDRESS;
;      e1of4[32] -DATA;
;      e1of2[3]  -MOD;
;      e1of4     -EOPS;
;      e1of2     -SOP;
;      e1of2     -BAD_PARITY;
;      e1of2     -SOP_ADDR_BAD_PARITY;
;
;      e1of2     -ALT_DATA_VALID;
;      e1of4[4]  -ALT_ADDRESS;                 // alternative data inputs
;      e1of4[16] -ALT_DATA;
;      e1of4     -ALT_MOD;
;      e1of4     -ALT_EOPS;
;      e1of2     -ALT_SOP;
;      e1of2     -ALT_BAD_PARITY;
;      e1of2     -ALT_SOP_ADDR_BAD_PARITY;
;
;      e1of2     -CALENDAR_RD;                 // configure calendar
;      e1of4[4]  -CALENDAR_RDADDR;
;      e1of4[2]  +CALENDAR_RDDATA;
;      e1of3     +STATUS;                      // status outputs
;      e1of4[2]  +STATUS_ADDRESS;
;      e1of2     -ERROR;                       // core monitor
;      e1of2     -GOOD_SYNC;
;      e1of2     -TRAINING_ACTIVE;
;      //    e1of4[16] -CAPTURE_DATA_TRAIN;
;      e1of2[5]  +SELECT_DATA_TRAIN;
;      e1of2     +nRST                         // reset output
;      ){}


XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.WATCHDOG"            ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.DATA_VALID"          ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_channels.ADDRESS"             ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 31 1 ) ?ChanName "rx_channels.DATA"                ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 2 1 )  ?ChanName "rx_channels.MOD"                 ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.EOPS"                ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.SOP"                 ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.BAD_PARITY"          ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.SOP_ADDR_BAD_PARITY" ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_DATA_VALID"      ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_channels.ALT_ADDRESS"         ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 15 1 ) ?ChanName "rx_channels.ALT_DATA"            ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_MOD"             ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_EOPS"            ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_SOP"             ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_BAD_PARITY"      ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_SOP_ADDR_BAD_PARITY" ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.CALENDAR_RD"         ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_channels.CALENDAR_RDADDR"     ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 1 1 )  ?ChanName "rx_channels.CALENDAR_RDDATA"     ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of3onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.STATUS"              ?ChanTemplate list( "0" "1" "e" "2" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 1 1 )  ?ChanName "rx_channels.STATUS_ADDRESS"      ?ChanTemplate list( "0" "1" "e" "2" "3" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ERROR"               ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.GOOD_SYNC"           ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.TRAINING_ACTIVE"     ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 4 1 )  ?ChanName "rx_channels.SELECT_DATA_TRAIN"   ?ChanTemplate list( "0" "e" "1" ))
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.nRST"                ?ChanTemplate list( "0" "e" "1" ))




	X = 259.2
	Y = 76.8
	XY = X:Y


;  defchan RxFifoManagerNodes ()(
;  
;      node[8]   +CALENDAR_LEN;
;      node[8]   +CALENDAR_M;
;      node[1]   +RSCLK_EDGE;
;      node[4]   +MIN_GOOD_PARITY;             // misc configure outputs
;      node[4]   +MAX_BAD_PARITY;
;      node[4]   +AVERAGE;

;      node[1]   +ENABLE_MANUAL_DESKEW;
;      node[1]   +ENABLE_STATIC_DESKEW;
;      node[1]   +ENABLE_REALTIME_TUNING;
;      node[85]  +MANUAL_DESKEW
;      ){}
;  


XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "rx_core.i_calendar_len"           ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "rx_core.i_calendar_m"             ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_core.i_rsclk_edge"             ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_core.i_min_good_parity"        ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_core.i_max_bad_parity"         ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_core.i_average"                ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_core.i_enable_manual_deskew"   ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_core.i_enable_static_deskew"   ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_core.i_enable_realtime_tuning" ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 84 1 ) ?ChanName "rx_core.i_manual_deskew"          ?ChanTemplate nil )
if( cadr(XY)>=576 then XY=297.6:cadr(XY))

	); end let

); end procedure



; ===============================================================================================================
; ===============================================================================================================
; ===============================================================================================================
; ===============================================================================================================

procedure( DrawPins_TX_FM( )

	let((	e1of4onM5
		e1of2onM5
		X Y XY
		)

	X = 1012.8 - 28.8 - 4.8
	Y = 1056
	XY = X:Y

	e1of4onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:1.44 0.36:1.80 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.00 0.36:3.36 )
				list( 0:3.72 0.36:4.08 )
				)

	e1of2onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.72 0.36:4.08 )
				)

	ONEof2onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:3.72 0.36:4.08 )
				)

	EonM5 = list( 
				list( 0:2.16 0.36:2.64 )
				)

	e1of1onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:2.16 0.36:2.64 )
				)

	node4onM7 = list( 
				list( 0:1.00 0.36:1.40 )
				list( 0:1.80 0.36:2.20 )
				list( 0:2.60 0.36:3.00 )
				list( 0:3.40 0.36:3.80 )
				)



	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of1onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"S2AERR" 
			?ChanTemplate	list( "0" "e")
			)

;    // status IO
;    e1of4     -STATUS;          // 2-bit, FIFO status channel, core -> glue

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of4onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.STATUS" 
			?ChanTemplate	list( "0" "1" "e" "2" "3" )
			)

;    e1of2     -NEW_CALENDAR;    // 1-bit, start of new calendar, core -> glue

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.NEW_CALENDAR" 
			?ChanTemplate	list( "0" "e" "1" )
			)
;
;    // core states
;    e1of2     -CORE_ERROR;      // 1-bit, device fatal error, core -> glue

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.CORE_ERROR" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     -GOOD_SYNC[0..1]; // 1-bit, is in good sync, core -> glue
;                                // Two signals are needed. Might come at 
;                                // different times. 

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 1 1 )
			?ChanName	"CHAN.GOOD_SYNC" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     -TRAINING_ACTIVE; // 1-bit, is sending training pattern

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.TRAINING_ACTIVE" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     -DIP2_ERROR;      // 1-bit, status bad parity detected

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.DIP2_ERROR" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     -STALL;           // 1-bit, stalled; data flow control

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.STALL" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    // reset output
;    e1of2     +nRST             // hard reset command

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.nRST" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;checkpoint
;    e1of2[3]  +LAST_BYTE;       // number of valid bytes

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 2 1 )
			?ChanName	"CHAN.TX_OUT.LAST_BYTE" 
			?ChanTemplate	list( "0" "e" "1" )
			)

;    e1of2     +SOP;             // start-of-packet channel

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	ONEof2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.TX_OUT.SOP" 
			?ChanTemplate	list( "0" "1" )
			)

;    e1of2     +FBP              // force-bad-parity channel


	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of2onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.TX_OUT.FBP" 
			?ChanTemplate	list( "0" "e" "1" )
			)


;    e1of4     +EOPS; ==> chantxdata[1] // end-of-packet status channel

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	list( X+0 Y+192 )
			?PinTemplate	e1of4onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"CHAN.TX_OUT.EOPS" 
			?ChanTemplate	list( "0" "1" "e" "2" "3" )
			)

;    e1of4[32] +DATA;      ==> chantxdata[4..35]         // data channel
;    e1of4[4]  +PORT_ADDR; ==> chantxdata[36..39]       // port ID channel

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of4onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 31 1 )
			?ChanName	"CHAN.TX_OUT.DATA" 
			?ChanTemplate	list( "0" "1" "e" "2" "3" )
			)

	TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of4onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 3 1 )
			?ChanName	"CHAN.TX_OUT.PORT_ADDR" 
			?ChanTemplate	list( "0" "1" "e" "2" "3" )
			)



; All "tx_nodes" changed to "tx_core"
;    node[8]  +i_calendar_m;     // calendar multiplicity
; ==========================================================

	X = 1012.8 - 28.8 - 4.8
	Y = 1056
	XY= X:Y

	XY=TR_DrawPins(	?PinLpp list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "NODE.i_calendar_m"      ?ChanTemplate nil )
	XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "NODE.i_calendar_len"    ?ChanTemplate nil )
	XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "NODE.i_tsclk_edge"      ?ChanTemplate nil )
	XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "NODE.i_max_bad_parity"  ?ChanTemplate nil )
	XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "NODE.i_max_good_parity" ?ChanTemplate nil )
	XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "NODE.i_alpha"           ?ChanTemplate nil )
	XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 4.8 ) ?BitTemplate list( 0 31 1 ) ?ChanName "NODE.i_data_max_t"      ?ChanTemplate nil )
	XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "NODE.i_stall_wm_low"    ?ChanTemplate nil )
	XY=TR_DrawPins(	?PinLpp list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "NODE.i_stall_wm_high"   ?ChanTemplate nil )

	); end let

); end procedure

; ===============================================================================================================
; ===============================================================================================================
; ===============================================================================================================
; ===============================================================================================================

; ===============================================================================================================

procedure( DrawPins_RX_FM( )

	let((	e1of4onM5
		e1of2onM5
		X Y XY
		)

	X = 259.2 + 1440
	Y = 76.8 + 316.8
	XY = X:Y

	e1of4onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:1.44 0.36:1.80 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.00 0.36:3.36 )
				list( 0:3.72 0.36:4.08 )
				)

	e1of3onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:1.44 0.36:1.80 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.00 0.36:3.36 )
				)

	e1of2onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:2.16 0.36:2.64 )
				list( 0:3.72 0.36:4.08 )
				)

	ONEof2onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:3.72 0.36:4.08 )
				)

	EonM5 = list( 
				list( 0:2.16 0.36:2.64 )
				)

	e1of1onM5 = list( 
				list( 0:0.72 0.36:1.08 )
				list( 0:2.16 0.36:2.64 )
				)

	node4onM7 = list( 
				list( 0:1.00 0.36:1.40 )
				list( 0:1.80 0.36:2.20 )
				list( 0:2.60 0.36:3.00 )
				list( 0:3.40 0.36:3.80 )
				)



	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of1onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	list( 0 3 1 )
			?ChanName	"rx_s2aerr" 
			?ChanTemplate	list( "0" "e")
			)

	XY=TR_DrawPins(	?PinLpp		list( "METAL5" "pin" )
			?PinOrigin	XY
			?PinTemplate	e1of1onM5
			?BitPitch	list( 0 9.6 )
			?BitTemplate	nil
			?ChanName	"rx_watchdog_intr" 
			?ChanTemplate	list( "0" "e" )
			)


;  defchan RxFifoManagerChannels ()(
;  
;      e1of2     -WATCHDOG;                    // watchdog (pseudo-clock ticks)
;      e1of2     -DATA_VALID;                  // data inputs
;      e1of4[4]  -ADDRESS;
;      e1of4[32] -DATA;
;      e1of2[3]  -MOD;
;      e1of4     -EOPS;
;      e1of2     -SOP;
;      e1of2     -BAD_PARITY;
;      e1of2     -SOP_ADDR_BAD_PARITY;
;
;      e1of2     -ALT_DATA_VALID;
;      e1of4[4]  -ALT_ADDRESS;                 // alternative data inputs
;      e1of4[16] -ALT_DATA;
;      e1of4     -ALT_MOD;
;      e1of4     -ALT_EOPS;
;      e1of2     -ALT_SOP;
;      e1of2     -ALT_BAD_PARITY;
;      e1of2     -ALT_SOP_ADDR_BAD_PARITY;
;
;      e1of2     -CALENDAR_RD;                 // configure calendar
;      e1of4[4]  -CALENDAR_RDADDR;
;      e1of4[2]  +CALENDAR_RDDATA;
;      e1of3     +STATUS;                      // status outputs
;      e1of4[2]  +STATUS_ADDRESS;
;      e1of2     -ERROR;                       // core monitor
;      e1of2     -GOOD_SYNC;
;      e1of2     -TRAINING_ACTIVE;
;      //    e1of4[16] -CAPTURE_DATA_TRAIN;
;      e1of2[5]  +SELECT_DATA_TRAIN;
;      e1of2     +nRST                         // reset output
;      ){}


XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.WATCHDOG"            ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.DATA_VALID"          ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_channels.ADDRESS"             ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 31 1 ) ?ChanName "rx_channels.DATA"                ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 2 1 )  ?ChanName "rx_channels.MOD"                 ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.EOPS"                ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.SOP"                 ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.BAD_PARITY"          ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.SOP_ADDR_BAD_PARITY" ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_DATA_VALID"      ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_channels.ALT_ADDRESS"         ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 15 1 ) ?ChanName "rx_channels.ALT_DATA"            ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_MOD"             ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_EOPS"            ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_SOP"             ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_BAD_PARITY"      ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ALT_SOP_ADDR_BAD_PARITY" ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.CALENDAR_RD"         ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_channels.CALENDAR_RDADDR"     ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 1 1 )  ?ChanName "rx_channels.CALENDAR_RDDATA"     ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of3onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.STATUS"              ?ChanTemplate list( "0" "1" "e" "2" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of4onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 1 1 )  ?ChanName "rx_channels.STATUS_ADDRESS"      ?ChanTemplate list( "0" "1" "e" "2" "3" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.ERROR"               ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.GOOD_SYNC"           ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.TRAINING_ACTIVE"     ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 4 1 )  ?ChanName "rx_channels.SELECT_DATA_TRAIN"   ?ChanTemplate list( "0" "e" "1" ))
XY=TR_DrawPins(	?PinLpp	list( "METAL5" "pin" ) ?PinOrigin XY ?PinTemplate e1of2onM5 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_channels.nRST"                ?ChanTemplate list( "0" "e" "1" ))




	X = 259.2 + 1440
	Y = 76.8 + 316.8
	XY = X:Y


;  defchan RxFifoManagerNodes ()(
;  
;      node[8]   +CALENDAR_LEN;
;      node[8]   +CALENDAR_M;
;      node[1]   +RSCLK_EDGE;
;      node[4]   +MIN_GOOD_PARITY;             // misc configure outputs
;      node[4]   +MAX_BAD_PARITY;
;      node[4]   +AVERAGE;

;      node[1]   +ENABLE_MANUAL_DESKEW;
;      node[1]   +ENABLE_STATIC_DESKEW;
;      node[1]   +ENABLE_REALTIME_TUNING;
;      node[85]  +MANUAL_DESKEW
;      ){}
;  


XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "rx_nodes.i_calendar_len"           ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 7 1 )  ?ChanName "rx_nodes.i_calendar_m"             ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_nodes.i_rsclk_edge"             ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_nodes.i_min_good_parity"        ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_nodes.i_max_bad_parity"         ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 3 1 )  ?ChanName "rx_nodes.i_average"                ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_nodes.i_enable_manual_deskew"   ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_nodes.i_enable_static_deskew"   ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate nil            ?ChanName "rx_nodes.i_enable_realtime_tuning" ?ChanTemplate nil )
XY=TR_DrawPins(	?PinLpp	list( "METAL7" "pin" ) ?PinOrigin XY ?PinTemplate node4onM7 ?BitPitch list( 0 9.6 ) ?BitTemplate list( 0 84 1 ) ?ChanName "rx_nodes.i_manual_deskew"          ?ChanTemplate nil )

	); end let

); end procedure



; ===============================================================================================================
; ===============================================================================================================
; ===============================================================================================================
; ===============================================================================================================
