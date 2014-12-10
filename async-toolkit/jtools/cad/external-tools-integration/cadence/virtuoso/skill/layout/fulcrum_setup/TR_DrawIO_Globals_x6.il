; ===================================================================
; This is a procedure used to compile global variables for "x6" Space
; ===================================================================
procedure( DrawIO_ProcCompileGlobals_x6( @key
					( DriverPitch 35.0 )
					)

	let((	
		LVTTL_libName	LVTTL_cellHeight
		LVDS_libName	LVDS_cellHeight
		)

	; Local Variables
	; ===============
	LVTTL_libName = "vendor.nurlogic.io.lvttl"
	LVTTL_cellHeight = 458.40 
	LVDS_libName = "vendor.nurlogic.io.lvds"
	LVDS_cellHeight = 458.40 


	; Globals
	; =======
	; DrawIO_GlobalConfigFile

	DrawIO_GlobalChipWidth   = 12200.0
	DrawIO_GlobalChipHeight  = 11850.0
	DrawIO_GlobalChipOffset  = 458.40 

	if( DriverPitch
		then
			DrawIO_GlobalDriverPitch = DriverPitch
		else
			DrawIO_GlobalDriverPitch = 40.0
	); end if


	DrawIO_GlobalFeederList =
		list(
			list(	list( "LVDS" "F" "F01" )   list( LVDS_libName "vendor.nurlogic.io.lvds.filler_01g.0" "layout" )  list( 0.1 LVDS_cellHeight ) list( 0 0 ) 0 nil nil )
			list(	list( "LVDS" "F" "F1" )    list( LVDS_libName "vendor.nurlogic.io.lvds.filler_1g.0" "layout" )   list( 1 LVDS_cellHeight )   list( 0 0 ) 0 nil nil )
			list(	list( "LVDS" "F" "F2" )    list( LVDS_libName "vendor.nurlogic.io.lvds.filler_2g.0" "layout" )   list( 2 LVDS_cellHeight )   list( 0 0 ) 0 nil nil )
			list(	list( "LVDS" "F" "F4" )    list( LVDS_libName "vendor.nurlogic.io.lvds.filler_4g.0" "layout" )   list( 4 LVDS_cellHeight )   list( 0 0 ) 0 nil nil )
			list(	list( "LVDS" "F" "F8" )    list( LVDS_libName "vendor.nurlogic.io.lvds.filler_8g.0" "layout" )   list( 8 LVDS_cellHeight )   list( 0 0 ) 0 nil nil )

			list(	list( "LVTTL" "F" "F01" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.filler_01g.0" "layout" )  list( 0.1 LVTTL_cellHeight )  list( 0 0 ) 0 nil nil )
			list(	list( "LVTTL" "F" "F1" )   list( LVTTL_libName "vendor.nurlogic.io.lvttl.filler_1g.0" "layout" )   list( 1 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
			list(	list( "LVTTL" "F" "F2" )   list( LVTTL_libName "vendor.nurlogic.io.lvttl.filler_2g.0" "layout" )   list( 2 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
			list(	list( "LVTTL" "F" "F4" )   list( LVTTL_libName "vendor.nurlogic.io.lvttl.filler_4g.0" "layout" )   list( 4 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
			list(	list( "LVTTL" "F" "F8" )   list( LVTTL_libName "vendor.nurlogic.io.lvttl.filler_8g.0" "layout" )   list( 8 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
			list(	list( "LVTTL" "F" "F16" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.filler_16g.0" "layout" )  list( 16 LVTTL_cellHeight )   list( 0 0 ) 0 nil nil )
			list(	list( "LVTTL" "F" "F32" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.filler_32g.0" "layout" )  list( 32 LVTTL_cellHeight )   list( 0 0 ) 0 nil nil )
			list(	list( "LVTTL" "F" "F64" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.filler_64g.0" "layout" )  list( 64 LVTTL_cellHeight )   list( 0 0 ) 0 nil nil )

			list(	list( "RingBreaker" "F" "RB3" )    list( LVDS_libName "vendor.nurlogic.io.lvds.filler_brk_3g.0" "layout" )  list( 3 LVDS_cellHeight )  list( 0 0 ) 0 nil nil ) 

			)
		

	DrawIO_GlobalCornerPadList =
	list(
		list(	list( "LVDS" "C" "CORNERPAD" )  list( LVDS_libName "vendor.nurlogic.io.lvds.iocrnr.0" "layout" ) list( LVDS_cellHeight LVDS_cellHeight )   list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "C" "CORNERPAD" ) list( LVTTL_libName "vendor.nurlogic.io.lvttl.iocrnr.0" "layout" )     list( LVTTL_cellHeight LVTTL_cellHeight ) list( 0 0 ) 0 nil nil )
		)

	DrawIO_GlobalSignalDriverList =
	list(
		list(	list( "LVDS"  "S" "IN" )       list( LVDS_libName "vendor.nurlogic.io.lvds.sp_se_in.0" "layout" )        list( 70 LVDS_cellHeight )  list( 0 0 ) 2 list( 0 35 )      list( "DI" "GND" "PADN" "PADP" "PD" "SGND" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS"  "S" "OUT_VC" )   list( LVDS_libName "vendor.nurlogic.io.lvds.85_out_vc.0" "layout" )   list( 120 LVDS_cellHeight ) list( 0 0 ) 3 list( 2.5 40 40 ) list( "DO" "GND" "OE" "PAD" "PADN" "PADP" "SGND" "TN" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS"  "S" "OUT_VO" )   list( LVDS_libName "vendor.nurlogic.io.lvds.85_out_vo.0" "layout" )   list( 120 LVDS_cellHeight ) list( 0 0 ) 3 list( 2.5 40 40 ) list( "DO" "GND" "OE" "PAD" "PADN" "PADP" "SGND" "TN" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS"  "S" "OUT_VOP" )  list( LVDS_libName "vendor.nurlogic.io.lvds.85_out_vop.0" "layout" )   list( 120 LVDS_cellHeight ) list( 0 0 ) 3 list( 2.5 40 40 ) list( "DO" "GND" "OE" "PAD" "PADN" "PADP" "SGND" "TN" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS"  "S" "OUT_GC" )   list( LVDS_libName "vendor.nurlogic.io.lvds.85_out_gcs.0" "layout" )   list( 120 LVDS_cellHeight ) list( 0 0 ) 3 list( 2.5 40 40 ) list( "DO" "GND" "OE" "PAD" "PADN" "PADP" "SGND" "TN" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS"  "S" "OUT_GO" )   list( LVDS_libName "vendor.nurlogic.io.lvds.85_out_go.0" "layout" )   list( 120 LVDS_cellHeight ) list( 0 0 ) 3 list( 2.5 40 40 ) list( "DO" "GND" "OE" "PAD" "PADN" "PADP" "SGND" "TN" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS"  "S" "OUT_VREF" ) list( LVDS_libName "vendor.nurlogic.io.lvds.85_out_vref.0" "layout" ) list( 120 LVDS_cellHeight ) list( 0 0 ) 3 list( 2.5 40 40 ) list( "DO" "GND" "OE" "PAD" "PADN" "PADP" "SGND" "TN" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS"  "S" "VREF" )     list( LVDS_libName "vendor.nurlogic.io.lvds.vref.0" "layout" )        list( 35 LVDS_cellHeight )  list( 0 0 ) 1 list( 0 )         list( "GND" "PAD" "SGND" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))

		list(	list( "LVTTL" "S" "INnn2" )    list( LVTTL_libName "vendor.nurlogic.io.lvttl.it2nn2.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "INpd2" )    list( LVTTL_libName "vendor.nurlogic.io.lvttl.it2pd2.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "INpd2_2" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.it2pd2_2.0" "layout" )     list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_GND" "n_PAD" "n_PI" "n_PO" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "INpu2" )    list( LVTTL_libName "vendor.nurlogic.io.lvttl.it2pu2.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "INnn8" )    list( LVTTL_libName "vendor.nurlogic.io.lvttl.it2nn8.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "INpd8" )    list( LVTTL_libName "vendor.nurlogic.io.lvttl.it2pd8.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "INOUT2nn2" )    list( LVTTL_libName "vendor.nurlogic.io.lvttl.tf02it0nn2.0" "layout" )   list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_DO" "n_GND" "n_OE" "n_PAD" "n_SGND" "n_TN" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "INOUT4nn2" )    list( LVTTL_libName "vendor.nurlogic.io.lvttl.tf04it0nn2.0" "layout" )   list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_DO" "n_GND" "n_OE" "n_PAD" "n_SGND" "n_TN" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "INOUT8nn2" )    list( LVTTL_libName "vendor.nurlogic.io.lvttl.tf08it0nn2.0" "layout" )   list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_DIN" "n_DO" "n_GND" "n_OE" "n_PAD" "n_SGND" "n_TN" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "S" "AIO" )      list( LVTTL_libName "vendor.nurlogic.io.lvttl.aio.0" "layout" )   list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_GND" "n_PAD" "n_PADX" "n_RIN" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		)

	DrawIO_GlobalPowerRingHighDriverList =
	list(
		list(	list( "LVDS" "PRH" "VP" )   list( LVDS_libName "vendor.nurlogic.io.lvds.vp_lvds.0" "layout" )   list( 35 LVDS_cellHeight ) list( 0 0 ) 1 list( 0 )   list( "GND" "PAD" "SGND" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS" "PRH" "VO" )   list( LVDS_libName "vendor.nurlogic.io.lvds.vo_lvds.0" "layout" )   list( 35 LVDS_cellHeight ) list( 0 0 ) 1 list( 0 )   list( "GND" "PAD" "SGND" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVDS" "PRH" "VOP" )  list( LVDS_libName "vendor.nurlogic.io.lvds.vop_lvds.0" "layout" )   list( 35 LVDS_cellHeight ) list( 0 0 ) 1 list( 0 )   list( "GND" "PAD" "SGND" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))

		list(	list( "LVTTL" "PRH" "VP" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.vp.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "PRH" "VOO" ) list( LVTTL_libName "vendor.nurlogic.io.lvttl.voo.0" "layout" )      list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "PRH" "VOP" ) list( LVTTL_libName "vendor.nurlogic.io.lvttl.vop.0" "layout" )      list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))

		list(	list( "LVTTL" "PRH" "VOP_2" ) list( LVTTL_libName "vendor.nurlogic.io.lvttl.vop.1" "layout" )      list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "PRH" "VP_2" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.vp.1" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 )  list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))

		)


	DrawIO_GlobalPowerRingLowDriverList =
	list(
		list(	list( "LVDS" "PRL" "GO" )   list( LVDS_libName "vendor.nurlogic.io.lvds.go_lvds.0" "layout" )   list( 35 LVDS_cellHeight )  list( 0 0 ) 1 list( 0 ) list( "GND" "PAD" "SGND" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVTTL" "PRL" "GO" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.go.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 ) list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		list(	list( "LVTTL" "PRL" "GO_2" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.go.1" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 ) list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		)

	DrawIO_GlobalPowerCoreHighDriverList =
	list(
		list(	list( "LVDS" "PCH" "VC" )   list( LVDS_libName "vendor.nurlogic.io.lvds.vc_lvds.0" "layout" )   list( 35 LVDS_cellHeight )  list( 0 0 ) 1 list( 0 ) list( "GND" "PAD" "SGND" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVTTL" "PCH" "VC" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.vc.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 ) list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		)

	DrawIO_GlobalPowerCoreLowDriverList =
	list(
		list(	list( "LVDS" "PRL" "GCS" )   list( LVDS_libName "vendor.nurlogic.io.lvds.gcs_lvds.0" "layout" )   list( 35 LVDS_cellHeight )  list( 0 0 ) 1 list( 0 ) list( "GND" "PAD" "SGND" "VDD" "VDDO" "VDDP" "VGG" "VREF" "VSSO" ))
		list(	list( "LVTTL" "PCL" "GCS" )  list( LVTTL_libName "vendor.nurlogic.io.lvttl.gcs.0" "layout" )       list( 35 LVTTL_cellHeight ) list( 0 0 ) 1 list( 0 ) list( "n_GND" "n_PAD" "n_SGND" "n_VDD" "n_VDDO" "n_VDDP" "n_VGG" "n_VSSO" ))
		)


	; Use LVTTL Bond Pads for LVDS Drivers
	; ====================================
	DrawIO_GlobalBondPadList =
	list(
		list(	list( "LVDS" "B" "BI" )  list( LVDS_libName "vendor.nurlogic.io.lvds.stgin_50x50.0" "layout" )  list( 35.0 LVDS_cellHeight ) list( 0.0 0.0 ) 0 nil nil )
		list(	list( "LVDS" "B" "BO" )  list( LVDS_libName "vendor.nurlogic.io.lvds.stgout_50x50.0" "layout" ) list( 35.0 LVDS_cellHeight ) list( 0.0 0.0 ) 0 nil nil )
		list(	list( "LVTTL" "B" "BI" ) list( LVTTL_libName "vendor.nurlogic.io.lvttl.CUpadTSMC_stgin_50x50.0" "layout" )  list( 35.0 LVTTL_cellHeight ) list( 0.0 0.0 ) 0 nil nil )
		list(	list( "LVTTL" "B" "BO" ) list( LVTTL_libName "vendor.nurlogic.io.lvttl.CUpadTSMC_stgout_50x50.0" "layout" ) list( 35.0 LVTTL_cellHeight ) list( 0.0 0.0 ) 0 nil nil )
		)

	DrawIO_GlobalDummyDriverList =
	list( 	
		list(	list( "LVDS" "D" "D1" )       nil list( 35.0 LVDS_cellHeight )    list( 0 0 ) 1 nil nil )
		list(	list( "LVDS" "D" "D3_4u" )    nil list( 3.4  LVDS_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVDS" "D" "D5_0u" )    nil list( 5.0  LVDS_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVDS" "D" "D13_0u" )   nil list( 13.0 LVDS_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVDS" "D" "D20_0u" )   nil list( 20.0 LVDS_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVDS" "D" "D28_8u" )   nil list( 28.8 LVDS_cellHeight )    list( 0 0 ) 0 nil nil )


		list(	list( "LVTTL" "D" "D1" )       nil list( 35.0 LVTTL_cellHeight )    list( 0 0 ) 1 nil nil )
		list(	list( "LVTTL" "D" "D0_6u" )    nil list( 0.6 LVTTL_cellHeight )     list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D2_0u" )    nil list( 2.0 LVTTL_cellHeight )     list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D8_0u" )    nil list( 8.0 LVTTL_cellHeight )     list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D9_6u" )    nil list( 9.6 LVTTL_cellHeight )     list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D12_0u" )   nil list( 12.0 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D10_0u" )   nil list( 10.0 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D16_4u" )   nil list( 16.4 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D17_0u" )   nil list( 17.0 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D26_0u" )   nil list( 26.0 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
		list(	list( "LVTTL" "D" "D26_6u" )   nil list( 26.6 LVTTL_cellHeight )    list( 0 0 ) 0 nil nil )
		)

;	DrawIO_GlobalRawIOFile       = "/home/user/samson/myskill/IO.input.x6.OutputTest"
	DrawIO_GlobalRawIOFile       = "/home/user/samson/myskill/IO.input.x6"
	DrawIO_GlobalRawIOList       = nil
	DrawIO_GlobalRawIOList_Alias = nil

	DrawIO_GlobalAliasFile = "/home/user/samson/myskill/IO.input.x6.alias"
	DrawIO_GlobalAliasList = nil

	DrawIO_GlobalFullIOList = nil

	); end let

	t

); end procedure




