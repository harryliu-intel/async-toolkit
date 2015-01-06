; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/layout/cable/templates.il#1 $
; $DateTime: 2012/07/27 17:24:08 $
; $Author: pankala $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar CableBufferSplitIdx 12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLCreateTempArray (Template)
 let( (
       TempArray
       )

   declare(TempArray[180])

   case( Template

       ( "Template_DENSE"
           TempArray[0]  =  list(   "[0]"  dense_e1of4   0*TrackPitch   BusMetal45 )
           TempArray[1]  =  list(   "[1]"  dense_e1of4   2*TrackPitch   BusMetal45 )

           TempArray[2]  =  list(   "[2]"  dense_e1of4   0*TrackPitch   BusMetal67 )
           TempArray[3]  =  list(   "[3]"  dense_e1of4   1*TrackPitch   BusMetal67 )
           TempArray[4]  =  list(   "[4]"  dense_e1of4   2*TrackPitch   BusMetal67 )
           TempArray[5]  =  list(   "[5]"  dense_e1of4   3*TrackPitch   BusMetal67 )

           TempArray[6]  =  list(   "[6]"  dense_e1of4   4*TrackPitch   BusMetal23 )
           TempArray[7]  =  list(   "[7]"  dense_e1of4   4*TrackPitch   BusMetal45 )
           TempArray[8]  =  list(   "[8]"  dense_e1of4   4*TrackPitch   BusMetal67 )

           TempArray[9]  =  list(   "[9]"  dense_e1of4   5*TrackPitch   BusMetal23 )
          TempArray[10]  =  list(  "[10]"  dense_e1of4   5*TrackPitch   BusMetal45 )
          TempArray[11]  =  list(  "[11]"  dense_e1of4   5*TrackPitch   BusMetal67 )

          TempArray[12]  =  list(  "[12]"  dense_e1of4   6*TrackPitch   BusMetal23 )
          TempArray[13]  =  list(  "[13]"  dense_e1of4   6*TrackPitch   BusMetal45 )
          TempArray[14]  =  list(  "[14]"  dense_e1of4   6*TrackPitch   BusMetal67 )

          TempArray[15]  =  list(  "[15]"  dense_e1of4   7*TrackPitch   BusMetal23 )
          TempArray[16]  =  list(  "[16]"  dense_e1of4   7*TrackPitch   BusMetal45 )
          TempArray[17]  =  list(  "[17]"  dense_e1of4   7*TrackPitch   BusMetal67 )

          TempArray[18]  =  list(  "[18]"  dense_e1of4   8*TrackPitch   BusMetal23 )
          TempArray[19]  =  list(  "[19]"  dense_e1of4   8*TrackPitch   BusMetal45 )
          TempArray[20]  =  list(  "[20]"  dense_e1of4   8*TrackPitch   BusMetal67 )

          TempArray[21]  =  list(  "[21]"  dense_e1of4   9*TrackPitch   BusMetal23 )
          TempArray[22]  =  list(  "[22]"  dense_e1of4   9*TrackPitch   BusMetal45 )
          TempArray[23]  =  list(  "[23]"  dense_e1of4   9*TrackPitch   BusMetal67 )

          TempArray[24]  =  list(  "[24]"  dense_e1of4  10*TrackPitch   BusMetal23 )
          TempArray[25]  =  list(  "[25]"  dense_e1of4  10*TrackPitch   BusMetal45 )
          TempArray[26]  =  list(  "[26]"  dense_e1of4  10*TrackPitch   BusMetal67 )

          TempArray[27]  =  list(  "[27]"  dense_e1of4  11*TrackPitch   BusMetal23 )
          TempArray[28]  =  list(  "[28]"  dense_e1of4  11*TrackPitch   BusMetal45 )
          TempArray[29]  =  list(  "[29]"  dense_e1of4  11*TrackPitch   BusMetal67 )
        )

       ( "Template_DENSE_R90"
           TempArray[0]  =  list(   "[0]"  dense_e1of4   0*TrackPitch   BusMetal54 )
           TempArray[1]  =  list(   "[1]"  dense_e1of4   2*TrackPitch   BusMetal54 )

           TempArray[2]  =  list(   "[2]"  dense_e1of4   0*TrackPitch   BusMetal76 )
           TempArray[3]  =  list(   "[3]"  dense_e1of4   1*TrackPitch   BusMetal76 )
           TempArray[4]  =  list(   "[4]"  dense_e1of4   2*TrackPitch   BusMetal76 )
           TempArray[5]  =  list(   "[5]"  dense_e1of4   3*TrackPitch   BusMetal76 )

           TempArray[6]  =  list(   "[6]"  dense_e1of4   4*TrackPitch   BusMetal32 )
           TempArray[7]  =  list(   "[7]"  dense_e1of4   4*TrackPitch   BusMetal54 )
           TempArray[8]  =  list(   "[8]"  dense_e1of4   4*TrackPitch   BusMetal76 )

           TempArray[9]  =  list(   "[9]"  dense_e1of4   5*TrackPitch   BusMetal32 )
          TempArray[10]  =  list(  "[10]"  dense_e1of4   5*TrackPitch   BusMetal54 )
          TempArray[11]  =  list(  "[11]"  dense_e1of4   5*TrackPitch   BusMetal76 )

          TempArray[12]  =  list(  "[12]"  dense_e1of4   6*TrackPitch   BusMetal32 )
          TempArray[13]  =  list(  "[13]"  dense_e1of4   6*TrackPitch   BusMetal54 )
          TempArray[14]  =  list(  "[14]"  dense_e1of4   6*TrackPitch   BusMetal76 )

          TempArray[15]  =  list(  "[15]"  dense_e1of4   7*TrackPitch   BusMetal32 )
          TempArray[16]  =  list(  "[16]"  dense_e1of4   7*TrackPitch   BusMetal54 )
          TempArray[17]  =  list(  "[17]"  dense_e1of4   7*TrackPitch   BusMetal76 )

          TempArray[18]  =  list(  "[18]"  dense_e1of4   8*TrackPitch   BusMetal32 )
          TempArray[19]  =  list(  "[19]"  dense_e1of4   8*TrackPitch   BusMetal54 )
          TempArray[20]  =  list(  "[20]"  dense_e1of4   8*TrackPitch   BusMetal76 )

          TempArray[21]  =  list(  "[21]"  dense_e1of4   9*TrackPitch   BusMetal32 )
          TempArray[22]  =  list(  "[22]"  dense_e1of4   9*TrackPitch   BusMetal54 )
          TempArray[23]  =  list(  "[23]"  dense_e1of4   9*TrackPitch   BusMetal76 )

          TempArray[24]  =  list(  "[24]"  dense_e1of4  10*TrackPitch   BusMetal32 )
          TempArray[25]  =  list(  "[25]"  dense_e1of4  10*TrackPitch   BusMetal54 )
          TempArray[26]  =  list(  "[26]"  dense_e1of4  10*TrackPitch   BusMetal76 )

          TempArray[27]  =  list(  "[27]"  dense_e1of4  11*TrackPitch   BusMetal32 )
          TempArray[28]  =  list(  "[28]"  dense_e1of4  11*TrackPitch   BusMetal54 )
          TempArray[29]  =  list(  "[29]"  dense_e1of4  11*TrackPitch   BusMetal76 )
        )

       ( "Template_TS"
           TempArray[0]  = list(   "[0]"    ts_e1of4    0*TrackPitch   BusMetal45 )
           TempArray[1]  = list(   "[1]"    ts_e1of4    1*TrackPitch   BusMetal45 )
           TempArray[2]  = list(   "[2]"    ts_e1of4    3*TrackPitch   BusMetal45 )

           TempArray[3]  = list(   "[3]"    ts_e1of4    0*TrackPitch   BusMetal67 )
           TempArray[4]  = list(   "[4]"    ts_e1of4    1*TrackPitch   BusMetal67 )
           TempArray[5]  = list(   "[5]"    ts_e1of4    2*TrackPitch   BusMetal67 )
           TempArray[6]  = list(   "[6]"    ts_e1of4    3*TrackPitch   BusMetal67 )

           TempArray[7]  = list(   "[7]"    ts_e1of4    4*TrackPitch   BusMetal45 )
           TempArray[8]  = list(   "[8]"    ts_e1of4    4*TrackPitch   BusMetal67 )

           TempArray[9]  = list(   "[9]"    ts_e1of4    5*TrackPitch   BusMetal23 )
          TempArray[10]  = list(  "[10]"    ts_e1of4    5*TrackPitch   BusMetal45 )
          TempArray[11]  = list(  "[11]"    ts_e1of4    5*TrackPitch   BusMetal67 )

          TempArray[12]  = list(  "[12]"    ts_e1of4    6*TrackPitch   BusMetal23 )
          TempArray[13]  = list(  "[13]"    ts_e1of4    6*TrackPitch   BusMetal45 )
          TempArray[14]  = list(  "[14]"    ts_e1of4    6*TrackPitch   BusMetal67 )

          TempArray[15]  = list(  "[15]"    ts_e1of4    7*TrackPitch   BusMetal23 )
          TempArray[16]  = list(  "[16]"    ts_e1of4    7*TrackPitch   BusMetal45 )
          TempArray[17]  = list(  "[17]"    ts_e1of4    7*TrackPitch   BusMetal67 )

          TempArray[18]  = list(  "[18]"    ts_e1of4    8*TrackPitch   BusMetal23 )
          TempArray[19]  = list(  "[19]"    ts_e1of4    8*TrackPitch   BusMetal45 )
          TempArray[20]  = list(  "[20]"    ts_e1of4    8*TrackPitch   BusMetal67 )

          TempArray[21]  = list(  "[21]"    ts_e1of4    9*TrackPitch   BusMetal23 )
          TempArray[22]  = list(  "[22]"    ts_e1of4    9*TrackPitch   BusMetal45 )
          TempArray[23]  = list(  "[23]"    ts_e1of4    9*TrackPitch   BusMetal67 )

          TempArray[24]  = list(  "[24]"    ts_e1of4   10*TrackPitch   BusMetal23 )
          TempArray[25]  = list(  "[25]"    ts_e1of4   10*TrackPitch   BusMetal45 )
          TempArray[26]  = list(  "[26]"    ts_e1of4   10*TrackPitch   BusMetal67 )

          TempArray[27]  = list(  "[27]"    ts_e1of4   11*TrackPitch   BusMetal23 )
          TempArray[28]  = list(  "[28]"    ts_e1of4   11*TrackPitch   BusMetal45 )
          TempArray[29]  = list(  "[29]"    ts_e1of4   11*TrackPitch   BusMetal67 )
        )

       ( "Template_TS_R90"
           TempArray[0]  = list(   "[0]"    ts_e1of4    0*TrackPitch   BusMetal54 )
           TempArray[1]  = list(   "[1]"    ts_e1of4    1*TrackPitch   BusMetal54 )
           TempArray[2]  = list(   "[2]"    ts_e1of4    3*TrackPitch   BusMetal54 )

           TempArray[3]  = list(   "[3]"    ts_e1of4    0*TrackPitch   BusMetal76 )
           TempArray[4]  = list(   "[4]"    ts_e1of4    1*TrackPitch   BusMetal76 )
           TempArray[5]  = list(   "[5]"    ts_e1of4    2*TrackPitch   BusMetal76 )
           TempArray[6]  = list(   "[6]"    ts_e1of4    3*TrackPitch   BusMetal76 )

           TempArray[7]  = list(   "[7]"    ts_e1of4    4*TrackPitch   BusMetal54 )
           TempArray[8]  = list(   "[8]"    ts_e1of4    4*TrackPitch   BusMetal76 )

           TempArray[9]  = list(   "[9]"    ts_e1of4    5*TrackPitch   BusMetal32 )
          TempArray[10]  = list(  "[10]"    ts_e1of4    5*TrackPitch   BusMetal54 )
          TempArray[11]  = list(  "[11]"    ts_e1of4    5*TrackPitch   BusMetal76 )

          TempArray[12]  = list(  "[12]"    ts_e1of4    6*TrackPitch   BusMetal32 )
          TempArray[13]  = list(  "[13]"    ts_e1of4    6*TrackPitch   BusMetal54 )
          TempArray[14]  = list(  "[14]"    ts_e1of4    6*TrackPitch   BusMetal76 )

          TempArray[15]  = list(  "[15]"    ts_e1of4    7*TrackPitch   BusMetal32 )
          TempArray[16]  = list(  "[16]"    ts_e1of4    7*TrackPitch   BusMetal54 )
          TempArray[17]  = list(  "[17]"    ts_e1of4    7*TrackPitch   BusMetal76 )

          TempArray[18]  = list(  "[18]"    ts_e1of4    8*TrackPitch   BusMetal32 )
          TempArray[19]  = list(  "[19]"    ts_e1of4    8*TrackPitch   BusMetal54 )
          TempArray[20]  = list(  "[20]"    ts_e1of4    8*TrackPitch   BusMetal76 )

          TempArray[21]  = list(  "[21]"    ts_e1of4    9*TrackPitch   BusMetal32 )
          TempArray[22]  = list(  "[22]"    ts_e1of4    9*TrackPitch   BusMetal54 )
          TempArray[23]  = list(  "[23]"    ts_e1of4    9*TrackPitch   BusMetal76 )

          TempArray[24]  = list(  "[24]"    ts_e1of4   10*TrackPitch   BusMetal32 )
          TempArray[25]  = list(  "[25]"    ts_e1of4   10*TrackPitch   BusMetal54 )
          TempArray[26]  = list(  "[26]"    ts_e1of4   10*TrackPitch   BusMetal76 )

          TempArray[27]  = list(  "[27]"    ts_e1of4   11*TrackPitch   BusMetal32 )
          TempArray[28]  = list(  "[28]"    ts_e1of4   11*TrackPitch   BusMetal54 )
          TempArray[29]  = list(  "[29]"    ts_e1of4   11*TrackPitch   BusMetal76 )
        )


       ( "Template_NYB"
           TempArray[0]  =  list(   "[0]"  nyb_e1of4   0*TrackPitch   BusMetal45 )
           TempArray[1]  =  list(   "[1]"  nyb_e1of4   1*TrackPitch   BusMetal45 )

           TempArray[2]  =  list(   "[2]"  nyb_e1of4   0*TrackPitch   BusMetal67 )
           TempArray[3]  =  list(   "[3]"  nyb_e1of4   1*TrackPitch   BusMetal67 )
           TempArray[4]  =  list(   "[4]"  nyb_e1of4   2*TrackPitch   BusMetal67 )
           TempArray[5]  =  list(   "[5]"  nyb_e1of4   3*TrackPitch   BusMetal67 )

           TempArray[6]  =  list(   "[6]"  nyb_e1of4   4*TrackPitch   BusMetal23 )
           TempArray[7]  =  list(   "[7]"  nyb_e1of4   4*TrackPitch   BusMetal45 )
           TempArray[8]  =  list(   "[8]"  nyb_e1of4   4*TrackPitch   BusMetal67 )

           TempArray[9]  =  list(   "[9]"  nyb_e1of4   5*TrackPitch   BusMetal23 )
          TempArray[10]  =  list(  "[10]"  nyb_e1of4   5*TrackPitch   BusMetal45 )
          TempArray[11]  =  list(  "[11]"  nyb_e1of4   5*TrackPitch   BusMetal67 )

          TempArray[12]  =  list(  "[12]"  nyb_e1of4   6*TrackPitch   BusMetal23 )
          TempArray[13]  =  list(  "[13]"  nyb_e1of4   6*TrackPitch   BusMetal45 )
          TempArray[14]  =  list(  "[14]"  nyb_e1of4   6*TrackPitch   BusMetal67 )

          TempArray[15]  =  list(  "[15]"  nyb_e1of4   7*TrackPitch   BusMetal23 )
          TempArray[16]  =  list(  "[16]"  nyb_e1of4   7*TrackPitch   BusMetal45 )
          TempArray[17]  =  list(  "[17]"  nyb_e1of4   7*TrackPitch   BusMetal67 )

          TempArray[18]  =  list(  "[18]"  nyb_e1of4   8*TrackPitch   BusMetal23 )
          TempArray[19]  =  list(  "[19]"  nyb_e1of4   8*TrackPitch   BusMetal45 )
          TempArray[20]  =  list(  "[20]"  nyb_e1of4   8*TrackPitch   BusMetal67 )

          TempArray[21]  =  list(  "[21]"  nyb_e1of4   9*TrackPitch   BusMetal23 )
          TempArray[22]  =  list(  "[22]"  nyb_e1of4   9*TrackPitch   BusMetal45 )
          TempArray[23]  =  list(  "[23]"  nyb_e1of4   9*TrackPitch   BusMetal67 )

          TempArray[24]  =  list(  "[24]"  nyb_e1of4  10*TrackPitch   BusMetal23 )
          TempArray[25]  =  list(  "[25]"  nyb_e1of4  10*TrackPitch   BusMetal45 )
          TempArray[26]  =  list(  "[26]"  nyb_e1of4  10*TrackPitch   BusMetal67 )

          TempArray[27]  =  list(  "[27]"  nyb_e1of4  11*TrackPitch   BusMetal23 )
          TempArray[28]  =  list(  "[28]"  nyb_e1of4  11*TrackPitch   BusMetal45 )
          TempArray[29]  =  list(  "[29]"  nyb_e1of4  11*TrackPitch   BusMetal67 )
        )

       ( "Template_NYB_R90"
           TempArray[0]  =  list(   "[0]"  nyb_e1of4   0*TrackPitch   BusMetal54 )
           TempArray[1]  =  list(   "[1]"  nyb_e1of4   1*TrackPitch   BusMetal54 )

           TempArray[2]  =  list(   "[2]"  nyb_e1of4   0*TrackPitch   BusMetal76 )
           TempArray[3]  =  list(   "[3]"  nyb_e1of4   1*TrackPitch   BusMetal76 )
           TempArray[4]  =  list(   "[4]"  nyb_e1of4   2*TrackPitch   BusMetal76 )
           TempArray[5]  =  list(   "[5]"  nyb_e1of4   3*TrackPitch   BusMetal76 )

           TempArray[6]  =  list(   "[6]"  nyb_e1of4   4*TrackPitch   BusMetal32 )
           TempArray[7]  =  list(   "[7]"  nyb_e1of4   4*TrackPitch   BusMetal54 )
           TempArray[8]  =  list(   "[8]"  nyb_e1of4   4*TrackPitch   BusMetal76 )

           TempArray[9]  =  list(   "[9]"  nyb_e1of4   5*TrackPitch   BusMetal32 )
          TempArray[10]  =  list(  "[10]"  nyb_e1of4   5*TrackPitch   BusMetal54 )
          TempArray[11]  =  list(  "[11]"  nyb_e1of4   5*TrackPitch   BusMetal76 )

          TempArray[12]  =  list(  "[12]"  nyb_e1of4   6*TrackPitch   BusMetal32 )
          TempArray[13]  =  list(  "[13]"  nyb_e1of4   6*TrackPitch   BusMetal54 )
          TempArray[14]  =  list(  "[14]"  nyb_e1of4   6*TrackPitch   BusMetal76 )

          TempArray[15]  =  list(  "[15]"  nyb_e1of4   7*TrackPitch   BusMetal32 )
          TempArray[16]  =  list(  "[16]"  nyb_e1of4   7*TrackPitch   BusMetal54 )
          TempArray[17]  =  list(  "[17]"  nyb_e1of4   7*TrackPitch   BusMetal76 )

          TempArray[18]  =  list(  "[18]"  nyb_e1of4   8*TrackPitch   BusMetal32 )
          TempArray[19]  =  list(  "[19]"  nyb_e1of4   8*TrackPitch   BusMetal54 )
          TempArray[20]  =  list(  "[20]"  nyb_e1of4   8*TrackPitch   BusMetal76 )

          TempArray[21]  =  list(  "[21]"  nyb_e1of4   9*TrackPitch   BusMetal32 )
          TempArray[22]  =  list(  "[22]"  nyb_e1of4   9*TrackPitch   BusMetal54 )
          TempArray[23]  =  list(  "[23]"  nyb_e1of4   9*TrackPitch   BusMetal76 )

          TempArray[24]  =  list(  "[24]"  nyb_e1of4  10*TrackPitch   BusMetal32 )
          TempArray[25]  =  list(  "[25]"  nyb_e1of4  10*TrackPitch   BusMetal54 )
          TempArray[26]  =  list(  "[26]"  nyb_e1of4  10*TrackPitch   BusMetal76 )

          TempArray[27]  =  list(  "[27]"  nyb_e1of4  11*TrackPitch   BusMetal32 )
          TempArray[28]  =  list(  "[28]"  nyb_e1of4  11*TrackPitch   BusMetal54 )
          TempArray[29]  =  list(  "[29]"  nyb_e1of4  11*TrackPitch   BusMetal76 )
        )

       ( "Template_XBUS"
          TempArray[0]  = list( ".D[0]"       inlv_e1of4_A 0*TrackPitch BusMetal45)
          TempArray[1]  = list( ".D[1]"       inlv_e1of4_B 0*TrackPitch BusMetal45)
          TempArray[2]  = list( ".D[2]"       inlv_e1of4_A 3*TrackPitch BusMetal45)
          TempArray[3]  = list( ".D[3]"       inlv_e1of4_B 3*TrackPitch BusMetal45)
          TempArray[4]  = list( ".D[4]"       inlv_e1of4_A 0*TrackPitch BusMetal67)
          TempArray[5]  = list( ".D[5]"       inlv_e1of4_B 0*TrackPitch BusMetal67)
          TempArray[6]  = list( ".D[6]"       inlv_e1of4_A 1*TrackPitch BusMetal67)
          TempArray[7]  = list( ".D[7]"       inlv_e1of4_B 1*TrackPitch BusMetal67)
          TempArray[8]  = list( ".D[8]"       inlv_e1of4_A 2*TrackPitch BusMetal67)
          TempArray[9]  = list( ".D[9]"       inlv_e1of4_B 2*TrackPitch BusMetal67)
          TempArray[10] = list( ".D[10]"      inlv_e1of4_A 3*TrackPitch BusMetal67)
          TempArray[11] = list( ".D[11]"      inlv_e1of4_B 3*TrackPitch BusMetal67)
          TempArray[12] = list( ".D[12]"      inlv_e1of4_A 4*TrackPitch BusMetal23)
          TempArray[13] = list( ".D[13]"      inlv_e1of4_B 4*TrackPitch BusMetal23)
          TempArray[14] = list( ".D[14]"      inlv_e1of4_A 4*TrackPitch BusMetal45)
          TempArray[15] = list( ".D[15]"      inlv_e1of4_B 4*TrackPitch BusMetal45)
          TempArray[16] = list( ".D[16]"      inlv_e1of4_A 4*TrackPitch BusMetal67)
          TempArray[17] = list( ".D[17]"      inlv_e1of4_B 4*TrackPitch BusMetal67)

          TempArray[18] = list( ".A[0]"      inlv_e1of4_A 5*TrackPitch BusMetal23)
          TempArray[19] = list( ".A[1]"      inlv_e1of4_B 5*TrackPitch BusMetal23)
          TempArray[20] = list( ".T"         inlv_e1of2_A 5*TrackPitch BusMetal45)
        )

       ( "Template_XBUS_R90"
          TempArray[0]  = list( ".D[0]"       inlv_e1of4_A 0*TrackPitch BusMetal54)
          TempArray[1]  = list( ".D[1]"       inlv_e1of4_B 0*TrackPitch BusMetal54)
          TempArray[2]  = list( ".D[2]"       inlv_e1of4_A 3*TrackPitch BusMetal54)
          TempArray[3]  = list( ".D[3]"       inlv_e1of4_B 3*TrackPitch BusMetal54)
          TempArray[4]  = list( ".D[4]"       inlv_e1of4_A 0*TrackPitch BusMetal76)
          TempArray[5]  = list( ".D[5]"       inlv_e1of4_B 0*TrackPitch BusMetal76)
          TempArray[6]  = list( ".D[6]"       inlv_e1of4_A 1*TrackPitch BusMetal76)
          TempArray[7]  = list( ".D[7]"       inlv_e1of4_B 1*TrackPitch BusMetal76)
          TempArray[8]  = list( ".D[8]"       inlv_e1of4_A 2*TrackPitch BusMetal76)
          TempArray[9]  = list( ".D[9]"       inlv_e1of4_B 2*TrackPitch BusMetal76)
          TempArray[10] = list( ".D[10]"      inlv_e1of4_A 3*TrackPitch BusMetal76)
          TempArray[11] = list( ".D[11]"      inlv_e1of4_B 3*TrackPitch BusMetal76)
          TempArray[12] = list( ".D[12]"      inlv_e1of4_A 4*TrackPitch BusMetal32)
          TempArray[13] = list( ".D[13]"      inlv_e1of4_B 4*TrackPitch BusMetal32)
          TempArray[14] = list( ".D[14]"      inlv_e1of4_A 4*TrackPitch BusMetal54)
          TempArray[15] = list( ".D[15]"      inlv_e1of4_B 4*TrackPitch BusMetal54)
          TempArray[16] = list( ".D[16]"      inlv_e1of4_A 4*TrackPitch BusMetal76)
          TempArray[17] = list( ".D[17]"      inlv_e1of4_B 4*TrackPitch BusMetal76)

          TempArray[18] = list( ".A[0]"       inlv_e1of4_A 5*TrackPitch BusMetal32)
          TempArray[19] = list( ".A[1]"       inlv_e1of4_B 5*TrackPitch BusMetal32)
          TempArray[20] = list( ".T"          inlv_e1of2_A 5*TrackPitch BusMetal54)
        )

       ( "Template_XBUS_CABLE_R90"
          TempArray[0]  = list( ".D[0]"		dense_e1of4		0*TrackPitch BusMetal54)
          TempArray[1]  = list( ".D[1]"		dense_e1of4		3*TrackPitch BusMetal54)
          TempArray[2]  = list( ".D[2]"		dense_e1of4		0*TrackPitch BusMetal76)
          TempArray[3]  = list( ".D[3]"		dense_e1of4		1*TrackPitch BusMetal76)
          TempArray[4]  = list( ".D[4]"		dense_e1of4		2*TrackPitch BusMetal76)
          TempArray[5]  = list( ".D[5]"		dense_e1of4		3*TrackPitch BusMetal76)
          TempArray[6]  = list( ".D[6]"		dense_e1of4		4*TrackPitch BusMetal32)
          TempArray[7]  = list( ".D[7]"		dense_e1of4		4*TrackPitch BusMetal54)
          TempArray[8]  = list( ".D[8]"		dense_e1of4		4*TrackPitch BusMetal76)
          TempArray[9]  = list( ".A[0]"		dense_e1of4		5*TrackPitch BusMetal32)
          TempArray[10] = list( ".T"			inlv_e1of2_A	5*TrackPitch BusMetal54)
        )

       ( "Template_XBUS_CABLE"
          TempArray[0]  = list( ".D[0]"   dense_e1of4		0*TrackPitch BusMetal45)
          TempArray[1]  = list( ".D[1]"   dense_e1of4		3*TrackPitch BusMetal45)
          TempArray[2]  = list( ".D[2]"   dense_e1of4		0*TrackPitch BusMetal67)
          TempArray[3]  = list( ".D[3]"   dense_e1of4		1*TrackPitch BusMetal67)
          TempArray[4]  = list( ".D[4]"   dense_e1of4		2*TrackPitch BusMetal67)
          TempArray[5]  = list( ".D[5]"   dense_e1of4		3*TrackPitch BusMetal67)
          TempArray[6]  = list( ".D[6]"   dense_e1of4		4*TrackPitch BusMetal23)
          TempArray[7]  = list( ".D[7]"   dense_e1of4		4*TrackPitch BusMetal45)
          TempArray[8]  = list( ".D[8]"   dense_e1of4		4*TrackPitch BusMetal67)
          TempArray[9]  = list( ".A[0]"		dense_e1of4		5*TrackPitch BusMetal23) 
          TempArray[10] = list( ".T"      inlv_e1of2_A	5*TrackPitch BusMetal45) 
        )

       ( "Template_SYNC_NODE_R90"
	        TempArray[0]    =  list(   "[0]"  node_1W -DefaultWiringPitch*4.5 BusMetal76)
	        TempArray[1]    =  list(   "[1]"  node_1W -DefaultWiringPitch*3.5 BusMetal76)
	        TempArray[2]    =  list(   "[2]"  node_1W -DefaultWiringPitch*2.5 BusMetal76)
	        TempArray[3]    =  list(   "[3]"  node_1W -DefaultWiringPitch*1.5 BusMetal76)
	        TempArray[4]    =  list(   "[4]"  node_1W -DefaultWiringPitch*0.5 BusMetal76)
	        TempArray[5]    =  list(   "[5]"  node_1W  DefaultWiringPitch*0.5 BusMetal76)
	        TempArray[6]    =  list(   "[6]"  node_1W  DefaultWiringPitch*1.5 BusMetal76)
	        TempArray[7]    =  list(   "[7]"  node_1W  DefaultWiringPitch*2.5 BusMetal76)
	        TempArray[8]    =  list(   "[8]"  node_1W  DefaultWiringPitch*3.5 BusMetal76)
	        TempArray[9]    =  list(   "[9]"  node_1W  DefaultWiringPitch*4.5 BusMetal76)
	        TempArray[10]   =  list(  "[10]"  node_1W  DefaultWiringPitch*7.5 BusMetal76)
	        TempArray[11]   =  list(  "[11]"  node_1W  DefaultWiringPitch*8.5 BusMetal76)
	        TempArray[12]   =  list(  "[12]"  node_1W  DefaultWiringPitch*9.5 BusMetal76)
	        TempArray[13]   =  list(  "[13]"  node_1W  DefaultWiringPitch*10.5 BusMetal76)
	        TempArray[14]   =  list(  "[14]"  node_1W  DefaultWiringPitch*11.5 BusMetal76)
	        TempArray[15]   =  list(  "[15]"  node_1W  DefaultWiringPitch*12.5 BusMetal76)
	        TempArray[16]   =  list(  "[16]"  node_1W  DefaultWiringPitch*13.5 BusMetal76)
	        TempArray[17]   =  list(  "[17]"  node_1W  DefaultWiringPitch*14.5 BusMetal76)
	        TempArray[18]   =  list(  "[18]"  node_1W  DefaultWiringPitch*15.5 BusMetal76)
	        TempArray[19]   =  list(  "[19]"  node_1W  DefaultWiringPitch*16.5 BusMetal76)

	        TempArray[20]   =  list(  "[20]"  node_1W  DefaultWiringPitch*19.5 BusMetal32)
	        TempArray[21]   =  list(  "[21]"  node_1W  DefaultWiringPitch*20.5 BusMetal32)
	        TempArray[22]   =  list(  "[22]"  node_1W  DefaultWiringPitch*21.5 BusMetal32)
	        TempArray[23]   =  list(  "[23]"  node_1W  DefaultWiringPitch*22.5 BusMetal32)
	        TempArray[24]   =  list(  "[24]"  node_1W  DefaultWiringPitch*23.5 BusMetal32)
	        TempArray[25]   =  list(  "[25]"  node_1W  DefaultWiringPitch*24.5 BusMetal32)
	        TempArray[26]   =  list(  "[26]"  node_1W  DefaultWiringPitch*25.5 BusMetal32)
	        TempArray[27]   =  list(  "[27]"  node_1W  DefaultWiringPitch*26.5 BusMetal32)
	        TempArray[28]   =  list(  "[28]"  node_1W  DefaultWiringPitch*27.5 BusMetal32)
	        TempArray[29]   =  list(  "[29]"  node_1W  DefaultWiringPitch*28.5 BusMetal32)
	        TempArray[30]   =  list(  "[30]"  node_1W  DefaultWiringPitch*19.5 BusMetal54)
	        TempArray[31]   =  list(  "[31]"  node_1W  DefaultWiringPitch*20.5 BusMetal54)
	        TempArray[32]   =  list(  "[32]"  node_1W  DefaultWiringPitch*21.5 BusMetal54)
	        TempArray[33]   =  list(  "[33]"  node_1W  DefaultWiringPitch*22.5 BusMetal54)
	        TempArray[34]   =  list(  "[34]"  node_1W  DefaultWiringPitch*23.5 BusMetal54)
	        TempArray[35]   =  list(  "[35]"  node_1W  DefaultWiringPitch*24.5 BusMetal54)
	        TempArray[36]   =  list(  "[36]"  node_1W  DefaultWiringPitch*25.5 BusMetal54)
	        TempArray[37]   =  list(  "[37]"  node_1W  DefaultWiringPitch*26.5 BusMetal54)
	        TempArray[38]   =  list(  "[38]"  node_1W  DefaultWiringPitch*27.5 BusMetal54)
	        TempArray[39]   =  list(  "[39]"  node_1W  DefaultWiringPitch*28.5 BusMetal54)
	        TempArray[40]   =  list(  "[40]"  node_1W  DefaultWiringPitch*19.5 BusMetal76)
	        TempArray[41]   =  list(  "[41]"  node_1W  DefaultWiringPitch*20.5 BusMetal76)
	        TempArray[42]   =  list(  "[42]"  node_1W  DefaultWiringPitch*21.5 BusMetal76)
	        TempArray[43]   =  list(  "[43]"  node_1W  DefaultWiringPitch*22.5 BusMetal76)
	        TempArray[44]   =  list(  "[44]"  node_1W  DefaultWiringPitch*23.5 BusMetal76)
	        TempArray[45]   =  list(  "[45]"  node_1W  DefaultWiringPitch*24.5 BusMetal76)
	        TempArray[46]   =  list(  "[46]"  node_1W  DefaultWiringPitch*25.5 BusMetal76)
	        TempArray[47]   =  list(  "[47]"  node_1W  DefaultWiringPitch*26.5 BusMetal76)
	        TempArray[48]   =  list(  "[48]"  node_1W  DefaultWiringPitch*27.5 BusMetal76)
	        TempArray[49]   =  list(  "[49]"  node_1W  DefaultWiringPitch*28.5 BusMetal76)

	        TempArray[50]   =  list(  "[50]"  node_1W  DefaultWiringPitch*31.5 BusMetal32)
	        TempArray[51]   =  list(  "[51]"  node_1W  DefaultWiringPitch*32.5 BusMetal32)
	        TempArray[52]   =  list(  "[52]"  node_1W  DefaultWiringPitch*33.5 BusMetal32)
	        TempArray[53]   =  list(  "[53]"  node_1W  DefaultWiringPitch*34.5 BusMetal32)
	        TempArray[54]   =  list(  "[54]"  node_1W  DefaultWiringPitch*35.5 BusMetal32)
	        TempArray[55]   =  list(  "[55]"  node_1W  DefaultWiringPitch*36.5 BusMetal32)
	        TempArray[56]   =  list(  "[56]"  node_1W  DefaultWiringPitch*37.5 BusMetal32)
	        TempArray[57]   =  list(  "[57]"  node_1W  DefaultWiringPitch*38.5 BusMetal32)
	        TempArray[58]   =  list(  "[58]"  node_1W  DefaultWiringPitch*39.5 BusMetal32)
	        TempArray[59]   =  list(  "[59]"  node_1W  DefaultWiringPitch*40.5 BusMetal32)
	        TempArray[60]   =  list(  "[60]"  node_1W  DefaultWiringPitch*31.5 BusMetal54)
	        TempArray[61]   =  list(  "[61]"  node_1W  DefaultWiringPitch*32.5 BusMetal54)
	        TempArray[62]   =  list(  "[62]"  node_1W  DefaultWiringPitch*33.5 BusMetal54)
	        TempArray[63]   =  list(  "[63]"  node_1W  DefaultWiringPitch*34.5 BusMetal54)
	        TempArray[64]   =  list(  "[64]"  node_1W  DefaultWiringPitch*35.5 BusMetal54)
	        TempArray[65]   =  list(  "[65]"  node_1W  DefaultWiringPitch*36.5 BusMetal54)
	        TempArray[66]   =  list(  "[66]"  node_1W  DefaultWiringPitch*37.5 BusMetal54)
	        TempArray[67]   =  list(  "[67]"  node_1W  DefaultWiringPitch*38.5 BusMetal54)
	        TempArray[68]   =  list(  "[68]"  node_1W  DefaultWiringPitch*39.5 BusMetal54)
	        TempArray[69]   =  list(  "[69]"  node_1W  DefaultWiringPitch*40.5 BusMetal54)
	        TempArray[70]   =  list(  "[70]"  node_1W  DefaultWiringPitch*31.5 BusMetal76)
	        TempArray[71]   =  list(  "[71]"  node_1W  DefaultWiringPitch*32.5 BusMetal76)
	        TempArray[72]   =  list(  "[72]"  node_1W  DefaultWiringPitch*33.5 BusMetal76)
	        TempArray[73]   =  list(  "[73]"  node_1W  DefaultWiringPitch*34.5 BusMetal76)
	        TempArray[74]   =  list(  "[74]"  node_1W  DefaultWiringPitch*35.5 BusMetal76)
	        TempArray[75]   =  list(  "[75]"  node_1W  DefaultWiringPitch*36.5 BusMetal76)
	        TempArray[76]   =  list(  "[76]"  node_1W  DefaultWiringPitch*37.5 BusMetal76)
	        TempArray[77]   =  list(  "[77]"  node_1W  DefaultWiringPitch*38.5 BusMetal76)
	        TempArray[78]   =  list(  "[78]"  node_1W  DefaultWiringPitch*39.5 BusMetal76)
	        TempArray[79]   =  list(  "[79]"  node_1W  DefaultWiringPitch*40.5 BusMetal76)

	        TempArray[80]  =  list(   "[80]" node_1W DefaultWiringPitch*43.5 BusMetal32)
	        TempArray[81]  =  list(   "[81]" node_1W DefaultWiringPitch*44.5 BusMetal32)
	        TempArray[82]  =  list(   "[82]" node_1W DefaultWiringPitch*45.5 BusMetal32)
	        TempArray[83]  =  list(   "[83]" node_1W DefaultWiringPitch*46.5 BusMetal32)
	        TempArray[84]  =  list(   "[84]" node_1W DefaultWiringPitch*47.5 BusMetal32)
	        TempArray[85]  =  list(   "[85]" node_1W DefaultWiringPitch*48.5 BusMetal32)
	        TempArray[86]  =  list(   "[86]" node_1W DefaultWiringPitch*49.5 BusMetal32)
	        TempArray[87]  =  list(   "[87]" node_1W DefaultWiringPitch*50.5 BusMetal32)
	        TempArray[88]  =  list(   "[88]" node_1W DefaultWiringPitch*51.5 BusMetal32)
	        TempArray[89]  =  list(   "[89]" node_1W DefaultWiringPitch*52.5 BusMetal32)
	        TempArray[90]  =  list(   "[90]" node_1W DefaultWiringPitch*43.5 BusMetal54)
	        TempArray[91]  =  list(   "[91]" node_1W DefaultWiringPitch*44.5 BusMetal54)
	        TempArray[92]  =  list(   "[92]" node_1W DefaultWiringPitch*45.5 BusMetal54)
	        TempArray[93]  =  list(   "[93]" node_1W DefaultWiringPitch*46.5 BusMetal54)
	        TempArray[94]  =  list(   "[94]" node_1W DefaultWiringPitch*47.5 BusMetal54)
	        TempArray[95]  =  list(   "[95]" node_1W DefaultWiringPitch*48.8 BusMetal54)
	        TempArray[96]  =  list(   "[96]" node_1W DefaultWiringPitch*49.5 BusMetal54)
	        TempArray[97]  =  list(   "[97]" node_1W DefaultWiringPitch*50.5 BusMetal54)
	        TempArray[98]  =  list(   "[98]" node_1W DefaultWiringPitch*51.5 BusMetal54)
	        TempArray[99]  =  list(   "[99]" node_1W DefaultWiringPitch*52.5 BusMetal54)
	        TempArray[100]  =  list(   "[100]" node_1W DefaultWiringPitch*43.5 BusMetal76)
	        TempArray[101]  =  list(   "[101]" node_1W DefaultWiringPitch*44.5 BusMetal76)
	        TempArray[102]  =  list(   "[102]" node_1W DefaultWiringPitch*45.5 BusMetal76)
	        TempArray[103]  =  list(   "[103]" node_1W DefaultWiringPitch*46.5 BusMetal76)
	        TempArray[104]  =  list(   "[104]" node_1W DefaultWiringPitch*47.5 BusMetal76)
	        TempArray[105]  =  list(   "[105]" node_1W DefaultWiringPitch*48.5 BusMetal76)
	        TempArray[106]  =  list(   "[106]" node_1W DefaultWiringPitch*49.5 BusMetal76)
	        TempArray[107]  =  list(   "[107]" node_1W DefaultWiringPitch*50.5 BusMetal76)
	        TempArray[108]  =  list(   "[108]" node_1W DefaultWiringPitch*51.5 BusMetal76)
	        TempArray[109]  =  list(   "[109]" node_1W DefaultWiringPitch*52.5 BusMetal76)

	        TempArray[110]  =  list(   "[110]" node_1W DefaultWiringPitch*55.5 BusMetal32)
	        TempArray[111]  =  list(   "[111]" node_1W DefaultWiringPitch*56.5 BusMetal32)
	        TempArray[112]  =  list(   "[112]" node_1W DefaultWiringPitch*57.5 BusMetal32)
	        TempArray[113]  =  list(   "[113]" node_1W DefaultWiringPitch*58.5 BusMetal32)
	        TempArray[114]  =  list(   "[114]" node_1W DefaultWiringPitch*59.5 BusMetal32)
	        TempArray[115]  =  list(   "[115]" node_1W DefaultWiringPitch*60.5 BusMetal32)
	        TempArray[116]  =  list(   "[116]" node_1W DefaultWiringPitch*61.5 BusMetal32)
	        TempArray[117]  =  list(   "[117]" node_1W DefaultWiringPitch*62.5 BusMetal32)
	        TempArray[118]  =  list(   "[118]" node_1W DefaultWiringPitch*63.5 BusMetal32)
	        TempArray[119]  =  list(   "[119]" node_1W DefaultWiringPitch*64.5 BusMetal32)
	        TempArray[120]  =  list(   "[120]" node_1W DefaultWiringPitch*55.5 BusMetal54)
	        TempArray[121]  =  list(   "[121]" node_1W DefaultWiringPitch*56.5 BusMetal54)
	        TempArray[122]  =  list(   "[122]" node_1W DefaultWiringPitch*57.5 BusMetal54)
	        TempArray[123]  =  list(   "[123]" node_1W DefaultWiringPitch*58.5 BusMetal54)
	        TempArray[124]  =  list(   "[124]" node_1W DefaultWiringPitch*59.5 BusMetal54)
	        TempArray[125]  =  list(   "[125]" node_1W DefaultWiringPitch*60.5 BusMetal54)
	        TempArray[126]  =  list(   "[126]" node_1W DefaultWiringPitch*61.5 BusMetal54)
	        TempArray[127]  =  list(   "[127]" node_1W DefaultWiringPitch*62.5 BusMetal54)
	        TempArray[128]  =  list(   "[128]" node_1W DefaultWiringPitch*63.5 BusMetal54)
	        TempArray[129]  =  list(   "[129]" node_1W DefaultWiringPitch*64.5 BusMetal54)
	        TempArray[130]  =  list(   "[130]" node_1W DefaultWiringPitch*55.5 BusMetal76)
	        TempArray[131]  =  list(   "[131]" node_1W DefaultWiringPitch*56.5 BusMetal76)
	        TempArray[132]  =  list(   "[132]" node_1W DefaultWiringPitch*57.5 BusMetal76)
	        TempArray[133]  =  list(   "[133]" node_1W DefaultWiringPitch*58.5 BusMetal76)
	        TempArray[134]  =  list(   "[134]" node_1W DefaultWiringPitch*59.5 BusMetal76)
	        TempArray[135]  =  list(   "[135]" node_1W DefaultWiringPitch*60.5 BusMetal76)
	        TempArray[136]  =  list(   "[136]" node_1W DefaultWiringPitch*61.5 BusMetal76)
	        TempArray[137]  =  list(   "[137]" node_1W DefaultWiringPitch*62.5 BusMetal76)
	        TempArray[138]  =  list(   "[138]" node_1W DefaultWiringPitch*63.5 BusMetal76)
	        TempArray[139]  =  list(   "[139]" node_1W DefaultWiringPitch*64.4 BusMetal76)
      )

    ( "Template_SYNC_NODE"
	        TempArray[0]    =  list(   "[0]"  node_1W -DefaultWiringPitch*4.5 BusMetal67)
	        TempArray[1]    =  list(   "[1]"  node_1W -DefaultWiringPitch*3.5 BusMetal67)
	        TempArray[2]    =  list(   "[2]"  node_1W -DefaultWiringPitch*2.5 BusMetal67)
	        TempArray[3]    =  list(   "[3]"  node_1W -DefaultWiringPitch*1.5 BusMetal67)
	        TempArray[4]    =  list(   "[4]"  node_1W -DefaultWiringPitch*0.5 BusMetal67)
	        TempArray[5]    =  list(   "[5]"  node_1W  DefaultWiringPitch*0.5 BusMetal67)
	        TempArray[6]    =  list(   "[6]"  node_1W  DefaultWiringPitch*1.5 BusMetal67)
	        TempArray[7]    =  list(   "[7]"  node_1W  DefaultWiringPitch*2.5 BusMetal67)
	        TempArray[8]    =  list(   "[8]"  node_1W  DefaultWiringPitch*3.5 BusMetal67)
	        TempArray[9]    =  list(   "[9]"  node_1W  DefaultWiringPitch*4.5 BusMetal67)
	        TempArray[10]   =  list(  "[10]"  node_1W  DefaultWiringPitch*7.5 BusMetal67)
	        TempArray[11]   =  list(  "[11]"  node_1W  DefaultWiringPitch*8.5 BusMetal67)
	        TempArray[12]   =  list(  "[12]"  node_1W  DefaultWiringPitch*9.5 BusMetal67)
	        TempArray[13]   =  list(  "[13]"  node_1W  DefaultWiringPitch*10.5 BusMetal67)
	        TempArray[14]   =  list(  "[14]"  node_1W  DefaultWiringPitch*11.5 BusMetal67)
	        TempArray[15]   =  list(  "[15]"  node_1W  DefaultWiringPitch*12.5 BusMetal67)
	        TempArray[16]   =  list(  "[16]"  node_1W  DefaultWiringPitch*13.5 BusMetal67)
	        TempArray[17]   =  list(  "[17]"  node_1W  DefaultWiringPitch*14.5 BusMetal67)
	        TempArray[18]   =  list(  "[18]"  node_1W  DefaultWiringPitch*15.5 BusMetal67)
	        TempArray[19]   =  list(  "[19]"  node_1W  DefaultWiringPitch*16.5 BusMetal67)

	        TempArray[20]   =  list(  "[20]"  node_1W  DefaultWiringPitch*19.5 BusMetal23)
	        TempArray[21]   =  list(  "[21]"  node_1W  DefaultWiringPitch*20.5 BusMetal23)
	        TempArray[22]   =  list(  "[22]"  node_1W  DefaultWiringPitch*21.5 BusMetal23)
	        TempArray[23]   =  list(  "[23]"  node_1W  DefaultWiringPitch*22.5 BusMetal23)
	        TempArray[24]   =  list(  "[24]"  node_1W  DefaultWiringPitch*23.5 BusMetal23)
	        TempArray[25]   =  list(  "[25]"  node_1W  DefaultWiringPitch*24.5 BusMetal23)
	        TempArray[26]   =  list(  "[26]"  node_1W  DefaultWiringPitch*25.5 BusMetal23)
	        TempArray[27]   =  list(  "[27]"  node_1W  DefaultWiringPitch*26.5 BusMetal23)
	        TempArray[28]   =  list(  "[28]"  node_1W  DefaultWiringPitch*27.5 BusMetal23)
	        TempArray[29]   =  list(  "[29]"  node_1W  DefaultWiringPitch*28.5 BusMetal23)
	        TempArray[30]   =  list(  "[30]"  node_1W  DefaultWiringPitch*19.5 BusMetal45)
	        TempArray[31]   =  list(  "[31]"  node_1W  DefaultWiringPitch*20.5 BusMetal45)
	        TempArray[32]   =  list(  "[32]"  node_1W  DefaultWiringPitch*21.5 BusMetal45)
	        TempArray[33]   =  list(  "[33]"  node_1W  DefaultWiringPitch*22.5 BusMetal45)
	        TempArray[34]   =  list(  "[34]"  node_1W  DefaultWiringPitch*23.5 BusMetal45)
	        TempArray[35]   =  list(  "[35]"  node_1W  DefaultWiringPitch*24.5 BusMetal45)
	        TempArray[36]   =  list(  "[36]"  node_1W  DefaultWiringPitch*25.5 BusMetal45)
	        TempArray[37]   =  list(  "[37]"  node_1W  DefaultWiringPitch*26.5 BusMetal45)
	        TempArray[38]   =  list(  "[38]"  node_1W  DefaultWiringPitch*27.5 BusMetal45)
	        TempArray[39]   =  list(  "[39]"  node_1W  DefaultWiringPitch*28.5 BusMetal45)
	        TempArray[40]   =  list(  "[40]"  node_1W  DefaultWiringPitch*19.5 BusMetal67)
	        TempArray[41]   =  list(  "[41]"  node_1W  DefaultWiringPitch*20.5 BusMetal67)
	        TempArray[42]   =  list(  "[42]"  node_1W  DefaultWiringPitch*21.5 BusMetal67)
	        TempArray[43]   =  list(  "[43]"  node_1W  DefaultWiringPitch*22.5 BusMetal67)
	        TempArray[44]   =  list(  "[44]"  node_1W  DefaultWiringPitch*23.5 BusMetal67)
	        TempArray[45]   =  list(  "[45]"  node_1W  DefaultWiringPitch*24.5 BusMetal67)
	        TempArray[46]   =  list(  "[46]"  node_1W  DefaultWiringPitch*25.5 BusMetal67)
	        TempArray[47]   =  list(  "[47]"  node_1W  DefaultWiringPitch*26.5 BusMetal67)
	        TempArray[48]   =  list(  "[48]"  node_1W  DefaultWiringPitch*27.5 BusMetal67)
	        TempArray[49]   =  list(  "[49]"  node_1W  DefaultWiringPitch*28.5 BusMetal67)

	        TempArray[50]   =  list(  "[50]"  node_1W  DefaultWiringPitch*31.5 BusMetal23)
	        TempArray[51]   =  list(  "[51]"  node_1W  DefaultWiringPitch*32.5 BusMetal23)
	        TempArray[52]   =  list(  "[52]"  node_1W  DefaultWiringPitch*33.5 BusMetal23)
	        TempArray[53]   =  list(  "[53]"  node_1W  DefaultWiringPitch*34.5 BusMetal23)
	        TempArray[54]   =  list(  "[54]"  node_1W  DefaultWiringPitch*35.5 BusMetal23)
	        TempArray[55]   =  list(  "[55]"  node_1W  DefaultWiringPitch*36.5 BusMetal23)
	        TempArray[56]   =  list(  "[56]"  node_1W  DefaultWiringPitch*37.5 BusMetal23)
	        TempArray[57]   =  list(  "[57]"  node_1W  DefaultWiringPitch*38.5 BusMetal23)
	        TempArray[58]   =  list(  "[58]"  node_1W  DefaultWiringPitch*39.5 BusMetal23)
	        TempArray[59]   =  list(  "[59]"  node_1W  DefaultWiringPitch*40.5 BusMetal23)
	        TempArray[60]   =  list(  "[60]"  node_1W  DefaultWiringPitch*31.5 BusMetal45)
	        TempArray[61]   =  list(  "[61]"  node_1W  DefaultWiringPitch*32.5 BusMetal45)
	        TempArray[62]   =  list(  "[62]"  node_1W  DefaultWiringPitch*33.5 BusMetal45)
	        TempArray[63]   =  list(  "[63]"  node_1W  DefaultWiringPitch*34.5 BusMetal45)
	        TempArray[64]   =  list(  "[64]"  node_1W  DefaultWiringPitch*35.5 BusMetal45)
	        TempArray[65]   =  list(  "[65]"  node_1W  DefaultWiringPitch*36.5 BusMetal45)
	        TempArray[66]   =  list(  "[66]"  node_1W  DefaultWiringPitch*37.5 BusMetal45)
	        TempArray[67]   =  list(  "[67]"  node_1W  DefaultWiringPitch*38.5 BusMetal45)
	        TempArray[68]   =  list(  "[68]"  node_1W  DefaultWiringPitch*39.5 BusMetal45)
	        TempArray[69]   =  list(  "[69]"  node_1W  DefaultWiringPitch*40.5 BusMetal45)
	        TempArray[70]   =  list(  "[70]"  node_1W  DefaultWiringPitch*31.5 BusMetal67)
	        TempArray[71]   =  list(  "[71]"  node_1W  DefaultWiringPitch*32.5 BusMetal67)
	        TempArray[72]   =  list(  "[72]"  node_1W  DefaultWiringPitch*33.5 BusMetal67)
	        TempArray[73]   =  list(  "[73]"  node_1W  DefaultWiringPitch*34.5 BusMetal67)
	        TempArray[74]   =  list(  "[74]"  node_1W  DefaultWiringPitch*35.5 BusMetal67)
	        TempArray[75]   =  list(  "[75]"  node_1W  DefaultWiringPitch*36.5 BusMetal67)
	        TempArray[76]   =  list(  "[76]"  node_1W  DefaultWiringPitch*37.5 BusMetal67)
	        TempArray[77]   =  list(  "[77]"  node_1W  DefaultWiringPitch*38.5 BusMetal67)
	        TempArray[78]   =  list(  "[78]"  node_1W  DefaultWiringPitch*39.5 BusMetal67)
	        TempArray[79]   =  list(  "[79]"  node_1W  DefaultWiringPitch*40.5 BusMetal67)

	        TempArray[80]  =  list(   "[80]" node_1W DefaultWiringPitch*43.5 BusMetal23)
	        TempArray[81]  =  list(   "[81]" node_1W DefaultWiringPitch*44.5 BusMetal23)
	        TempArray[82]  =  list(   "[82]" node_1W DefaultWiringPitch*45.5 BusMetal23)
	        TempArray[83]  =  list(   "[83]" node_1W DefaultWiringPitch*46.5 BusMetal23)
	        TempArray[84]  =  list(   "[84]" node_1W DefaultWiringPitch*47.5 BusMetal23)
	        TempArray[85]  =  list(   "[85]" node_1W DefaultWiringPitch*48.5 BusMetal23)
	        TempArray[86]  =  list(   "[86]" node_1W DefaultWiringPitch*49.5 BusMetal23)
	        TempArray[87]  =  list(   "[87]" node_1W DefaultWiringPitch*50.5 BusMetal23)
	        TempArray[88]  =  list(   "[88]" node_1W DefaultWiringPitch*51.5 BusMetal23)
	        TempArray[89]  =  list(   "[89]" node_1W DefaultWiringPitch*52.5 BusMetal23)
	        TempArray[90]  =  list(   "[90]" node_1W DefaultWiringPitch*43.5 BusMetal45)
	        TempArray[91]  =  list(   "[91]" node_1W DefaultWiringPitch*44.5 BusMetal45)
	        TempArray[92]  =  list(   "[92]" node_1W DefaultWiringPitch*45.5 BusMetal45)
	        TempArray[93]  =  list(   "[93]" node_1W DefaultWiringPitch*46.5 BusMetal45)
	        TempArray[94]  =  list(   "[94]" node_1W DefaultWiringPitch*47.5 BusMetal45)
	        TempArray[95]  =  list(   "[95]" node_1W DefaultWiringPitch*48.8 BusMetal45)
	        TempArray[96]  =  list(   "[96]" node_1W DefaultWiringPitch*49.5 BusMetal45)
	        TempArray[97]  =  list(   "[97]" node_1W DefaultWiringPitch*50.5 BusMetal45)
	        TempArray[98]  =  list(   "[98]" node_1W DefaultWiringPitch*51.5 BusMetal45)
	        TempArray[99]  =  list(   "[99]" node_1W DefaultWiringPitch*52.5 BusMetal45)
	        TempArray[100]  =  list(   "[100]" node_1W DefaultWiringPitch*43.5 BusMetal67)
	        TempArray[101]  =  list(   "[101]" node_1W DefaultWiringPitch*44.5 BusMetal67)
	        TempArray[102]  =  list(   "[102]" node_1W DefaultWiringPitch*45.5 BusMetal67)
	        TempArray[103]  =  list(   "[103]" node_1W DefaultWiringPitch*46.5 BusMetal67)
	        TempArray[104]  =  list(   "[104]" node_1W DefaultWiringPitch*47.5 BusMetal67)
	        TempArray[105]  =  list(   "[105]" node_1W DefaultWiringPitch*48.5 BusMetal67)
	        TempArray[106]  =  list(   "[106]" node_1W DefaultWiringPitch*49.5 BusMetal67)
	        TempArray[107]  =  list(   "[107]" node_1W DefaultWiringPitch*50.5 BusMetal67)
	        TempArray[108]  =  list(   "[108]" node_1W DefaultWiringPitch*51.5 BusMetal67)
	        TempArray[109]  =  list(   "[109]" node_1W DefaultWiringPitch*52.5 BusMetal67)

	        TempArray[110]  =  list(   "[110]" node_1W DefaultWiringPitch*55.5 BusMetal23)
	        TempArray[111]  =  list(   "[111]" node_1W DefaultWiringPitch*56.5 BusMetal23)
	        TempArray[112]  =  list(   "[112]" node_1W DefaultWiringPitch*57.5 BusMetal23)
	        TempArray[113]  =  list(   "[113]" node_1W DefaultWiringPitch*58.5 BusMetal23)
	        TempArray[114]  =  list(   "[114]" node_1W DefaultWiringPitch*59.5 BusMetal23)
	        TempArray[115]  =  list(   "[115]" node_1W DefaultWiringPitch*60.5 BusMetal23)
	        TempArray[116]  =  list(   "[116]" node_1W DefaultWiringPitch*61.5 BusMetal23)
	        TempArray[117]  =  list(   "[117]" node_1W DefaultWiringPitch*62.5 BusMetal23)
	        TempArray[118]  =  list(   "[118]" node_1W DefaultWiringPitch*63.5 BusMetal23)
	        TempArray[119]  =  list(   "[119]" node_1W DefaultWiringPitch*64.5 BusMetal23)
	        TempArray[120]  =  list(   "[120]" node_1W DefaultWiringPitch*55.5 BusMetal45)
	        TempArray[121]  =  list(   "[121]" node_1W DefaultWiringPitch*56.5 BusMetal45)
	        TempArray[122]  =  list(   "[122]" node_1W DefaultWiringPitch*57.5 BusMetal45)
	        TempArray[123]  =  list(   "[123]" node_1W DefaultWiringPitch*58.5 BusMetal45)
	        TempArray[124]  =  list(   "[124]" node_1W DefaultWiringPitch*59.5 BusMetal45)
	        TempArray[125]  =  list(   "[125]" node_1W DefaultWiringPitch*60.5 BusMetal45)
	        TempArray[126]  =  list(   "[126]" node_1W DefaultWiringPitch*61.5 BusMetal45)
	        TempArray[127]  =  list(   "[127]" node_1W DefaultWiringPitch*62.5 BusMetal45)
	        TempArray[128]  =  list(   "[128]" node_1W DefaultWiringPitch*63.5 BusMetal45)
	        TempArray[129]  =  list(   "[129]" node_1W DefaultWiringPitch*64.5 BusMetal45)
	        TempArray[130]  =  list(   "[130]" node_1W DefaultWiringPitch*55.5 BusMetal67)
	        TempArray[131]  =  list(   "[131]" node_1W DefaultWiringPitch*56.5 BusMetal67)
	        TempArray[132]  =  list(   "[132]" node_1W DefaultWiringPitch*57.5 BusMetal67)
	        TempArray[133]  =  list(   "[133]" node_1W DefaultWiringPitch*58.5 BusMetal67)
	        TempArray[134]  =  list(   "[134]" node_1W DefaultWiringPitch*59.5 BusMetal67)
	        TempArray[135]  =  list(   "[135]" node_1W DefaultWiringPitch*60.5 BusMetal67)
	        TempArray[136]  =  list(   "[136]" node_1W DefaultWiringPitch*61.5 BusMetal67)
	        TempArray[137]  =  list(   "[137]" node_1W DefaultWiringPitch*62.5 BusMetal67)
	        TempArray[138]  =  list(   "[138]" node_1W DefaultWiringPitch*63.5 BusMetal67)
	        TempArray[139]  =  list(   "[139]" node_1W DefaultWiringPitch*64.4 BusMetal67)
      )

     ( "Template_DENSE_RESET_R90"
        TempArray[0]  = list( "_L" node_1W  DefaultWiringPitch*14.5  BusMetal32 )
        TempArray[1]  = list( "_L" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
        TempArray[2]  = list( "_R" node_1W  DefaultWiringPitch*14.5  BusMetal32 )
        TempArray[3]  = list( "_R" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
     )
     ( "Template_NYB_RESET_R90"
        TempArray[0]  = list( "_L" node_1W  DefaultWiringPitch*16.5  BusMetal32 )
        TempArray[1]  = list( "_L" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
        TempArray[2]  = list( "_R" node_1W  DefaultWiringPitch*16.5  BusMetal32 )
        TempArray[3]  = list( "_R" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
     )
     ( "Template_XBUS_RESET_R90"
        TempArray[0]  = list( "_L" node_1W  DefaultWiringPitch*14.5  BusMetal32 )
        TempArray[1]  = list( "_L" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
        TempArray[2]  = list( "_R" node_1W  DefaultWiringPitch*14.5  BusMetal32 )
        TempArray[3]  = list( "_R" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
     )
     ( "Template_XBUS_CABLE_RESET_R90"
        TempArray[0]  = list( "_L" node_1W  DefaultWiringPitch*14.5  BusMetal32 )
        TempArray[1]  = list( "_L" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
        TempArray[2]  = list( "_R" node_1W  DefaultWiringPitch*14.5  BusMetal32 )
        TempArray[3]  = list( "_R" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
     )
     ( "Template_TS_RESET_R90"
        TempArray[0]  = list( "_BACK_I" node_1W  DefaultWiringPitch*20.5  BusMetal32 )
        TempArray[1]  = list( "_BACK_I" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
        TempArray[2]  = list( "_BACK_O" node_1W  DefaultWiringPitch*20.5  BusMetal32 )
        TempArray[3]  = list( "_BACK_O" node_1W  DefaultWiringPitch*00.0  BusMetal32 )
        TempArray[4]  = list( "_L"      node_2W  DefaultWiringPitch*24.0  BusMetal43 )
        TempArray[5]  = list( "_L"      node_2W  DefaultWiringPitch*36.0  BusMetal43 )
        TempArray[6]  = list( "_R"      node_2W  DefaultWiringPitch*24.0  BusMetal43 )
        TempArray[7]  = list( "_R"      node_2W  DefaultWiringPitch*36.0  BusMetal43 )
     )
     )

   TempArray

 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLCreateBufArray ( @key (Size 60) (Split t))
 let( ( BufArray )

   declare(BufArray[Size])

   for( I 0 Size-1
  
    if(Split
     then
      when( Size <= GetCableSplitIdx(Size) BufArray[I] = I )
      when( Size > GetCableSplitIdx(Size)
        when( I <= (Size-1 - GetCableSplitIdx(Size)) BufArray[I] = GetCableSplitIdx(Size) + I )
        when( I >  (Size-1 - GetCableSplitIdx(Size)) BufArray[I] = I - (Size - GetCableSplitIdx(Size)) )
         )
     else
      BufArray[I] = I
    )
   )
  
  BufArray
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun GetCableSplitIdx ( size )
 (let ( split )

     when( evenp(round(size/2)) split = round(size/2) )
     when(  oddp(round(size/2)) split = round(size/2) + 1 )

  split

 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;defun( CBLCreateBufArray ()
; let( ( BufArray )
;
;   declare(BufArray[60])
;
;   for( I 0 59
;  
;    when( I <= 35
;     BufArray[I] = 24 + I
;        )
;
;    when( I > 35
;     BufArray[I] = I - 36
;        )
;
;    )
;  
;  BufArray
; )
;)

defun( CableDefChanTemplate (Template)
 let( (
       TempArray
       )

   declare(TempArray[180])

   case( Template

     ( "Template_DENSE"
           TempArray[0]  =  "list(   \"[0]\"  dense_e1of4   0*TrackPitch   BusMetal45 )"
           TempArray[1]  =  "list(   \"[1]\"  dense_e1of4   2*TrackPitch   BusMetal45 )"
           TempArray[2]  =  "list(   \"[2]\"  dense_e1of4   0*TrackPitch   BusMetal67 )"
           TempArray[3]  =  "list(   \"[3]\"  dense_e1of4   1*TrackPitch   BusMetal67 )"
           TempArray[4]  =  "list(   \"[4]\"  dense_e1of4   2*TrackPitch   BusMetal67 )"
           TempArray[5]  =  "list(   \"[5]\"  dense_e1of4   3*TrackPitch   BusMetal67 )"
           TempArray[6]  =  "list(   \"[6]\"  dense_e1of4   4*TrackPitch   BusMetal23 )"
           TempArray[7]  =  "list(   \"[7]\"  dense_e1of4   4*TrackPitch   BusMetal45 )"
           TempArray[8]  =  "list(   \"[8]\"  dense_e1of4   4*TrackPitch   BusMetal67 )"
           TempArray[9]  =  "list(   \"[9]\"  dense_e1of4   5*TrackPitch   BusMetal23 )"
          TempArray[10]  =  "list(  \"[10]\"  dense_e1of4   5*TrackPitch   BusMetal45 )"
          TempArray[11]  =  "list(  \"[11]\"  dense_e1of4   5*TrackPitch   BusMetal67 )"
          TempArray[12]  =  "list(  \"[12]\"  dense_e1of4   6*TrackPitch   BusMetal23 )"
          TempArray[13]  =  "list(  \"[13]\"  dense_e1of4   6*TrackPitch   BusMetal45 )"
          TempArray[14]  =  "list(  \"[14]\"  dense_e1of4   6*TrackPitch   BusMetal67 )"
          TempArray[15]  =  "list(  \"[15]\"  dense_e1of4   7*TrackPitch   BusMetal23 )"
          TempArray[16]  =  "list(  \"[16]\"  dense_e1of4   7*TrackPitch   BusMetal45 )"
          TempArray[17]  =  "list(  \"[17]\"  dense_e1of4   7*TrackPitch   BusMetal67 )"
          TempArray[18]  =  "list(  \"[18]\"  dense_e1of4   8*TrackPitch   BusMetal23 )"
          TempArray[19]  =  "list(  \"[19]\"  dense_e1of4   8*TrackPitch   BusMetal45 )"
          TempArray[20]  =  "list(  \"[20]\"  dense_e1of4   8*TrackPitch   BusMetal67 )"
          TempArray[21]  =  "list(  \"[21]\"  dense_e1of4   9*TrackPitch   BusMetal23 )"
          TempArray[22]  =  "list(  \"[22]\"  dense_e1of4   9*TrackPitch   BusMetal45 )"
          TempArray[23]  =  "list(  \"[23]\"  dense_e1of4   9*TrackPitch   BusMetal67 )"
          TempArray[24]  =  "list(  \"[24]\"  dense_e1of4  10*TrackPitch   BusMetal23 )"
          TempArray[25]  =  "list(  \"[25]\"  dense_e1of4  10*TrackPitch   BusMetal45 )"
          TempArray[26]  =  "list(  \"[26]\"  dense_e1of4  10*TrackPitch   BusMetal67 )"
          TempArray[27]  =  "list(  \"[27]\"  dense_e1of4  11*TrackPitch   BusMetal23 )"
          TempArray[28]  =  "list(  \"[28]\"  dense_e1of4  11*TrackPitch   BusMetal45 )"
          TempArray[29]  =  "list(  \"[29]\"  dense_e1of4  11*TrackPitch   BusMetal67 )"
        )
       ( "Template_DENSE_R90"
           TempArray[0]  =  "list(  \"[0]\"  dense_e1of4   0*TrackPitch   BusMetal54 )"
           TempArray[1]  =  "list(  \"[1]\"  dense_e1of4   2*TrackPitch   BusMetal54 )"
           TempArray[2]  =  "list(  \"[2]\"  dense_e1of4   0*TrackPitch   BusMetal76 )"
           TempArray[3]  =  "list(  \"[3]\"  dense_e1of4   1*TrackPitch   BusMetal76 )"
           TempArray[4]  =  "list(  \"[4]\"  dense_e1of4   2*TrackPitch   BusMetal76 )"
           TempArray[5]  =  "list(  \"[5]\"  dense_e1of4   3*TrackPitch   BusMetal76 )"
           TempArray[6]  =  "list(  \"[6]\"  dense_e1of4   4*TrackPitch   BusMetal32 )"
           TempArray[7]  =  "list(  \"[7]\"  dense_e1of4   4*TrackPitch   BusMetal54 )"
           TempArray[8]  =  "list(  \"[8]\"  dense_e1of4   4*TrackPitch   BusMetal76 )"
           TempArray[9]  =  "list(  \"[9]\"  dense_e1of4   5*TrackPitch   BusMetal32 )"
          TempArray[10]  =  "list(  \"[10]\"  dense_e1of4   5*TrackPitch   BusMetal54 )"
          TempArray[11]  =  "list(  \"[11]\"  dense_e1of4   5*TrackPitch   BusMetal76 )"
          TempArray[12]  =  "list(  \"[12]\"  dense_e1of4   6*TrackPitch   BusMetal32 )"
          TempArray[13]  =  "list(  \"[13]\"  dense_e1of4   6*TrackPitch   BusMetal54 )"
          TempArray[14]  =  "list(  \"[14]\"  dense_e1of4   6*TrackPitch   BusMetal76 )"
          TempArray[15]  =  "list(  \"[15]\"  dense_e1of4   7*TrackPitch   BusMetal32 )"
          TempArray[16]  =  "list(  \"[16]\"  dense_e1of4   7*TrackPitch   BusMetal54 )"
          TempArray[17]  =  "list(  \"[17]\"  dense_e1of4   7*TrackPitch   BusMetal76 )"
          TempArray[18]  =  "list(  \"[18]\"  dense_e1of4   8*TrackPitch   BusMetal32 )"
          TempArray[19]  =  "list(  \"[19]\"  dense_e1of4   8*TrackPitch   BusMetal54 )"
          TempArray[20]  =  "list(  \"[20]\"  dense_e1of4   8*TrackPitch   BusMetal76 )"
          TempArray[21]  =  "list(  \"[21]\"  dense_e1of4   9*TrackPitch   BusMetal32 )"
          TempArray[22]  =  "list(  \"[22]\"  dense_e1of4   9*TrackPitch   BusMetal54 )"
          TempArray[23]  =  "list(  \"[23]\"  dense_e1of4   9*TrackPitch   BusMetal76 )"
          TempArray[24]  =  "list(  \"[24]\"  dense_e1of4  10*TrackPitch   BusMetal32 )"
          TempArray[25]  =  "list(  \"[25]\"  dense_e1of4  10*TrackPitch   BusMetal54 )"
          TempArray[26]  =  "list(  \"[26]\"  dense_e1of4  10*TrackPitch   BusMetal76 )"
          TempArray[27]  =  "list(  \"[27]\"  dense_e1of4  11*TrackPitch   BusMetal32 )"
          TempArray[28]  =  "list(  \"[28]\"  dense_e1of4  11*TrackPitch   BusMetal54 )"
          TempArray[29]  =  "list(  \"[29]\"  dense_e1of4  11*TrackPitch   BusMetal76 )"
        )

       ( "Template_TS"
           TempArray[0]  = "list(  \"[0]\"    ts_e1of4    0*TrackPitch   BusMetal45 )"
           TempArray[1]  = "list(  \"[1]\"    ts_e1of4    1*TrackPitch   BusMetal45 )"
           TempArray[2]  = "list(  \"[2]\"    ts_e1of4    3*TrackPitch   BusMetal45 )"
           TempArray[3]  = "list(  \"[3]\"    ts_e1of4    0*TrackPitch   BusMetal67 )"
           TempArray[4]  = "list(  \"[4]\"    ts_e1of4    1*TrackPitch   BusMetal67 )"
           TempArray[5]  = "list(  \"[5]\"    ts_e1of4    2*TrackPitch   BusMetal67 )"
           TempArray[6]  = "list(  \"[6]\"    ts_e1of4    3*TrackPitch   BusMetal67 )"
           TempArray[7]  = "list(  \"[7]\"    ts_e1of4    4*TrackPitch   BusMetal45 )"
           TempArray[8]  = "list(  \"[8]\"    ts_e1of4    4*TrackPitch   BusMetal67 )"
           TempArray[9]  = "list(  \"[9]\"    ts_e1of4    5*TrackPitch   BusMetal23 )"
          TempArray[10]  = "list(  \"[10]\"    ts_e1of4    5*TrackPitch   BusMetal45 )"
          TempArray[11]  = "list(  \"[11]\"    ts_e1of4    5*TrackPitch   BusMetal67 )"
          TempArray[12]  = "list(  \"[12]\"    ts_e1of4    6*TrackPitch   BusMetal23 )"
          TempArray[13]  = "list(  \"[13]\"    ts_e1of4    6*TrackPitch   BusMetal45 )"
          TempArray[14]  = "list(  \"[14]\"    ts_e1of4    6*TrackPitch   BusMetal67 )"
          TempArray[15]  = "list(  \"[15]\"    ts_e1of4    7*TrackPitch   BusMetal23 )"
          TempArray[16]  = "list(  \"[16]\"    ts_e1of4    7*TrackPitch   BusMetal45 )"
          TempArray[17]  = "list(  \"[17]\"    ts_e1of4    7*TrackPitch   BusMetal67 )"
          TempArray[18]  = "list(  \"[18]\"    ts_e1of4    8*TrackPitch   BusMetal23 )"
          TempArray[19]  = "list(  \"[19]\"    ts_e1of4    8*TrackPitch   BusMetal45 )"
          TempArray[20]  = "list(  \"[20]\"    ts_e1of4    8*TrackPitch   BusMetal67 )"
          TempArray[21]  = "list(  \"[21]\"    ts_e1of4    9*TrackPitch   BusMetal23 )"
          TempArray[22]  = "list(  \"[22]\"    ts_e1of4    9*TrackPitch   BusMetal45 )"
          TempArray[23]  = "list(  \"[23]\"    ts_e1of4    9*TrackPitch   BusMetal67 )"
          TempArray[24]  = "list(  \"[24]\"    ts_e1of4   10*TrackPitch   BusMetal23 )"
          TempArray[25]  = "list(  \"[25]\"    ts_e1of4   10*TrackPitch   BusMetal45 )"
          TempArray[26]  = "list(  \"[26]\"    ts_e1of4   10*TrackPitch   BusMetal67 )"
          TempArray[27]  = "list(  \"[27]\"    ts_e1of4   11*TrackPitch   BusMetal23 )"
          TempArray[28]  = "list(  \"[28]\"    ts_e1of4   11*TrackPitch   BusMetal45 )"
          TempArray[29]  = "list(  \"[29]\"    ts_e1of4   11*TrackPitch   BusMetal67 )"
        )

       ( "Template_TS_R90"
           TempArray[0]  = "list(  \"[0]\"    ts_e1of4    0*TrackPitch   BusMetal54 )"
           TempArray[1]  = "list(  \"[1]\"    ts_e1of4    1*TrackPitch   BusMetal54 )"
           TempArray[2]  = "list(  \"[2]\"    ts_e1of4    3*TrackPitch   BusMetal54 )"
           TempArray[3]  = "list(  \"[3]\"    ts_e1of4    0*TrackPitch   BusMetal76 )"
           TempArray[4]  = "list(  \"[4]\"    ts_e1of4    1*TrackPitch   BusMetal76 )"
           TempArray[5]  = "list(  \"[5]\"    ts_e1of4    2*TrackPitch   BusMetal76 )"
           TempArray[6]  = "list(  \"[6]\"    ts_e1of4    3*TrackPitch   BusMetal76 )"
           TempArray[7]  = "list(  \"[7]\"    ts_e1of4    4*TrackPitch   BusMetal54 )"
           TempArray[8]  = "list(  \"[8]\"    ts_e1of4    4*TrackPitch   BusMetal76 )"
           TempArray[9]  = "list(  \"[9]\"    ts_e1of4    5*TrackPitch   BusMetal32 )"
          TempArray[10]  = "list(  \"[10]\"    ts_e1of4    5*TrackPitch   BusMetal54 )"
          TempArray[11]  = "list(  \"[11]\"    ts_e1of4    5*TrackPitch   BusMetal76 )"
          TempArray[12]  = "list(  \"[12]\"    ts_e1of4    6*TrackPitch   BusMetal32 )"
          TempArray[13]  = "list(  \"[13]\"    ts_e1of4    6*TrackPitch   BusMetal54 )"
          TempArray[14]  = "list(  \"[14]\"    ts_e1of4    6*TrackPitch   BusMetal76 )"
          TempArray[15]  = "list(  \"[15]\"    ts_e1of4    7*TrackPitch   BusMetal32 )"
          TempArray[16]  = "list(  \"[16]\"    ts_e1of4    7*TrackPitch   BusMetal54 )"
          TempArray[17]  = "list(  \"[17]\"    ts_e1of4    7*TrackPitch   BusMetal76 )"
          TempArray[18]  = "list(  \"[18]\"    ts_e1of4    8*TrackPitch   BusMetal32 )"
          TempArray[19]  = "list(  \"[19]\"    ts_e1of4    8*TrackPitch   BusMetal54 )"
          TempArray[20]  = "list(  \"[20]\"    ts_e1of4    8*TrackPitch   BusMetal76 )"
          TempArray[21]  = "list(  \"[21]\"    ts_e1of4    9*TrackPitch   BusMetal32 )"
          TempArray[22]  = "list(  \"[22]\"    ts_e1of4    9*TrackPitch   BusMetal54 )"
          TempArray[23]  = "list(  \"[23]\"    ts_e1of4    9*TrackPitch   BusMetal76 )"
          TempArray[24]  = "list(  \"[24]\"    ts_e1of4   10*TrackPitch   BusMetal32 )"
          TempArray[25]  = "list(  \"[25]\"    ts_e1of4   10*TrackPitch   BusMetal54 )"
          TempArray[26]  = "list(  \"[26]\"    ts_e1of4   10*TrackPitch   BusMetal76 )"
          TempArray[27]  = "list(  \"[27]\"    ts_e1of4   11*TrackPitch   BusMetal32 )"
          TempArray[28]  = "list(  \"[28]\"    ts_e1of4   11*TrackPitch   BusMetal54 )"
          TempArray[29]  = "list(  \"[29]\"    ts_e1of4   11*TrackPitch   BusMetal76 )"
        )

       ( "Template_NYB"
           TempArray[0]  =  "list(  \"[0]\"  nyb_e1of4   0*TrackPitch   BusMetal45 )"
           TempArray[1]  =  "list(  \"[1]\"  nyb_e1of4   1*TrackPitch   BusMetal45 )"
           TempArray[2]  =  "list(  \"[2]\"  nyb_e1of4   0*TrackPitch   BusMetal67 )"
           TempArray[3]  =  "list(  \"[3]\"  nyb_e1of4   1*TrackPitch   BusMetal67 )"
           TempArray[4]  =  "list(  \"[4]\"  nyb_e1of4   2*TrackPitch   BusMetal67 )"
           TempArray[5]  =  "list(  \"[5]\"  nyb_e1of4   3*TrackPitch   BusMetal67 )"
           TempArray[6]  =  "list(  \"[6]\"  nyb_e1of4   4*TrackPitch   BusMetal23 )"
           TempArray[7]  =  "list(  \"[7]\"  nyb_e1of4   4*TrackPitch   BusMetal45 )"
           TempArray[8]  =  "list(  \"[8]\"  nyb_e1of4   4*TrackPitch   BusMetal67 )"
           TempArray[9]  =  "list(  \"[9]\"  nyb_e1of4   5*TrackPitch   BusMetal23 )"
          TempArray[10]  =  "list(  \"[10]\"  nyb_e1of4   5*TrackPitch   BusMetal45 )"
          TempArray[11]  =  "list(  \"[11]\"  nyb_e1of4   5*TrackPitch   BusMetal67 )"
          TempArray[12]  =  "list(  \"[12]\"  nyb_e1of4   6*TrackPitch   BusMetal23 )"
          TempArray[13]  =  "list(  \"[13]\"  nyb_e1of4   6*TrackPitch   BusMetal45 )"
          TempArray[14]  =  "list(  \"[14]\"  nyb_e1of4   6*TrackPitch   BusMetal67 )"
          TempArray[15]  =  "list(  \"[15]\"  nyb_e1of4   7*TrackPitch   BusMetal23 )"
          TempArray[16]  =  "list(  \"[16]\"  nyb_e1of4   7*TrackPitch   BusMetal45 )"
          TempArray[17]  =  "list(  \"[17]\"  nyb_e1of4   7*TrackPitch   BusMetal67 )"
          TempArray[18]  =  "list(  \"[18]\"  nyb_e1of4   8*TrackPitch   BusMetal23 )"
          TempArray[19]  =  "list(  \"[19]\"  nyb_e1of4   8*TrackPitch   BusMetal45 )"
          TempArray[20]  =  "list(  \"[20]\"  nyb_e1of4   8*TrackPitch   BusMetal67 )"
          TempArray[21]  =  "list(  \"[21]\"  nyb_e1of4   9*TrackPitch   BusMetal23 )"
          TempArray[22]  =  "list(  \"[22]\"  nyb_e1of4   9*TrackPitch   BusMetal45 )"
          TempArray[23]  =  "list(  \"[23]\"  nyb_e1of4   9*TrackPitch   BusMetal67 )"
          TempArray[24]  =  "list(  \"[24]\"  nyb_e1of4  10*TrackPitch   BusMetal23 )"
          TempArray[25]  =  "list(  \"[25]\"  nyb_e1of4  10*TrackPitch   BusMetal45 )"
          TempArray[26]  =  "list(  \"[26]\"  nyb_e1of4  10*TrackPitch   BusMetal67 )"
          TempArray[27]  =  "list(  \"[27]\"  nyb_e1of4  11*TrackPitch   BusMetal23 )"
          TempArray[28]  =  "list(  \"[28]\"  nyb_e1of4  11*TrackPitch   BusMetal45 )"
          TempArray[29]  =  "list(  \"[29]\"  nyb_e1of4  11*TrackPitch   BusMetal67 )"
        )

       ( "Template_NYB_R90"
           TempArray[0]  =  "list(  \"[0]\"  nyb_e1of4   0*TrackPitch   BusMetal54 )"
           TempArray[1]  =  "list(  \"[1]\"  nyb_e1of4   1*TrackPitch   BusMetal54 )"
           TempArray[2]  =  "list(  \"[2]\"  nyb_e1of4   0*TrackPitch   BusMetal76 )"
           TempArray[3]  =  "list(  \"[3]\"  nyb_e1of4   1*TrackPitch   BusMetal76 )"
           TempArray[4]  =  "list(  \"[4]\"  nyb_e1of4   2*TrackPitch   BusMetal76 )"
           TempArray[5]  =  "list(  \"[5]\"  nyb_e1of4   3*TrackPitch   BusMetal76 )"
           TempArray[6]  =  "list(  \"[6]\"  nyb_e1of4   4*TrackPitch   BusMetal32 )"
           TempArray[7]  =  "list(  \"[7]\"  nyb_e1of4   4*TrackPitch   BusMetal54 )"
           TempArray[8]  =  "list(  \"[8]\"  nyb_e1of4   4*TrackPitch   BusMetal76 )"
           TempArray[9]  =  "list(  \"[9]\"  nyb_e1of4   5*TrackPitch   BusMetal32 )"
          TempArray[10]  =  "list(  \"[10]\"  nyb_e1of4   5*TrackPitch   BusMetal54 )"
          TempArray[11]  =  "list(  \"[11]\"  nyb_e1of4   5*TrackPitch   BusMetal76 )"
          TempArray[12]  =  "list(  \"[12]\"  nyb_e1of4   6*TrackPitch   BusMetal32 )"
          TempArray[13]  =  "list(  \"[13]\"  nyb_e1of4   6*TrackPitch   BusMetal54 )"
          TempArray[14]  =  "list(  \"[14]\"  nyb_e1of4   6*TrackPitch   BusMetal76 )"
          TempArray[15]  =  "list(  \"[15]\"  nyb_e1of4   7*TrackPitch   BusMetal32 )"
          TempArray[16]  =  "list(  \"[16]\"  nyb_e1of4   7*TrackPitch   BusMetal54 )"
          TempArray[17]  =  "list(  \"[17]\"  nyb_e1of4   7*TrackPitch   BusMetal76 )"
          TempArray[18]  =  "list(  \"[18]\"  nyb_e1of4   8*TrackPitch   BusMetal32 )"
          TempArray[19]  =  "list(  \"[19]\"  nyb_e1of4   8*TrackPitch   BusMetal54 )"
          TempArray[20]  =  "list(  \"[20]\"  nyb_e1of4   8*TrackPitch   BusMetal76 )"
          TempArray[21]  =  "list(  \"[21]\"  nyb_e1of4   9*TrackPitch   BusMetal32 )"
          TempArray[22]  =  "list(  \"[22]\"  nyb_e1of4   9*TrackPitch   BusMetal54 )"
          TempArray[23]  =  "list(  \"[23]\"  nyb_e1of4   9*TrackPitch   BusMetal76 )"
          TempArray[24]  =  "list(  \"[24]\"  nyb_e1of4  10*TrackPitch   BusMetal32 )"
          TempArray[25]  =  "list(  \"[25]\"  nyb_e1of4  10*TrackPitch   BusMetal54 )"
          TempArray[26]  =  "list(  \"[26]\"  nyb_e1of4  10*TrackPitch   BusMetal76 )"
          TempArray[27]  =  "list(  \"[27]\"  nyb_e1of4  11*TrackPitch   BusMetal32 )"
          TempArray[28]  =  "list(  \"[28]\"  nyb_e1of4  11*TrackPitch   BusMetal54 )"
          TempArray[29]  =  "list(  \"[29]\"  nyb_e1of4  11*TrackPitch   BusMetal76 )"
        )

       ( "Template_XBUS"
          TempArray[0]  = "list( \".D[0]\"       inlv_e1of4_A 0*TrackPitch BusMetal45)"
          TempArray[1]  = "list( \".D[1]\"       inlv_e1of4_B 0*TrackPitch BusMetal45)"
          TempArray[2]  = "list( \".D[2]\"       inlv_e1of4_A 3*TrackPitch BusMetal45)"
          TempArray[3]  = "list( \".D[3]\"       inlv_e1of4_B 3*TrackPitch BusMetal45)"
          TempArray[4]  = "list( \".D[4]\"       inlv_e1of4_A 0*TrackPitch BusMetal67)"
          TempArray[5]  = "list( \".D[5]\"       inlv_e1of4_B 0*TrackPitch BusMetal67)"
          TempArray[6]  = "list( \".D[6]\"       inlv_e1of4_A 1*TrackPitch BusMetal67)"
          TempArray[7]  = "list( \".D[7]\"       inlv_e1of4_B 1*TrackPitch BusMetal67)"
          TempArray[8]  = "list( \".D[8]\"       inlv_e1of4_A 2*TrackPitch BusMetal67)"
          TempArray[9]  = "list( \".D[9]\"       inlv_e1of4_B 2*TrackPitch BusMetal67)"
          TempArray[10] = "list( \".D[10]\"      inlv_e1of4_A 3*TrackPitch BusMetal67)"
          TempArray[11] = "list( \".D[11]\"      inlv_e1of4_B 3*TrackPitch BusMetal67)"
          TempArray[12] = "list( \".D[12]\"      inlv_e1of4_A 4*TrackPitch BusMetal23)"
          TempArray[13] = "list( \".D[13]\"      inlv_e1of4_B 4*TrackPitch BusMetal23)"
          TempArray[14] = "list( \".D[14]\"      inlv_e1of4_A 4*TrackPitch BusMetal45)"
          TempArray[15] = "list( \".D[15]\"      inlv_e1of4_B 4*TrackPitch BusMetal45)"
          TempArray[16] = "list( \".D[16]\"      inlv_e1of4_A 4*TrackPitch BusMetal67)"
          TempArray[17] = "list( \".D[17]\"      inlv_e1of4_B 4*TrackPitch BusMetal67)"
          TempArray[18] = "list( \".A[0]\"      inlv_e1of4_A 5*TrackPitch BusMetal23) "
          TempArray[19] = "list( \".A[1]\"      inlv_e1of4_B 5*TrackPitch BusMetal23) "
          TempArray[20] = "list( \"T\"          inlv_e1of4_A 5*TrackPitch BusMetal45) "
        )

       ( "Template_XBUS_R90"
          TempArray[0]  = "list( \".D[0]\"       inlv_e1of4_A 0*TrackPitch BusMetal54)"
          TempArray[1]  = "list( \".D[1]\"       inlv_e1of4_B 0*TrackPitch BusMetal54)"
          TempArray[2]  = "list( \".D[2]\"       inlv_e1of4_A 3*TrackPitch BusMetal54)"
          TempArray[3]  = "list( \".D[3]\"       inlv_e1of4_B 3*TrackPitch BusMetal54)"
          TempArray[4]  = "list( \".D[4]\"       inlv_e1of4_A 0*TrackPitch BusMetal76)"
          TempArray[5]  = "list( \".D[5]\"       inlv_e1of4_B 0*TrackPitch BusMetal76)"
          TempArray[6]  = "list( \".D[6]\"       inlv_e1of4_A 1*TrackPitch BusMetal76)"
          TempArray[7]  = "list( \".D[7]\"       inlv_e1of4_B 1*TrackPitch BusMetal76)"
          TempArray[8]  = "list( \".D[8]\"       inlv_e1of4_A 2*TrackPitch BusMetal76)"
          TempArray[9]  = "list( \".D[9]\"       inlv_e1of4_B 2*TrackPitch BusMetal76)"
          TempArray[10] = "list( \".D[10]\"      inlv_e1of4_A 3*TrackPitch BusMetal76)"
          TempArray[11] = "list( \".D[11]\"      inlv_e1of4_B 3*TrackPitch BusMetal76)"
          TempArray[12] = "list( \".D[12]\"      inlv_e1of4_A 4*TrackPitch BusMetal32)"
          TempArray[13] = "list( \".D[13]\"      inlv_e1of4_B 4*TrackPitch BusMetal32)"
          TempArray[14] = "list( \".D[14]\"      inlv_e1of4_A 4*TrackPitch BusMetal54)"
          TempArray[15] = "list( \".D[15]\"      inlv_e1of4_B 4*TrackPitch BusMetal54)"
          TempArray[16] = "list( \".D[16]\"      inlv_e1of4_A 4*TrackPitch BusMetal76)"
          TempArray[17] = "list( \".D[17]\"      inlv_e1of4_B 4*TrackPitch BusMetal76)"
          TempArray[18] = "list( \".A[0]\"      inlv_e1of4_A 5*TrackPitch BusMetal32) "
          TempArray[19] = "list( \".A[1]\"      inlv_e1of4_B 5*TrackPitch BusMetal32) "
          TempArray[20] = "list( \"T\"          inlv_e1of4_A 5*TrackPitch BusMetal54) "
        )

       ( "Template_SYNC_NODE_R90"
	        TempArray[0]    = "list(   \"[0]\"  node_1W -DefaultWiringPitch*4.5 BusMetal76)"
	        TempArray[1]    = "list(   \"[1]\"  node_1W -DefaultWiringPitch*3.5 BusMetal76)"
	        TempArray[2]    = "list(   \"[2]\"  node_1W -DefaultWiringPitch*2.5 BusMetal76)"
	        TempArray[3]    = "list(   \"[3]\"  node_1W -DefaultWiringPitch*1.5 BusMetal76)"
	        TempArray[4]    = "list(   \"[4]\"  node_1W -DefaultWiringPitch*0.5 BusMetal76)"
	        TempArray[5]    = "list(   \"[5]\"  node_1W  DefaultWiringPitch*0.5 BusMetal76)"
	        TempArray[6]    = "list(   \"[6]\"  node_1W  DefaultWiringPitch*1.5 BusMetal76)"
	        TempArray[7]    = "list(   \"[7]\"  node_1W  DefaultWiringPitch*2.5 BusMetal76)"
	        TempArray[8]    = "list(   \"[8]\"  node_1W  DefaultWiringPitch*3.5 BusMetal76)"
	        TempArray[9]    = "list(   \"[9]\"  node_1W  DefaultWiringPitch*4.5 BusMetal76)"
	        TempArray[10]   = "list(  \"[10]\"  node_1W  DefaultWiringPitch*7.5 BusMetal76)"
	        TempArray[11]   = "list(  \"[11]\"  node_1W  DefaultWiringPitch*8.5 BusMetal76)"
	        TempArray[12]   = "list(  \"[12]\"  node_1W  DefaultWiringPitch*9.5 BusMetal76)"
	        TempArray[13]   = "list(  \"[13]\"  node_1W  DefaultWiringPitch*10.5 BusMetal76)"
	        TempArray[14]   = "list(  \"[14]\"  node_1W  DefaultWiringPitch*11.5 BusMetal76)"
	        TempArray[15]   = "list(  \"[15]\"  node_1W  DefaultWiringPitch*12.5 BusMetal76)"
	        TempArray[16]   = "list(  \"[16]\"  node_1W  DefaultWiringPitch*13.5 BusMetal76)"
	        TempArray[17]   = "list(  \"[17]\"  node_1W  DefaultWiringPitch*14.5 BusMetal76)"
	        TempArray[18]   = "list(  \"[18]\"  node_1W  DefaultWiringPitch*15.5 BusMetal76)"
	        TempArray[19]   = "list(  \"[19]\"  node_1W  DefaultWiringPitch*16.5 BusMetal76)"
	        TempArray[20]   = "list(  \"[20]\"  node_1W  DefaultWiringPitch*19.5 BusMetal32)"
	        TempArray[21]   = "list(  \"[21]\"  node_1W  DefaultWiringPitch*20.5 BusMetal32)"
	        TempArray[22]   = "list(  \"[22]\"  node_1W  DefaultWiringPitch*21.5 BusMetal32)"
	        TempArray[23]   = "list(  \"[23]\"  node_1W  DefaultWiringPitch*22.5 BusMetal32)"
	        TempArray[24]   = "list(  \"[24]\"  node_1W  DefaultWiringPitch*23.5 BusMetal32)"
	        TempArray[25]   = "list(  \"[25]\"  node_1W  DefaultWiringPitch*24.5 BusMetal32)"
	        TempArray[26]   = "list(  \"[26]\"  node_1W  DefaultWiringPitch*25.5 BusMetal32)"
	        TempArray[27]   = "list(  \"[27]\"  node_1W  DefaultWiringPitch*26.5 BusMetal32)"
	        TempArray[28]   = "list(  \"[28]\"  node_1W  DefaultWiringPitch*27.5 BusMetal32)"
	        TempArray[29]   = "list(  \"[29]\"  node_1W  DefaultWiringPitch*28.5 BusMetal32)"
	        TempArray[30]   = "list(  \"[30]\"  node_1W  DefaultWiringPitch*19.5 BusMetal54)"
	        TempArray[31]   = "list(  \"[31]\"  node_1W  DefaultWiringPitch*20.5 BusMetal54)"
	        TempArray[32]   = "list(  \"[32]\"  node_1W  DefaultWiringPitch*21.5 BusMetal54)"
	        TempArray[33]   = "list(  \"[33]\"  node_1W  DefaultWiringPitch*22.5 BusMetal54)"
	        TempArray[34]   = "list(  \"[34]\"  node_1W  DefaultWiringPitch*23.5 BusMetal54)"
	        TempArray[35]   = "list(  \"[35]\"  node_1W  DefaultWiringPitch*24.5 BusMetal54)"
	        TempArray[36]   = "list(  \"[36]\"  node_1W  DefaultWiringPitch*25.5 BusMetal54)"
	        TempArray[37]   = "list(  \"[37]\"  node_1W  DefaultWiringPitch*26.5 BusMetal54)"
	        TempArray[38]   = "list(  \"[38]\"  node_1W  DefaultWiringPitch*27.5 BusMetal54)"
	        TempArray[39]   = "list(  \"[39]\"  node_1W  DefaultWiringPitch*28.5 BusMetal54)"
	        TempArray[40]   = "list(  \"[40]\"  node_1W  DefaultWiringPitch*19.5 BusMetal76)"
	        TempArray[41]   = "list(  \"[41]\"  node_1W  DefaultWiringPitch*20.5 BusMetal76)"
	        TempArray[42]   = "list(  \"[42]\"  node_1W  DefaultWiringPitch*21.5 BusMetal76)"
	        TempArray[43]   = "list(  \"[43]\"  node_1W  DefaultWiringPitch*22.5 BusMetal76)"
	        TempArray[44]   = "list(  \"[44]\"  node_1W  DefaultWiringPitch*23.5 BusMetal76)"
	        TempArray[45]   = "list(  \"[45]\"  node_1W  DefaultWiringPitch*24.5 BusMetal76)"
	        TempArray[46]   = "list(  \"[46]\"  node_1W  DefaultWiringPitch*25.5 BusMetal76)"
	        TempArray[47]   = "list(  \"[47]\"  node_1W  DefaultWiringPitch*26.5 BusMetal76)"
	        TempArray[48]   = "list(  \"[48]\"  node_1W  DefaultWiringPitch*27.5 BusMetal76)"
	        TempArray[49]   = "list(  \"[49]\"  node_1W  DefaultWiringPitch*28.5 BusMetal76)"
	        TempArray[50]   = "list(  \"[50]\"  node_1W  DefaultWiringPitch*31.5 BusMetal32)"
	        TempArray[51]   = "list(  \"[51]\"  node_1W  DefaultWiringPitch*32.5 BusMetal32)"
	        TempArray[52]   = "list(  \"[52]\"  node_1W  DefaultWiringPitch*33.5 BusMetal32)"
	        TempArray[53]   = "list(  \"[53]\"  node_1W  DefaultWiringPitch*34.5 BusMetal32)"
	        TempArray[54]   = "list(  \"[54]\"  node_1W  DefaultWiringPitch*35.5 BusMetal32)"
	        TempArray[55]   = "list(  \"[55]\"  node_1W  DefaultWiringPitch*36.5 BusMetal32)"
	        TempArray[56]   = "list(  \"[56]\"  node_1W  DefaultWiringPitch*37.5 BusMetal32)"
	        TempArray[57]   = "list(  \"[57]\"  node_1W  DefaultWiringPitch*38.5 BusMetal32)"
	        TempArray[58]   = "list(  \"[58]\"  node_1W  DefaultWiringPitch*39.5 BusMetal32)"
	        TempArray[59]   = "list(  \"[59]\"  node_1W  DefaultWiringPitch*40.5 BusMetal32)"
	        TempArray[60]   = "list(  \"[60]\"  node_1W  DefaultWiringPitch*31.5 BusMetal54)"
	        TempArray[61]   = "list(  \"[61]\"  node_1W  DefaultWiringPitch*32.5 BusMetal54)"
	        TempArray[62]   = "list(  \"[62]\"  node_1W  DefaultWiringPitch*33.5 BusMetal54)"
	        TempArray[63]   = "list(  \"[63]\"  node_1W  DefaultWiringPitch*34.5 BusMetal54)"
	        TempArray[64]   = "list(  \"[64]\"  node_1W  DefaultWiringPitch*35.5 BusMetal54)"
	        TempArray[65]   = "list(  \"[65]\"  node_1W  DefaultWiringPitch*36.5 BusMetal54)"
	        TempArray[66]   = "list(  \"[66]\"  node_1W  DefaultWiringPitch*37.5 BusMetal54)"
	        TempArray[67]   = "list(  \"[67]\"  node_1W  DefaultWiringPitch*38.5 BusMetal54)"
	        TempArray[68]   = "list(  \"[68]\"  node_1W  DefaultWiringPitch*39.5 BusMetal54)"
	        TempArray[69]   = "list(  \"[69]\"  node_1W  DefaultWiringPitch*40.5 BusMetal54)"
	        TempArray[70]   = "list(  \"[70]\"  node_1W  DefaultWiringPitch*31.5 BusMetal76)"
	        TempArray[71]   = "list(  \"[71]\"  node_1W  DefaultWiringPitch*32.5 BusMetal76)"
	        TempArray[72]   = "list(  \"[72]\"  node_1W  DefaultWiringPitch*33.5 BusMetal76)"
	        TempArray[73]   = "list(  \"[73]\"  node_1W  DefaultWiringPitch*34.5 BusMetal76)"
	        TempArray[74]   = "list(  \"[74]\"  node_1W  DefaultWiringPitch*35.5 BusMetal76)"
	        TempArray[75]   = "list(  \"[75]\"  node_1W  DefaultWiringPitch*36.5 BusMetal76)"
	        TempArray[76]   = "list(  \"[76]\"  node_1W  DefaultWiringPitch*37.5 BusMetal76)"
	        TempArray[77]   = "list(  \"[77]\"  node_1W  DefaultWiringPitch*38.5 BusMetal76)"
	        TempArray[78]   = "list(  \"[78]\"  node_1W  DefaultWiringPitch*39.5 BusMetal76)"
	        TempArray[79]   = "list(  \"[79]\"  node_1W  DefaultWiringPitch*40.5 BusMetal76)"
	        TempArray[80]  =  "list(   \"[80]\" node_1W DefaultWiringPitch*43.5 BusMetal32)"
	        TempArray[81]  =  "list(   \"[81]\" node_1W DefaultWiringPitch*44.5 BusMetal32)"
	        TempArray[82]  =  "list(   \"[82]\" node_1W DefaultWiringPitch*45.5 BusMetal32)"
	        TempArray[83]  =  "list(   \"[83]\" node_1W DefaultWiringPitch*46.5 BusMetal32)"
	        TempArray[84]  =  "list(   \"[84]\" node_1W DefaultWiringPitch*47.5 BusMetal32)"
	        TempArray[85]  =  "list(   \"[85]\" node_1W DefaultWiringPitch*48.5 BusMetal32)"
	        TempArray[86]  =  "list(   \"[86]\" node_1W DefaultWiringPitch*49.5 BusMetal32)"
	        TempArray[87]  =  "list(   \"[87]\" node_1W DefaultWiringPitch*50.5 BusMetal32)"
	        TempArray[88]  =  "list(   \"[88]\" node_1W DefaultWiringPitch*51.5 BusMetal32)"
	        TempArray[89]  =  "list(   \"[89]\" node_1W DefaultWiringPitch*52.5 BusMetal32)"
	        TempArray[90]  =  "list(   \"[90]\" node_1W DefaultWiringPitch*43.5 BusMetal54)"
	        TempArray[91]  =  "list(   \"[91]\" node_1W DefaultWiringPitch*44.5 BusMetal54)"
	        TempArray[92]  =  "list(   \"[92]\" node_1W DefaultWiringPitch*45.5 BusMetal54)"
	        TempArray[93]  =  "list(   \"[93]\" node_1W DefaultWiringPitch*46.5 BusMetal54)"
	        TempArray[94]  =  "list(   \"[94]\" node_1W DefaultWiringPitch*47.5 BusMetal54)"
	        TempArray[95]  =  "list(   \"[95]\" node_1W DefaultWiringPitch*48.8 BusMetal54)"
	        TempArray[96]  =  "list(   \"[96]\" node_1W DefaultWiringPitch*49.5 BusMetal54)"
	        TempArray[97]  =  "list(   \"[97]\" node_1W DefaultWiringPitch*50.5 BusMetal54)"
	        TempArray[98]  =  "list(   \"[98]\" node_1W DefaultWiringPitch*51.5 BusMetal54)"
	        TempArray[99]  =  "list(   \"[99]\" node_1W DefaultWiringPitch*52.5 BusMetal54)"
	        TempArray[100]  = "list(   \"[100]\" node_1W DefaultWiringPitch*43.5 BusMetal76)"
	        TempArray[101]  = "list(   \"[101]\" node_1W DefaultWiringPitch*44.5 BusMetal76)"
	        TempArray[102]  = "list(   \"[102]\" node_1W DefaultWiringPitch*45.5 BusMetal76)"
	        TempArray[103]  = "list(   \"[103]\" node_1W DefaultWiringPitch*46.5 BusMetal76)"
	        TempArray[104]  = "list(   \"[104]\" node_1W DefaultWiringPitch*47.5 BusMetal76)"
	        TempArray[105]  = "list(   \"[105]\" node_1W DefaultWiringPitch*48.5 BusMetal76)"
	        TempArray[106]  = "list(   \"[106]\" node_1W DefaultWiringPitch*49.5 BusMetal76)"
	        TempArray[107]  = "list(   \"[107]\" node_1W DefaultWiringPitch*50.5 BusMetal76)"
	        TempArray[108]  = "list(   \"[108]\" node_1W DefaultWiringPitch*51.5 BusMetal76)"
	        TempArray[109]  = "list(   \"[109]\" node_1W DefaultWiringPitch*52.5 BusMetal76)"
	        TempArray[110]  = "list(   \"[110]\" node_1W DefaultWiringPitch*55.5 BusMetal32)"
	        TempArray[111]  = "list(   \"[111]\" node_1W DefaultWiringPitch*56.5 BusMetal32)"
	        TempArray[112]  = "list(   \"[112]\" node_1W DefaultWiringPitch*57.5 BusMetal32)"
	        TempArray[113]  = "list(   \"[113]\" node_1W DefaultWiringPitch*58.5 BusMetal32)"
	        TempArray[114]  = "list(   \"[114]\" node_1W DefaultWiringPitch*59.5 BusMetal32)"
	        TempArray[115]  = "list(   \"[115]\" node_1W DefaultWiringPitch*60.5 BusMetal32)"
	        TempArray[116]  = "list(   \"[116]\" node_1W DefaultWiringPitch*61.5 BusMetal32)"
	        TempArray[117]  = "list(   \"[117]\" node_1W DefaultWiringPitch*62.5 BusMetal32)"
	        TempArray[118]  = "list(   \"[118]\" node_1W DefaultWiringPitch*63.5 BusMetal32)"
	        TempArray[119]  = "list(   \"[119]\" node_1W DefaultWiringPitch*64.5 BusMetal32)"
	        TempArray[120]  = "list(   \"[120]\" node_1W DefaultWiringPitch*55.5 BusMetal54)"
	        TempArray[121]  = "list(   \"[121]\" node_1W DefaultWiringPitch*56.5 BusMetal54)"
	        TempArray[122]  = "list(   \"[122]\" node_1W DefaultWiringPitch*57.5 BusMetal54)"
	        TempArray[123]  = "list(   \"[123]\" node_1W DefaultWiringPitch*58.5 BusMetal54)"
	        TempArray[124]  = "list(   \"[124]\" node_1W DefaultWiringPitch*59.5 BusMetal54)"
	        TempArray[125]  = "list(   \"[125]\" node_1W DefaultWiringPitch*60.5 BusMetal54)"
	        TempArray[126]  = "list(   \"[126]\" node_1W DefaultWiringPitch*61.5 BusMetal54)"
	        TempArray[127]  = "list(   \"[127]\" node_1W DefaultWiringPitch*62.5 BusMetal54)"
	        TempArray[128]  = "list(   \"[128]\" node_1W DefaultWiringPitch*63.5 BusMetal54)"
	        TempArray[129]  = "list(   \"[129]\" node_1W DefaultWiringPitch*64.5 BusMetal54)"
	        TempArray[130]  = "list(   \"[130]\" node_1W DefaultWiringPitch*55.5 BusMetal76)"
	        TempArray[131]  = "list(   \"[131]\" node_1W DefaultWiringPitch*56.5 BusMetal76)"
	        TempArray[132]  = "list(   \"[132]\" node_1W DefaultWiringPitch*57.5 BusMetal76)"
	        TempArray[133]  = "list(   \"[133]\" node_1W DefaultWiringPitch*58.5 BusMetal76)"
	        TempArray[134]  = "list(   \"[134]\" node_1W DefaultWiringPitch*59.5 BusMetal76)"
	        TempArray[135]  = "list(   \"[135]\" node_1W DefaultWiringPitch*60.5 BusMetal76)"
	        TempArray[136]  = "list(   \"[136]\" node_1W DefaultWiringPitch*61.5 BusMetal76)"
	        TempArray[137]  = "list(   \"[137]\" node_1W DefaultWiringPitch*62.5 BusMetal76)"
	        TempArray[138]  = "list(   \"[138]\" node_1W DefaultWiringPitch*63.5 BusMetal76)"
	        TempArray[139]  = "list(   \"[139]\" node_1W DefaultWiringPitch*64.4 BusMetal76)"
      )

     ( "Template_SYNC_NODE"
	        TempArray[0]    = "list(   \"[0]\"  node_1W -DefaultWiringPitch*4.5 BusMetal67)"
	        TempArray[1]    = "list(   \"[1]\"  node_1W -DefaultWiringPitch*3.5 BusMetal67)"
	        TempArray[2]    = "list(   \"[2]\"  node_1W -DefaultWiringPitch*2.5 BusMetal67)"
	        TempArray[3]    = "list(   \"[3]\"  node_1W -DefaultWiringPitch*1.5 BusMetal67)"
	        TempArray[4]    = "list(   \"[4]\"  node_1W -DefaultWiringPitch*0.5 BusMetal67)"
	        TempArray[5]    = "list(   \"[5]\"  node_1W  DefaultWiringPitch*0.5 BusMetal67)"
	        TempArray[6]    = "list(   \"[6]\"  node_1W  DefaultWiringPitch*1.5 BusMetal67)"
	        TempArray[7]    = "list(   \"[7]\"  node_1W  DefaultWiringPitch*2.5 BusMetal67)"
	        TempArray[8]    = "list(   \"[8]\"  node_1W  DefaultWiringPitch*3.5 BusMetal67)"
	        TempArray[9]    = "list(   \"[9]\"  node_1W  DefaultWiringPitch*4.5 BusMetal67)"
	        TempArray[10]   = "list(  \"[10]\"  node_1W  DefaultWiringPitch*7.5 BusMetal67)"
	        TempArray[11]   = "list(  \"[11]\"  node_1W  DefaultWiringPitch*8.5 BusMetal67)"
	        TempArray[12]   = "list(  \"[12]\"  node_1W  DefaultWiringPitch*9.5 BusMetal67)"
	        TempArray[13]   = "list(  \"[13]\"  node_1W  DefaultWiringPitch*10.5 BusMetal67)"
	        TempArray[14]   = "list(  \"[14]\"  node_1W  DefaultWiringPitch*11.5 BusMetal67)"
	        TempArray[15]   = "list(  \"[15]\"  node_1W  DefaultWiringPitch*12.5 BusMetal67)"
	        TempArray[16]   = "list(  \"[16]\"  node_1W  DefaultWiringPitch*13.5 BusMetal67)"
	        TempArray[17]   = "list(  \"[17]\"  node_1W  DefaultWiringPitch*14.5 BusMetal67)"
	        TempArray[18]   = "list(  \"[18]\"  node_1W  DefaultWiringPitch*15.5 BusMetal67)"
	        TempArray[19]   = "list(  \"[19]\"  node_1W  DefaultWiringPitch*16.5 BusMetal67)"
	        TempArray[20]   = "list(  \"[20]\"  node_1W  DefaultWiringPitch*19.5 BusMetal23)"
	        TempArray[21]   = "list(  \"[21]\"  node_1W  DefaultWiringPitch*20.5 BusMetal23)"
	        TempArray[22]   = "list(  \"[22]\"  node_1W  DefaultWiringPitch*21.5 BusMetal23)"
	        TempArray[23]   = "list(  \"[23]\"  node_1W  DefaultWiringPitch*22.5 BusMetal23)"
	        TempArray[24]   = "list(  \"[24]\"  node_1W  DefaultWiringPitch*23.5 BusMetal23)"
	        TempArray[25]   = "list(  \"[25]\"  node_1W  DefaultWiringPitch*24.5 BusMetal23)"
	        TempArray[26]   = "list(  \"[26]\"  node_1W  DefaultWiringPitch*25.5 BusMetal23)"
	        TempArray[27]   = "list(  \"[27]\"  node_1W  DefaultWiringPitch*26.5 BusMetal23)"
	        TempArray[28]   = "list(  \"[28]\"  node_1W  DefaultWiringPitch*27.5 BusMetal23)"
	        TempArray[29]   = "list(  \"[29]\"  node_1W  DefaultWiringPitch*28.5 BusMetal23)"
	        TempArray[30]   = "list(  \"[30]\"  node_1W  DefaultWiringPitch*19.5 BusMetal45)"
	        TempArray[31]   = "list(  \"[31]\"  node_1W  DefaultWiringPitch*20.5 BusMetal45)"
	        TempArray[32]   = "list(  \"[32]\"  node_1W  DefaultWiringPitch*21.5 BusMetal45)"
	        TempArray[33]   = "list(  \"[33]\"  node_1W  DefaultWiringPitch*22.5 BusMetal45)"
	        TempArray[34]   = "list(  \"[34]\"  node_1W  DefaultWiringPitch*23.5 BusMetal45)"
	        TempArray[35]   = "list(  \"[35]\"  node_1W  DefaultWiringPitch*24.5 BusMetal45)"
	        TempArray[36]   = "list(  \"[36]\"  node_1W  DefaultWiringPitch*25.5 BusMetal45)"
	        TempArray[37]   = "list(  \"[37]\"  node_1W  DefaultWiringPitch*26.5 BusMetal45)"
	        TempArray[38]   = "list(  \"[38]\"  node_1W  DefaultWiringPitch*27.5 BusMetal45)"
	        TempArray[39]   = "list(  \"[39]\"  node_1W  DefaultWiringPitch*28.5 BusMetal45)"
	        TempArray[40]   = "list(  \"[40]\"  node_1W  DefaultWiringPitch*19.5 BusMetal67)"
	        TempArray[41]   = "list(  \"[41]\"  node_1W  DefaultWiringPitch*20.5 BusMetal67)"
	        TempArray[42]   = "list(  \"[42]\"  node_1W  DefaultWiringPitch*21.5 BusMetal67)"
	        TempArray[43]   = "list(  \"[43]\"  node_1W  DefaultWiringPitch*22.5 BusMetal67)"
	        TempArray[44]   = "list(  \"[44]\"  node_1W  DefaultWiringPitch*23.5 BusMetal67)"
	        TempArray[45]   = "list(  \"[45]\"  node_1W  DefaultWiringPitch*24.5 BusMetal67)"
	        TempArray[46]   = "list(  \"[46]\"  node_1W  DefaultWiringPitch*25.5 BusMetal67)"
	        TempArray[47]   = "list(  \"[47]\"  node_1W  DefaultWiringPitch*26.5 BusMetal67)"
	        TempArray[48]   = "list(  \"[48]\"  node_1W  DefaultWiringPitch*27.5 BusMetal67)"
	        TempArray[49]   = "list(  \"[49]\"  node_1W  DefaultWiringPitch*28.5 BusMetal67)"
	        TempArray[50]   = "list(  \"[50]\"  node_1W  DefaultWiringPitch*31.5 BusMetal23)"
	        TempArray[51]   = "list(  \"[51]\"  node_1W  DefaultWiringPitch*32.5 BusMetal23)"
	        TempArray[52]   = "list(  \"[52]\"  node_1W  DefaultWiringPitch*33.5 BusMetal23)"
	        TempArray[53]   = "list(  \"[53]\"  node_1W  DefaultWiringPitch*34.5 BusMetal23)"
	        TempArray[54]   = "list(  \"[54]\"  node_1W  DefaultWiringPitch*35.5 BusMetal23)"
	        TempArray[55]   = "list(  \"[55]\"  node_1W  DefaultWiringPitch*36.5 BusMetal23)"
	        TempArray[56]   = "list(  \"[56]\"  node_1W  DefaultWiringPitch*37.5 BusMetal23)"
	        TempArray[57]   = "list(  \"[57]\"  node_1W  DefaultWiringPitch*38.5 BusMetal23)"
	        TempArray[58]   = "list(  \"[58]\"  node_1W  DefaultWiringPitch*39.5 BusMetal23)"
	        TempArray[59]   = "list(  \"[59]\"  node_1W  DefaultWiringPitch*40.5 BusMetal23)"
	        TempArray[60]   = "list(  \"[60]\"  node_1W  DefaultWiringPitch*31.5 BusMetal45)"
	        TempArray[61]   = "list(  \"[61]\"  node_1W  DefaultWiringPitch*32.5 BusMetal45)"
	        TempArray[62]   = "list(  \"[62]\"  node_1W  DefaultWiringPitch*33.5 BusMetal45)"
	        TempArray[63]   = "list(  \"[63]\"  node_1W  DefaultWiringPitch*34.5 BusMetal45)"
	        TempArray[64]   = "list(  \"[64]\"  node_1W  DefaultWiringPitch*35.5 BusMetal45)"
	        TempArray[65]   = "list(  \"[65]\"  node_1W  DefaultWiringPitch*36.5 BusMetal45)"
	        TempArray[66]   = "list(  \"[66]\"  node_1W  DefaultWiringPitch*37.5 BusMetal45)"
	        TempArray[67]   = "list(  \"[67]\"  node_1W  DefaultWiringPitch*38.5 BusMetal45)"
	        TempArray[68]   = "list(  \"[68]\"  node_1W  DefaultWiringPitch*39.5 BusMetal45)"
	        TempArray[69]   = "list(  \"[69]\"  node_1W  DefaultWiringPitch*40.5 BusMetal45)"
	        TempArray[70]   = "list(  \"[70]\"  node_1W  DefaultWiringPitch*31.5 BusMetal67)"
	        TempArray[71]   = "list(  \"[71]\"  node_1W  DefaultWiringPitch*32.5 BusMetal67)"
	        TempArray[72]   = "list(  \"[72]\"  node_1W  DefaultWiringPitch*33.5 BusMetal67)"
	        TempArray[73]   = "list(  \"[73]\"  node_1W  DefaultWiringPitch*34.5 BusMetal67)"
	        TempArray[74]   = "list(  \"[74]\"  node_1W  DefaultWiringPitch*35.5 BusMetal67)"
	        TempArray[75]   = "list(  \"[75]\"  node_1W  DefaultWiringPitch*36.5 BusMetal67)"
	        TempArray[76]   = "list(  \"[76]\"  node_1W  DefaultWiringPitch*37.5 BusMetal67)"
	        TempArray[77]   = "list(  \"[77]\"  node_1W  DefaultWiringPitch*38.5 BusMetal67)"
	        TempArray[78]   = "list(  \"[78]\"  node_1W  DefaultWiringPitch*39.5 BusMetal67)"
	        TempArray[79]   = "list(  \"[79]\"  node_1W  DefaultWiringPitch*40.5 BusMetal67)"
	        TempArray[80]  =  "list(   \"[80]\" node_1W DefaultWiringPitch*43.5 BusMetal23)"
	        TempArray[81]  =  "list(   \"[81]\" node_1W DefaultWiringPitch*44.5 BusMetal23)"
	        TempArray[82]  =  "list(   \"[82]\" node_1W DefaultWiringPitch*45.5 BusMetal23)"
	        TempArray[83]  =  "list(   \"[83]\" node_1W DefaultWiringPitch*46.5 BusMetal23)"
	        TempArray[84]  =  "list(   \"[84]\" node_1W DefaultWiringPitch*47.5 BusMetal23)"
	        TempArray[85]  =  "list(   \"[85]\" node_1W DefaultWiringPitch*48.5 BusMetal23)"
	        TempArray[86]  =  "list(   \"[86]\" node_1W DefaultWiringPitch*49.5 BusMetal23)"
	        TempArray[87]  =  "list(   \"[87]\" node_1W DefaultWiringPitch*50.5 BusMetal23)"
	        TempArray[88]  =  "list(   \"[88]\" node_1W DefaultWiringPitch*51.5 BusMetal23)"
	        TempArray[89]  =  "list(   \"[89]\" node_1W DefaultWiringPitch*52.5 BusMetal23)"
	        TempArray[90]  =  "list(   \"[90]\" node_1W DefaultWiringPitch*43.5 BusMetal45)"
	        TempArray[91]  =  "list(   \"[91]\" node_1W DefaultWiringPitch*44.5 BusMetal45)"
	        TempArray[92]  =  "list(   \"[92]\" node_1W DefaultWiringPitch*45.5 BusMetal45)"
	        TempArray[93]  =  "list(   \"[93]\" node_1W DefaultWiringPitch*46.5 BusMetal45)"
	        TempArray[94]  =  "list(   \"[94]\" node_1W DefaultWiringPitch*47.5 BusMetal45)"
	        TempArray[95]  =  "list(   \"[95]\" node_1W DefaultWiringPitch*48.8 BusMetal45)"
	        TempArray[96]  =  "list(   \"[96]\" node_1W DefaultWiringPitch*49.5 BusMetal45)"
	        TempArray[97]  =  "list(   \"[97]\" node_1W DefaultWiringPitch*50.5 BusMetal45)"
	        TempArray[98]  =  "list(   \"[98]\" node_1W DefaultWiringPitch*51.5 BusMetal45)"
	        TempArray[99]  =  "list(   \"[99]\" node_1W DefaultWiringPitch*52.5 BusMetal45)"
	        TempArray[100]  = "list(   \"[100]\" node_1W DefaultWiringPitch*43.5 BusMetal67)"
	        TempArray[101]  = "list(   \"[101]\" node_1W DefaultWiringPitch*44.5 BusMetal67)"
	        TempArray[102]  = "list(   \"[102]\" node_1W DefaultWiringPitch*45.5 BusMetal67)"
	        TempArray[103]  = "list(   \"[103]\" node_1W DefaultWiringPitch*46.5 BusMetal67)"
	        TempArray[104]  = "list(   \"[104]\" node_1W DefaultWiringPitch*47.5 BusMetal67)"
	        TempArray[105]  = "list(   \"[105]\" node_1W DefaultWiringPitch*48.5 BusMetal67)"
	        TempArray[106]  = "list(   \"[106]\" node_1W DefaultWiringPitch*49.5 BusMetal67)"
	        TempArray[107]  = "list(   \"[107]\" node_1W DefaultWiringPitch*50.5 BusMetal67)"
	        TempArray[108]  = "list(   \"[108]\" node_1W DefaultWiringPitch*51.5 BusMetal67)"
	        TempArray[109]  = "list(   \"[109]\" node_1W DefaultWiringPitch*52.5 BusMetal67)"
	        TempArray[110]  = "list(   \"[110]\" node_1W DefaultWiringPitch*55.5 BusMetal23)"
	        TempArray[111]  = "list(   \"[111]\" node_1W DefaultWiringPitch*56.5 BusMetal23)"
	        TempArray[112]  = "list(   \"[112]\" node_1W DefaultWiringPitch*57.5 BusMetal23)"
	        TempArray[113]  = "list(   \"[113]\" node_1W DefaultWiringPitch*58.5 BusMetal23)"
	        TempArray[114]  = "list(   \"[114]\" node_1W DefaultWiringPitch*59.5 BusMetal23)"
	        TempArray[115]  = "list(   \"[115]\" node_1W DefaultWiringPitch*60.5 BusMetal23)"
	        TempArray[116]  = "list(   \"[116]\" node_1W DefaultWiringPitch*61.5 BusMetal23)"
	        TempArray[117]  = "list(   \"[117]\" node_1W DefaultWiringPitch*62.5 BusMetal23)"
	        TempArray[118]  = "list(   \"[118]\" node_1W DefaultWiringPitch*63.5 BusMetal23)"
	        TempArray[119]  = "list(   \"[119]\" node_1W DefaultWiringPitch*64.5 BusMetal23)"
	        TempArray[120]  = "list(   \"[120]\" node_1W DefaultWiringPitch*55.5 BusMetal45)"
	        TempArray[121]  = "list(   \"[121]\" node_1W DefaultWiringPitch*56.5 BusMetal45)"
	        TempArray[122]  = "list(   \"[122]\" node_1W DefaultWiringPitch*57.5 BusMetal45)"
	        TempArray[123]  = "list(   \"[123]\" node_1W DefaultWiringPitch*58.5 BusMetal45)"
	        TempArray[124]  = "list(   \"[124]\" node_1W DefaultWiringPitch*59.5 BusMetal45)"
	        TempArray[125]  = "list(   \"[125]\" node_1W DefaultWiringPitch*60.5 BusMetal45)"
	        TempArray[126]  = "list(   \"[126]\" node_1W DefaultWiringPitch*61.5 BusMetal45)"
	        TempArray[127]  = "list(   \"[127]\" node_1W DefaultWiringPitch*62.5 BusMetal45)"
	        TempArray[128]  = "list(   \"[128]\" node_1W DefaultWiringPitch*63.5 BusMetal45)"
	        TempArray[129]  = "list(   \"[129]\" node_1W DefaultWiringPitch*64.5 BusMetal45)"
	        TempArray[130]  = "list(   \"[130]\" node_1W DefaultWiringPitch*55.5 BusMetal67)"
	        TempArray[131]  = "list(   \"[131]\" node_1W DefaultWiringPitch*56.5 BusMetal67)"
	        TempArray[132]  = "list(   \"[132]\" node_1W DefaultWiringPitch*57.5 BusMetal67)"
	        TempArray[133]  = "list(   \"[133]\" node_1W DefaultWiringPitch*58.5 BusMetal67)"
	        TempArray[134]  = "list(   \"[134]\" node_1W DefaultWiringPitch*59.5 BusMetal67)"
	        TempArray[135]  = "list(   \"[135]\" node_1W DefaultWiringPitch*60.5 BusMetal67)"
	        TempArray[136]  = "list(   \"[136]\" node_1W DefaultWiringPitch*61.5 BusMetal67)"
	        TempArray[137]  = "list(   \"[137]\" node_1W DefaultWiringPitch*62.5 BusMetal67)"
	        TempArray[138]  = "list(   \"[138]\" node_1W DefaultWiringPitch*63.5 BusMetal67)"
	        TempArray[139]  = "list(   \"[139]\" node_1W DefaultWiringPitch*64.4 BusMetal67)"
      )

   )

   TempArray
 )
)

(defun CreateCableDefChanFile ()
  (let ( k1 k2 k3 K I s J )
    k1 = list( "_DENSE" "_DENSE_R90" "_NYB" "_NYB_R90" "_TS" "_TS_R90"  "_XBUS" "_XBUS_R90" )
    
    k2 = outfile("cable_defchan.il")
    
    for( K 0 length(k1)-1
      s = nth( K k1 )
      k3=CableDefChanTemplate(strcat("Template" s))
      when( s != "_XBUS" && s != "_XBUS_R90"
         for(I 0 29 
           fprintf( k2 "CABLE%s_%d = (DefChan (list \n" s I+1)
             for( J 0 I
              fprintf( k2 "\t%s\n" k3[J] )
             )
           fprintf( k2 ") ) \n" )
           fprintf( k2 "(DrawChannels nil CABLE%s_%d nil \"CABLE%s_%d\")\n" s I+1 s I+1)
         )
      )
      when( s == "_XBUS" || s == "_XBUS_R90"
           fprintf( k2 "CABLE%s = (DefChan (list \n" s )
             for( J 0 20
              fprintf( k2 "\t%s\n" k3[J] )
             )
           fprintf( k2 ") ) \n" )
           fprintf( k2 "(DrawChannels nil CABLE%s nil \"CABLE%s\")\n" s s )
      )
    )
    
    close(k2)
 )
)

(defun CreateNodeCableDefChanFile ()
  (let ( k1 k2 k3 K I s J )
    k1 = list( "_SYNC_NODE" "_SYNC_NODE_R90" )
    k2 = outfile("node_cable_defchan.il")

    for( K 0 length(k1)-1
      s = nth( K k1 )    
      k3=CableDefChanTemplate(strcat("Template" s))
      println(strcat("Template" s))
          for(I 0 139
            fprintf( k2 "CABLE%s_%d = (DefChan (list \n" s I+1)
              for( J 0 I
               fprintf( k2 "\t%s\n" k3[J] )
              )
            fprintf( k2 ") ) \n" )
            fprintf( k2 "(DrawChannels nil CABLE%s_%d nil \"CABLE%s_%d\")\n" s I+1 s I+1)
          )
      )
    
    close(k2)
 )
)
