;; -*- Mode:Common-Lisp; Package:devo; Base:10 -*-
(IN-PACKAGE :devo)


(defparameter *n5-d0*		.07)
(defparameter *n5-nfactor*	32);;34.25 for N7

(defun n5-area-yield (area &optional &key
				     (ram-area 0)
				     (redundant-area 0)
				     (channel-area 0)
				     (nfactor *n5-nfactor*
					      ;;(- 34.25 3.25) ;;est N6
					      )
				     (channel-nfactor 9)
				     (d0 *n5-d0*)
				     (ram-d0 (* 2 d0)) ;;area includes ram-area
				     teg?
				     ;;(teg? t)
				     )
  (if teg? 
      (teg-area-yield area ram-area
		      :redundant-area	redundant-area
		      :channel-area	channel-area
		      :nfactor		nfactor
		      :channel-nfactor	channel-nfactor
		      :d0		d0
		      :ram-d0		ram-d0)
    (area-yield area ram-area
		:redundant-area		redundant-area
		:channel-area		channel-area
		:nfactor		nfactor
		:channel-nfactor	channel-nfactor
		:d0			d0
		:ram-d0			ram-d0)))
	      

(defun teg-area-yield (area &optional (ram-area 0) &key (redundant-area 0)
							(channel-area 0)
							(nfactor 32
								 ;;(- 34.25 3.25) ;;est N6
								 )
							(channel-nfactor 9)
							(d0 .07)
							(repaired-ram-k .6);;.6 ;;area includes ram-area
							ram-d0
							(alpha .05))
  ram-d0
  (let*((logic-area		(- area ram-area redundant-area channel-area))
	(logic-yield		(%teg-yield logic-area			d0 nfactor alpha)) ;;(expt (+ 1 (* eff-area d0-mm)) (- nfactor))
	(ram-repaired-yield	(%teg-yield (* ram-area repaired-ram-k) d0 nfactor alpha))
	(channel-yield		(%teg-yield channel-area		d0 channel-nfactor alpha))
	)
    (* logic-yield ram-repaired-yield channel-yield)
    ))

(defun %teg-yield (area d0 nfactor alpha)
  "yield = (1 + area * d0 / 25.4**2) ** -nfactor,
   yield = (expt (1+ (/ (* area nfactor d0) alpha)) (- (* n alpha)))
   which is approximately e ** -(area * d0 * nfactor / 25.4**2)
   area is in mm**2. so the 25.4**2 converts to 
   d0 defects per square inch on each of nfactor layers"
  (let*((d0-mm		(/ d0 25.4 25.4)))
    (expt (1+ (/ (* area d0-mm) alpha)) (- (* nfactor alpha)))))

(defun tfc-yield (&optional &key
			    (d0		*n5-d0*)
			    (nfactor	*n5-nfactor*)
			    summary-only?
			    )
  "yield improvements:
   Spare mac per quadrant
   Mark one cdm memory block bad in s/w (out of 192)
   Use spare bit in cdm for logic/wiring redundancy
   Mark cdm block bad if address logic/wiring bad to that block
   Use spare bit in stm for memory and logic defect tolerance
   Use spare wire in mac channels
   25T options:
   Even/odd pipes, still requires both tm_cores, all cdm functional
   Left/right pipes, requires only one tm_core, half cdm functional.
  "
  (let*((*n5-d0*		d0)	;;used by n5-area-yield
	(*n5-nfactor*		nfactor);;used by n5-area-yield
	;;sram module areas
	(stm-unit-sram-area	(* .0373830  .1264480))
	(ppu-unit-tcam-area	(* .1292850  .0382200))
	;;ppu params
	(npipes			16)
	(ppu-nstages		13)
	(ppu-num-stm-rows	12) ;;ingress + egress
	(ppu-num-ram-cols/stg	8)
	(num-tcams/stage	16)
	(stm-row-y		(/ 1.059424 8))
	(parde-x		.98)	;;leftover x in 4.7mm after 13ppu stages. just an estimate
	(ppu-x			.339456);;per stage
	(ppu-core-y		.3234)
	(tcam-array-y		.40656)
	;;cdm params
	(cdm-unit-sram-x	.0233070)
	(cdm-unit-sram-y	.1222480)
	(cdm-unit-sram-area	(* cdm-unit-sram-x cdm-unit-sram-y))
	(num-cdm-ram-cols	128)
	(num-cdm-ram-rows/subwd 6)
	(num-cdm-ram-subwords	34)
	(num-cdm-ram-rows	(* num-cdm-ram-rows/subwd num-cdm-ram-subwords))
	(num-cdm-unit-rams	(* num-cdm-ram-rows num-cdm-ram-cols))
	(cdm-x-overhead		(/ (* 50 .204) (* 2 23.652))) ;; 0.21562658 50 gates per rampair
	(cdm-y-overhead		(/ (+  (* 5 52)(* 1 148))
				   (* 6 588) 1.0));; 5x52G + 1x148G per 6 ram rows = 0.11564626 y overhead
	(cdm-area-mlpr		(* (1+ cdm-x-overhead)(1+ cdm-y-overhead)))
	(cdm-x			(* cdm-unit-sram-x num-cdm-ram-cols (1+ cdm-x-overhead)))
	(cdm-y			(* cdm-unit-sram-y num-cdm-ram-rows (1+ cdm-y-overhead)))
	(cdm-sram-area		(* cdm-unit-sram-area num-cdm-unit-rams))
	(cdm-area		(* cdm-sram-area cdm-area-mlpr))
	(cdm-logic-area		(- cdm-area cdm-sram-area))
	;;tm core
	(tm-core-area		(* 6.3 6.9))
	(tm-core-ramfraction	.33)	;;just a guess
	;;mac
	(mac-area		(* 1.5 .675))
	;;mac channels
	(mac-channel-lr-width	.6) ;;est 30mm tall
	(mac-channel-tb-width	.5) ;;est 20mm wide
	;;serdes
	(serdes-area		(+ (* 32 3.7 1.5)(* 8 5 1.5)))
	(serdes-kfactor		.8)
	;;misc blocks
	(misc-areas		(+ 2.72 ;;host+sbc
				   3.4	;;risc
				   (* 1.5 .675) ;;eth800G
				   .45		;;ethcpu
				   .45))	;;tcu
	(gpio-area		(+ 1.96 5.69))
	;;sizes and yields
	;;misc/gpio
	(misc-yield		(n5-area-yield misc-areas :ram-area (* misc-areas .25) :channel-area (* misc-areas .25)))
	(gpio-yield		(n5-area-yield gpio-area :channel-area gpio-area))
	;;serdes
	(serdes-yield		(n5-area-yield (* serdes-area serdes-kfactor)))
	;;mac
	(onemac-yield		(n5-area-yield mac-area))
	(mac-yield		(n5-area-yield (* 64 mac-area)))
	;;mac channel
	(mac-channel-area	(* 2 (+ (* mac-channel-lr-width 30)
					(* mac-channel-tb-width 20))))
	(mac-channel-yield	(n5-area-yield mac-channel-area :channel-area mac-channel-area))
	;;ppu, parde
	(ppu-ysize		(+ (* ppu-num-stm-rows stm-row-y)(* 2 ppu-core-y) tcam-array-y))
	(parde-area		(* parde-x ppu-ysize npipes))
	(parde-ramfraction	.33)
	(parde-yield		(n5-area-yield parde-area :ram-area (* parde-area parde-ramfraction)))
	(ppu-core-area		(* ppu-nstages 2 npipes ppu-x ppu-core-y))
	(ppu-core-yield		(n5-area-yield ppu-core-area))
	;;tcam
	(tcam-area		(* tcam-array-y ppu-x ppu-nstages npipes))
	(tcam-memory-area	(* ppu-unit-tcam-area num-tcams/stage ppu-nstages npipes))
	(tcam-array-yield	(n5-area-yield tcam-area :ram-area tcam-memory-area))
	;;stm
	(stm-area		(* ppu-nstages ppu-num-stm-rows npipes ppu-x stm-row-y))
	(stm-ram-area		(* stm-unit-sram-area ppu-num-stm-rows ppu-num-ram-cols/stg ppu-nstages npipes))
	(stm-ram-array-efficiency(/ stm-ram-area stm-area))
	(stm-ram-yield		(n5-area-yield stm-ram-area :ram-area stm-ram-area))
	(stm-logic-yield	(n5-area-yield (- stm-area stm-ram-area)))
	(stm-yield		(* stm-ram-yield stm-logic-yield))
	(ppu-pipe-yield		(* parde-yield stm-yield ppu-core-yield tcam-array-yield))
	;;tm core
	(tm-core-yield		(n5-area-yield (* tm-core-area 2) :ram-area (* tm-core-area 2 tm-core-ramfraction)))
	;;cdm
	(cdm-sram-yield		(n5-area-yield cdm-sram-area :ram-area cdm-sram-area))
	(cdm-logic-yield	(n5-area-yield cdm-logic-area))
	(cdm-yield		(* cdm-sram-yield cdm-logic-yield))
	;;base yield
	(base-yield		(* serdes-yield
				   mac-yield
				   parde-yield
				   stm-yield
				   ppu-core-yield
				   tcam-array-yield
				   tm-core-yield
				   cdm-yield
				   misc-yield
				   gpio-yield
				   mac-channel-yield))
	;;mac redundancy
	(mac-yield-withspare	(expt (prob-of-yield onemac-yield 1 17) 4))
	(macspare-deltayield	(/ mac-yield-withspare mac-yield))
	;;mac channel spare wire
	(mac-channel-repairable .95)
	(mac-channel-repaired-yield (- 1 (* (- 1 mac-channel-yield) (- 1 mac-channel-repairable))))
	(mac-channel-deltayield	(/ mac-channel-repaired-yield mac-channel-yield))
	;;cdm memory
	(cdm-numblocks		(* num-cdm-ram-rows/subwd num-cdm-ram-cols 1/4 1.0))
	(cdm-oneblock-yield	(expt (* cdm-sram-yield 1d0)(/ cdm-numblocks)))
	(cdm-sram-yield-1bad	(prob-of-yield cdm-oneblock-yield 1 cdm-numblocks))
	(cdm-sram-deltayield	(/ cdm-sram-yield-1bad cdm-sram-yield))
	(cdm-logic-repairable	.95)
	(cdm-logic-repaired-yield (- 1 (* (- 1 cdm-logic-yield) (- 1 cdm-logic-repairable))))
	(cdm-logic-deltayield	(/ cdm-logic-repaired-yield cdm-logic-yield))
	(cdm-repaired-yield	(* cdm-sram-yield-1bad cdm-logic-repaired-yield))
	;;stm memory
	(stm-ram-repairable	.9)  ;;assume spare bit
	(stm-logic-repairable	.87) ;;20b address not repairable out of 137b data + 20b address
	(stm-ram-repaired-yield   (- 1 (* (- 1 stm-ram-yield) (- 1 stm-ram-repairable))))
	(stm-logic-repaired-yield (- 1 (* (- 1 stm-logic-yield)  (- 1 stm-logic-repairable))))
	
	(stm-ram-deltayield	(/ stm-ram-repaired-yield stm-ram-yield))
	(stm-logic-deltayield	(/ stm-logic-repaired-yield stm-logic-yield))
	(ppu-pipe-repaired-yield(* parde-yield stm-ram-repaired-yield stm-logic-repaired-yield ppu-core-yield tcam-array-yield))
	;;total
	(yield-improvement	(* macspare-deltayield cdm-sram-deltayield cdm-logic-deltayield stm-ram-deltayield stm-logic-deltayield mac-channel-deltayield))
	(final-yield		(* base-yield yield-improvement))
	;;half
	(evenodd-yield		(* serdes-yield
				   mac-yield-withspare	;;doesn't include tm_core
				   ppu-pipe-repaired-yield
				   ;;tm-core-yield
				   cdm-repaired-yield
				   ;;misc-yield
				   ;;gpio-yield
				   mac-channel-repaired-yield))
	(half-yield		(* evenodd-yield tm-core-yield))
	(halfchip-yield		(sqrt half-yield))
	(either-lrhalf-yield	(* (- 1 (expt (- 1 halfchip-yield) 2))
				   misc-yield
				   gpio-yield))
	(lrhalf-25t-+yield	(- either-lrhalf-yield final-yield))
	(evenodd-half-yield	(sqrt evenodd-yield))
	(either-eohalf-yield	(* (- 1 (expt (- 1 evenodd-half-yield) 2))
				   tm-core-yield
				   misc-yield
				   gpio-yield))
	(eohalf-25t-+yield	(- either-eohalf-yield final-yield))
	;;half starting w/ base
	(base-evenodd-yield	  (/ base-yield 
				     tm-core-yield
				     misc-yield
				     gpio-yield))
	(base-half-yield	  (* base-evenodd-yield tm-core-yield))
	(base-halfchip-yield	  (sqrt base-half-yield))
	(base-either-lrhalf-yield (* (- 1 (expt (- 1 base-halfchip-yield) 2))
				     misc-yield
				     gpio-yield))
	(base-lrhalf-25t-+yield	  (- base-either-lrhalf-yield base-yield))
	(base-evenodd-half-yield  (sqrt base-evenodd-yield))
	(base-either-eohalf-yield (* (- 1 (expt (- 1 base-evenodd-half-yield) 2))
				     tm-core-yield
				     misc-yield
				     gpio-yield))
	(base-eohalf-25t-+yield	  (- base-either-eohalf-yield base-yield))
	)
    ;;missing: serdes yield
    (unless summary-only?
      ;;(formatted-eval "~(~@{~%~28a ~7,3f~}~)" d0 nfactor)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" serdes-yield mac-yield)
      ;;    (formatted-eval "~(~@{~%~28a ~7,3f~}~)" mac-yield mac-yield-withspare macspare-deltayield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" stm-area stm-ram-area stm-ram-array-efficiency)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" stm-ram-yield stm-logic-yield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" ppu-core-area  ppu-core-yield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" tcam-area tcam-memory-area tcam-array-yield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" ppu-ysize parde-area parde-yield ppu-pipe-yield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" tm-core-yield)    
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" cdm-sram-area cdm-x-overhead cdm-y-overhead cdm-area-mlpr cdm-logic-area cdm-area cdm-x cdm-y)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" cdm-sram-yield cdm-logic-yield cdm-yield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" mac-channel-area mac-channel-yield)    
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" misc-areas gpio-area misc-yield gpio-yield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" base-yield)    
      (terpri)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" mac-yield mac-yield-withspare macspare-deltayield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" mac-channel-yield mac-channel-repaired-yield mac-channel-deltayield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" cdm-sram-yield cdm-sram-yield-1bad cdm-sram-deltayield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" cdm-logic-yield cdm-logic-repaired-yield cdm-logic-deltayield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" stm-ram-yield stm-ram-repaired-yield stm-ram-deltayield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" stm-logic-yield stm-logic-repaired-yield stm-logic-deltayield)
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" base-yield yield-improvement final-yield)
      (format t "~%;;Base yields:")
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" 
		      mac-yield
		      parde-yield
		      stm-yield
		      ppu-core-yield
		      tcam-array-yield
		      tm-core-yield
		      cdm-yield
		      misc-yield
		      gpio-yield
		      mac-channel-yield
		      base-yield)
      (format t "~%;;Final yields:")
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" 
		      mac-yield-withspare	;;mac-yield
		      parde-yield
		      stm-ram-repaired-yield stm-logic-repaired-yield	;;stm-yield
		      ppu-core-yield
		      tcam-array-yield
		      ppu-pipe-repaired-yield
		      tm-core-yield
		      cdm-sram-yield-1bad cdm-logic-repaired-yield	;;cdm-yield
		      misc-yield
		      gpio-yield
		      mac-channel-repaired-yield ;;mac-channel-yield
		      final-yield))
    (unless (eq summary-only? :table)
      (format t "~2%;;Final toplevel component yields")
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" 
		      mac-yield
		      ppu-pipe-yield
		      tm-core-yield
		      cdm-yield
		      misc-yield
		      gpio-yield
		      mac-channel-yield ;;mac-channel-yield
		      base-yield)
      (format t "~%;;Final toplevel component yields, with yield improvement features:")
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" 
		      mac-yield-withspare	;;mac-yield
		      ;;parde-yield
		      ;;stm-ram-repaired-yield stm-logic-repaired-yield	;;stm-yield
		      ;;ppu-core-yield
		      ;;tcam-array-yield
		      ppu-pipe-repaired-yield
		      tm-core-yield
		      ;;cdm-sram-yield-1bad cdm-logic-repaired-yield	;;cdm-yield
		      cdm-repaired-yield
		      misc-yield
		      gpio-yield
		      mac-channel-repaired-yield ;;mac-channel-yield
		      final-yield)
      (format t "~2%;;25T yields:")
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" 
		      base-halfchip-yield
		      base-either-lrhalf-yield base-lrhalf-25t-+yield
		      base-either-eohalf-yield base-eohalf-25t-+yield)
      (format t "~%;;25T yields, with yield improvement features:")
      (formatted-eval "~(~@{~%~28a ~7,3f~}~)" 
		      halfchip-yield
		      either-lrhalf-yield lrhalf-25t-+yield
		      either-eohalf-yield eohalf-25t-+yield))
    (format t "~2%Yield                51T      25T      25T         25T      25T")
    (format t "~%                           even/odd left/right  e/o delta l/r delta")
    (format t "~%Base                ~5,3f    ~5,3f    ~5,3f      +~5,3f   +~5,3f" base-yield  base-either-eohalf-yield base-either-lrhalf-yield base-eohalf-25t-+yield base-lrhalf-25t-+yield)
    (format t "~%Yield-improved      ~5,3f    ~5,3f    ~5,3f      +~5,3f   +~5,3f" final-yield      either-eohalf-yield      either-lrhalf-yield      eohalf-25t-+yield      lrhalf-25t-+yield)

    ))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;without serdes. Bose-Einstein model. d0 = .07
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DEVO(287): (tfc-yield)

mac-yield                      0.799
stm-area                     112.204
stm-ram-area                  94.389
stm-ram-array-efficiency       0.841
stm-ram-yield                  0.905
stm-logic-yield                0.940
ppu-core-area                 45.669
ppu-core-yield                 0.854
tcam-area                     28.706
tcam-memory-area              16.445
tcam-array-yield               0.938
ppu-ysize                      2.642
parde-area                    41.434
parde-yield                    0.892
ppu-pipe-yield                 0.607
tm-core-yield                  0.788
cdm-sram-area                 74.399
cdm-x-overhead                 0.216
cdm-y-overhead                 0.116
cdm-area-mlpr                  1.356
cdm-logic-area                26.502
cdm-area                     100.901
cdm-x                          3.627
cdm-y                         27.823
cdm-sram-yield                 0.920
cdm-logic-yield                0.912
cdm-yield                      0.839
mac-channel-area              56.000
mac-channel-yield              0.947
misc-areas                     8.032
gpio-area                      7.650
misc-yield                     0.981
gpio-yield                     0.993
base-yield                     0.296

mac-yield                      0.799
mac-yield-withspare            0.994
macspare-deltayield            1.243
mac-channel-yield              0.947
mac-channel-repaired-yield     0.997
mac-channel-deltayield         1.053
cdm-sram-yield                 0.920
cdm-sram-yield-1bad            0.997
cdm-sram-deltayield            1.084
cdm-logic-yield                0.912
cdm-logic-repaired-yield       0.996
cdm-logic-deltayield           1.091
stm-ram-yield                  0.905
stm-ram-repaired-yield         0.990
stm-ram-deltayield             1.095
stm-logic-yield                0.940
stm-logic-repaired-yield       0.992
stm-logic-deltayield           1.055
base-yield                     0.296
yield-improvement              1.790
final-yield                    0.530
;;Base yields:
mac-yield                      0.799
parde-yield                    0.892
stm-yield                      0.850
ppu-core-yield                 0.854
tcam-array-yield               0.938
tm-core-yield                  0.788
cdm-yield                      0.839
misc-yield                     0.981
gpio-yield                     0.993
mac-channel-yield              0.947
base-yield                     0.296
;;Final yields:
mac-yield-withspare            0.994
parde-yield                    0.892
stm-ram-repaired-yield         0.990
stm-logic-repaired-yield       0.992
ppu-core-yield                 0.854
tcam-array-yield               0.938
ppu-pipe-repaired-yield        0.702
tm-core-yield                  0.788
cdm-sram-yield-1bad            0.997
cdm-logic-repaired-yield       0.996
misc-yield                     0.981
gpio-yield                     0.993
mac-channel-repaired-yield     0.997
final-yield                    0.530

;;Final toplevel component yields
mac-yield                      0.799
ppu-pipe-yield                 0.607
tm-core-yield                  0.788
cdm-yield                      0.839
misc-yield                     0.981
gpio-yield                     0.993
mac-channel-yield              0.947
base-yield                     0.296
;;Final toplevel component yields, with yield improvement features:
mac-yield-withspare            0.994
ppu-pipe-repaired-yield        0.702
tm-core-yield                  0.788
cdm-repaired-yield             0.992
misc-yield                     0.981
gpio-yield                     0.993
mac-channel-repaired-yield     0.997
final-yield                    0.530

;;25T yields:
base-halfchip-yield            0.551
base-either-lrhalf-yield       0.778
base-lrhalf-25t-+yield         0.482
base-either-eohalf-yield       0.657
base-eohalf-25t-+yield         0.361
;;25T yields, with yield improvement features:
halfchip-yield                 0.737
either-lrhalf-yield            0.907
lrhalf-25t-+yield              0.377
either-eohalf-yield            0.746
eohalf-25t-+yield              0.216

Yield                51T      25T      25T         25T      25T
                           even/odd left/right  e/o delta l/r delta
Base                0.296    0.657    0.778      +0.361   +0.482
Yield-improved      0.530    0.746    0.907      +0.216   +0.377
NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;with serdes. Bose-Einstein model d0 = .05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DEVO(412): (tfc-yield :summary-only? nil :d0 .05)

serdes-yield                   0.626
mac-yield                      0.852
stm-area                     112.204
stm-ram-area                  94.389
stm-ram-array-efficiency       0.841
stm-ram-yield                  0.926
stm-logic-yield                0.957
ppu-core-area                 45.669
ppu-core-yield                 0.893
tcam-area                     28.706
tcam-memory-area              16.445
tcam-array-yield               0.955
ppu-ysize                      2.642
parde-area                    41.434
parde-yield                    0.921
ppu-pipe-yield                 0.696
tm-core-yield                  0.843
cdm-sram-area                 74.399
cdm-x-overhead                 0.216
cdm-y-overhead                 0.116
cdm-area-mlpr                  1.356
cdm-logic-area                26.502
cdm-area                     100.901
cdm-x                          3.627
cdm-y                         27.823
cdm-sram-yield                 0.939
cdm-logic-yield                0.936
cdm-yield                      0.879
mac-channel-area              56.000
mac-channel-yield              0.962
misc-areas                     8.032
gpio-area                      7.650
misc-yield                     0.987
gpio-yield                     0.995
base-yield                     0.260

mac-yield                      0.852
mac-yield-withspare            0.997
macspare-deltayield            1.170
mac-channel-yield              0.962
mac-channel-repaired-yield     0.998
mac-channel-deltayield         1.038
cdm-sram-yield                 0.939
cdm-sram-yield-1bad            0.998
cdm-sram-deltayield            1.063
cdm-logic-yield                0.936
cdm-logic-repaired-yield       0.997
cdm-logic-deltayield           1.064
stm-ram-yield                  0.926
stm-ram-repaired-yield         0.993
stm-ram-deltayield             1.072
stm-logic-yield                0.957
stm-logic-repaired-yield       0.994
stm-logic-deltayield           1.039
base-yield                     0.260
yield-improvement              1.532
final-yield                    0.398
;;Base yields:
mac-yield                      0.852
parde-yield                    0.921
stm-yield                      0.886
ppu-core-yield                 0.893
tcam-array-yield               0.955
tm-core-yield                  0.843
cdm-yield                      0.879
misc-yield                     0.987
gpio-yield                     0.995
mac-channel-yield              0.962
base-yield                     0.260
;;Final yields:
mac-yield-withspare            0.997
parde-yield                    0.921
stm-ram-repaired-yield         0.993
stm-logic-repaired-yield       0.994
ppu-core-yield                 0.893
tcam-array-yield               0.955
ppu-pipe-repaired-yield        0.775
tm-core-yield                  0.843
cdm-sram-yield-1bad            0.998
cdm-logic-repaired-yield       0.997
misc-yield                     0.987
gpio-yield                     0.995
mac-channel-repaired-yield     0.998
final-yield                    0.398

;;Final toplevel component yields
mac-yield                      0.852
ppu-pipe-yield                 0.696
tm-core-yield                  0.843
cdm-yield                      0.879
misc-yield                     0.987
gpio-yield                     0.995
mac-channel-yield              0.962
base-yield                     0.260
;;Final toplevel component yields, with yield improvement features:
mac-yield-withspare            0.997
ppu-pipe-repaired-yield        0.775
tm-core-yield                  0.843
cdm-repaired-yield             0.995
misc-yield                     0.987
gpio-yield                     0.995
mac-channel-repaired-yield     0.998
final-yield                    0.398

;;25T yields:
base-halfchip-yield            0.514
base-either-lrhalf-yield       0.750
base-lrhalf-25t-+yield         0.490
base-either-eohalf-yield       0.667
base-eohalf-25t-+yield         0.408
;;25T yields, with yield improvement features:
halfchip-yield                 0.636
either-lrhalf-yield            0.852
lrhalf-25t-+yield              0.454
either-eohalf-yield            0.749
eohalf-25t-+yield              0.352

Yield                51T      25T      25T         25T      25T
                           even/odd left/right  e/o delta l/r delta
Base                0.260    0.667    0.750      +0.408   +0.490
Yield-improved      0.398    0.749    0.852      +0.352   +0.454
NIL
|#

(comment
 (loop with pgood = (sqrt .685) ;;l/r pipe good
	       with pbad = (- 1 pgood)
	       with tgood = (sqrt .775) ;;l/r tmcore good
	       with tbad = (- 1 tgood)
	       for (n pl pr tl tr) in (loop for n below 15 collect (cons n (loop for bit below 4 collect (ldb-test (byte 1 bit) n))))
				      
	       for prob = (* (if pl pbad pgood)(if pr pbad pgood)
			     (if tl tbad tgood)(if tr tbad tgood))
	       unless  (or (= n 0) ;;tl tr
			   (and pl pr)(and tl tr)(and pl tr)(and pr tl))
	       do  (format t "~%~2d ~3a ~3a ~3a ~3a ~6,4f" n pl pr tl tr prob) and
     sum prob)
 
 )
