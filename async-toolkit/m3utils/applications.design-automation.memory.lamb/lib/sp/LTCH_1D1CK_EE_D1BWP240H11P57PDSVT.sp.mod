**.SUBCKT NCH_SVT_MAC a b c d L=1u W=1u NFIN=NFIN
**M1 a b c d nch_svt_mac  W=W L=L NFIN=NFIN
**.ENDS

**.SUBCKT PCH_SVT_MAC a b c d L=1u W=1u NFIN=NFIN
**M1 a b c d pch_svt_mac  W=W L=L NFIN=NFIN
**.ENDS

* SPICE export by:  S-Edit 2016.2.4
* Export time:      Thu Feb  1 15:02:30 2018
* Design:           George_Barefoot_T7_20171212
* Cell:             LTCH_1D1CK_EE_D1BWP240H11P57PDSVT
* Interface:        view_1
* View:             view_1
* View type:        connectivity
* Export as:        subcircuit definition
* Export mode:      hierarchical
* Exclude empty cells: no
* Exclude .model:   yes
* Exclude .end:     no
* Exclude simulator commands:     no
* Expand paths:     yes
* Wrap lines:       no
* Root path:        \\FIBBAR664C\home\george\projects\TSMC\TSMC7N\George_Barefoot_T7_20171212
* Exclude global pins:   no
* Exclude instance locations: no
* Control property name(s): SPICE
* Testbench:        Spice

*************** Subcircuits *****************
.subckt LTCH_1D1CK_EE_D1BWP240H11P57PDSVT CK CKB DX Q VBB VPP VDD VSS 
XMN_1 DX DX DX VBB NCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=1344 $y=-912 $w=192 $h=96 $r=90
XMN_7 N CK DX VBB NCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=384 $y=-912 $w=192 $h=96 $r=90
XMN_8 N_1 CKB N VBB NCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=576 $y=-912 $w=192 $h=96 $r=90
XMN_9 VSS Q N_1 VBB NCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=768 $y=-912 $w=192 $h=96 $r=90
XMN_10 Q N VSS VBB NCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=960 $y=-912 $w=192 $h=96 $r=90
XMP_1 P P P VPP PCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=1344 $y=-656 $w=192 $h=96 $r=270
XMP_2 N DX N VPP PCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=1632 $y=-656 $w=192 $h=96 $r=270
XMP_6 N CKB DX VPP PCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=384 $y=-656 $w=192 $h=96 $r=270
XMP_7 P CK N VPP PCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=576 $y=-656 $w=192 $h=96 $r=270
XMP_8 VDD Q P VPP PCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=768 $y=-656 $w=192 $h=96 $r=270
XMP_9 Q N VDD VPP PCH_SVT_MAC NFIN='2' L='11n' M=1 MISMATCHFLAG='1' $ $x=960 $y=-656 $w=192 $h=96 $r=270
.ends



