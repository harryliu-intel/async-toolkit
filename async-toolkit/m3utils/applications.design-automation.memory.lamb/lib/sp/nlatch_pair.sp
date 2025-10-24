* nlatch_pair.sp by mika.nystroem@intel.com 5/27/2022

* note we're using 6e-09 as the drawn length of the gates
* is that the correct value?

.subckt nlatch_pair_svt CK[0] CKB[0] CK[1] CKB[1] DX Q[0] Q[1] VBP VBN VDD VSS

*
* naked latch 0
*

* input PG
MMN000 DX     CK[0]    X0      VBN  nch_svt_mac l=6e-09 nfin=2 ppitch=0
MMP000 DX     CKB[0]   X0      VBP  pch_svt_mac l=6e-09 nfin=2 ppitch=0

* inverter to Q
MMN001 VSS    X0       Q[0]    VBN  nch_svt_mac l=6e-09 nfin=2 ppitch=0
MMP001 VDD    X0       Q[0]    VBP  pch_svt_mac l=6e-09 nfin=2 ppitch=0

* 3-state feedback
MMN002 VSS    Q[0]     IN0     VBN  nch_svt_mac l=6e-09 nfin=2 ppitch=0
MMN003 IN0    CKB[0]   X0      VBN  nch_svt_mac l=6e-09 nfin=2 ppitch=0
MMP002 VDD    Q[0]     IP0     VBP  pch_svt_mac l=6e-09 nfin=2 ppitch=0
MMP003 IP0    CK[0]    X0      VBP  pch_svt_mac l=6e-09 nfin=2 ppitch=0

*
* naked latch 1 (identical to NL 0)
*

MMN010 DX     CK[1]    X1      VBN  nch_svt_mac l=6e-09 nfin=2 ppitch=0
MMP010 DX     CKB[1]   X1      VBP  pch_svt_mac l=6e-09 nfin=2 ppitch=0
MMN011 VSS    X1       Q[1]    VBN  nch_svt_mac l=6e-09 nfin=2 ppitch=0
MMP011 VDD    X1       Q[1]    VBP  pch_svt_mac l=6e-09 nfin=2 ppitch=0
MMN012 VSS    Q[1]     IN1     VBN  nch_svt_mac l=6e-09 nfin=2 ppitch=0
MMN013 IN1    CKB[1]   X1      VBN  nch_svt_mac l=6e-09 nfin=2 ppitch=0
MMP012 VDD    Q[1]     IP1     VBP  pch_svt_mac l=6e-09 nfin=2 ppitch=0
MMP013 IP1    CK[1]    X1      VBP  pch_svt_mac l=6e-09 nfin=2 ppitch=0

.ends



