primitive UDP_WC_TIMING_MGM_IQ_LATCH_UDP( Q, CK, D );
output Q;
reg Q;
input CK, D;
table
//     CK  D  :  Q  :  Q
        0  ?  :  ?  :  -;
        1  1  :  ?  :  1;
        ?  1  :  1  :  1;
        1  0  :  ?  :  0;
        ?  0  :  0  :  0;

endtable
endprimitive

`celldefine
module LATCHBLK4N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CKB0, CKB1, CKB2, 
        CKB3, RWL0, RWL1, RWL2, 
        RWL3, RWLX0, RWLX1, RWLX2, 
        RWLX3, DX0, DX2, Z, 
        Q0, Q1, Q2, Q3, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CKB0, CKB1, CKB2, 
        CKB3, RWL0, RWL1, RWL2, 
        RWL3, RWLX0, RWLX1, RWLX2, 
        RWLX3, DX0, DX2, Z ;
output  Q0, Q1, Q2, 
        Q3, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK5N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CKB0, CKB1, 
        CKB2, CKB3, CKB4, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, DX0, DX2, DX4, 
        Z, Q0, Q1, Q2, 
        Q3, Q4, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CKB0, CKB1, 
        CKB2, CKB3, CKB4, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, DX0, DX2, DX4, 
        Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK6N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, DX0, DX2, DX4, 
        Z, Q0, Q1, Q2, 
        Q3, Q4, Q5, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, DX0, DX2, DX4, 
        Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK7N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, DX0, DX2, DX4, 
        DX6, Z, Q0, Q1, 
        Q2, Q3, Q4, Q5, 
        Q6, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, DX0, DX2, DX4, 
        DX6, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK8N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, DX0, DX2, DX4, 
        DX6, Z, Q0, Q1, 
        Q2, Q3, Q4, Q5, 
        Q6, Q7, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, DX0, DX2, DX4, 
        DX6, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK9N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, DX0, DX2, DX4, 
        DX6, DX8, Z, Q0, 
        Q1, Q2, Q3, Q4, 
        Q5, Q6, Q7, Q8, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, DX0, DX2, DX4, 
        DX6, DX8, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK10N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, DX0, DX2, DX4, 
        DX6, DX8, Z, Q0, 
        Q1, Q2, Q3, Q4, 
        Q5, Q6, Q7, Q8, 
        Q9, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, DX0, DX2, DX4, 
        DX6, DX8, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK11N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, DX0, DX2, DX4, 
        DX6, DX8, DX10, Z, 
        Q0, Q1, Q2, Q3, 
        Q4, Q5, Q6, Q7, 
        Q8, Q9, Q10, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, DX0, DX2, DX4, 
        DX6, DX8, DX10, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK12N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, DX0, DX2, DX4, 
        DX6, DX8, DX10, Z, 
        Q0, Q1, Q2, Q3, 
        Q4, Q5, Q6, Q7, 
        Q8, Q9, Q10, Q11, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, DX0, DX2, DX4, 
        DX6, DX8, DX10, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK13N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, CKB9, 
        CKB10, CKB11, CKB12, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, RWLX9, RWLX10, RWLX11, 
        RWLX12, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        Z, Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, CKB9, 
        CKB10, CKB11, CKB12, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, RWLX9, RWLX10, RWLX11, 
        RWLX12, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK14N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, CKB10, CKB11, CKB12, 
        CKB13, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, RWLX10, RWLX11, RWLX12, 
        RWLX13, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        Z, Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, CKB10, CKB11, CKB12, 
        CKB13, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, RWLX10, RWLX11, RWLX12, 
        RWLX13, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK15N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, CKB11, 
        CKB12, CKB13, CKB14, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, RWLX11, RWLX12, RWLX13, 
        RWLX14, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, Z, Q0, Q1, 
        Q2, Q3, Q4, Q5, 
        Q6, Q7, Q8, Q9, 
        Q10, Q11, Q12, Q13, 
        Q14, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, CKB11, 
        CKB12, CKB13, CKB14, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, RWLX11, RWLX12, RWLX13, 
        RWLX14, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK16N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, CKB12, CKB13, CKB14, 
        CKB15, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, RWLX12, RWLX13, RWLX14, 
        RWLX15, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, Z, Q0, Q1, 
        Q2, Q3, Q4, Q5, 
        Q6, Q7, Q8, Q9, 
        Q10, Q11, Q12, Q13, 
        Q14, Q15, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, CKB12, CKB13, CKB14, 
        CKB15, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, RWLX12, RWLX13, RWLX14, 
        RWLX15, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK17N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, CKB9, 
        CKB10, CKB11, CKB12, CKB13, 
        CKB14, CKB15, CKB16, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWL15, RWL16, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, RWLX9, RWLX10, RWLX11, 
        RWLX12, RWLX13, RWLX14, RWLX15, 
        RWLX16, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, Z, Q0, 
        Q1, Q2, Q3, Q4, 
        Q5, Q6, Q7, Q8, 
        Q9, Q10, Q11, Q12, 
        Q13, Q14, Q15, Q16, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, CKB9, 
        CKB10, CKB11, CKB12, CKB13, 
        CKB14, CKB15, CKB16, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWL15, RWL16, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, RWLX9, RWLX10, RWLX11, 
        RWLX12, RWLX13, RWLX14, RWLX15, 
        RWLX16, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Q16, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( D16,     DX16  ) ;
	not  ( CKBX16,  CKB16 ) ;
	and  ( CKEN16,  CKBX16, CK16 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch16 ( Q16, CKEN16, D16 ) ;
	not  ( RWLXX16, RWLX16 ) ;
	and  ( DOUT16, Q16, RWLXX16, RWL16 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15, DOUT16 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK18N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, CKB10, CKB11, CKB12, 
        CKB13, CKB14, CKB15, CKB16, 
        CKB17, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWL16, RWL17, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, RWLX10, RWLX11, RWLX12, 
        RWLX13, RWLX14, RWLX15, RWLX16, 
        RWLX17, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, Z, Q0, 
        Q1, Q2, Q3, Q4, 
        Q5, Q6, Q7, Q8, 
        Q9, Q10, Q11, Q12, 
        Q13, Q14, Q15, Q16, 
        Q17, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, CKB10, CKB11, CKB12, 
        CKB13, CKB14, CKB15, CKB16, 
        CKB17, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWL16, RWL17, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, RWLX10, RWLX11, RWLX12, 
        RWLX13, RWLX14, RWLX15, RWLX16, 
        RWLX17, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Q16, Q17, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( D16,     DX16  ) ;
	not  ( CKBX16,  CKB16 ) ;
	and  ( CKEN16,  CKBX16, CK16 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch16 ( Q16, CKEN16, D16 ) ;
	not  ( RWLXX16, RWLX16 ) ;
	and  ( DOUT16, Q16, RWLXX16, RWL16 ) ;
	not  ( CKBX17,  CKB17 ) ;
	and  ( CKEN17,  CKBX17, CK17 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch17 ( Q17, CKEN17, D16 ) ;
	not  ( RWLXX17, RWLX17 ) ;
	and  ( DOUT17, Q17, RWLXX17, RWL17 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15, DOUT16, DOUT17 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK19N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CK18, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, CKB11, 
        CKB12, CKB13, CKB14, CKB15, 
        CKB16, CKB17, CKB18, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWL15, RWL16, 
        RWL17, RWL18, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, RWLX11, RWLX12, RWLX13, 
        RWLX14, RWLX15, RWLX16, RWLX17, 
        RWLX18, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, DX18, Z, 
        Q0, Q1, Q2, Q3, 
        Q4, Q5, Q6, Q7, 
        Q8, Q9, Q10, Q11, 
        Q12, Q13, Q14, Q15, 
        Q16, Q17, Q18, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CK18, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, CKB11, 
        CKB12, CKB13, CKB14, CKB15, 
        CKB16, CKB17, CKB18, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWL15, RWL16, 
        RWL17, RWL18, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, RWLX11, RWLX12, RWLX13, 
        RWLX14, RWLX15, RWLX16, RWLX17, 
        RWLX18, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, DX18, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Q16, Q17, Q18, 
        Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( D16,     DX16  ) ;
	not  ( CKBX16,  CKB16 ) ;
	and  ( CKEN16,  CKBX16, CK16 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch16 ( Q16, CKEN16, D16 ) ;
	not  ( RWLXX16, RWLX16 ) ;
	and  ( DOUT16, Q16, RWLXX16, RWL16 ) ;
	not  ( CKBX17,  CKB17 ) ;
	and  ( CKEN17,  CKBX17, CK17 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch17 ( Q17, CKEN17, D16 ) ;
	not  ( RWLXX17, RWLX17 ) ;
	and  ( DOUT17, Q17, RWLXX17, RWL17 ) ;
	not  ( D18,     DX18  ) ;
	not  ( CKBX18,  CKB18 ) ;
	and  ( CKEN18,  CKBX18, CK18 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch18 ( Q18, CKEN18, D18 ) ;
	not  ( RWLXX18, RWLX18 ) ;
	and  ( DOUT18, Q18, RWLXX18, RWL18 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15, DOUT16, DOUT17, DOUT18 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK20N_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CK18, 
        CK19, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, CKB12, CKB13, CKB14, 
        CKB15, CKB16, CKB17, CKB18, 
        CKB19, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWL16, RWL17, RWL18, 
        RWL19, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, RWLX12, RWLX13, RWLX14, 
        RWLX15, RWLX16, RWLX17, RWLX18, 
        RWLX19, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, DX18, Z, 
        Q0, Q1, Q2, Q3, 
        Q4, Q5, Q6, Q7, 
        Q8, Q9, Q10, Q11, 
        Q12, Q13, Q14, Q15, 
        Q16, Q17, Q18, Q19, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CK18, 
        CK19, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, CKB12, CKB13, CKB14, 
        CKB15, CKB16, CKB17, CKB18, 
        CKB19, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWL16, RWL17, RWL18, 
        RWL19, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, RWLX12, RWLX13, RWLX14, 
        RWLX15, RWLX16, RWLX17, RWLX18, 
        RWLX19, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, DX18, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Q16, Q17, Q18, 
        Q19, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( D16,     DX16  ) ;
	not  ( CKBX16,  CKB16 ) ;
	and  ( CKEN16,  CKBX16, CK16 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch16 ( Q16, CKEN16, D16 ) ;
	not  ( RWLXX16, RWLX16 ) ;
	and  ( DOUT16, Q16, RWLXX16, RWL16 ) ;
	not  ( CKBX17,  CKB17 ) ;
	and  ( CKEN17,  CKBX17, CK17 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch17 ( Q17, CKEN17, D16 ) ;
	not  ( RWLXX17, RWLX17 ) ;
	and  ( DOUT17, Q17, RWLXX17, RWL17 ) ;
	not  ( D18,     DX18  ) ;
	not  ( CKBX18,  CKB18 ) ;
	and  ( CKEN18,  CKBX18, CK18 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch18 ( Q18, CKEN18, D18 ) ;
	not  ( RWLXX18, RWLX18 ) ;
	and  ( DOUT18, Q18, RWLXX18, RWL18 ) ;
	not  ( CKBX19,  CKB19 ) ;
	and  ( CKEN19,  CKBX19, CK19 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch19 ( Q19, CKEN19, D18 ) ;
	not  ( RWLXX19, RWLX19 ) ;
	and  ( DOUT19, Q19, RWLXX19, RWL19 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15, DOUT16, DOUT17, DOUT18, DOUT19 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK4S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CKB0, CKB1, CKB2, 
        CKB3, RWL0, RWL1, RWL2, 
        RWL3, RWLX0, RWLX1, RWLX2, 
        RWLX3, DX0, DX2, Z, 
        Q0, Q1, Q2, Q3, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CKB0, CKB1, CKB2, 
        CKB3, RWL0, RWL1, RWL2, 
        RWL3, RWLX0, RWLX1, RWLX2, 
        RWLX3, DX0, DX2, Z ;
output  Q0, Q1, Q2, 
        Q3, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK5S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CKB0, CKB1, 
        CKB2, CKB3, CKB4, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, DX0, DX2, DX4, 
        Z, Q0, Q1, Q2, 
        Q3, Q4, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CKB0, CKB1, 
        CKB2, CKB3, CKB4, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, DX0, DX2, DX4, 
        Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK6S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, DX0, DX2, DX4, 
        Z, Q0, Q1, Q2, 
        Q3, Q4, Q5, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, DX0, DX2, DX4, 
        Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK7S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, DX0, DX2, DX4, 
        DX6, Z, Q0, Q1, 
        Q2, Q3, Q4, Q5, 
        Q6, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, DX0, DX2, DX4, 
        DX6, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK8S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, DX0, DX2, DX4, 
        DX6, Z, Q0, Q1, 
        Q2, Q3, Q4, Q5, 
        Q6, Q7, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, DX0, DX2, DX4, 
        DX6, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK9S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, DX0, DX2, DX4, 
        DX6, DX8, Z, Q0, 
        Q1, Q2, Q3, Q4, 
        Q5, Q6, Q7, Q8, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, DX0, DX2, DX4, 
        DX6, DX8, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK10S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, DX0, DX2, DX4, 
        DX6, DX8, Z, Q0, 
        Q1, Q2, Q3, Q4, 
        Q5, Q6, Q7, Q8, 
        Q9, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, DX0, DX2, DX4, 
        DX6, DX8, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK11S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, DX0, DX2, DX4, 
        DX6, DX8, DX10, Z, 
        Q0, Q1, Q2, Q3, 
        Q4, Q5, Q6, Q7, 
        Q8, Q9, Q10, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, DX0, DX2, DX4, 
        DX6, DX8, DX10, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK12S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, DX0, DX2, DX4, 
        DX6, DX8, DX10, Z, 
        Q0, Q1, Q2, Q3, 
        Q4, Q5, Q6, Q7, 
        Q8, Q9, Q10, Q11, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, DX0, DX2, DX4, 
        DX6, DX8, DX10, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK13S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, CKB9, 
        CKB10, CKB11, CKB12, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, RWLX9, RWLX10, RWLX11, 
        RWLX12, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        Z, Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, CKB9, 
        CKB10, CKB11, CKB12, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, RWLX9, RWLX10, RWLX11, 
        RWLX12, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK14S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, CKB10, CKB11, CKB12, 
        CKB13, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, RWLX10, RWLX11, RWLX12, 
        RWLX13, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        Z, Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, CKB10, CKB11, CKB12, 
        CKB13, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, RWLX10, RWLX11, RWLX12, 
        RWLX13, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK15S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, CKB11, 
        CKB12, CKB13, CKB14, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, RWLX11, RWLX12, RWLX13, 
        RWLX14, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, Z, Q0, Q1, 
        Q2, Q3, Q4, Q5, 
        Q6, Q7, Q8, Q9, 
        Q10, Q11, Q12, Q13, 
        Q14, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, CKB11, 
        CKB12, CKB13, CKB14, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, RWLX11, RWLX12, RWLX13, 
        RWLX14, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK16S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, CKB12, CKB13, CKB14, 
        CKB15, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, RWLX12, RWLX13, RWLX14, 
        RWLX15, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, Z, Q0, Q1, 
        Q2, Q3, Q4, Q5, 
        Q6, Q7, Q8, Q9, 
        Q10, Q11, Q12, Q13, 
        Q14, Q15, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, CKB12, CKB13, CKB14, 
        CKB15, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, RWLX12, RWLX13, RWLX14, 
        RWLX15, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK17S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, CKB9, 
        CKB10, CKB11, CKB12, CKB13, 
        CKB14, CKB15, CKB16, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWL15, RWL16, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, RWLX9, RWLX10, RWLX11, 
        RWLX12, RWLX13, RWLX14, RWLX15, 
        RWLX16, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, Z, Q0, 
        Q1, Q2, Q3, Q4, 
        Q5, Q6, Q7, Q8, 
        Q9, Q10, Q11, Q12, 
        Q13, Q14, Q15, Q16, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CKB0, CKB1, 
        CKB2, CKB3, CKB4, CKB5, 
        CKB6, CKB7, CKB8, CKB9, 
        CKB10, CKB11, CKB12, CKB13, 
        CKB14, CKB15, CKB16, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWL15, RWL16, 
        RWLX0, RWLX1, RWLX2, RWLX3, 
        RWLX4, RWLX5, RWLX6, RWLX7, 
        RWLX8, RWLX9, RWLX10, RWLX11, 
        RWLX12, RWLX13, RWLX14, RWLX15, 
        RWLX16, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Q16, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( D16,     DX16  ) ;
	not  ( CKBX16,  CKB16 ) ;
	and  ( CKEN16,  CKBX16, CK16 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch16 ( Q16, CKEN16, D16 ) ;
	not  ( RWLXX16, RWLX16 ) ;
	and  ( DOUT16, Q16, RWLXX16, RWL16 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15, DOUT16 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK18S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, CKB10, CKB11, CKB12, 
        CKB13, CKB14, CKB15, CKB16, 
        CKB17, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWL16, RWL17, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, RWLX10, RWLX11, RWLX12, 
        RWLX13, RWLX14, RWLX15, RWLX16, 
        RWLX17, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, Z, Q0, 
        Q1, Q2, Q3, Q4, 
        Q5, Q6, Q7, Q8, 
        Q9, Q10, Q11, Q12, 
        Q13, Q14, Q15, Q16, 
        Q17, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CKB0, 
        CKB1, CKB2, CKB3, CKB4, 
        CKB5, CKB6, CKB7, CKB8, 
        CKB9, CKB10, CKB11, CKB12, 
        CKB13, CKB14, CKB15, CKB16, 
        CKB17, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWL16, RWL17, RWLX0, 
        RWLX1, RWLX2, RWLX3, RWLX4, 
        RWLX5, RWLX6, RWLX7, RWLX8, 
        RWLX9, RWLX10, RWLX11, RWLX12, 
        RWLX13, RWLX14, RWLX15, RWLX16, 
        RWLX17, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Q16, Q17, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( D16,     DX16  ) ;
	not  ( CKBX16,  CKB16 ) ;
	and  ( CKEN16,  CKBX16, CK16 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch16 ( Q16, CKEN16, D16 ) ;
	not  ( RWLXX16, RWLX16 ) ;
	and  ( DOUT16, Q16, RWLXX16, RWL16 ) ;
	not  ( CKBX17,  CKB17 ) ;
	and  ( CKEN17,  CKBX17, CK17 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch17 ( Q17, CKEN17, D16 ) ;
	not  ( RWLXX17, RWLX17 ) ;
	and  ( DOUT17, Q17, RWLXX17, RWL17 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15, DOUT16, DOUT17 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK19S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CK18, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, CKB11, 
        CKB12, CKB13, CKB14, CKB15, 
        CKB16, CKB17, CKB18, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWL15, RWL16, 
        RWL17, RWL18, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, RWLX11, RWLX12, RWLX13, 
        RWLX14, RWLX15, RWLX16, RWLX17, 
        RWLX18, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, DX18, Z, 
        Q0, Q1, Q2, Q3, 
        Q4, Q5, Q6, Q7, 
        Q8, Q9, Q10, Q11, 
        Q12, Q13, Q14, Q15, 
        Q16, Q17, Q18, Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CK18, 
        CKB0, CKB1, CKB2, CKB3, 
        CKB4, CKB5, CKB6, CKB7, 
        CKB8, CKB9, CKB10, CKB11, 
        CKB12, CKB13, CKB14, CKB15, 
        CKB16, CKB17, CKB18, RWL0, 
        RWL1, RWL2, RWL3, RWL4, 
        RWL5, RWL6, RWL7, RWL8, 
        RWL9, RWL10, RWL11, RWL12, 
        RWL13, RWL14, RWL15, RWL16, 
        RWL17, RWL18, RWLX0, RWLX1, 
        RWLX2, RWLX3, RWLX4, RWLX5, 
        RWLX6, RWLX7, RWLX8, RWLX9, 
        RWLX10, RWLX11, RWLX12, RWLX13, 
        RWLX14, RWLX15, RWLX16, RWLX17, 
        RWLX18, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, DX18, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Q16, Q17, Q18, 
        Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( D16,     DX16  ) ;
	not  ( CKBX16,  CKB16 ) ;
	and  ( CKEN16,  CKBX16, CK16 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch16 ( Q16, CKEN16, D16 ) ;
	not  ( RWLXX16, RWLX16 ) ;
	and  ( DOUT16, Q16, RWLXX16, RWL16 ) ;
	not  ( CKBX17,  CKB17 ) ;
	and  ( CKEN17,  CKBX17, CK17 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch17 ( Q17, CKEN17, D16 ) ;
	not  ( RWLXX17, RWLX17 ) ;
	and  ( DOUT17, Q17, RWLXX17, RWL17 ) ;
	not  ( D18,     DX18  ) ;
	not  ( CKBX18,  CKB18 ) ;
	and  ( CKEN18,  CKBX18, CK18 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch18 ( Q18, CKEN18, D18 ) ;
	not  ( RWLXX18, RWLX18 ) ;
	and  ( DOUT18, Q18, RWLXX18, RWL18 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15, DOUT16, DOUT17, DOUT18 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

`celldefine
module LATCHBLK20S_D1BWP240H11P57PDSVT (CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CK18, 
        CK19, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, CKB12, CKB13, CKB14, 
        CKB15, CKB16, CKB17, CKB18, 
        CKB19, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWL16, RWL17, RWL18, 
        RWL19, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, RWLX12, RWLX13, RWLX14, 
        RWLX15, RWLX16, RWLX17, RWLX18, 
        RWLX19, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, DX18, Z, 
        Q0, Q1, Q2, Q3, 
        Q4, Q5, Q6, Q7, 
        Q8, Q9, Q10, Q11, 
        Q12, Q13, Q14, Q15, 
        Q16, Q17, Q18, Q19, 
        Y);
input   CK0, CK1, CK2, 
        CK3, CK4, CK5, CK6, 
        CK7, CK8, CK9, CK10, 
        CK11, CK12, CK13, CK14, 
        CK15, CK16, CK17, CK18, 
        CK19, CKB0, CKB1, CKB2, 
        CKB3, CKB4, CKB5, CKB6, 
        CKB7, CKB8, CKB9, CKB10, 
        CKB11, CKB12, CKB13, CKB14, 
        CKB15, CKB16, CKB17, CKB18, 
        CKB19, RWL0, RWL1, RWL2, 
        RWL3, RWL4, RWL5, RWL6, 
        RWL7, RWL8, RWL9, RWL10, 
        RWL11, RWL12, RWL13, RWL14, 
        RWL15, RWL16, RWL17, RWL18, 
        RWL19, RWLX0, RWLX1, RWLX2, 
        RWLX3, RWLX4, RWLX5, RWLX6, 
        RWLX7, RWLX8, RWLX9, RWLX10, 
        RWLX11, RWLX12, RWLX13, RWLX14, 
        RWLX15, RWLX16, RWLX17, RWLX18, 
        RWLX19, DX0, DX2, DX4, 
        DX6, DX8, DX10, DX12, 
        DX14, DX16, DX18, Z ;
output  Q0, Q1, Q2, 
        Q3, Q4, Q5, Q6, 
        Q7, Q8, Q9, Q10, 
        Q11, Q12, Q13, Q14, 
        Q15, Q16, Q17, Q18, 
        Q19, Y ;
	not  ( D0,     DX0  ) ;
	not  ( CKBX0,  CKB0 ) ;
	and  ( CKEN0,  CKBX0, CK0 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch0 ( Q0, CKEN0, D0 ) ;
	not  ( RWLXX0, RWLX0 ) ;
	and  ( DOUT0, Q0, RWLXX0, RWL0 ) ;
	not  ( CKBX1,  CKB1 ) ;
	and  ( CKEN1,  CKBX1, CK1 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch1 ( Q1, CKEN1, D0 ) ;
	not  ( RWLXX1, RWLX1 ) ;
	and  ( DOUT1, Q1, RWLXX1, RWL1 ) ;
	not  ( D2,     DX2  ) ;
	not  ( CKBX2,  CKB2 ) ;
	and  ( CKEN2,  CKBX2, CK2 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch2 ( Q2, CKEN2, D2 ) ;
	not  ( RWLXX2, RWLX2 ) ;
	and  ( DOUT2, Q2, RWLXX2, RWL2 ) ;
	not  ( CKBX3,  CKB3 ) ;
	and  ( CKEN3,  CKBX3, CK3 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch3 ( Q3, CKEN3, D2 ) ;
	not  ( RWLXX3, RWLX3 ) ;
	and  ( DOUT3, Q3, RWLXX3, RWL3 ) ;
	not  ( D4,     DX4  ) ;
	not  ( CKBX4,  CKB4 ) ;
	and  ( CKEN4,  CKBX4, CK4 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch4 ( Q4, CKEN4, D4 ) ;
	not  ( RWLXX4, RWLX4 ) ;
	and  ( DOUT4, Q4, RWLXX4, RWL4 ) ;
	not  ( CKBX5,  CKB5 ) ;
	and  ( CKEN5,  CKBX5, CK5 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch5 ( Q5, CKEN5, D4 ) ;
	not  ( RWLXX5, RWLX5 ) ;
	and  ( DOUT5, Q5, RWLXX5, RWL5 ) ;
	not  ( D6,     DX6  ) ;
	not  ( CKBX6,  CKB6 ) ;
	and  ( CKEN6,  CKBX6, CK6 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch6 ( Q6, CKEN6, D6 ) ;
	not  ( RWLXX6, RWLX6 ) ;
	and  ( DOUT6, Q6, RWLXX6, RWL6 ) ;
	not  ( CKBX7,  CKB7 ) ;
	and  ( CKEN7,  CKBX7, CK7 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch7 ( Q7, CKEN7, D6 ) ;
	not  ( RWLXX7, RWLX7 ) ;
	and  ( DOUT7, Q7, RWLXX7, RWL7 ) ;
	not  ( D8,     DX8  ) ;
	not  ( CKBX8,  CKB8 ) ;
	and  ( CKEN8,  CKBX8, CK8 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch8 ( Q8, CKEN8, D8 ) ;
	not  ( RWLXX8, RWLX8 ) ;
	and  ( DOUT8, Q8, RWLXX8, RWL8 ) ;
	not  ( CKBX9,  CKB9 ) ;
	and  ( CKEN9,  CKBX9, CK9 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch9 ( Q9, CKEN9, D8 ) ;
	not  ( RWLXX9, RWLX9 ) ;
	and  ( DOUT9, Q9, RWLXX9, RWL9 ) ;
	not  ( D10,     DX10  ) ;
	not  ( CKBX10,  CKB10 ) ;
	and  ( CKEN10,  CKBX10, CK10 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch10 ( Q10, CKEN10, D10 ) ;
	not  ( RWLXX10, RWLX10 ) ;
	and  ( DOUT10, Q10, RWLXX10, RWL10 ) ;
	not  ( CKBX11,  CKB11 ) ;
	and  ( CKEN11,  CKBX11, CK11 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch11 ( Q11, CKEN11, D10 ) ;
	not  ( RWLXX11, RWLX11 ) ;
	and  ( DOUT11, Q11, RWLXX11, RWL11 ) ;
	not  ( D12,     DX12  ) ;
	not  ( CKBX12,  CKB12 ) ;
	and  ( CKEN12,  CKBX12, CK12 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch12 ( Q12, CKEN12, D12 ) ;
	not  ( RWLXX12, RWLX12 ) ;
	and  ( DOUT12, Q12, RWLXX12, RWL12 ) ;
	not  ( CKBX13,  CKB13 ) ;
	and  ( CKEN13,  CKBX13, CK13 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch13 ( Q13, CKEN13, D12 ) ;
	not  ( RWLXX13, RWLX13 ) ;
	and  ( DOUT13, Q13, RWLXX13, RWL13 ) ;
	not  ( D14,     DX14  ) ;
	not  ( CKBX14,  CKB14 ) ;
	and  ( CKEN14,  CKBX14, CK14 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch14 ( Q14, CKEN14, D14 ) ;
	not  ( RWLXX14, RWLX14 ) ;
	and  ( DOUT14, Q14, RWLXX14, RWL14 ) ;
	not  ( CKBX15,  CKB15 ) ;
	and  ( CKEN15,  CKBX15, CK15 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch15 ( Q15, CKEN15, D14 ) ;
	not  ( RWLXX15, RWLX15 ) ;
	and  ( DOUT15, Q15, RWLXX15, RWL15 ) ;
	not  ( D16,     DX16  ) ;
	not  ( CKBX16,  CKB16 ) ;
	and  ( CKEN16,  CKBX16, CK16 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch16 ( Q16, CKEN16, D16 ) ;
	not  ( RWLXX16, RWLX16 ) ;
	and  ( DOUT16, Q16, RWLXX16, RWL16 ) ;
	not  ( CKBX17,  CKB17 ) ;
	and  ( CKEN17,  CKBX17, CK17 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch17 ( Q17, CKEN17, D16 ) ;
	not  ( RWLXX17, RWLX17 ) ;
	and  ( DOUT17, Q17, RWLXX17, RWL17 ) ;
	not  ( D18,     DX18  ) ;
	not  ( CKBX18,  CKB18 ) ;
	and  ( CKEN18,  CKBX18, CK18 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch18 ( Q18, CKEN18, D18 ) ;
	not  ( RWLXX18, RWLX18 ) ;
	and  ( DOUT18, Q18, RWLXX18, RWL18 ) ;
	not  ( CKBX19,  CKB19 ) ;
	and  ( CKEN19,  CKBX19, CK19 ) ;
	UDP_WC_TIMING_MGM_IQ_LATCH_UDP latch19 ( Q19, CKEN19, D18 ) ;
	not  ( RWLXX19, RWLX19 ) ;
	and  ( DOUT19, Q19, RWLXX19, RWL19 ) ;
	not  ( ZX, Z) ;
	or   (Y1, DOUT0, DOUT1, DOUT2, DOUT3, DOUT4, DOUT5, DOUT6, DOUT7, DOUT8, DOUT9, DOUT10, DOUT11, DOUT12, DOUT13, DOUT14, DOUT15, DOUT16, DOUT17, DOUT18, DOUT19 ) ;
	and  ( Y, ZX, Y1 ) ;
endmodule
`endcelldefine

