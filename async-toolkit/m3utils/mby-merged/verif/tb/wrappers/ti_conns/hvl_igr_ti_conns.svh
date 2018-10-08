// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Subodh Nanal 
// Created On   :  10/04/2018
// Description  :  IGR test island instantiation and connectivity
//                 Based on the fc model there will be variable number of ingress ti's
//                 fc_8 - 4 igr TI's (2 MPP's)
//                 fc_64 - 16 igr TI's (8 MPP's)
//------------------------------------------------------------------------------

`ifdef IGR_ENV_ENABLE

    //Ingress env interface
    ingress_env_if ingress_if[`NUM_IGR]();   

    //Client Data interface. This is the interface between the ETH MAC and ingress.
    //4 EPL + 1 for VP
    //IGR0
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_0[`NUM_IGR]();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_0[`NUM_IGR]();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_1[`NUM_IGR]();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_1[`NUM_IGR]();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_2[`NUM_IGR]();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_2[`NUM_IGR]();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_3[`NUM_IGR]();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_3[`NUM_IGR]();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_4[`NUM_IGR]();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_4[`NUM_IGR]();

    genvar i;
    generate
        for (i=0; i< `NUM_IGR; i++) begin :generate_igr_ti
            ingress_ti_high #()
                u_ingress_ti[i] (
                    .ingress_if          (ingress_if[i])
                   ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr_0[i])
                   ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr_0[i])
                   ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr_1[i])
                   ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr_1[i])
                   ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr_2[i])
                   ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr_2[i])
                   ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr_3[i])
                   ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr_3[i])
                   ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr_4[i])
                   ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr_4[i])
                  );
        end
    endgenerate


/*
    //Ingress env interface
    ingress_env_if ingress_if_0();   
    ingress_env_if ingress_if_1();   
    ingress_env_if ingress_if_2();   
    ingress_env_if ingress_if_3();   
    `ifdef FC_64
        ingress_env_if ingress_if_4();   
        ingress_env_if ingress_if_5();   
        ingress_env_if ingress_if_6();   
        ingress_env_if ingress_if_7();   
        ingress_env_if ingress_if_8();   
        ingress_env_if ingress_if_9();   
        ingress_env_if ingress_if_10();   
        ingress_env_if ingress_if_11();   
        ingress_env_if ingress_if_12();   
        ingress_env_if ingress_if_13();   
        ingress_env_if ingress_if_14();   
        ingress_env_if ingress_if_15();   
    `endif

    //Client Data interface. This is the interface between the ETH MAC and ingress.
    //4 EPL + 1 for VP
    //IGR0
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr0_0();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr0_0();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr0_1();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr0_1();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr0_2();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr0_2();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr0_3();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr0_3();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr0_4();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr0_4();

    //IGR1
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr1_0();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr1_0();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr1_1();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr1_1();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr1_2();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr1_2();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr1_3();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr1_3();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr1_4();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr1_4();

    //IGR2
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr2_0();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr2_0();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr2_1();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr2_1();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr2_2();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr2_2();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr2_3();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr2_3();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr2_4();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr2_4();

    //IGR3
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr3_0();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr3_0();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr3_1();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr3_1();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr3_2();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr3_2();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr3_3();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr3_3();
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr3_4();
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr3_4();

    `ifdef FC_64
        //IGR4
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr4_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr4_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr4_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr4_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr4_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr4_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr4_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr4_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr4_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr4_4();

        //IGR5
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr5_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr5_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr5_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr5_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr5_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr5_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr5_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr5_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr5_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr5_4();

        //IGR6
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr6_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr6_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr6_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr6_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr6_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr6_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr6_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr6_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr6_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr6_4();

        //IGR7
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr7_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr7_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr7_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr7_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr7_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr7_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr7_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr7_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr7_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr7_4();

        //IGR8
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr8_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr8_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr8_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr8_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr8_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr8_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr8_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr8_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr8_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr8_4();

        //IGR9
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr9_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr9_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr9_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr9_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr9_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr9_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr9_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr9_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr9_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr9_4();

        //IGR10
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr10_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr10_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr10_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr10_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr10_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr10_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr10_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr10_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr10_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr10_4();

        //IGR11
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr11_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr11_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr11_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr11_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr11_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr11_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr11_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr11_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr11_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr11_4();

        //IGR12
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr12_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr12_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr12_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr12_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr12_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr12_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr12_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr12_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr12_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr12_4();

        //IGR13
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr13_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr13_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr13_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr13_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr13_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr13_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr13_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr13_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr13_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr13_4();

        //IGR14
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr14_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr14_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr14_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr14_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr14_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr14_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr14_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr14_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr14_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr14_4();

        //IGR15
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr15_0();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr15_0();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr15_1();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr15_1();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr15_2();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr15_2();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr15_3();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr15_3();
        mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr15_4();
        mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr15_4();
    `endif

    // ===============================================
    // Test Island instance
    // ===============================================
    ingress_ti_high #()
        u_ingress_ti_0 (
                    .ingress_if          (ingress_if_0)
                   ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr0_0)
                   ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr0_0)
                   ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr0_1)
                   ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr0_1)
                   ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr0_2)
                   ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr0_2)
                   ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr0_3)
                   ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr0_3)
                   ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr0_4)
                   ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr0_4)
                  );

    ingress_ti_high #()
        u_ingress_ti_1 (
                    .ingress_if          (ingress_if_1)
                   ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr1_0)
                   ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr1_0)
                   ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr1_1)
                   ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr1_1)
                   ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr1_2)
                   ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr1_2)
                   ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr1_3)
                   ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr1_3)
                   ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr1_4)
                   ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr1_4)
                  );


    ingress_ti_high #()
        u_ingress_ti_2 (
                    .ingress_if          (ingress_if_2)
                   ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr2_0)
                   ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr2_0)
                   ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr2_1)
                   ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr2_1)
                   ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr2_2)
                   ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr2_2)
                   ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr2_3)
                   ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr2_3)
                   ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr2_4)
                   ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr2_4)
                  );

    ingress_ti_high #()
        u_ingress_ti_3 (
                    .ingress_if          (ingress_if_3)
                   ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr3_0)
                   ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr3_0)
                   ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr3_1)
                   ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr3_1)
                   ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr3_2)
                   ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr3_2)
                   ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr3_3)
                   ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr3_3)
                   ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr3_4)
                   ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr3_4)
                  );

    `ifdef FC_64
        ingress_ti_high #()
            u_ingress_ti_4 (
                        .ingress_if          (ingress_if_4)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr4_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr4_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr4_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr4_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr4_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr4_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr4_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr4_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr4_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr4_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_5 (
                        .ingress_if          (ingress_if_5)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr5_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr5_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr5_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr5_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr5_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr5_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr5_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr5_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr5_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr5_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_6 (
                        .ingress_if          (ingress_if_6)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr6_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr6_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr6_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr6_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr6_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr6_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr6_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr6_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr6_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr6_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_7 (
                        .ingress_if          (ingress_if_7)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr7_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr7_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr7_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr7_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr7_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr7_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr7_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr7_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr7_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr7_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_8 (
                        .ingress_if          (ingress_if_8)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr8_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr8_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr8_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr8_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr8_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr8_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr8_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr8_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr8_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr8_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_9 (
                        .ingress_if          (ingress_if_9)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr9_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr9_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr9_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr9_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr9_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr9_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr9_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr9_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr9_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr9_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_10 (
                        .ingress_if          (ingress_if_10)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr10_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr10_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr10_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr10_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr10_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr10_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr10_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr10_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr10_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr10_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_11 (
                        .ingress_if          (ingress_if_11)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr11_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr11_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr11_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr11_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr11_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr11_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr11_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr11_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr11_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr11_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_12 (
                        .ingress_if          (ingress_if_12)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr12_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr12_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr12_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr12_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr12_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr12_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr12_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr12_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr12_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr12_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_13 (
                        .ingress_if          (ingress_if_13)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr13_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr13_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr13_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr13_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr13_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr13_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr13_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr13_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr13_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr13_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_14 (
                        .ingress_if          (ingress_if_14)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr14_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr14_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr14_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr14_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr14_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr14_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr14_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr14_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr14_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr14_4)
                      );

        ingress_ti_high #()
            u_ingress_ti_15 (
                        .ingress_if          (ingress_if_15)
                       ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr15_0)
                       ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr15_0)
                       ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr15_1)
                       ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr15_1)
                       ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr15_2)
                       ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr15_2)
                       ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr15_3)
                       ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr15_3)
                       ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr15_4)
                       ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr15_4)
                      );
    `endif
*/
`endif

// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

