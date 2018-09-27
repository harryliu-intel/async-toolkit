/*  --------------- Example code  ----------------------
//a01_FUNC_l2q_rd_conflict_chk_1 : When CMDSEL Issues a READ REQ to CSB1 for L2Q Read there shouldn¿t be a PIPE VALII
D Set on the MBY_L2Q_RD_REQ_PIPESTAGE
`MBY_ASSERTS_TRIGGER(a01_FUNC_l2q_rd_conflict_chk_1,
         psm_memifc_l2q_cmdsel_req.req,
         !i_psm_hs.i_psm_hs_csb1.cmd_valid_pipe[i_psm_hs.i_psm_hs_csb1.MBY_L2Q_RD_REQ_PIPESTAGE],
         posedge psm_clk,
         ~psm_rst_n,
         `ERR_MSG("The cmdsel l2q request must be on a cycle that does not collide with the other l2q read access done by hs.")
    );

//a02_FUNC_l2q_rd_conflict_chk_2 : When CMDSEL Issues a READ REQ to CSB1 for L2Q Read there shouldn¿t be a PIPE VALII
D Set on the MBY_L2Q_WR_REQ_PIPESTAGE
`MBY_ASSERTS_TRIGGER(a02_FUNC_l2q_rd_conflict_chk_2,
         psm_memifc_l2q_cmdsel_req.req,
         !i_psm_hs.i_psm_hs_csb1.cmd_valid_pipe[i_psm_hs.i_psm_hs_csb1.MBY_L2Q_WR_REQ_PIPESTAGE],
         posedge psm_clk,
         ~psm_rst_n,
         `ERR_MSG("The cmdsel l2q request must be on a cycle that does not collide with the other l2q write access done by hs.")
    );

//a03_FUNC_l2q_rd_conflict_chk_3 : When CMDSEL Issues a READ REQ to CSB1 , then MBY_MEM_RD_REQ2DATA_DELAY+1 clock cycles later there shouldn¿t be a pipe valid set on MBY_L2Q_WR_CACHE_PIPESTAGE
`MBY_ASSERTS_DELAYED_TRIGGER(a03_FUNC_l2q_rd_conflict_chk_3,
         psm_memifc_l2q_cmdsel_req.req,
         MBY_MEM_RD_REQ2DATA_DELAY + 1,
         !i_psm_hs.i_psm_hs_csb1.cmd_valid_pipe[i_psm_hs.i_psm_hs_csb1.MBY_L2Q_WR_CACHE_PIPESTAGE],
         posedge psm_clk,
         ~psm_rst_n,
         `ERR_MSG("The cmdsel l2q request must be on a cycle that does not collide with the other l2q write access done by hs.")
    );

*/
