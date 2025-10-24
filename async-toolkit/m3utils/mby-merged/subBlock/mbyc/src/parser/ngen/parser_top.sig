reset_n logic
    no_pins
;

input_fifo_wr_en [1:0] logic
;

input_fifo_wr_adr [1:0][2:0] logic
;

input_data_fifo_wr_data [1:0][1095:0] logic
;

input_fifo_rd_en [1:0] logic
;

input_fifo_rd_adr [1:0][2:0] logic
;

input_data_fifo_rd_data [1:0][1095:0] logic
;

input_md_fifo_wr_data [1:0][123:0] logic
;

input_md_fifo_rd_data [1:0][123:0] logic
;

md_dly_fifo_wr_en [1:0] logic
;

md_dly_fifo_wr_adr [1:0][5:0] logic
;

md_dly_fifo_wr_data [1:0][123:0] logic
;

md_dly_fifo_rd_en [1:0] logic
;

md_dly_fifo_rd_adr [1:0][5:0] logic
;

md_dly_fifo_rd_data [1:0][123:0] logic
;

tail_dly_fifo_wr_en [1:0] logic
;

tail_dly_fifo_wr_adr [1:0][5:0] logic
;

tail_dly_fifo_wr_data [1:0][30:0] logic
;

tail_dly_fifo_rd_en [1:0] logic
;

tail_dly_fifo_rd_adr [1:0][5:0] logic
;

tail_dly_fifo_rd_data [1:0][30:0] logic
;

extract_cfg_wr_en [79:0] logic
;

extract_cfg_wr_adr [79:0][3:0] logic
;

extract_cfg_wr_data [79:0][15:0] logic
;

extract_cfg_rd_en [79:0] logic
;

extract_cfg_rd_adr [79:0][3:0] logic
;

extract_cfg_rd_data [79:0][15:0] logic
;
