#ifndef _SBIOSF_H_
#define _SBIOSF_H_

enum sbiosf_opcode {
    SBIOSF_REG_READ        = 0x00,
    SBIOSF_REG_WRITE       = 0x01,
    SBIOSF_IO_READ         = 0x02,
    SBIOSF_IO_WRITE        = 0x03,
    SBIOSF_CFG_READ        = 0x04,
    SBIOSF_CFG_WRITE       = 0x05,
    SBIOSF_WR_PRIV_CTRL    = 0x07,
    SBIOSF_BLOCK_RD        = 0x10,
    SBIOSF_BLOCK_WR        = 0x11,
    SBIOSF_CMP_NODATA      = 0x20,
    SBIOSF_CMP_WDATA       = 0x21,
    SBIOSF_FUSE_REQ        = 0x45,
    SBIOSF_IP_READY        = 0xD0,
};

struct sbiosf_data {
    unsigned int    addr;
    unsigned char   ndw;        /*Number of 32b words*/
    unsigned long  *data;
    
};

struct sbiosf_msg {
    unsigned int        EH:1;
    unsigned int        AL:1;
    unsigned int        rsp:2;
    unsigned int        bar:3;
    unsigned int        tag:3;
    enum sbiosf_opcode  opcode;
    unsigned char       source;
    unsigned char       dest;
    unsigned int        RS:3;
    unsigned short      SAI;
    unsigned int        EH2:1;
    unsigned int        EXPHDR:7;
    unsigned char       fid;
    unsigned int        sbe:4;
    unsigned int        fbe:4;
    unsigned int        num_data;    /*size of data array*/
    struct sbiosf_data *data;
};

struct sbiosf_msg *create_block_read_sbiosf_msg(int num_reads);
struct sbiosf_msg *create_block_write_sbiosf_msg(int num_writes);
struct sbiosf_msg *create_reg_read_sbiosf_msg();
struct sbiosf_msg *create_reg_write_sbiosf_msg();
void add_rd_wr_op_to_msg(struct sbiosf_msg *msg, 
                    unsigned int pos, 
                    unsigned int addr,
                    unsigned int ndw,
                    unsigned long *data);
struct sbiosf_msg *create_cmp_sbiosf_msg(unsigned int numData, 
                                         unsigned long *data);
struct sbiosf_msg *create_wr_priv_ctrl_sbiosf_msg(unsigned int addr, 
                                                 unsigned long data);
struct sbiosf_msg *create_fuse_req_sbiosf_msg();
struct sbiosf_msg *create_ip_ready_sbiosf_msg();
void free_sbiosf_msg(struct sbiosf_msg *msg);
void print_sbiosf_msg(struct sbiosf_msg *msg);
unsigned int *pack_sbiosf_msg(struct sbiosf_msg *msg, unsigned int *size);
struct sbiosf_msg *unpack_sbiosf_msg(unsigned int *rawData, unsigned int size);
void print_packed_array(unsigned int *array, unsigned int size);

#endif
