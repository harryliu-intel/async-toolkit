`ifndef CFGPKG_V6
`define CFGPKG_V6

// HEY!  Whenever we change this file, we must also change the name of the
// package and the `ifndefs too!  And there's an endpackage at the bottom of
// the file!
// There is the $rtlgen_pkg_version in specman.pm which must also be changed.

// HEY!  Whenever we change this file, we must also make the following changes
// in the nebulon_qa directory.  (If we are doing nebulon_qa testing.)
// 1.	./nebulon_qa/tests/RTL_tests/common/test_reg_common.sv
// 2.	Add symlinks to ./nebulon_qa/tests/RTL_tests/src/
// 3.	Add symlinks to ./nebulon_qa/tests/RTL_tests/common/

package rtlgen_pkg_v6;

// ==========================================================================
// Config Opcodes (same as IOSF Sideband)

// Version 1
//typedef enum logic [3:0] {
//    MRD   = 4'h0,
//    MWR   = 4'h1,
//    IORD  = 4'h2,
//    IOWR  = 4'h3,
//    CFGRD = 4'h4,
//    CFGWR = 4'h5,
//    MSGRD  = 4'h6,
//    MSGWR  = 4'h7,
//    MRD_SB  = 4'h8,
//    MWR_SB  = 4'h9,
//    IORD_SB  = 4'hA,
//    IOWR_SB  = 4'hB,
//    CFGRD_SB  = 4'hC,
//    CFGWR_SB  = 4'hD,
//    CRRD_SB  = 4'hE,
//    CRWR_SB  = 4'hF
//} cfg_opcode_t;


// Version 2
//typedef enum logic [3:0] {
//    MRD   = 4'h0,
//    MWR   = 4'h1,
//    IORD  = 4'h2,
//    IOWR  = 4'h3,
//    CFGRD = 4'h4,
//    CFGWR = 4'h5,
//    MSGRD  = 4'h6,
//    MSGWR  = 4'h7,
//    MRD_SB  = 4'h0,
//    MWR_SB  = 4'h1,
//    IORD_SB  = 4'h2,
//    IOWR_SB  = 4'h3,
//    CFGRD_SB  = 4'h4,
//    CFGWR_SB  = 4'h5,
//    CRRD_SB  = 4'h6,
//    CRWR_SB  = 4'h7
//} cfg_opcode_t;

// Version 3
typedef enum logic [3:0] {
    MRD   = 4'h0,
    MWR   = 4'h1,
    IORD  = 4'h2,
    IOWR  = 4'h3,
    CFGRD = 4'h4,
    CFGWR = 4'h5,
    CRRD  = 4'h6,
    CRWR  = 4'h7
} cfg_opcode_t;

// Obsolete labels
typedef enum logic [3:0] {
    MRD_SB  = 4'h0,
    MWR_SB  = 4'h1,
    IORD_SB  = 4'h2,
    IOWR_SB  = 4'h3,
    CFGRD_SB  = 4'h4,
    CFGWR_SB  = 4'h5,
    CRRD_SB  = 4'h6,
    CRWR_SB  = 4'h7
} cfg_old_opcode_t;
// Obsolete labels
typedef enum logic [3:0] {
    MSGRD  = 4'h6,
    MSGWR  = 4'h7
} cfg_old2_opcode_t;



// ==========================================================================
// Config Request Address formats for...

localparam CR_REQ_ADDR_LEN = 48;
localparam CR_REQ_ADDR_HI = 47;
// Mem
localparam CR_MEM_ADDR_HI = 47;
typedef struct packed { // 48
    logic [CR_MEM_ADDR_HI:0] offset;
} cfg_addr_mem_t;

// IO
localparam CR_IO_ADDR_HI = 15;
typedef struct packed { // 32+16=48
    logic [31:0] pad;
    logic [CR_IO_ADDR_HI:0] offset;
} cfg_addr_io_t;

// Cfg
localparam CR_CFG_ADDR_HI = 11;
typedef struct packed { // 36+12=48
    logic [35:0] pad;
    logic [CR_CFG_ADDR_HI:0] offset;
} cfg_addr_cfg_t;

// MSG
localparam CR_MSG_ADDR_HI = 15;
typedef struct packed { // 32+16=48
    logic [31:0] pad;
    logic [CR_MSG_ADDR_HI:0] offset;
} cfg_addr_msg_t;

// CR
localparam CR_CR_ADDR_HI = 15;
typedef struct packed { // 32+16=48
    logic [31:0] pad;
    logic [CR_CR_ADDR_HI:0] offset;
} cfg_addr_cr_t;

typedef union packed { // All structs must be 48
    cfg_addr_mem_t mem;
    cfg_addr_io_t  io;
    cfg_addr_cfg_t cfg;
    cfg_addr_msg_t  msg;
    cfg_addr_cr_t  cr;
} cfg_addr_t;

// ==========================================================================

// for 64bit bus
typedef struct packed { 
    logic        valid;
    cfg_opcode_t opcode;
    cfg_addr_t   addr;
    logic  [7:0] be;
    logic [63:0] data;
    logic [7:0] sai;
    logic  [7:0] fid;
    logic [2:0] bar;
} cfg_req_64bit_t;

typedef struct packed {
    logic        read_valid;
    logic        read_miss;
    logic        write_valid;
    logic        write_miss;
    logic        sai_successfull;
    logic [63:0] data;
} cfg_ack_64bit_t;

// for 32bit bus
typedef struct packed { 
    logic        valid;
    cfg_opcode_t opcode;
    cfg_addr_t   addr;
    logic  [3:0] be;
    logic [31:0] data;
    logic [7:0] sai;
    logic  [7:0] fid;
    logic [2:0] bar;
} cfg_req_32bit_t;

typedef struct packed {
    logic        read_valid;
    logic        read_miss;
    logic        write_valid;
    logic        write_miss;
    logic        sai_successfull;
    logic [31:0] data;
} cfg_ack_32bit_t;

// for 8bit bus
typedef struct packed { 
    logic        valid;
    cfg_opcode_t opcode;
    cfg_addr_t   addr;
    logic  [0:0] be;
    logic  [7:0] data;
    logic [7:0] sai;
    logic  [7:0] fid;
    logic [2:0] bar;
} cfg_req_8bit_t;

typedef struct packed {
    logic        read_valid;
    logic        read_miss;
    logic        write_valid;
    logic        write_miss;
    logic        sai_successfull;
    logic  [7:0] data;
} cfg_ack_8bit_t;

typedef struct packed {
    cfg_opcode_t opcode;
    cfg_addr_t   addr;
    logic  [2:0] count;
} cfgrem_req_t;

typedef struct packed {
    logic        hit;
    cfg_opcode_t opcode;
    logic  [7:0] fid;
    cfg_addr_t   addr;
    logic        last;
    logic  [7:0] dest;
} cfgrem_ack_t;

typedef struct packed {
    // TODO - fix fixed width
    logic [63:0] value;
    logic [63:0] mask;
} cfg_mbar_t;

typedef struct packed {
    // TODO - fix fixed width???
    logic [31:0] value;
    logic [31:0] mask;
} cfg_ibar_t;

//`define  FMT(cmd) cmd.opcode[6:5]
//`define TYPE(cmd) cmd.opcode[4:0]

// ==========================================================================

typedef enum logic [6:0] {
    // Memory Read
    IOSF_MRD32       = 7'b00_00000,
    IOSF_MRD64       = 7'b01_00000,
    IOSF_MRDLK32     = 7'b00_00001,
    IOSF_MRDLK64     = 7'b01_00001,
    IOSF_LTMRD32     = 7'b00_00111,
    IOSF_LTMRD64     = 7'b01_00111,

    // Memory Write
    IOSF_MWR32       = 7'b10_00000,
    IOSF_MWR64       = 7'b11_00000,
    IOSF_LTMWR32     = 7'b10_00111,
    IOSF_LTMWR64     = 7'b11_00111,

    // IO
    IOSF_IORD        = 7'b00_00010,
    IOSF_IOWR        = 7'b10_00010,

    // Config
    IOSF_CFGRD0      = 7'b00_00100,
    IOSF_CFGWR0      = 7'b10_00100,
    IOSF_CFGRD1      = 7'b00_00101,
    IOSF_CFGWR1      = 7'b10_00101,

    // Message
//  IOSF_MSG         = 7'b01_10xxx,
//  IOSF_MSGD        = 7'b11_10xxx,

    // Completion
    IOSF_CPL         = 7'b00_01010,
    IOSF_CPLD        = 7'b10_01010,
    IOSF_CPLLK       = 7'b00_01011,
    IOSF_CPLDLK      = 7'b10_01011,

    // Atomic
    IOSF_FETCHADD32  = 7'b10_01100,
    IOSF_FETCHADD64  = 7'b11_01100,
    IOSF_SWAP32      = 7'b10_01101,
    IOSF_SWAP64      = 7'b11_01101,
    IOSF_CAS32       = 7'b10_01110,
    IOSF_CAS64       = 7'b11_01110
} cfg_iosf_opcode_t;

typedef struct packed {
    // DW 3-4
    logic [63:0] address;
    // DW 2
    logic [15:0] rqid;
    logic  [7:0] tag;
    logic  [3:0] lbe;
    logic  [3:0] fbe;
    // DW 1
    logic        rsvd0_7;
    //logic  [1:0] fmt;
    //logic  [4:0] type;
    cfg_iosf_opcode_t opcode;
    logic        rsvd1_7;
    logic  [2:0] tc;
    logic        rsvd1_3;
    logic        ido;
    logic        rsvd1_1;
    logic        th;
    logic        td;
    logic        ep;
    // attr[1:0]
    logic  [1:0] at;
    logic  [9:0] length;

    logic        ro;
    logic        ns;
} cfg_iosf_cmd_t;

function automatic logic CmdParity (input cfg_iosf_cmd_t cmd);
    CmdParity = ^{
        cmd.opcode,
        cmd.tc,
        cmd.ep,
        cmd.ro,
        cmd.ns,
        cmd.ido,
        cmd.th,
        cmd.at,
        cmd.length,
        cmd.rqid,
        cmd.tag,
        cmd.lbe,
        cmd.fbe,
        cmd.address,
        cmd.td,
        cmd.rsvd0_7,
        cmd.rsvd1_1,
        cmd.rsvd1_3,
        cmd.rsvd1_7
    };
endfunction : CmdParity

endpackage: rtlgen_pkg_v6

`endif

