/**
 * @file		ies_sbiosf.h
 * @date		May 2016
 * @author		Thanh Nguyen
 * @brief		Sideband IOSF support functions
 *
 * Functions to send and receive data using sideband IOSF.
 *
 * TODO: Insert license text here.
 */


#define SBIOSF_MAX_MSG		512
#define AQ_DESC_SIZE		32
#define SBIOSF_MAX_BASE_NDW	124
#define SBIOSF_MAX_SUBS_NDW	14


/*SBIOSF OPCODE */
#define SBIOSF_REG_READ			0x0
#define SBIOSF_REG_WRITE		0x1
#define SBIOSF_REG_WR_PRIV_CTRL		0x7
#define SBIOSF_REG_BLK_READ		0x10
#define SBIOSF_REG_BLK_WRITE		0x11
#define SBIOSF_COMP_NO_DATA		0x20
#define SBIOSF_COMP_DATA		0x21
#define SBIOSF_FUSE_REQ			0x45
#define SBIOSF_IP_READY			0xD0


//FIXME
typedef unsigned char		bool;

typedef unsigned char		u8;

/** Unsigned 16-bit word. */
typedef unsigned short		u16;

/** Unsigned 32-bit word. */
typedef unsigned int		u32;

/** Unsigned 64-bit word. */
typedef unsigned long long	u64;

/** Unsigned int. */
typedef unsigned int		uint;

#define true	1
#define false	0

struct admin_queue_desc
{
//FIXME: packed
	u16 flags;

	u16 opcode;

	u16 data_len;

	u16 rv_fvid;

	u32 cookie_hi;

	u32 cookie_low;

	u32 parm0;

	u32 parm1;

	u32 addr_hi;

	u32 addr_low;

};

int ies_sbiosf_init(void);
int ies_sbiosf_cleanup(void);
int ies_sbiosf_request_msg(uint *sbiosf_id);
int ies_sbiosf_send_msg(uint sbiosf_id);
int ies_sbiosf_send_raw_msg(uint sbiosf_id, u8 *buf, uint len);
int ies_sbiosf_add_read(uint sbiosf_id, u32 addr, uint n);
int ies_sbiosf_add_write1(uint sbiosf_id, u32 addr, u64 data);
int ies_sbiosf_add_write(uint sbiosf_id, u32 addr, u64 *data, uint n);
int ies_sbiosf_get_comp_data(uint *sbiosf_id, uint *rsp, u64 **data);
int ies_sbiosf_get_raw_comp_data(uint *sbiosf_id, u8 **data, uint *len);
int ies_sbiosf_get_intr_data(u64 *data);

