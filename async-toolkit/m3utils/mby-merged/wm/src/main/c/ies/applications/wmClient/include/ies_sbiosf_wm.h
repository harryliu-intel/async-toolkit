/**
 * @file		ies_sbiosf_wm.h
 * @date		May 2016
 * @author		Thanh Nguyen
 * @brief		Sideband IOSF support functions
 *
 * Functions to send and receive data using sideband IOSF.
 *
 * TODO: Insert license text here.
 */


#define IES_SBIOSF_NUM_TXQ		4
#define IES_SBIOSF_NUM_RXQ		2

#define IES_MODEL_MSG_HEADER_SIZE	12
#define IES_MODEL_VERSION		2	
#define IES_MODEL_MSG_IOSF 		11

struct cpk_queue
{
	unsigned long base;
	unsigned int n;
	unsigned long head;
	unsigned long tail;
	unsigned int desc_size;
};


struct sbiosf_txq
{
	/* -- 16/32-bit members must be aligned to a 2/4-byte boundary -- */

	/** Length of the entire message in bytes, including the header */
	u32 msg_length;

	/** The ''fm_modelMessage'' version. */
	u16 version;

	/** Message type (see ''fm_modelMsgType''). */
	u16 type;

	/** Target model switch number for this message. */
	u16 sw;

	/** Target model switch port for this message. */
	u16 port;

	/* SBIOSF buffer */
	u8 data[SBIOSF_MAX_MSG];
};

struct sbiosf_rxq
{
	/* SBIOSF buffer to receive event message */
	u8 data[SBIOSF_MAX_MSG];
};

int ies_sbiosf_wm_init(void);
int ies_sbiosf_wm_cleanup(void);

u64 ies_sbiosf_wm_atq_read_base(void);
u64 ies_sbiosf_wm_atq_read_n(void);
u64 ies_sbiosf_wm_atq_read_head(void);
void ies_sbiosf_wm_atq_set_head(u64 head);
u64 ies_sbiosf_wm_atq_read_tail(void);
u64 ies_sbiosf_wm_arq_read_base(void);
u64 ies_sbiosf_wm_arq_read_n(void);
u64 ies_sbiosf_wm_arq_read_head(void);
u64 ies_sbiosf_wm_arq_read_tail(void);
void ies_sbiosf_wm_arq_set_head(u64 head);
