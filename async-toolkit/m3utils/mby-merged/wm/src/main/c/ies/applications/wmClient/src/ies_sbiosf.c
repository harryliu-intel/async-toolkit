/**
 * @file		ies_sbiosf.c
 * @date		May 2016
 * @author		Thanh Nguyen
 * @brief		Sideband IOSF support functions
 *
 * Functions to send and receive data using sideband IOSF.
 *
 * TODO: Insert license text here.
 */

#define WM

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "ies_sbiosf.h"
#ifdef WM
#include "ies_sbiosf_wm.h"
#endif

struct atq_state {
	/* Entry is reserved */
	bool reserved;

	/* number of bytes for existing command */
	u32 cmd_num_bytes;

	/* number of bytes for response message */
	u32 rsp_num_bytes;

	u32 num_read;
	u32 num_write;
};

/* NOTE: This implementation assumes single threaded call */

static struct atq_state *atq_state;

/* processed response, if tail_processed is the same
 * as tail, then all responses are processed */
static unsigned long atq_tail_processed;
static unsigned long arq_tail_processed;

static u64 ies_sbiosf_atq_read_base(void)
{
#ifdef WM
	return ies_sbiosf_wm_atq_read_base();
#endif
}

static u64 ies_sbiosf_atq_read_n(void)
{
#ifdef WM
	return ies_sbiosf_wm_atq_read_n();
#endif
}

static u64 ies_sbiosf_atq_read_head(void)
{
#ifdef WM
	return ies_sbiosf_wm_atq_read_head();
#endif
}

static void ies_sbiosf_atq_set_head(u64 head)
{
#ifdef WM
	ies_sbiosf_wm_atq_set_head(head);
#endif
}

static u64 ies_sbiosf_atq_read_tail(void)
{
#ifdef WM
	return ies_sbiosf_wm_atq_read_tail();
#endif
}

static u64 ies_sbiosf_arq_read_base(void)
{
#ifdef WM
	return ies_sbiosf_wm_arq_read_base();
#endif
}

static u64 ies_sbiosf_arq_read_n(void)
{
#ifdef WM
	return ies_sbiosf_wm_arq_read_n();
#endif
}

static u64 ies_sbiosf_arq_read_head(void)
{
#ifdef WM
	return ies_sbiosf_wm_arq_read_head();
#endif
}

static u64 ies_sbiosf_arq_read_tail(void)
{
#ifdef WM
	return ies_sbiosf_wm_arq_read_tail();
#endif
}

static void ies_sbiosf_arq_set_head(u64 head)
{
#ifdef WM
	ies_sbiosf_wm_arq_set_head(head);
#endif
}

static u16 sbiosf_txq_get_data_len(uint sbiosf_id)
{
	struct admin_queue_desc *desc;
	unsigned long data;
	u64 base;

	base = ies_sbiosf_atq_read_base();
	desc = (struct admin_queue_desc *)(base + sbiosf_id * AQ_DESC_SIZE);
	return desc->data_len;
}

static u8 *sbiosf_txq_get_buf(uint sbiosf_id)
{
	struct admin_queue_desc *desc;
	unsigned long data;
	u64 base;

	base = ies_sbiosf_atq_read_base();
	desc = (struct admin_queue_desc *)(base + sbiosf_id * AQ_DESC_SIZE);
	data = desc->addr_hi;
	data <<= 32;
	data |= desc->addr_low;

	return (u8 *)data;
}

static u8 *sbiosf_rxq_get_buf(uint sbiosf_id)
{
	struct admin_queue_desc *desc;
	unsigned long data;
	u64 base;

	base = ies_sbiosf_arq_read_base();
	desc = (struct admin_queue_desc *)(base + sbiosf_id * AQ_DESC_SIZE);
	data = desc->addr_hi;
	data <<= 32;
	data |= desc->addr_low;

	return (u8 *)data;
}

/**
 * ies_sbiosf_init() - Initialize sideband IOSF TX and RX Queue.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_init(void)
{
	uint n;
	int err;
	int i;

#ifdef WM
	err = ies_sbiosf_wm_init();
	if (err)
		return err;
#endif

	n = ies_sbiosf_atq_read_n();
	atq_state = malloc(n * sizeof(struct atq_state));
	if (!atq_state)
		return -ENOMEM;

	for (i = 0; i < n; i++)
		atq_state[i].reserved = false;

	atq_tail_processed = ies_sbiosf_atq_read_tail();
	arq_tail_processed = ies_sbiosf_arq_read_tail();

	return 0;
}

/**
 * ies_sbiosf_cleanup() - Cleanup sideband IOSF TX and RX Queue.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_cleanup(void)
{
	/* FIXME: stop the queue */

#ifdef WM
	ies_sbiosf_wm_cleanup();
#endif

	if (atq_state)
		free(atq_state);

	return 0;
}

/**
 * ies_sbiosf_request_msg() - Request a free sideband IOSF message.
 * @sbiosf_id:  The id of the sbiosf message, which can be used to
 *            add read or write operations before sending
 *            for the operations to be executed.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_request_msg(uint *sbiosf_id)
{
	uint i, n, id;
	u64 base, head, tail;
	u64 next;

	n = ies_sbiosf_atq_read_n();
	if (n == 0) {
		printf("ATQ N is zero\n");
		return -1;
	}
	base = ies_sbiosf_atq_read_base();
	head = ies_sbiosf_atq_read_head();
	tail = ies_sbiosf_atq_read_tail();

	for (i = 0; i < n; i++) {
		next = head + AQ_DESC_SIZE;
		id = (((next - base) / AQ_DESC_SIZE) + i) % n;
		if (tail == (base + id * AQ_DESC_SIZE)) {
			printf("No free entry\n");
			return -2;
		}
		if (!atq_state[id].reserved) {
			atq_state[id].reserved = true;
			atq_state[id].num_read = 0;
			atq_state[id].num_write = 0;
			*sbiosf_id = id;
			return  0;
		}
	}

	return -3;
}

/**
 * ies_sbiosf_send_raw_msg() - Send the raw message with specific id.
 * @sbiosf_id: The id of the sbiosf message obtained from
 *	       ies_sbiosf_request_msg.
 * @buf: The buffer containing the raw SBIOSF message. If null, then
 *	 use the exsiting pre-built data buffer from add operations.
 * @len: The length of the data buffer.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_send_raw_msg(uint sbiosf_id, u8 *buf, uint len)
{
	struct admin_queue_desc *desc;
	uint i, n, next_id;
	u64 base, head, tail;
	u64 next;
	u8 *data;

	if (!atq_state[sbiosf_id].reserved) {
		printf("Id %d has not been requested\n", sbiosf_id);
		return -1;
	}

	n = ies_sbiosf_atq_read_n();
	if (n == 0) {
		printf("ATQ N is zero\n");
		return -1;
	}
	base = ies_sbiosf_atq_read_base();
	head = ies_sbiosf_atq_read_head();
	tail = ies_sbiosf_atq_read_tail();

	next_id = (((head - base) / AQ_DESC_SIZE) + 1) % n;

	if (next_id != sbiosf_id) {
		printf("Invalid id %d to be sent next. Expect %d\n",
		       sbiosf_id, next_id);
		return -1;
	}

	if (!(atq_state[next_id].num_read || atq_state[next_id].num_write)) {
		printf("No operations has been added to this entry %d\n",
		       sbiosf_id);
		return -2;
	}

	/* Fill out the SBIOSF header */
	data = sbiosf_txq_get_buf(sbiosf_id);
	if (buf) {
		if (len > SBIOSF_MAX_MSG) {
			printf("Len %d too long\n", len);
			return -2;
		}
		memcpy(data, buf, len);
		atq_state[next_id].cmd_num_bytes = len;
	} else {
		data[2] = atq_state[next_id].num_read ?
			SBIOSF_REG_BLK_READ : SBIOSF_REG_BLK_WRITE;
	}

	/* Fill out the descriptor header */
	head = base + next_id * AQ_DESC_SIZE;
	desc = (struct admin_queue_desc *)head;
	desc->data_len = atq_state[next_id].cmd_num_bytes;

	/* Lastly increase head pointer */
	ies_sbiosf_atq_set_head(head);

	return 0;
}

/**
 * ies_sbiosf_send_msg() - Send the message with specific id after
 *                         read or write operation has been added.
 * @sbiosf_id: The id of the sbiosf message obtained from
 *	       ies_sbiosf_request_msg.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_send_msg(uint sbiosf_id)
{
	return ies_sbiosf_send_raw_msg(sbiosf_id, NULL, 0);
}

/**
 * ies_sbiosf_add_read() - Add read operation to the specific sbiosf message.
 * NOTE: Hardware limitations:
 *		- Total size for read request and response message is 512 bytes
 *		- n for first entry is limited to 62.
 *		- n for subsequent entry is limited to 7.
 *              - Cannot mix read and write operations together.
 * @id: The id of the sbiosf message obtained from ies_sbiosf_request_msg.
 * @addr: The address to read from. This must be aligned to 64-bit.
 * @n: The number of 64-bit data to return.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_add_read(uint id, u32 addr, uint n)
{
	u32 num_bytes;
	u8 *data;

	if (addr >> 28) {
		printf("Address 0x%x is out of range\n", addr);
		return -ERANGE;
	}
	if (addr & 0x1) {
		//FIXME: change for byte offset
		printf("Address 0x%x not aligned\n", addr);
		return -ERANGE;
	}
	if (id < 0 || id >= ies_sbiosf_atq_read_n()) {
		printf("Id %d is out of range\n", id);
		return -1;
	}
	if (!atq_state[id].reserved) {
		printf("Id %d has not been requested\n", id);
		return -1;
	}
	if (atq_state[id].num_write) {
		printf("Id %d has %d write operations already\n",
		       id, atq_state[id].num_write);
		return -1;
	}
	if (atq_state[id].num_read) {
		if (n > SBIOSF_MAX_SUBS_NDW / 2) {
			printf("Exceed %d ndw limit of %d\n",
			       n * 2, SBIOSF_MAX_SUBS_NDW);
			return -2;
		}
		num_bytes = atq_state[id].cmd_num_bytes + 4;
		if (num_bytes > SBIOSF_MAX_MSG) {
			printf("Overflow %d IOSF max size\n", num_bytes);
			return -3;
		}
		data = sbiosf_txq_get_buf(id);
		*(u32 *)(data + atq_state[id].cmd_num_bytes) =
			(addr << 4) | n * 2;
		atq_state[id].cmd_num_bytes = num_bytes;

		num_bytes = atq_state[id].rsp_num_bytes + 8 * n;
		if (num_bytes > SBIOSF_MAX_MSG) {
			printf("Overflow response IOSF max size\n");
			return -3;
		}
		atq_state[id].rsp_num_bytes = num_bytes;
	} else {
		if (n > SBIOSF_MAX_BASE_NDW / 2) {
			printf("Exceed %d ndw limit of %d\n",
			       n * 2, SBIOSF_MAX_BASE_NDW);
			return -4;
		}
		atq_state[id].cmd_num_bytes = 16;
		atq_state[id].rsp_num_bytes = 8 + n * 8;
		data = sbiosf_txq_get_buf(id);
		data[8] = n * 2;
		data[9] = 0;
		*(u32 *)&data[10] = addr;
		data[14] = 0;
		data[15] = 0;
	}

	atq_state[id].num_read++;
	return 0;
}

/**
 * ies_sbiosf_add_write1() - Add a single 64-bit write operation to
 *			     the specific sbiosf message.
 * NOTE: Hardware limitations:
 *		- Total size for all message types is 512 bytes
 *              - Cannot mix read and write operations together.
 * @sbiosf_id: The id of the sbiosf message obtained from
 *	       ies_sbiosf_request_msg.
 * @addr: The address to read from. This must be aligned to 64-bit.
 * @data: The array of 64-bit data to write.
 * @n: The number of 64-bit data.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_add_write1(uint sbiosf_id, u32 addr, u64 data)
{
	return ies_sbiosf_add_write(sbiosf_id, addr, &data, 1);
}

/**
 * ies_sbiosf_add_write() - Add n 64-bit write operations to the
			    specific sideband IOSF message.
 * NOTE: Hardware limitations:
 *		- Total size for all message types is 512 bytes
 *		- n for first entry is limited to 62.
 *		- n for subsequent entry is limited to 7.
 *		- Cannot mix read and write operations together.
 * @id: The id of the sbiosf message obtained from ies_sbiosf_request_msg.
 * @addr: The address to read from. This must be aligned to 64-bit.
 * @data: The array of 64-bit data to write.
 * @n: The number of 64-bit data.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_add_write(uint id, u32 addr, u64 *val64, uint n)
{
	u32 num_bytes;
	u8 *data;

	if (addr >> 28) {
		printf("Address 0x%x is out of range\n", addr);
		return -ERANGE;
	}
	if (addr & 0x1) {
		//FIXME: change for byte offset
		printf("Address 0x%x not aligned\n", addr);
		return -ERANGE;
	}
	if (id < 0 || id >= ies_sbiosf_atq_read_n()) {
		printf("Id %d is out of range\n", id);
		return -1;
	}
	if (!atq_state[id].reserved) {
		printf("Id %d has not been requested\n", id);
		return -1;
	}
	if (atq_state[id].num_read) {
		printf("Id %d has %d read operations already\n",
		       id, atq_state[id].num_read);
		return -1;
	}
	if (atq_state[id].num_write) {
		if (n > SBIOSF_MAX_SUBS_NDW / 2) {
			printf("Exceed %d ndw limit of %d\n",
			       n * 2, SBIOSF_MAX_SUBS_NDW);
			return -2;
		}
		num_bytes = atq_state[id].cmd_num_bytes + 4 + 8 * n;
		if (num_bytes > SBIOSF_MAX_MSG) {
			printf("Overflow %d IOSF max size.\n", num_bytes);
			return -3;
		}
		data = sbiosf_txq_get_buf(id);
		*(u32 *)(data + atq_state[id].cmd_num_bytes) =
			(addr << 4) | n * 2;
		memcpy(data + atq_state[id].cmd_num_bytes + 4, val64, n * 8);
		atq_state[id].cmd_num_bytes = num_bytes;
	} else {
		if (n > SBIOSF_MAX_BASE_NDW / 2) {
			printf("Exceed %d ndw limit of %d\n",
			       n * 2, SBIOSF_MAX_BASE_NDW);
			return -4;
		}
		atq_state[id].cmd_num_bytes = 16 + 8 * n;
		atq_state[id].rsp_num_bytes = 16;
		data = sbiosf_txq_get_buf(id);
		data[8] = n * 2;
		data[9] = 0;
		*(u32 *)&data[10] = addr;
		data[14] = 0;
		data[15] = 0;
		memcpy(data + 16, val64, n * 8);
	}

	atq_state[id].num_write++;
	return 0;
}

/**
 * ies_sbiosf_get_raw_comp_data() - Get raw completion data if available.
 * @sbiosf_id: The id of the sbiosf message that has completion data.
 * @data: The pointer to raw result if available.
 * @len: The length of the raw completion message.
 *
 * Returns: 0 if successful.
 * Returns: ENODATA if no more data.
 */
int ies_sbiosf_get_raw_comp_data(uint *sbiosf_id, u8 **data, uint *len)
{
	int id;
	u8 *buf;

	if (atq_tail_processed == ies_sbiosf_atq_read_tail())
		return -ENODATA;

	/* Get next response */
	id = (atq_tail_processed - ies_sbiosf_atq_read_base()) / AQ_DESC_SIZE;
	id  = (id + 1) % ies_sbiosf_atq_read_n();

	atq_tail_processed = ies_sbiosf_atq_read_base() + id * AQ_DESC_SIZE;
	atq_state[id].reserved = false;

	/* FIXME: Need to check for reply length?
	 * Responsiblity of the caller to verify? */

	buf = sbiosf_txq_get_buf(id);
	if (!(buf[2] == SBIOSF_COMP_DATA ||
	      buf[2] == SBIOSF_COMP_NO_DATA)) {
		printf("Unexpected OPCODE 0x%x\n", buf[2]);
		return -1;
	}
	*sbiosf_id = id;
	*len = sbiosf_txq_get_data_len(id);

	/* FIXME: NOTE: data must be processed before the same id
         * requested and new operations are added to it.
         * Should we just add a note in the function for this behavior
         * or change the function to make the user pass in the buffer
         * and do copy here */

	*data = buf;

	return 0;
}


/**
 * ies_sbiosf_get_comp_data() - Get completion data if available.
 * @sbiosf_id: The id of the sbiosf message that has completion data.
 * @data: The pointer to the 64-bit result if available.
 * @rsp: The completion status.
 *
 * Returns: 0 if successful.
 * Returns: ENODATA if no more data.
 */
int ies_sbiosf_get_comp_data(uint *sbiosf_id, uint *rsp, u64 **data)
{
	uint len;
	uint id;
	u8 *buf;
	int rv;

	rv = ies_sbiosf_get_raw_comp_data(sbiosf_id, &buf, &len);
	if (rv)
		return rv;

	*rsp = (buf[3] >> 3) & 0x3;

	*data = (u64 *)&buf[8];

	return 0;
}

/**
 * ies_sbiosf_get_intr_data() - Get interrupt data on the RxQ if available.
 * @data: The 64-bit global interrupt result if available.
 *
 * Returns: 0 if successful.
 * Returns: ENODATA if no valid data.
 */
int ies_sbiosf_get_intr_data(u64 *data)
{
	uint id;
	u8 *buf;

	if (arq_tail_processed == ies_sbiosf_arq_read_tail())
		return -ENODATA;

	/* Get next response */
	id = (arq_tail_processed - ies_sbiosf_arq_read_base()) / AQ_DESC_SIZE;
	id  = (id + 1) % ies_sbiosf_arq_read_n();

	arq_tail_processed = ies_sbiosf_arq_read_base() + id * AQ_DESC_SIZE;

	/* Increase head pointer */
	ies_sbiosf_arq_set_head(arq_tail_processed);

	buf = sbiosf_rxq_get_buf(id);
	if (buf[2] != SBIOSF_REG_WR_PRIV_CTRL) {
		printf("Unexpected OPCODE 0x%x\n", buf[2]);
		return -1;
	}
	*data = *(u64 *)&buf[12];

	return 0;
}

