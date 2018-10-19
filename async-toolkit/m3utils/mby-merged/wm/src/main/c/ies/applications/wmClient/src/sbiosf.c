#include "sbiosf.h"
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>

struct sbiosf_msg *create_block_read_sbiosf_msg(int num_reads)
{
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));

	if (!msg) {
		printf("Error allocating sbiosf message\n");
		exit(0);
	}

	bzero(msg, sizeof(*msg));

	msg->EH      = 1;
	msg->AL      = 1;
	msg->bar     = 0;
	msg->tag     = 0;
	msg->opcode  = SBIOSF_BLOCK_RD;
	msg->source  = 0;
	msg->dest    = 0;
	msg->RS      = 0;
	msg->SAI     = 0;
	msg->EH2     = 0;
	msg->EXPHDR  = 0;
	msg->fid     = 0;

	if (num_reads == 1) {
		msg->sbe     = 0xF;
		msg->fbe     = 0xF;
	}

	msg->num_data = num_reads;

	msg->data = malloc(num_reads * sizeof(*msg->data));

	if (!msg->data) {
		printf("Error allocating sbiosf data array\n");
		exit(0);
	}

	return msg;
}

struct sbiosf_msg *create_block_write_sbiosf_msg(int num_writes)
{
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));

	if (!msg) {
		printf("Error allocating sbiosf message\n");
		exit(0);
	}

	bzero(msg, sizeof(*msg));

	msg->EH      = 1;
	msg->AL      = 1;
	msg->bar     = 0;
	msg->tag     = 0;
	msg->opcode  = SBIOSF_BLOCK_WR;
	msg->source  = 0;
	msg->dest    = 0;
	msg->RS      = 0;
	msg->SAI     = 0;
	msg->EH2     = 0;
	msg->EXPHDR  = 0;
	msg->fid     = 0;

	if (num_writes == 1) {
		msg->sbe     = 0xF;
		msg->fbe     = 0xF;
	}

	msg->num_data = num_writes;

	msg->data = malloc(num_writes * sizeof(*msg->data));

	if (!msg->data) {
		printf("Error allocating sbiosf data array\n");
		exit(0);
	}

	return msg;
}

struct sbiosf_msg *create_reg_read_sbiosf_msg(int num_reads)
{
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));

	if (!msg) {
		printf("Error allocating sbiosf message\n");
		exit(0);
	}

	bzero(msg, sizeof(*msg));

	msg->EH      = 1;
	msg->AL      = 1;
	msg->bar     = 0;
	msg->tag     = 0;
	msg->opcode  = (num_reads == 1) ? SBIOSF_REG_READ : SBIOSF_BLOCK_RD;
	msg->source  = 0;
	msg->dest    = 0;
	msg->RS      = 0;
	msg->SAI     = 0;
	msg->EH2     = 0;
	msg->EXPHDR  = 0;
	msg->fid     = 0;

	if (num_reads == 1) {
		msg->sbe     = 0xF;
		msg->fbe     = 0xF;
	}

	msg->num_data = num_reads;

	msg->data = malloc(num_reads * sizeof(*msg->data));

	if (!msg->data) {
		printf("Error allocating sbiosf data array\n");
		exit(0);
	}

	return msg;
}

struct sbiosf_msg *create_reg_write_sbiosf_msg(int num_writes)
{
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));

	if (!msg) {
		printf("Error allocating sbiosf message\n");
		exit(0);
	}

	bzero(msg, sizeof(*msg));

	msg->EH      = 1;
	msg->AL      = 1;
	msg->bar     = 0;
	msg->tag     = 0;
	msg->opcode  = (num_writes == 1) ? SBIOSF_REG_WRITE : SBIOSF_BLOCK_WR;
	msg->source  = 0;
	msg->dest    = 0;
	msg->RS      = 0;
	msg->SAI     = 0;
	msg->EH2     = 0;
	msg->EXPHDR  = 0;
	msg->fid     = 0;

	if (num_writes == 1) {
		msg->sbe     = 0xF;
		msg->fbe     = 0xF;
	}

	msg->num_data = num_writes;

	msg->data = malloc(num_writes * sizeof(*msg->data));

	if (!msg->data) {
		printf("Error allocating sbiosf data array\n");
		exit(0);
	}

	return msg;
}

void add_rd_wr_op_to_msg(struct sbiosf_msg *msg,
			 unsigned int pos,
			 unsigned int addr,
			 unsigned int ndw,
			 unsigned long *data)
{
	if (pos >= msg->num_data) {
		printf("Error add_rd_wr_op_to_msg pos is greater");
		printf(" than msg->num_data\n");
		exit(0);
	}

	if (!msg->data) {
		printf("Error data is null\n");
		exit(0);
	}

	if (pos == 0 && ndw > 124) {
		printf("Error ndw for position 0 is > 124");
		exit(0);
	} else if (pos != 0 && ndw > 15) {
		printf("Error ndw is > 15");
		exit(0);
	}

	if ((ndw & 1) == 1) {
		printf("Error ndw must be even\n");
		exit(0);
	}

	if ((msg->opcode == SBIOSF_REG_WRITE) &&
	    (ndw != 2)) {
		printf("Error REG_WRITE opcode only supports 1 64b write\n");
		exit(0);
	}

	msg->data[pos].addr  = addr;
	msg->data[pos].ndw   = ndw;
	msg->data[pos].data  = data;
}

/* set data to NULL for completion without data */
struct sbiosf_msg
	*create_cmp_sbiosf_msg(unsigned int num_data,
						   unsigned long *data)
{
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));

	if (!msg) {
		printf("Error allocating sbiosf message\n");
		exit(0);
	}

	bzero(msg, sizeof(*msg));

	msg->EH      = 1;
	msg->RS      = 0;
	msg->tag     = 0;
	msg->source  = 0;
	msg->dest    = 0;
	msg->RS      = 0;
	msg->SAI     = 0;
	msg->EH2     = 0;
	msg->EXPHDR  = 0;

	if (num_data == 0) {
		msg->opcode  = SBIOSF_CMP_NODATA;
		msg->num_data = 0;
		msg->data = NULL;
	} else {
		msg->opcode  = SBIOSF_CMP_WDATA;

		/* One sbiosf data array with one or more data words in it*/
		msg->num_data = 1;
		msg->data = malloc(sizeof(*msg->data));
		msg->data->ndw  = num_data * 2;
		msg->data->data = data;
	}

	return msg;
}

struct sbiosf_msg
	*create_wr_priv_ctrl_sbiosf_msg(unsigned int addr,
					unsigned long data)
{
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));

	if (!msg) {
		printf("Error allocating sbiosf message\n");
		exit(0);
	}

	bzero(msg, sizeof(*msg));

	msg->EH      = 1;
	msg->AL      = 0;
	msg->bar     = 0;
	msg->tag     = 0;
	msg->opcode  = SBIOSF_WR_PRIV_CTRL;
	msg->source  = 0;
	msg->dest    = 0;
	msg->RS      = 0;
	msg->SAI     = 0;
	msg->EH2     = 0;
	msg->EXPHDR  = 0;
	msg->fid     = 0;
	msg->sbe     = 0xF;
	msg->fbe     = 0xF;

	msg->num_data = 1;
	msg->data = malloc(sizeof(*msg->data));
	msg->data->data = malloc(sizeof(*msg->data->data));

	if (!msg->data || !msg->data->data) {
		printf("Error allocating sbiosf data array\n");
		exit(0);
	}

	msg->data->ndw      = 2;
	msg->data->addr     = addr;
	msg->data->data[0]  = data;

	return msg;
}

struct sbiosf_msg *create_fuse_req_sbiosf_msg()
{
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));

	if (!msg) {
		printf("Error allocating sbiosf message\n");
		exit(0);
	}

	bzero(msg, sizeof(*msg));

	msg->EH      = 1;
	msg->tag     = 0;
	msg->opcode  = SBIOSF_FUSE_REQ;
	msg->source  = 0;
	msg->dest    = 0;
	msg->RS      = 0;
	msg->SAI     = 0;
	msg->EH2     = 0;
	msg->EXPHDR  = 0;

	return msg;
}

struct sbiosf_msg *create_ip_ready_sbiosf_msg()
{
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));

	if (!msg) {
		printf("Error allocating sbiosf message\n");
		exit(0);
	}

	bzero(msg, sizeof(*msg));

	msg->EH      = 1;
	msg->AL      = 0;
	msg->tag     = 0;
	msg->opcode  = SBIOSF_IP_READY;
	msg->source  = 0;
	msg->dest    = 0;
	msg->RS      = 0;
	msg->SAI     = 0;
	msg->EH2     = 0;
	msg->EXPHDR  = 0;

	return msg;
}

void free_sbiosf_msg(struct sbiosf_msg *msg)
{
	if (!msg) {
		printf("Error msg is null\n");
		exit(0);
	}

	for (int i = 0; i < msg->num_data; i++)
		free(msg->data[i].data);

	free(msg->data);
	free(msg);
}

void print_sbiosf_msg(struct sbiosf_msg *msg)
{
	int i;
	int j;

	if ((msg->opcode == SBIOSF_REG_READ) ||
	    (msg->opcode == SBIOSF_BLOCK_RD) ||
	    (msg->opcode == SBIOSF_REG_WRITE) ||
	    (msg->opcode == SBIOSF_BLOCK_WR)) {
		printf(" EH:\t\t%1d\n", msg->EH);
		printf(" AL:\t\t%1d\n", msg->AL);
		printf(" bar:\t\t0x%1x\n", msg->bar);
		printf(" tag:\t\t0x%1x\n", msg->tag);

		switch (msg->opcode) {
		case SBIOSF_REG_READ:
			printf(" opcode:\tSBIOSF_REG_READ\n");
			break;
		case SBIOSF_REG_WRITE:
			printf(" opcode:\tSBIOSF_REG_WRITE\n");
			break;
		case SBIOSF_BLOCK_WR:
			printf(" opcode:\tSBIOSF_BLOCK_WR\n");
			break;
		case SBIOSF_BLOCK_RD:
			printf(" opcode:\tSBIOSF_BLOCK_RD\n");
			break;
		}
		printf(" source:\t0x%1x\n", msg->source);
		printf(" dest:\t\t0x%1x\n", msg->dest);
		printf(" RS:\t\t0x%1x\n", msg->RS);
		printf(" SAI:\t\t0x%02x\n", msg->SAI);
		printf(" EH:\t\t0x%1x\n", msg->EH2);
		printf(" EXPHDR:\t0x%1x\n", msg->EXPHDR);

		if ((msg->opcode == SBIOSF_REG_READ) ||
		    (msg->opcode == SBIOSF_REG_WRITE)) {
			printf(" sbe:\t\t0x%x\n", msg->sbe);
			printf(" fbe:\t\t0x%x\n", msg->fbe);
		}

		for (i = 0 ; i < msg->num_data; i++) {
			printf(" [%01d]:\n", i);
			printf("\taddr: 0x%06x\n", msg->data[i].addr);
			printf("\t ndw: %d\n", msg->data[i].ndw);

			if ((msg->opcode == SBIOSF_REG_WRITE) ||
			    (msg->opcode == SBIOSF_BLOCK_WR)) {
				for (j = 0;
				     j < msg->data[i].ndw / 2;
				     j++) {
					printf("    data[%03d]: 0x%lx\n",
					       j, msg->data[i].data[j]);
				}
			}
		}
	} else if ((msg->opcode == SBIOSF_CMP_NODATA) ||
			   (msg->opcode == SBIOSF_CMP_WDATA)) {
		printf(" EH:\t\t%1d\n", msg->EH);
		printf(" AL:\t\t%1d\n", msg->AL);
		printf(" rsp:\t\t0x%1x\n", msg->rsp);
		printf(" tag:\t\t0x%1x\n", msg->tag);
		printf(" opcode:\tSBIOSF_CMP_NODATA\n");

		printf(" source:\t0x%1x\n", msg->source);
		printf(" dest:\t\t0x%1x\n", msg->dest);
		printf(" RS:\t\t0x%1x\n", msg->RS);
		printf(" SAI:\t\t0x%02x\n", msg->SAI);
		printf(" EH:\t\t0x%1x\n", msg->EH2);
		printf(" EXPHDR:\t0x%1x\n", msg->EXPHDR);

		for (i = 0 ; i < msg->num_data; i++) {
			for (j = 0 ; j < msg->data[i].ndw / 2; j++)
				printf("    data[%03d]: 0x%0lx\n",
				       j, msg->data[i].data[j]);
		}
	} else if (msg->opcode == SBIOSF_FUSE_REQ) {
		printf(" EH:\t\t%1d\n", msg->EH);
		printf(" tag:\t\t0x%1x\n", msg->tag);
		printf(" opcode:\tSBIOSF_FUSE_REQ\n");

		printf(" source:\t0x%1x\n", msg->source);
		printf(" dest:\t\t0x%1x\n", msg->dest);
		printf(" RS:\t\t0x%1x\n", msg->RS);
		printf(" SAI:\t\t0x%02x\n", msg->SAI);
		printf(" EH:\t\t0x%1x\n", msg->EH2);
		printf(" EXPHDR:\t0x%1x\n", msg->EXPHDR);

	} else {
		printf("Unsupported message format %d\n", msg->opcode);
		exit(0);
	}
}

unsigned int *pack_sbiosf_msg(struct sbiosf_msg *msg, unsigned int *size)
{
	unsigned int *array;
	int i;
	int j;
	int idx;
	unsigned char field1;       /* DWord 0, bytes 24-31 */
	unsigned char field2;       /* DWord 1, bytes 24-31 */
	unsigned char field3;       /* DWord 2, bytes 0-7   */

	idx = 0;
	switch (msg->opcode) {
	case SBIOSF_REG_READ:
		*size = 4;
		field3 = (msg->fbe & 0xf) | ((msg->sbe & 0xf) << 4);
		break;
	case SBIOSF_REG_WRITE:
		*size = 6;
		field3 = (msg->fbe & 0xf) | ((msg->sbe & 0xf) << 4);
		break;
	case SBIOSF_BLOCK_RD:
		*size = 3 + msg->num_data;
		field3 = msg->data[0].ndw & 0xFF;
		break;
	case SBIOSF_BLOCK_WR:
		*size = 3;
		field3 = msg->data[0].ndw & 0xFF;
		for (i = 0 ; i < msg->num_data; i++)
			*size += 1 + msg->data[i].ndw;
		break;
	case SBIOSF_CMP_NODATA:
	case SBIOSF_FUSE_REQ:
		*size = 2;
		break;
	case SBIOSF_CMP_WDATA:
		*size = 2 + msg->data[0].ndw;
		break;
	default:
		printf("FIXME opcode: %d\n", msg->opcode);
		exit(0);
	}

	switch (msg->opcode) {
	case SBIOSF_REG_WRITE:
	case SBIOSF_REG_READ:
	case SBIOSF_BLOCK_RD:
	case SBIOSF_BLOCK_WR:
		field1 = (msg->tag & 0x7) |
				 ((msg->bar & 0x7) << 3) |
				 ((msg->AL  & 0x1) << 6) |
				 ((msg->EH  & 0x1) << 7);
		field2 = msg->RS & 0x7;
		break;
	case SBIOSF_CMP_NODATA:
	case SBIOSF_CMP_WDATA:
		field1 = (msg->tag & 0x7) |
				 ((msg->rsp & 0x3) << 3) |
				 ((msg->EH  & 0x1) << 7);
		field2 = msg->RS & 0x7;
		break;
	case SBIOSF_FUSE_REQ:
		field1 = (msg->tag & 0x7) |
				 ((msg->EH  & 0x1) << 7);
		field2 = msg->RS & 0xFF;
		break;
	default:
		printf("FIXME opcode: %d\n", msg->opcode);
		exit(0);
	}

	array = malloc(*size * sizeof(*array));
	if (!array) {
		printf("Error allocating array\n");
		exit(1);
	}

	bzero(array, *size * sizeof(*array));

	/*REG_READ, REG_WRITE, BLOCK_WR, BLOCK_RD, CMP_NODATA, CMP_WDATA*/
	array[idx++] = (msg->dest & 0xff) |
			   ((msg->source & 0xff) << 8) |
			   ((msg->opcode & 0xff) << 16) |
			   (field1 << 24);

	/*REG_READ, REG_WRITE, BLOCK_WR, BLOCK_RD, CMP_NODATA, CMP_WDATA:*/
	array[idx++] = (msg->EXPHDR & 0x7f) |
				   ((msg->SAI & 0xFFFF) << 8) |
				   (field2 << 24);

	/*REG_READ, REG_WRITE, BLOCK_RD, BLOCK_WR: */
	if ((msg->opcode == SBIOSF_CMP_NODATA) ||
	    (msg->opcode == SBIOSF_FUSE_REQ))
		return array;

	if (msg->opcode == SBIOSF_CMP_WDATA) {
		for (i = 0 ; i < msg->data[0].ndw / 2; i++) {
			array[idx++] = msg->data[0].data[i] & 0xffffffff;
			array[idx++] = (msg->data[0].data[i] >> 32) &
					    0xffffffff;
		}
		return array;
	}

	/*REG_READ, REG_WRITE, BLOCK_RD, BLOCK_WR: */
	array[idx++] = field3 |
				   ((msg->fid & 0xff) << 8) |
				   ((msg->data[0].addr & 0xffff) << 16);

	/*REG_READ, REG_WRITE, BLOCK_RD, BLOCK_WR:*/
	array[idx++] = (msg->data[0].addr >> 16) & 0xFFF;

	/*REG_WRITE, BLOCK_WR:*/
	for (i = 0 ; i < msg->num_data; i++) {
		if (i != 0) {
			array[idx]    = (msg->data[i].addr & 0xFFFFFFF) << 4;
			array[idx++] |= (msg->data[i].ndw & 0xF);
		}

		if ((msg->opcode != SBIOSF_REG_READ) &&
		    (msg->opcode != SBIOSF_BLOCK_RD)) {
			for (j = 0 ; j < msg->data[i].ndw / 2 ; j++) {
				array[idx++] = msg->data[i].data[j] &
								0xFFFFFFFF;
				array[idx++] = (msg->data[i].data[j] >> 32)
								& 0xFFFFFFFF;
			}
		}
	}

	return array;
}

struct sbiosf_msg *unpack_sbiosf_msg(unsigned int *raw_data,
				     unsigned int size)
{
	/* FIXME: add size checks */
	int i;
	int j;
	struct sbiosf_msg *msg;

	msg = malloc(sizeof(*msg));
	if (!msg) {
		printf("Error allocating memory\n");
		exit(0);
	}
	bzero(msg, sizeof(*msg));

	msg->EH         = (raw_data[0] >> 31) & 0x1;
	msg->opcode     = (raw_data[0] >> 16) & 0xff;
	msg->source     = (raw_data[0] >> 8) & 0xff;
	msg->dest       = raw_data[0] & 0xff;
	msg->tag        = (raw_data[0] >> 24) & 0x7;

	if ((msg->opcode == SBIOSF_REG_READ) ||
	    (msg->opcode == SBIOSF_REG_WRITE) ||
	    (msg->opcode == SBIOSF_BLOCK_RD) ||
	    (msg->opcode == SBIOSF_BLOCK_WR) ||
	    (msg->opcode == SBIOSF_WR_PRIV_CTRL)) {
		msg->AL     = (raw_data[0] >> 30) & 0x1;
		msg->bar    = (raw_data[0] >> 27) & 0x7;

	} else if ((msg->opcode == SBIOSF_CMP_WDATA) ||
			  (msg->opcode == SBIOSF_CMP_NODATA)) {
		msg->rsp    = (raw_data[0] >> 27) & 0x3;
	}

	msg->SAI        = (raw_data[1] >> 8) & 0xffff;
	msg->EH2        = (raw_data[1] >> 7) & 0x1;
	msg->EXPHDR     = raw_data[1] & 0x7f;
	if ((msg->opcode == SBIOSF_FUSE_REQ) ||
	    (msg->opcode == SBIOSF_WR_PRIV_CTRL))
		msg->RS         = (raw_data[1] >> 24) & 0xFF;
	else
		msg->RS         = (raw_data[1] >> 24) & 0x7;

	if ((msg->opcode == SBIOSF_CMP_NODATA) ||
	    (msg->opcode == SBIOSF_FUSE_REQ))
		return msg;

	if ((msg->opcode == SBIOSF_REG_READ)  ||
	    (msg->opcode == SBIOSF_REG_WRITE)) {
		msg->num_data = 1;
		msg->data = malloc(sizeof(*msg->data));
		if (!msg->data) {
			printf("Error allocating memory\n");
			exit(0);
		}
		msg->data[0].addr = (raw_data[2] >> 16) & 0xffff;
		msg->fid          = (raw_data[2] >> 8) & 0xff;
		msg->sbe          = (raw_data[2] >> 4) & 0xf;
		msg->fbe          = (raw_data[2]) & 0xf;
		msg->data[0].addr |= raw_data[3] << 16;
		msg->data[0].ndw  = 2;

		if (msg->opcode == SBIOSF_REG_WRITE) {
			msg->data[0].data = malloc(sizeof(unsigned long));

			if (!msg->data[0].data) {
				printf("Error allocating memory\n");
				exit(0);
			}
			msg->data[0].data[0] = raw_data[4] |
				(((unsigned long)raw_data[5]) << 32);
		}
		return msg;
	} else if (msg->opcode == SBIOSF_BLOCK_RD) {
		msg->num_data = 1 + (size - 4);

		msg->data = malloc(msg->num_data * sizeof(*msg->data));
		if (!msg->data) {
			printf("Error allocating memory\n");
			exit(0);
		}
		msg->data[0].addr = (raw_data[2] >> 16) & 0xffff;
		msg->fid          = (raw_data[2] >> 8) & 0xff;
		msg->data[0].ndw  = raw_data[2] & 0xff;
		msg->data[0].addr |= raw_data[3] << 16;

		for (i = 1 ; i < msg->num_data; i++) {
			msg->data[i].addr = raw_data[3 + i] >> 4;
			msg->data[i].ndw  = raw_data[3 + i] & 0xf;
		}
		return msg;
	} else if (msg->opcode == SBIOSF_BLOCK_WR) {
		msg->num_data = 0;
		for (i = 2 ; i < size ; ) {
			int ndw;

			if (i == 2) {
				ndw = raw_data[i] & 0xff;
				i++;
			} else {
				ndw = raw_data[i] & 0xf;
			}

			msg->num_data++;
			i += 1 + ndw;
		}

		msg->data = malloc(msg->num_data * sizeof(*msg->data));
		if (!msg->data) {
			printf("Error allocating memory\n");
			exit(0);
		}

		msg->data[0].addr = (raw_data[2] >> 16) & 0xffff;
		msg->fid          = (raw_data[2] >> 8) & 0xff;
		msg->data[0].ndw  = raw_data[2] & 0xff;
		msg->data[0].addr |= raw_data[3] << 16;

		int idx = 4;

		for (i = 0 ; i < msg->num_data; i++) {
			if (i != 0) {
				msg->data[i].ndw  = raw_data[idx] & 0xf;
				msg->data[i].addr = raw_data[idx++] >> 4;
			}

			msg->data[i].data = malloc(sizeof(unsigned long) *
						   msg->data[i].ndw / 2);

			if (!msg->data[i].data) {
				printf("Error allocating memory\n");
				exit(0);
			}

			for (j = 0;
			     j < msg->data[i].ndw / 2;
			     j++, idx += 2)
				msg->data[i].data[j] = raw_data[idx] |
				((unsigned long)raw_data[idx + 1] << 32);
		}
		return msg;
	} else if (msg->opcode == SBIOSF_CMP_WDATA) {
		msg->num_data = 1;
		msg->data = malloc(msg->num_data * sizeof(*msg->data));

		msg->data[0].ndw = size - 2;
		msg->data[0].data = malloc(((size - 2) / 2) *
					sizeof(*msg->data[0].data));

		if (!msg->data[0].data) {
			printf("Error allocating memory\n");
			exit(0);
		}

		for (i = 0 ; i < msg->data[0].ndw / 2; i++) {
			msg->data[0].data[i] = raw_data[2 + (i * 2)] |
			    ((unsigned long)raw_data[2 + (i * 2) + 1] << 32);
		}
		return msg;

	} else {
		printf("Error unknown message\n");
	}
	return NULL;
}

void print_packed_array(unsigned int *array, unsigned int size)
{
	int i;
	int j;

	for (i = 0, j = 0 ; i < size; j = (j + 1) % 4) {
		if (j == 0)
			printf("[%04d]: ", i * 4);

		printf("%02x ", (array[i] >> (24 - (j * 8))) & 0xff);

		if (j == 3) {
			i++;
			printf("\n");
		}
	}
}

#if 0

void main(void)
{
	struct sbiosf_msg *msg;
	int addr_base = 0x12340;
	unsigned int *array;
	int dwords;

	{
		printf("\nOne read of 1 64b values\n");
		msg = create_reg_read_sbiosf_msg(1);
		add_rd_wr_op_to_msg(msg, 0, addr_base, 2, NULL);
		printf("Message:\n");
		print_sbiosf_msg(msg);
		array = pack_sbiosf_msg(msg, &dwords);
		printf("Packed Message:\n");
		print_packed_array(array, dwords);
		free_sbiosf_msg(msg);
		msg = unpack_sbiosf_msg(array, dwords);
		free(array);
		printf("Unpacked Message:\n");
		print_sbiosf_msg(msg);
	}

	{
		printf("\nFive reads of 1 64b value each\n");
		msg = create_reg_read_sbiosf_msg(5);
		for (int i  = 0 ; i < 5; i++)
			add_rd_wr_op_to_msg(msg, i, addr_base + i * 8, 2, NULL);

		printf("Message:\n");
		print_sbiosf_msg(msg);
		array = pack_sbiosf_msg(msg, &dwords);
		free_sbiosf_msg(msg);
		printf("Packed Message: %d dwords\n", dwords);
		print_packed_array(array, dwords);
		msg = unpack_sbiosf_msg(array, dwords);
		free(array);
		printf("Unpacked Message:\n");
		print_sbiosf_msg(msg);
	}

	{
		printf("\nOne write of 1 64b values\n");
		msg = create_reg_write_sbiosf_msg(1);
		unsigned long *d = malloc(sizeof(*d));

		d[0] = 0x1111111111111111;
		add_rd_wr_op_to_msg(msg, 0, addr_base, 2, d);
		printf("Message:\n");
		print_sbiosf_msg(msg);

		array = pack_sbiosf_msg(msg, &dwords);
		free_sbiosf_msg(msg);
		printf("Packed Message:\n");
		print_packed_array(array, dwords);

		msg = unpack_sbiosf_msg(array, dwords);
		free(array);

		printf("Unpacked Message:\n");
		print_sbiosf_msg(msg);
	}

	{
		printf("\nEight writes of 1 64b value each\n");
		msg = create_reg_write_sbiosf_msg(8);
		for (int i  = 0 ; i < 8; i++) {
			unsigned long *d = malloc(sizeof(*d));

			d[0] = 0x0123456789abcdef + (unsigned long)i;
			add_rd_wr_op_to_msg(msg, i, addr_base + i * 8, 2, d);
		}
		printf("Message:\n");
		print_sbiosf_msg(msg);
		array = pack_sbiosf_msg(msg, &dwords);
		free_sbiosf_msg(msg);
		printf("Packed Message: %d dwords\n", dwords);
		print_packed_array(array, dwords);

		msg = unpack_sbiosf_msg(array, dwords);
		free(array);
		printf("Unpacked Message:\n");
		print_sbiosf_msg(msg);
	}

	{
		printf("\nCompletion without data\n");
		msg = create_cmp_sbiosf_msg(0, NULL);
		printf("Message:\n");
		print_sbiosf_msg(msg);

		array = pack_sbiosf_msg(msg, &dwords);
		free_sbiosf_msg(msg);

		printf("Packed Message: %d dwords\n", dwords);
		print_packed_array(array, dwords);

		msg = unpack_sbiosf_msg(array, dwords);
		free(array);
		printf("Unpacked Message:\n");
		print_sbiosf_msg(msg);
	}

	{
		printf("\nCompletion with 8 64b data\n");
		unsigned long *d = malloc(8 * sizeof(*d));

		for (int i = 0 ; i < 8; i++)
			d[i] = (unsigned long)i;

		msg = create_cmp_sbiosf_msg(8, d);
		printf("Message:\n");
		print_sbiosf_msg(msg);

		array = pack_sbiosf_msg(msg, &dwords);
		free_sbiosf_msg(msg);
		printf("Packed Message: %d dwords\n", dwords);
		print_packed_array(array, dwords);

		msg = unpack_sbiosf_msg(array, dwords);
		free(array);
		printf("Unpacked Message:\n");
		print_sbiosf_msg(msg);
	}
	{
		printf("\nFuse Request\n");
		msg = create_fuse_req_sbiosf_msg();
		print_sbiosf_msg(msg);
		array = pack_sbiosf_msg(msg, &dwords);
		print_packed_array(array, dwords);
		free(array);
		free_sbiosf_msg(msg);
	}
	{
		printf("\nIP Ready\n");
		msg = create_ip_ready_sbiosf_msg();
		print_sbiosf_msg(msg);
		array = pack_sbiosf_msg(msg, &dwords);
		print_packed_array(array, dwords);
		free(array);
		free_sbiosf_msg(msg);
	}
	{
		printf("\nWrPrivCtrl message\n");
		msg = create_wr_priv_ctrl_sbiosf_msg(addr_base,
						     0xaaaa5555aaaa5555);
		print_sbiosf_msg(msg);
		array = pack_sbiosf_msg(msg, &dwords);
		print_packed_array(array, dwords);
		free(array);
		free_sbiosf_msg(msg);
	}
}
#endif
