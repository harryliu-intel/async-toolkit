/*****************************************************************************
 * @file	client_regwrite.c
 * @brief	Simple reg write/read application for get_next_reg_name library
 *
 * INTEL CONFIDENTIAL
 * Copyright 2018 Intel Corporation.  All Rights Reserved.
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <termios.h>
#include <ctype.h>
#include <stdarg.h>
#include <signal.h>

#include "mby_tcp_client_library.h"

#define SERVER_PATH "../../../main/m3/"
#define SERVER_FILE SERVER_PATH "model_server/AMD64_LINUX/models.packetServer"

#define NOT_USED(a) ((void)(a))
#define NEXT_IDX(idx) (((idx) + 1) % MAX_CLI_HIST)
#define PREV_IDX(idx) (((idx) - 1 + MAX_CLI_HIST) % MAX_CLI_HIST)

#define MAX_BUF		1024
#define BACKSPACE		'\b'
#define BELL			'\a'

enum error {
	ERR_FAIL = WM_OK + 100,
	ERR_BUFFER_FULL
};

struct connection {
	int fd;

	char buf[MAX_BUF + 1];
	int pcur;
	int blen;

	int last_cmd_status;

	int debug;
};

struct command {
	char* name;
        int (*handler)(struct connection *, int, char**);
	char* args;
	char* help;
};


struct reg_struct {
	char* name;
	uint32_t addr;
};

static struct connection *sig_handler_con;
static char *prompt = "<0>%";

static int help_handler(struct connection *con, int argc, char **argv);
static int fd_print(struct connection *con, const char *format,  ...);
struct command * get_cmd(const char* name);
struct reg_struct * get_reg(const char* name);
char* get_next_cmd_name(void** it);
char* get_next_reg_name(void** it);
int reg_write(struct connection *con, int argc, char **argv);
int reg_read(struct connection *con, int argc, char **argv);
int reg_write_read(struct connection *con, int argc, char **argv);
int pkt_send(struct connection *con, int argc, char **argv);
int pkt_receive(struct connection *con, int argc, char **argv);

struct command commands[] = {
	{"write", reg_write, "[reg] [val]", "Write to register"},
	{"read", reg_read, "[reg]", "\tRead from register"},
	{"test", reg_write_read, "[reg] [val]", "Write, read and compare values"},
	{"send", pkt_send, "[port] [payload]", "\tSend a packet"},
	{"receive", pkt_receive, "", "\tReceive a packet"},
	{"help", help_handler, "", "\t\tPrints this help"},
	{"quit", NULL, "", "\t\tExits the program"},
	{"exit", NULL, "", "\t\tExits the program"},
	{"", NULL, NULL, NULL},
};

struct reg_struct registers [] = {
	{"example_a_reg_1", 0x0011000 },
	{"example_a_reg_2", 0x0012000 },
	{"example_a_reg_3", 0x0013000 },
	{"example_b_reg_1", 0x0021000 },
	{"example_b_reg_2", 0x0022000 },
	{"example_b_reg_3", 0x0023000 },
	{"example_c_reg_1", 0x0031000 },
	{"example_c_reg_2", 0x0032000 },
	{"example_d_reg_1", 0x0041000 },
	{"", 0},
};

static void print_help()
{
	printf("sample_client - C-DPI sample client application\n\n");
	printf("Start or connect to the model server and perform some basic\n");
	printf("tests: reg access.\n\n");
	printf("Options:\n");
	printf(" -s [scala/m3] 	Start the specified server. By default only connect to running process\n");
	printf(" -m <path>  	Path and name of existing models.packetServer used when connecting\n");
	printf(" -d         	Print debug messages\n");
	printf(" -h         	Print this help message and exit\n");
}

static int test_regs(void)
{
	uint64_t val = 0x1234567890abcdef;
	uint32_t addr = 0x0010000;
	uint64_t orig_val = 0x0;
	uint64_t out_val = 0x0;
	int err;

	printf("Register access test...\n");

	//get original value
	err = wm_reg_read(addr, &orig_val);
	if (err) {
		printf("Error reading register: %d\n", err);
		return WM_ERR_RUNTIME;
	}

	/* In HLP this is BSM_SCRATCH_0[0] */
	printf("Write: addr=0x%x val=0x%lx ", addr, val);
	err = wm_reg_write(addr, val);
	if (err) {
		printf("Error writing register: %d\n", err);
		return WM_ERR_RUNTIME;
	}

	printf("=> Read: addr=0x%x ", addr);
	err = wm_reg_read(addr, &out_val);
	if (err) {
		printf("Error reading register: %d\n", err);
		return WM_ERR_RUNTIME;
	}

	printf("val=0x%lx ", out_val);

	if (out_val != val)
		printf("Unexpected value: 0x%lx\n", val);
	else
		printf("=> OK\n");

	//restore original value
	printf("Restoring original value (0x%lx)...\n\n", orig_val);
	err = wm_reg_write(addr, orig_val);
	if (err) {
		printf("Error writing register: %d\n", err);
		return WM_ERR_RUNTIME;
	}

	return WM_OK;
}

static int parse_uint32(char *string, uint32_t *result)
{
	char *endptr = NULL;

	if (!string) {
		*result = 0;
		return -1;
	}

	*result = 0;

	if (string[1] == 'x')
		*result = strtoul(string + 2, &endptr, 16);
	else
		*result = strtoul(string, &endptr, 10);

	return (string == endptr) ? -2 : 0;

}

static int parse_uint64(char *string, uint64_t *result)
{
	char *endptr = NULL;

	if (!string) {
		*result = 0;
		return -1;
	}

	*result = 0;

	if (string[1] == 'x')
		*result = strtoull(string + 2, &endptr, 16);
	else
		*result = strtoull(string, &endptr, 10);

	return (string == endptr) ? -2 : 0;

}

static int write_byte(int fd, char c)
{
	int rv;

	if (c == '\n')
		write_byte(fd, '\r');
	rv = write(fd, &c, 1);
	if (rv < 0)
		return ERR_FAIL;
	if (rv != 1)
		return ERR_BUFFER_FULL;

	return WM_OK;
}

static int fd_print(struct connection *con, const char *format,  ...)
{
	char buf[MAX_BUF];
	unsigned int len;
	unsigned int i;
	va_list ap;

	if(!con)
		return WM_ERR_INVALID_ARG;

	va_start(ap, format);
	len = vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);
	if (len > sizeof(buf))
		len = sizeof(buf);

#if 0
	/* User-supplied print function */
	if (con->print)
		return con->print(buf);
#endif

	/* If no user-supplied print function, write to connection fd */
	for (i = 0; i < len; i++) {
		if (buf[i] == '\r')
			continue;
		write_byte(con->fd, buf[i]);
	}

	return WM_OK;
}

static inline void move_cursor_back(struct connection *con)
{
	int cnt;

	/* move the cursor back to the correct position */
	for (cnt = con->pcur; cnt < con->blen; cnt++)
		write_byte(con->fd, BACKSPACE);
}

static int add_chars_to_buf(struct connection *con, const char *c, int len)
{
	int cnt;

	if ((con->blen + len) >= MAX_BUF) {
		fd_print(con, "Not enough space to store command\n");
		return ERR_FAIL;
	}
	/* Move the characters after pcur */
	for (cnt = con->blen; cnt >= con->pcur; cnt--)
		con->buf[cnt + len] = con->buf[cnt];

	/* Copy to free location */
	for (cnt = 0; cnt < len; cnt++)
		con->buf[con->pcur + cnt] = c[cnt];
	con->blen += len;
	con->pcur += len;

	/* write over the char after the cursor */
	for (cnt = con->pcur - len; cnt < con->blen; cnt++)
		write_byte(con->fd, con->buf[cnt]);

	move_cursor_back(con);

	return WM_OK;
}

static int add_char_to_buf(struct connection *con, char c)
{
	return add_chars_to_buf(con, &c, 1);
}

/* Handler for SIGINT (i.e. CTRL-C) */
static void ctrlc_handler(int dummy)
{
	NOT_USED(dummy);

	if (sig_handler_con) {
		sig_handler_con->blen = 0;
		sig_handler_con->pcur = 0;
		sig_handler_con->buf[0] = '\0';
		fd_print(sig_handler_con, "\n%s ", prompt);
	}
}

static void home_key(struct connection *con)
{
	int cnt;

	if (con->pcur == 0)
		write_byte(con->fd, BELL);

	for (cnt = 0; cnt < con->pcur; cnt++)
		write_byte(con->fd, BACKSPACE);

	con->pcur = 0;
}

static void end_key(struct connection *con)
{
	if (con->pcur == con->blen) {
		write_byte(con->fd, BELL);
	} else {
		while (con->pcur < con->blen) {
			write_byte(con->fd,
				   con->buf[con->pcur]);
			con->pcur++;
		}
	}
}

static void arrow_key(struct connection *con, char c, char *esc_seq)
{
	if (c == 'D') {
		*esc_seq = 0;
		/* Left Arrow ^[[D or ^[OD */
		if (con->pcur) {
			con->pcur--;
			write_byte(con->fd, BACKSPACE);
		} else {
			write_byte(con->fd, BELL);
		}
	} else if (c == 'C') {
		*esc_seq = 0;
		/* Right Arrow ^[[C or ^[OC */
		if (con->pcur < con->blen) {
			write_byte(con->fd,
				   con->buf[con->pcur]);
			con->pcur++;
		} else {
			write_byte(con->fd, BELL);
		}
	}

	/* TODO: UP/DOWN arrows support */
	*esc_seq = 0;
}

static int help_handler(struct connection *con, int argc, char **argv)
{
	struct command *cmd = &commands[0];
	int i;

	if(!argc) {
		fd_print(con, "CTRL-A\tskip to the beginning of line\n");
		fd_print(con, "CTRL-E\tskip to the end of line\n");
		fd_print(con, "CTRL-C\tcancel the command\n");
		fd_print(con, "CTRL-D\texit\n");
		fd_print(con, "\n");

		while (strcmp(cmd->name, "")) {
			fd_print(con, "%s %s\t%s\n",cmd->name, cmd->args, cmd->help);
			++cmd;
		}

		fd_print(con, "\n");
		return WM_OK;
	}

	for(i=0; i<argc; ++i) {
		cmd = get_cmd(argv[i]);
		fd_print(con, "%s %s\t%s\n",cmd->name, cmd->args, cmd->help);
	}

	return WM_OK;
}


static int print_cmd_help(struct connection *con, char* command)
{
	struct command *cmd;
	int len;

	len = con->blen;
	con->buf[len] = '\0';

	if(command && strcmp(command, "")) {
		cmd = get_cmd(command);
		if (cmd)
			fd_print(con, "%s\n", cmd->args);
	} else {
		fd_print(con, "\n");
		help_handler(con, 0, NULL);
	}

	con->buf[con->blen] = '\0';
	fd_print(con, "\n%s %s", prompt, con->buf);

	move_cursor_back(con);
	return WM_OK;
}

static int print_reg_help(struct connection *con, char* command)
{
	struct command *cmd;
	int len;

	len = con->blen;
	con->buf[len] = '\0';

	if(command && strcmp(command, "")) {
		cmd = get_cmd(command);
		fd_print(con, "%s\n", cmd->args);
	} else {
		fd_print(con, "\n");
		help_handler(con, 0, NULL);
	}

	con->buf[con->blen] = '\0';
	fd_print(con, "\n%s %s", prompt, con->buf);

	move_cursor_back(con);
	return WM_OK;
}

static int parse_line(char *line, int len, char *command, int *argc, char **argv)
{
	const char *delim = " \n";
	char temp_line[MAX_BUF + 1];
	char *token;
	*argc = 0;

	if(!len)
		len = MAX_BUF;

	strncpy(temp_line, line, len);
	temp_line[len] = '\0';

	strcpy(command, "");
	token = strtok(temp_line, delim);
	if(!token)
		return WM_ERR_INVALID_ARG;
	strcpy(command, token);


	token = strtok(NULL, delim);
	while (token) {
		strcpy(argv[(*argc)++], token);
		token = strtok(NULL, delim);
	}

	return WM_OK;

}

static void cmd_line_to_str(char *command, int argc, char **argv, char* output)
{
	sprintf(output, "%s", command);
	for (int i = 0; i < argc; ++i)
		sprintf(output, " %s", argv[i]);
}

// return value:
// 0	- no proposals, partial match put into proposals[0]
// 1	- one proposal only
// n>1	- list of proposals of length n
static int match_name(const char* name, char **proposals, char* (*get_next_name)(void** it))
{
	char temp_name[MAX_BUF + 1];
	int init_len = strlen(name);
	int len = init_len;
	int first_cnt = 0;
	char* first_name = NULL;
	char* cur_name;
	void* it = NULL;
	int cnt = 0;

	if(!strcmp(name, ""))
			return -1;

        //count matches of given name
	cur_name = get_next_name(&it);
	while (strcmp(cur_name, "")) {
		if (cur_name == strstr(cur_name, name)) {
			first_name = cur_name;
			strcpy(proposals[cnt++], cur_name);
		}
		cur_name = get_next_name(&it);
	}

	if (!cnt)
		return -1;

	//only one match
	if (cnt == 1) {
		return cnt;
	}

	first_cnt = cnt;
	strcpy(temp_name, name);

	//add one letter per iteration until the matches number is the same
	do {
		temp_name[len] = first_name[len];
		temp_name[++len] = '\0';

		cnt = 0;
		it = NULL;
		cur_name = get_next_name(&it);
		while (strcmp(cur_name, "")) {
			if (cur_name == strstr(cur_name, temp_name)) {
				++cnt;
			}
			cur_name = get_next_name(&it);
		}

	} while (cnt >= first_cnt);

	temp_name[--len] = '\0';

	//some letters added - return a partial match
	if (len > init_len) {
		strcpy(proposals[0], temp_name);
		return 0;
	}

	//just return the list of proposals
	return first_cnt;
}

static void complete_command(struct connection *con)
{
	int (*print_help_function)(struct connection *, char*);
	char* (*get_next_name_function)(void**);
	char* proposals[sizeof(registers)];
	char command[MAX_BUF + 1];
	char name[MAX_BUF + 1];
	void* it = NULL;
	char* cur_name;
	char* argv[32];
	int argc = 0;
	int matches;
	int len;
	int cnt;

	for(cnt = 0; cnt<32; ++cnt) {
		argv[cnt] = (char*)malloc((MAX_BUF + 1) * sizeof(char));
		proposals[cnt] = (char*)malloc((MAX_BUF + 1) * sizeof(char));
	}

	parse_line(con->buf, con->pcur, command, &argc, argv);

	if (con->debug) {
		fd_print(con, "\ncommand: \"%s\" ", command);
		for (int i = 0; i < argc; ++i)
			fd_print(con, "arg[%d]: \"%s\" ", i, argv[i]);
		fd_print(con, "\n");
	}

	if(argc) {
		get_next_name_function = get_next_reg_name;
		print_help_function = print_reg_help;
		strcpy(name, argv[argc - 1]);
	}
	else {
		get_next_name_function = get_next_cmd_name;
		print_help_function = print_cmd_help;
		strcpy(name, command);
	}

	matches = match_name(name, proposals, get_next_name_function);
	if (con->debug)
		fd_print(con, "\nmatches: %d ", matches);

	//return only for registers
	if(argc && matches < 0)
		return;

	//only one match
	if(matches >=0 && matches <= 1){
		if(matches && con->buf[con->pcur-1] == ' ') {
			//name complete, print help
			print_help_function(con, name);
		} else {
			//complete
			len = strlen(proposals[0]) - strlen(name);
			add_chars_to_buf(con, proposals[0] + strlen(name), len);
			if (matches)
				add_char_to_buf(con, ' ');
		}
	} else if (matches > 0) {
		//just print possible matches
		fd_print(con, "\n");
		for(cnt = 0; cnt < matches; ++cnt)
			fd_print(con, " %s", proposals[cnt]);

		fd_print(con, "\n");
		con->buf[con->blen] = '\0';
		fd_print(con, "\n%s %s", prompt, con->buf);
		move_cursor_back(con);
	}

	//print all only for commands
	if(!argc && matches < 0 && !strcmp(name, "")) {
		//no command, print all commands
		fd_print(con, "\n");

		cur_name = get_next_cmd_name(&it);
		while (strcmp(cur_name, "")) {
			fd_print(con, " %s", cur_name);
			cur_name = get_next_cmd_name(&it);
		}

		fd_print(con, "\n");
		con->buf[con->blen] = '\0';
		fd_print(con, "\n%s %s", prompt, con->buf);
		move_cursor_back(con);
	}

	for(cnt = 0; cnt<32; ++cnt) {
		free(proposals[cnt]);
		free(argv[cnt]);
	}
}

struct command* get_cmd(const char* name) {
	struct command *cmd = commands;

	while (strcmp(cmd->name, "")) {
		if (!strcmp(cmd->name, name))
				return cmd;
		++cmd;
	}

	return NULL;
}

struct reg_struct* get_reg(const char* name) {
	struct reg_struct *reg = registers;

	while (strcmp(reg->name, "")) {
		if (!strcmp(reg->name, name))
				return reg;
		++reg;
	}

	return NULL;
}

char* get_next_reg_name(void** it) {
	struct reg_struct *reg = (struct reg_struct*) *it;
	if (reg)
		++reg;
	else
		reg = registers;
	*it = (void*) reg;
	return reg->name;
}

char* get_next_cmd_name(void** it) {
	struct command *cmd = (struct command*) *it;
	if (cmd)
		++cmd;
	else
		cmd = commands;
	*it = (void*) cmd;
	return cmd->name;
}


int reg_write(struct connection *con, int argc, char **argv)
{
	struct reg_struct* reg;
	uint32_t addr;
	uint64_t val;
	int err;

	if(argc < 2)
		return WM_ERR_INVALID_ARG;

	if(isdigit(argv[0][0])) {
		err = parse_uint32(argv[0], &addr);
		if(err)
			return WM_ERR_INVALID_ARG;
	}
	else {
		reg = get_reg(argv[0]);
		if (reg)
			addr = reg->addr;
		else return WM_ERR_INVALID_ARG;
	}

	err = parse_uint64(argv[1], &val);
	if(err)
		return WM_ERR_INVALID_ARG;

	fd_print(con, "Write: addr=0x%x val=0x%lx\n", addr, val);
	err = wm_reg_write(addr, val);
	if (err) {
		fd_print(con, "Error writing register: %d\n", err);
		return WM_ERR_RUNTIME;
	}

	return WM_OK;
}

int reg_read_int(struct connection *con, int argc, char **argv, uint64_t *ret_val)
{
	struct reg_struct* reg;
	uint32_t addr;
	uint64_t val;
	int err;

	if(argc < 1)
		return WM_ERR_INVALID_ARG;

	if(isdigit(argv[0][0])) {
		err = parse_uint32(argv[0], &addr);
		if(err)
			return WM_ERR_INVALID_ARG;
	}
	else {
		reg = get_reg(argv[0]);
		if (reg)
			addr = reg->addr;
		else return WM_ERR_INVALID_ARG;
	}

	fd_print(con, "Read: addr=0x%x\n", addr);
	err = wm_reg_read(addr, &val);
	if (err) {
		fd_print(con, "Error reading register: %d\n", err);
		return WM_ERR_RUNTIME;
	}
	fd_print(con, "0x%x -> 0x%lx\n", addr, val);

	if(ret_val)
		*ret_val = val;

	return WM_OK;
}


int reg_read(struct connection *con, int argc, char **argv)
{
	return reg_read_int(con, argc, argv, NULL);

}

int reg_write_read(struct connection *con, int argc, char **argv)
{
	uint64_t ret_val;
	int err = WM_OK;
	uint64_t val;

	if(argc < 2)
		return WM_ERR_INVALID_ARG;

	errno = 0;

	err = parse_uint64(argv[1], &val);
	if(err)
		return WM_ERR_INVALID_ARG;

	reg_write(con, argc, argv);
	reg_read_int(con, argc, argv, &ret_val);

	if (ret_val != val)
		fd_print(con, "Unexpected value: 0x%lx (should be: 0x%lx)\n", ret_val, val);
	else
		fd_print(con, "OK\n");

	return err;
}

int pkt_send(struct connection *con, int argc, char **argv)
{
	struct wm_pkt tx_pkt;
	uint32_t val = 0;
	unsigned int i;
	int err;

	if(argc < 2)
		return WM_ERR_INVALID_ARG;

	err = parse_uint32(argv[0], &val);
	if(err)
		return WM_ERR_INVALID_ARG;

	tx_pkt.port = (uint16_t) val;
	tx_pkt.len = argc - 1;

	if (tx_pkt.len > MAX_PKT_LEN)
		return ERR_BUFFER_FULL;

	for(i = 0; i < tx_pkt.len; ++i) {
		err = parse_uint32(argv[i+1], &val);
		if(err)
			return WM_ERR_INVALID_ARG;

		tx_pkt.data[i] = (uint8_t) val;
		if (con->debug)
			fd_print(con, "loaded %02x\n", tx_pkt.data[i]);
	}


	fd_print(con, "Sent %u bytes to port %u\n", tx_pkt.len, tx_pkt.port);

	err = wm_pkt_push(&tx_pkt);
	if (err) {
		fd_print(con, "Error sending traffic: %d\n", err);
		return err;
	}

	return WM_OK;
}

int pkt_receive(struct connection *con, int argc, char **argv)
{
	struct wm_pkt rx_pkt;
	unsigned int i;
	int err;

	NOT_USED(argc);
	NOT_USED(argv);

	err = wm_pkt_get(&rx_pkt);
	if (err && err != WM_NO_DATA) {
		fd_print(con, "Error receiving traffic: %d\n", err);
		return err;
	} else if (err == WM_NO_DATA) {
		fd_print(con, "EOT received\n");
		return WM_OK;
	}

	fd_print(con, "Received %u bytes on port %u\n", rx_pkt.len, rx_pkt.port);

	if (rx_pkt.len > MAX_PKT_LEN)
		return ERR_BUFFER_FULL;

	for(i = 0; i < rx_pkt.len; ++i)
		fd_print(con, " %2X", rx_pkt.data[i]);

	fd_print(con, "\n");

	return WM_OK;
}

int client_process(struct connection *con)
{
	struct command* cmd = NULL;
	struct termios old_options;
	char command[MAX_BUF + 1];
	bool show_prompt = true;
	struct termios options;
	struct sigaction act;
	char* argv[32];
	char esc_seq;
	int argc = 0;
	int err;
	int len;
	int cnt;
	char c;

	for(cnt = 0; cnt<32; ++cnt)
		argv[cnt] = (char*)malloc((MAX_BUF + 1) * sizeof(char));

	con->fd = STDOUT_FILENO;
	tcgetattr(con->fd, &old_options);
	options = old_options;
	options.c_lflag &= ~(ICANON | ECHO);
	options.c_cc[VMIN] = 1;
	options.c_cc[VTIME] = 0;
	tcsetattr(con->fd, TCSANOW, &options);


	help_handler(con, 0, NULL);

	sig_handler_con = con;
	act.sa_handler = ctrlc_handler;
	sigaction(SIGINT, &act, NULL);

	esc_seq = 0;
	while (true) {
		if (show_prompt) {
			fd_print(con, "%s ", prompt);
			show_prompt = false;
		}

		c = getchar();

		if (c == 0x1b) {
			/* ESC sequence */
			esc_seq = 0x1b;
		} else if (esc_seq == 0x1b && c == '[') {
			/* Wait for CTRL sequence (for arrows/delete) */
			esc_seq = '[';
		} else if (esc_seq == 0x1b && c == 'O') {
			/* Wait for CTRL sequence (for home/end) */
			esc_seq = 'O';
		} else if (esc_seq == 'O') {
			esc_seq = 0;
			if (c == 'H') {
				/* HOME ^[OH */
				home_key(con);
			} else if (c == 'F') {
				/* END ^[OF */
				end_key(con);
			} else if (c >= 'A' && c <= 'D') {
				arrow_key(con, c, &esc_seq);
			}
		} else if (esc_seq == '3' && c == '~') {
			/* delete ^[[3~ */
			if (con->blen && con->pcur != con->blen) {
				for (cnt = con->pcur; cnt < con->blen; cnt++) {
					con->buf[cnt] = con->buf[cnt + 1];
					write_byte(con->fd, con->buf[cnt]);
				}
				con->blen--;

				write_byte(con->fd, ' ');
				for (cnt = con->pcur; cnt <= con->blen; cnt++)
					write_byte(con->fd, BACKSPACE);
			}
		} else if (esc_seq == '1' && c == '~') {
			/* HOME ^[[1~ */
			home_key(con);
		} else if (esc_seq == '4' && c == '~') {
			/* END ^[[4~ */
			end_key(con);
		} else if (esc_seq == '[') {
			if (c >= 'A' && c <= 'D') {
				arrow_key(con, c, &esc_seq);
			} else if (c >= '0' && c <= '9') {
				/* ^[[n where 0 <= n <= 9  */
				esc_seq = c;
			}
		}
		else if (c == '\t') {
			complete_command(con);
		} else if (c == '?') {
			parse_line(con->buf, con->pcur, command, &argc, argv);
			print_cmd_help(con, command);
		} else if (isprint(c)) {
			add_char_to_buf(con, c);
		} else if (c == 0x7f || c == 0x08) {
			/* BackSpace */
			if (con->blen && con->pcur) {
				con->blen--;
				con->pcur--;
				write_byte(con->fd, BACKSPACE);
				for (cnt = con->pcur; cnt < con->blen; cnt++) {
					con->buf[cnt] = con->buf[cnt + 1];
					write_byte(con->fd, con->buf[cnt]);
				}
				/* Terminate new buf len */
				con->buf[con->blen] = '\0';
				write_byte(con->fd, ' ');
				for (cnt = con->pcur; cnt <= con->blen; cnt++)
					write_byte(con->fd, BACKSPACE);
			}
		} else if (c == 0x1) {
			/* CTRL-A */
			home_key(con);
		} else if (c == 0x3) {
			/* CTRL-C in Telnet */
			ctrlc_handler(0);
		} else if (c == 0x4) {
			/* CTRL-D */
			fd_print(con, "\n");
			break;
		} else if (c == 0x5) {
			/* CTRL-E */
			end_key(con);
		}

		len = 0;

		if (c == ';' || c == '\n' || c == '\r') {
			write_byte(con->fd, '\n');
			len = con->blen;

			con->blen = 0;
			con->pcur = 0;
			con->buf[len] = '\0';

			show_prompt = true;
		}

		if (len > 0) {
			err = parse_line(con->buf, con->blen, command, &argc, argv);

			if (err || !strcmp(command, ""))
				continue;

			if (con->debug) {
				fd_print(con, "command: \"%s\" ", command);
				for (int i = 0; i < argc; ++i)
					fd_print(con, "arg[%d]: \"%s\" ", i, argv[i]);
				fd_print(con, "\n");
			}

			if (!strcmp(command, "exit") || !strcmp(command, "quit"))
				break;

			cmd = get_cmd(command);

			if(cmd && cmd->handler) {
				err = cmd->handler(con, argc, argv);
				if(err)
					fd_print(con, "error %d\n", err);
			} else {
				fd_print(con, "Invalid command: %s\n", command);
			}
		}
	}

	tcsetattr(con->fd, TCSANOW, &old_options);

	for(cnt = 0; cnt<32; ++cnt)
		free(argv[cnt]);

	return WM_OK;
}

int main(int argc, char **argv)
{
	char *model_server_file = SERVER_FILE;
	struct connection console_con = {0};
	char *server_type = NULL;
	int err;
	int c;

	/********** Process command line arguments ***********/
	while ((c = getopt(argc, argv, "s:m:dh")) != -1) {
		switch (c) {
		case 's':
			server_type = optarg;
			break;
		case 'm':
			model_server_file = optarg;
			break;
		case 'd':
			console_con.debug = true;
			break;
		case 'h':
			print_help();
			return 0;
		default:
			printf("Invalid command line argument: %c\n\n", c);
			print_help();
			return 1;
		}
	}

	/********** Start or connect to the server ***********/
	if (server_type)
		err = wm_server_start(server_type);
	else
		err = wm_connect(model_server_file);

	if (err) {
		printf("Error while connecting or starting to the WM\n");
		return 1;
	}
	else {
		printf("Started/connected to WM\n");
	}

	/********** Test write/read register operations ***********/
	err = test_regs();
	if (err)
		goto CLEANUP;


	/***************** Run the client process *****************/
	err = client_process(&console_con);
	if (err)
		goto CLEANUP;

CLEANUP:
	/********** Disconnect (or stop) from the server ***********/
	err = server_type ? wm_server_stop() : wm_disconnect();
	if (err)
		printf("Error while disconnecting to the WM: %d\n", err);
	else
		printf("Disconnected from model_server\n");

	return err;
}
