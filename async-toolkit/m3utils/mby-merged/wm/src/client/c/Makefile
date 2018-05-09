CC = gcc
CFLAGS = -fPIC -Wall -Wextra -O2 -g
LDFLAGS = -shared
RM = rm -f
TARGET_LIB = libmbay_dpi_client.so
SAMPLE_EXE = client_sample

SRCS = mbay_dpi_client.c
OBJS = $(SRCS:.c=.o)

.PHONY: all
all: ${TARGET_LIB}

$(TARGET_LIB): $(OBJS)
	$(CC) ${LDFLAGS} -o $@ $^

$(SRCS:.c=.d):%.d:%.c
	$(CC) $(CFLAGS) -MM $< >$@

include $(SRCS:.c=.d)

.PHONY: sample
sample: all
	$(CC) $(CFLAGS) -I. -L. -lmbay_dpi_client ${SAMPLE_EXE}.c -o ${SAMPLE_EXE}

.PHONY: clean
clean:
	-${RM} ${TARGET_LIB} ${SAMPLE_EXE} ${OBJS} $(SRCS:.c=.d)
