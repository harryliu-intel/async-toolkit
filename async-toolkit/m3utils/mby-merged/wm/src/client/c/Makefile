CC = gcc
CFLAGS = -fPIC -Wall -Wextra -O2 -g -Wno-unused-function
LDFLAGS = -shared
RM = rm -f
TARGET_DIR = target
TARGET_LIB = $(TARGET_DIR)/libmbay_dpi_client.so
SAMPLE_EXE = $(TARGET_DIR)/client_sample

SRCS = mbay_dpi_client.c
OBJS = $(SRCS:.c=.o)

.PHONY: all
all: ${TARGET_LIB} ${SAMPLE_EXE}

$(TARGET_LIB): $(OBJS)
	mkdir -p $(dir $@)
	$(CC) ${LDFLAGS} -o $@ $^

$(SRCS:.c=.d):%.d:%.c
	$(CC) $(CFLAGS) -MM $< >$@

include $(SRCS:.c=.d)

.PHONY: sample
$(SAMPLE_EXE): ${TARGET_LIB} client_sample.c
	$(CC) $(CFLAGS) -I. -L $(TARGET_DIR) -lmbay_dpi_client $(notdir ${SAMPLE_EXE}.c) -o ${SAMPLE_EXE}

.PHONY: clean
clean:
	-${RM} ${TARGET_LIB} ${SAMPLE_EXE} ${OBJS} $(SRCS:.c=.d)
