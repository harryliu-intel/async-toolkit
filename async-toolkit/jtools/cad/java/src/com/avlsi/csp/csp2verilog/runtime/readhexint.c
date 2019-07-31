#define _GNU_SOURCE

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <poll.h>
#include <unistd.h>
#include <errno.h>

int cast2verilog_fopen(const char *filename, const char *mode) {
    int flags = 0;
    if (!strcmp(mode, "r") || !strcmp(mode, "rb")) {
        flags = O_RDONLY;
    } else if (!strcmp(mode, "w") || !strcmp(mode, "wb")) {
        flags = O_WRONLY | O_CREAT | O_TRUNC;
    } else if (!strcmp(mode, "a") || !strcmp(mode, "ab")) {
        flags = O_WRONLY | O_CREAT | O_APPEND;
    } else {
        return 0;
    }

    int fd = open(filename, flags, 0666);
    if (fd == -1) {
        return 0;
    } else {
        return fd + 1;
    }
}

int cast2verilog_fgetc(int fp) {
    int result = -1;
    if (fp > 0) {
        unsigned char c;
        if (read(fp - 1, &c, 1) == 1) result = c;
    }
    return result;
}

int cast2verilog_fputc(char c, int fp) {
    if (fp > 0) {
        return write(fp - 1, &c, 1);
    } else {
        return -1;
    }
}

int cast2verilog_fpoll(int fp) {
    if (fp > 0) {
        struct pollfd fds = { fp - 1, POLLIN | POLLOUT, 0 };
        int nfds = TEMP_FAILURE_RETRY(poll(&fds, 1, 0));
        if (nfds == 0) {
            return 0;
        } else if (nfds == 1) {
            return fds.revents;
        }
    }
    return EOF;
}

int cast2verilog_fclose(int fp) {
    return fp > 0 && close(fp - 1) == 0 ? 0 : EOF;
}
