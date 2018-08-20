/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * Copyright (c) 2018, Intel Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Intel Corporation nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "mby_model.h"
#include "mby_srv_log.h"
#include "mby_srv_utils.h"
#include "mby_srv_socket.h"
#include "mby_srv_message.h"
#include "mby_srv_handlers.h"
#include "mby_srv_nvmimg.h"

#define FM_FDS_POLL_TIMEOUT_USEC            1*1000

// TODO remove this global/extern varaibles
static int debug = 0;
extern fm_socket pktRecvSockets[MAX_PHYS_PORT];
extern fm_msg_stats msg_stat;

/*extern fm_uint64 log_cat_mask;*/
/*extern char *log_cat_names[];*/

void logPrintHandler(fm_uint64 level, char *log)
{
    FM_NOT_USED(level);
    printf("%s\n", log);
}

static void print_usage(char *cmd)
{
    printf("Usage: %s [options]\n", cmd);
    printf("    -h                    - This help.\n");
    printf("    -p <port>             - Set server port\n");
    printf("    -l                    - Disable all model logging output\n");
    printf("    -r                    - Don't reset chip.\n");
    printf("    -I                    - Don't check and send interrupt messages.\n");
    printf("    -f <file>             - Load specified NVM image.\n");
    printf("    -o <file>             - Save startup register writes to config file.\n");
    printf("    -d <debug>            - Specify logging level.\n");
    printf("    -v <cat1,cat2>        - Enable logging from selected categories.\n");

    printf("Allowed values for category names are: \n");
    printf(" - all: Enable output from all the categories \n");
    printf(" - none: Disable output from all the categories \n");
    printf(" - Comma separated list of the following category names:\n     ");

#if 0
    int i = 0;
    while (log_cat_names[i]) {
        printf("%s  ", log_cat_names[i]);
        if (++i % 8 == 0 && log_cat_names[i])
            printf("\n     ");
    }
#endif
    printf("\n");
    exit(0);
}

/*****************************************************************************/
/* main
 * \ingroup intModel
 *
 * \desc            Entry point for model server.
 *
 * \param[in]       argc is the number of command-line arguments
 *
 * \param[in]       argv points to an array of command-line argument strings
 *
 * \return          0 for success
 *
 *****************************************************************************/
int main(int argc, char *argv[])
{
    fm_status       status;
    fm_int          sw = 0;
    fm_int          i;
    fm_libCfg       libCfg;
    fm_bool         resetChip = TRUE;
    fm_text         nvmImgFile = NULL;
    fm_int          sendIntr = 1;
    //fm_int          intrStep = INTERRUPT_READ_DELAY;
    fm_socket       serverSocket;
    fm_int          serverPort = 0;
    fm_bool         dataPresent;
    fm_modelMessage imsg;
    fm_int32        msgLength;
    fm_timestamp    timeout;
    fm_socket       *sockets[MAX_PERSISTENT_CONNECTIONS + 1];
    fm_int          numSockets = 1;
    fm_int          eventsReceived[MAX_PERSISTENT_CONNECTIONS + 1];
    fd_set          rfds;
    struct          timeval tv;
    char            c;
    int             rv;
    int             fd = 0;

    memset(&libCfg, 0, sizeof(libCfg));
    libCfg.logLevel = debug;
    libCfg.logHandler = logPrintHandler;
    for (i = 1 ; i < argc ; i++)
    {
        if (!strcmp(argv[i], "-d") && (i+1 < argc) )
        {
           ParseInt(argv[i+1], &debug);
           printf("Debug is set to %d\n", debug);
           libCfg.logLevel = debug;
           i++;
        }
        else if (!strcmp(argv[i], "-p") && (i+1 < argc) )
        {
           ParseInt(argv[i+1], &serverPort);
           printf("Server port is %d\n", serverPort);
           i++;
        }
        else if (!strcmp(argv[i], "-l") )
        {
            libCfg.logLevel = -1 ;
            printf("Logging is set to disabled\n");
        }
        else if (!strcmp(argv[i], "-r"))
        {
            resetChip = !resetChip;
        }
        else if (!strcmp(argv[i], "-I"))
        {
            sendIntr = !sendIntr;
        }
        else if (!strcmp(argv[i], "-v"))
        {
            // TODO Add corresponding init functions for MBY
            // fmModelLibSetLogCat(argv[++i]);
        }
        else if (!strcmp(argv[i], "-f") && (i+1 < argc))
        {
            nvmImgFile = argv[i+1];
            i++;
        }
        else
        {
            print_usage(argv[0]);
            exit(0);
        }
    }

    // TODO Add corresponding init functions for MBY
#if 0
    status = fmModelLibInit(sw, &libCfg);
    if (status)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "FAILED to init model lib: err= %d\n", status);
        return status;
    }
#endif

    /* Can skip this for debug if defaults values are not important */
    if (resetChip)
    {
        status = mbyResetModel(sw);
        if (status)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "FAILED to reset chip. err=%d\n", status);
        }
    }
    else
    {
        printf("Skipping chip reset\n");
    }

    if (nvmImgFile)
    {
		status = loadNvmImg(nvmImgFile);
		if (status)
		{
			FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
						 "Unable to load NVM image!\n");
		}
    }

    status = fmInitializeSocketInfoFile();
    if (status != FM_OK)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to create socket info file!\n");

        return status;
    }

    fmCreateNetworkServer(&serverSocket,
                          FM_SOCKET_TYPE_TCP,
                          serverPort,
                          3);
    status = fmAddSocketInfoToFile("localhost", serverSocket.serverPort, FALSE);
    if (status != FM_OK)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to add socket info to file!\n");

        return status;
    }

    /***************************************************
     * Prepare for connections
     **************************************************/

    for ( i = 0 ; i < MAX_PERSISTENT_CONNECTIONS ; i++ )
    {
        sockets[i+1] = malloc(sizeof(fm_socket));

        if (!sockets[i+1])
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Unable to allocate memory for socket structure\n");

            return 1; // TODO add errors like  FM_ERR_NO_MEM;
        }
        sockets[i + 1]->type = FM_SOCKET_TYPE_TCP;
    }

    /* Initially just the server */
    sockets[0] = &serverSocket;
    numSockets = 1;
    for (i = 0; i < MAX_PHYS_PORT; i++)
    {
        pktRecvSockets[i].sock = -1;
        pktRecvSockets[i].type = FM_SOCKET_TYPE_TCP;
        // TODO portLinkState[i] = PORT_LINK_UP;
    }

    printf("Waiting for TCP connections on port %d\n", serverSocket.serverPort);
    printf("Type 'h' for help\n");
    fflush(0); /* for redirect stdout to file */

    while (TRUE)
    {
        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);
        tv.tv_sec = 0;
        tv.tv_usec = 0;

        rv = select(fd + 1, &rfds, NULL, NULL, &tv);
        if (rv) {
            read(fd, &c, 1);
            switch (c)
            {
                case 'h':
                    printf("Help:\n");
                    printf("\tq             - Quit\n");
                    printf("\ts             - Show messages statistics\n");
                    printf("\tr             - Reset messages statistics\n");
                    printf("\tl             - Toggle logging output\n");
                    printf("\t0..6          - Set debug level. 1=PRINT, 2=DEBUG, 4=DEBUG3, etc\n");
                    printf("\ta             - Enable output log for all categories\n");
                    printf("\tn             - Disable output log for all categories\n");
                    printf("\tt<c>          - Toggle output log for category <c> (see below)\n");
                    i = 0;
#if 0
                    while (log_cat_names[i]) {
                        printf("\tt%c            - Toggle %s (%s)\n",
                                'a' + i, log_cat_names[i],
                                log_cat_mask & (1<<i) ? "enabled" : "disabled");
                        i++;
                    }
#endif
                    break;
                case 's':
                    printf("Messages stats:\n");
                    printf("\tPACKET    : %d\n", msg_stat.packet);
                    printf("\tMGMT      : %d\n", msg_stat.mgmt);
                    printf("\tCTRL      : %d\n", msg_stat.ctrl);
                    printf("\tIOSF      : %d\n", msg_stat.iosf);
                    break;
                case 'r':
                    memset(&msg_stat, 0, sizeof(msg_stat));
                    break;
                case 'q':
                    exit(0);
                    break;
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                    debug = c - '0';
                    libCfg.logLevel = debug;
                    // fmModelLibSetLogLevel(libCfg.logLevel);
                    break;
                case 'l':
                    if (libCfg.logLevel < 0)
                    {
                        libCfg.logLevel = 0;
                        printf("Log level is set to %d\n", libCfg.logLevel);
                    }
                    else
                    {
                        libCfg.logLevel = -1;
                        printf("All logging is disabled\n");
                    }
                    // fmModelLibSetLogLevel(libCfg.logLevel);
                    break;
                case 'g':
                    for (i = 0; i < numSockets; i++)
                    {
                        printf("Socket#%d fd %d type %d\n", i,
                                sockets[i]->sock, sockets[i]->type);
                    }
                    break;
#if 0
                case 'a':
                    printf("Output log for all categories is enabled\n");
                    log_cat_mask = 0xFFFFFFFFFFFFFFFF;
                    break;
                case 'n':
                    printf("Output log for all categories is disabled\n");
                    log_cat_mask = 0;
                    break;
                case 't':
                    if (read(fd, &c, 1) <= 0 || c < 'a' || c > 'n') {
                        printf("Category is not valid. Type 'h' for help\n");
                        break;
                    }
                    unsigned int j = c - 'a';
                    if (log_cat_mask & (1<<j)) {
                        printf("Output of category %s is disabled\n", log_cat_names[j]);
                        log_cat_mask &= ~(1 << j);
                    }
                    else {
                        printf("Output of category %s is enabled\n", log_cat_names[j]);
                        log_cat_mask |= 1 << j;
                    }
                    break;
#endif
            }
        }

#if 0 && MBY_ENABLE_INTERRUPT // We will probably never use this
        if (sendIntr && intrStep <= 0)
        {
            handleIntr(numSockets, sockets);
            intrStep = INTERRUPT_READ_DELAY;
        }
        --intrStep;
#endif

        dataPresent = FALSE;

        timeout.sec  = 0;
        timeout.usec = FM_FDS_POLL_TIMEOUT_USEC;

        status = fmWaitForNetworkEvent(sockets,
                                       &numSockets,
                                       MAX_PERSISTENT_CONNECTIONS + 1,
                                       eventsReceived,
                                       &timeout);

        FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM,
                      status == FM_OK,
                      "Network corruption detected: %s\n",
                      fmErrorMsg(status));

        for ( i = 0 ; i < numSockets ; i++ )
        {
            if (eventsReceived[i] == FM_NETWORK_EVENT_DATA_AVAILABLE)
            {
                dataPresent = TRUE;
            }
        }

        if (!dataPresent)
        {
            /***************************************************
             * No data available, no packets to forward so go wait
             * for a message forever.
             **************************************************/
            continue;
        }
        /**************************************************
         * Process actual messages
         **************************************************/
        for (i = 0 ; i < numSockets ; i++)
        {
            if (eventsReceived[i] != FM_NETWORK_EVENT_DATA_AVAILABLE)
            {
                continue;
            }

            memset(&imsg, 0 , sizeof(imsg));

            if (ReceiveMessage(sockets[i], TRUE, &imsg) != FM_OK)
            {
                continue;
            }

            msgLength = ntohl(imsg.msgLength);
            (void)ProcessMessage(sockets[i], &imsg, msgLength);
        }

        fflush(0); /* for redirect stdout to file */

    }   /* end while (TRUE) [Process incoming messages] */

    exit(0);
}   /* end main */

