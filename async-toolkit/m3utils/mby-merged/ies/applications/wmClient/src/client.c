#define _GNU_SOURCE //For getline
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>

#include "clientLib.h"
#include "sbiosf.h"
#include "ies_sbiosf.h"

#include "reg_types.h"
#include "api/internal/hlp/hlp_api_regs_int.h"

static int debug =0;

int ParseUint(char *string, unsigned int *result)
{
    char *endptr = NULL;

    if (string == NULL)
    {
        *result = 0;
        return -1;
    }

    *result = 0;

    if (string[1] == 'x')
    {
        *result = strtoul(string + 2, &endptr, 16); 
    }
    else
    {
        *result = strtoul(string, &endptr, 10); 
    }

    return (string == endptr) ? -1 : 0;

}

int ParseUint64(char *string, unsigned long long int *result)
{
    char *endptr = NULL;

    if (string == NULL)
    {
        *result = 0;
        return -1;
    }

    *result = 0;

    if (string[1] == 'x')
    {
        *result = strtoull(string + 2, &endptr, 16); 
    }
    else
    {
        *result = strtoull(string, &endptr, 10); 
    }

    return (string == endptr) ? -2 : 0;

}

static int LoadCfg(char *fileName)
{
    FILE           *fp;
    char           *line = NULL;
    size_t          lineSize = 0;
    int             len;
    int             lineNum;
    char           *tok;
    int            i;
    unsigned int   addr;
    unsigned long long val64;

    if (fileName == NULL)
    {
        return -1;
    }

    fp = fopen(fileName, "r");

    if (fp == NULL)
    {
        printf("Unable to open '%s'\n", fileName);
        return -2;
    }

    printf("Loading '%s'\n", fileName);

    lineNum = 0;
    while ((len = getline(&line, &lineSize, fp)) != -1)
    {
        lineNum++;

        if (len <= 2)
        {
            continue;
        }
        //printf("LINE#%d:%d: %s\n", lineNum, len, line);

        if (line[0] == '#')
        {
            continue;
        }

        /* Remove CR */
        if ((line[len-1] == '\n') || (line[len-1] == '\r'))
        {
            len--;
            line[len] = '\0';
        }
        if ((line[len-1] == '\n') || (line[len-1] == '\r'))
        {
            len--;
            line[len] = '\0';
        }

        /* Skip reg dbg write64 0, not checking for valid command */
        for (i = 0; i < 5; i++)
        {
	    tok = strtok((i)?NULL:line, " ");
	}
        if (tok)
        {
            //printf("Addr %s\n", tok);
            if (ParseUint(tok, &addr))
            {
                printf("Error unable to parse addr: %s\n", tok);
                continue;
            }
            tok = strtok(NULL, " ");
            if (tok)
            {
                //printf("Val %s\n", tok);
                if (ParseUint64(tok, &val64))
                {
                    printf("Error unable to parse value: %s\n", tok);
                    continue;
                }
                if (debug >= 2) printf("Addr: 0x%x val: 0x%llx\n", addr, val64);
                writeUINT64(addr, val64);
            }
        }

    }

    if (line) 
    {
        free(line);
    }

    fclose(fp);

    return 0;

}

void printPacket(unsigned char *packetRcv, int lenRcv)
{
    int i,j;
    for(i = 0, j =  0 ; i < lenRcv; i++,j = (j+1)%16)
    {
        if(j == 0) printf("%04x: ", i);
        printf("%02x ", packetRcv[i]);
        if(j == 15) printf("\n");
    }
}

#define MAX_CFG_FILE 3

int main(int argc, char *argv[])
{
    unsigned int value1 = 0;
    unsigned int value2 = 0 ;
    unsigned long int val64 = 0 ;
    unsigned long int val64Mult[32];
    int i;
    char *loadCfgFile[MAX_CFG_FILE];
    int numCfgFile = 0;
    char *pktServerFile = NULL;
    int rv;
    int id;

    for (i = 1 ; i < argc ; i++)
    {
        if (!strcmp(argv[i], "-d") && (i+1 < argc) ) 
        {
            ParseUint(argv[i+1], &debug);
            printf("Debug is set to %d\n", debug);
            i++;
        }
        else if (!strcmp(argv[i], "-f") && (i+1 < argc) ) 
        {
            if (numCfgFile < MAX_CFG_FILE)
            {
                loadCfgFile[numCfgFile] = argv[i+1];
                numCfgFile++;
            }
            i++;
        }
        else
        {
            pktServerFile = argv[i];
        }
    }


    if (debug & 8)
    {
	u64 *pval64;
	u64 intr_status;
        int rsp;
        int id;

	/* Example on how to use the ies_sbiosf functions
	 * This will be used on API5 WM */
        rv = ies_sbiosf_init();
        printf("ies_sbiosf_init: %d\n", rv);

	sleep(1);

	rv = ies_sbiosf_request_msg(&id);
	printf("ies_sbiosf_request_msg: id=%d, rv=%d\n", id, rv);
	rv = ies_sbiosf_request_msg(&id);
	printf("ies_sbiosf_request_msg: id=%d, rv=%d\n", id, rv);
	rv = ies_sbiosf_request_msg(&id);
	printf("ies_sbiosf_request_msg: id=%d, rv=%d\n", id, rv);
	rv = ies_sbiosf_request_msg(&id);
	printf("ies_sbiosf_request_msg: id=%d, rv=%d\n", id, rv);


	/* id=1 is the first reserved one */
	rv = ies_sbiosf_send_msg(2);
        printf("ies_sbiosf_send_msg(2): %d\n", rv);

        /* Send a real read message */
	printf("======= Send Read\n");
	ies_sbiosf_add_read(1, 0x300, 1);
	rv = ies_sbiosf_send_msg(1);
        printf("ies_sbiosf_send_msg(1): %d\n", rv);

	printf("======= call write on read operation\n");
	rv = ies_sbiosf_add_write1(1, 0x1230, 0xfedcba9876543210);
        printf("ies_sbiosf_add_write1(1): should fail %d\n", rv);

	printf("======= Send Write\n");
	rv = ies_sbiosf_add_write1(2, 0x1230, 0xfedcba9876543210);
        printf("ies_sbiosf_add_write1(2): should pass %d\n", rv);
	rv = ies_sbiosf_send_msg(2);
        printf("ies_sbiosf_send_msg(2): %d\n", rv);


	printf("======= Send Read\n");
	ies_sbiosf_add_read(3, 0x1230, 2);
	rv = ies_sbiosf_send_msg(3);
        printf("ies_sbiosf_send_msg(3): %d\n", rv);
	rv = ies_sbiosf_send_msg(3);
        printf("ies_sbiosf_send_msg(3) again: should fail: %d\n", rv);




	sleep(2);

	/* Interrupt by some mean to tell data is present or
         * SW can keep polling */
	/* read respond */
	printf("======= Read response\n");
	pval64 = NULL;
        rv = ies_sbiosf_get_comp_data(&id, &rsp, &pval64);
        printf("ies_sbiosf_get_comp_data: rv=%d, id=%d rsp=%d data=%p\n", rv, id, rsp, pval64);
	printf("Val64[0]=%llx val64[1](not valid)=%llx\n", pval64[0], pval64[1]);

	/* write respond */
	printf("======= Write response\n");
	pval64 = NULL;
        rv = ies_sbiosf_get_comp_data(&id, &rsp, &pval64);
        printf("ies_sbiosf_get_comp_data: rv=%d, id=%d rsp=%d data=%p\n", rv, id, rsp, pval64);
	if (pval64) printf("Val64[0](not valid)=%llx val64[1](not valid)=%llx\n", pval64[0], pval64[1]);

	/* read respond */
	printf("======= Read response\n");
	pval64 = NULL;
        rv = ies_sbiosf_get_comp_data(&id, &rsp, &pval64);
        printf("ies_sbiosf_get_comp_data: rv=%d, id=%d rsp=%d data=%p\n", rv, id, rsp, pval64);
	if (pval64) printf("Val64[0]=%llx val64[1]=%llx\n", pval64[0], pval64[1]);








	printf("======= Send Write\n");
	rv = ies_sbiosf_request_msg(&id);
	printf("ies_sbiosf_request_msg: id=%d, rv=%d\n", id, rv);
	rv = ies_sbiosf_add_write1(id, 0x1230, 0x0);
        printf("ies_sbiosf_add_write1(%d): should pass %d\n", id, rv);
	rv = ies_sbiosf_send_msg(id);
        printf("ies_sbiosf_send_msg(%d): %d\n", id, rv);


	printf("======= Send Read\n");
	rv = ies_sbiosf_request_msg(&id);
	printf("ies_sbiosf_request_msg: id=%d, rv=%d\n", id, rv);
	ies_sbiosf_add_read(id, 0x1230, 2);
	rv = ies_sbiosf_send_msg(id);
        printf("ies_sbiosf_send_msg(%d) again: rv=%d\n", id, rv);
	rv = ies_sbiosf_send_msg(id);
        printf("ies_sbiosf_send_msg(%d) again: should fail %d\n", id, rv);

	sleep(2);


	/* write respond */
	printf("======= Write response\n");
	pval64 = NULL;
        rv = ies_sbiosf_get_comp_data(&id, &rsp, &pval64);
        printf("ies_sbiosf_get_comp_data: rv=%d, id=%d rsp=%d data=%p\n", rv, id, rsp, pval64);
	if (pval64) printf("Val64[0](not valid)=%llx val64[1](not valid)=%llx\n", pval64[0], pval64[1]);

	/* read respond */
	printf("======= Read response\n");
	pval64 = NULL;
        rv = ies_sbiosf_get_comp_data(&id, &rsp, &pval64);
        printf("ies_sbiosf_get_comp_data: rv=%d, id=%d rsp=%d data=%p\n", rv, id, rsp, pval64);
	if (pval64) printf("Val64[0]=%llx val64[1]=%llx\n", pval64[0], pval64[1]);


	/* Get again, should have no more data */
	printf("======= No data Read\n");
	pval64 = NULL;
        rv = ies_sbiosf_get_comp_data(&id, &rsp, &pval64);
        printf("ies_sbiosf_get_comp_data: rv=%d, id=%d rsp=%d data=%p\n", rv, id, rsp, pval64);



	printf("======= No Interrupt\n");
        rv = ies_sbiosf_get_intr_data(&intr_status);
        printf("ies_sbiosf_get_intr_data: rv=%d, data=0x%llx\n", rv, intr_status);

	printf("======= Generate an Interrupt\n");
	rv = ies_sbiosf_request_msg(&id);
	printf("ies_sbiosf_request_msg: id=%d, rv=%d\n", id, rv);
	rv = ies_sbiosf_add_write1(id, HLP_GLOBAL_INTERRUPT(0), 0xa000000001);
        printf("ies_sbiosf_add_write1(%d): should pass %d\n", id, rv);
	rv = ies_sbiosf_send_msg(id);
        printf("ies_sbiosf_send_msg(%d): %d\n", id, rv);
        printf("Wait for interrupt event message\n");
        sleep(2);

        rv = ies_sbiosf_get_intr_data(&intr_status);
        printf("ies_sbiosf_get_intr_data: rv=%d, data=0x%llx\n", rv, intr_status);


        rv = ies_sbiosf_cleanup();
        printf("ies_sbiosf_cleanup: %d\n", rv);


	exit (0);
    }


    if (!pktServerFile)
    {
        printf("Specify path and filename to WM client packetServer file\n");
        exit(0);
    }

    /* Initiate contact with WM and register ourselves */
    connectCpuToWm(pktServerFile);

    /* Connect to egressing WM ports */
    connectToWmEthernetPort(0);
    connectToWmEthernetPort(1);
    connectToWmEthernetPort(2);
    connectToWmEthernetPort(3);

    for (i = 0; i < numCfgFile; i++)
    {
        LoadCfg(loadCfgFile[i]);
    }


    if (debug & 1)
    {
        printf("########## 64-bit Read/Write ##########\n");
        readUINT64(0x304, &val64);
        printf("0x304 = 0x%llx\n", val64);

        readUINT64(HLP_PARSER_KEY_W(0,0,0), &val64);
        printf("HLP_PARSER_KEY_W[0][0] = 0x%016llx\n", val64);

        writeUINT64(HLP_PARSER_KEY_W(0,0,0), 0x123456789abcdef);

        readUINT64(HLP_PARSER_KEY_W(0,0,0), &val64);
        printf("HLP_PARSER_KEY_W[0][0] = 0x%016llx\n", val64);
        
        writeUINT64(0x43210, 0xfedcba9876543210);
        readUINT64(0x43210, &val64);
        printf("0x43210 = 0x%llx\n", val64);

        printf("########## 64-bit Multi Read/Write ##########\n");
	val64Mult[0] = 0xfedcba9876543210;
	val64Mult[1] = 0xaabbccddeeff9988;
	writeUINTMult64(0x43210, 2, val64Mult);

	readUINTMult64(0x43210, 2, val64Mult);
	for (i = 0; i < 2; i++)
        {
             printf("0x%06x = 0x%llx\n", 0x43210 + i*2, val64Mult[i]);
        }

	/* Return an error message */
        rv = readUINT64(0xFFFFFFFF, &val64);
        printf("Read 0xFFFFFFFF, rv = %d\n", rv);

    }

    if (debug & 2)
    {
        /* Cause the model to generate an IOSF interrupt message */
	writeUINT64(HLP_GLOBAL_INTERRUPT(0),  0xa000000001);
        printf("Wait for interrupt event message\n");
        sleep(2);

        readUINT64(0x304, &val64);
        printf("0x%x = 0x%llx\n", 0x304, val64);
        readUINT64(HLP_GLOBAL_INTERRUPT(0), &val64);
        printf("0x%x = 0x%llx\n", HLP_GLOBAL_INTERRUPT(0), val64);
    }

    if (debug & 4)
    {
        unsigned char packet[64];
        int len = 64;
        int physPort = 2;










	for (i = 0 ; i < 64 ; i++)
		writeUINT64(HLP_PARSER_PORT_CFG(i, 0), 0x0c14000000000000);
	
	/* Set vlan1_ptr to 1 to extract vid and vpri */
	for (i = 0 ; i < 24 ; i++) 
		writeUINT64(HLP_MAP_PORT_CFG(i, 0), 0x0000000000100000);

	/*Set the default SGLORT*/
	for (i = 0 ; i < 24 ; i++)
		writeUINT64(HLP_MAP_PORT_DEFAULT(i, 0,0), 0x2E2E | (i+1) << 16);

	/* Set default vlan to 1 */
	for (i = 0 ; i < 24 ; i++) 
		writeUINT64(HLP_MAP_PORT_DEFAULT(i, 1,0), 0x0000000100018484);


	/* setup vlan 1 and add all ports to it */
	writeUINT64(HLP_INGRESS_VID_TABLE(1,0), 0xffffff);
	writeUINT64(HLP_EGRESS_VID_TABLE(1, 0), 0xffffff);

	/* set STP state to forwarding */
	writeUINT64(HLP_INGRESS_MST_TABLE(1, 0), 0xffffffffffffffff);
	writeUINT64(HLP_EGRESS_MST_TABLE(1,0), 0xffffffff);

	/* Match on glorts 1..24 in cam addresses 0..23 and flood glort 0xffff in
	  address 63 */
	for (i = 0; i < 24; i++)
		writeUINT64(HLP_GLORT_CAM(i,0), ((i+1) & 0xffff) |
					 ((~(i+1))&0xffff) << 16);
	writeUINT64(HLP_GLORT_CAM(63,0), 0x0000ffff);

	/* Set dest table pointers */
	for (i = 0 ; i < 24; i++) 
		writeUINT64(HLP_GLORT_RAM(i,0), i << 2);
	writeUINT64(HLP_GLORT_RAM(63,0), 63 << 2);


	/* set dmask for port/sglorts 0-23 and for flood glort */
	for (i = 0 ; i < 24; i++)
		writeUINT64(HLP_GLORT_DEST_TABLE(i, 0), 1<<i);
	writeUINT64(HLP_GLORT_DEST_TABLE(63, 0), 0xffffff);

	/* Set flood UC, flood MC and flood BC glorts */
	writeUINT64(HLP_FLOOD_GLORT_TABLE(0,0), 0x0000ffffffffffff);

	/* Tell triggers to forward frame by default */
	writeUINT64(HLP_TRIGGER_CONDITION_CFG(0,0), 0x200aaaaa);
	writeUINT64(HLP_TRIGGER_CONDITION_RX(0,0), 0xffffff);
	writeUINT64(HLP_TRIGGER_CONDITION_AMASK_1(0,0), 0xffffffff);
	writeUINT64(HLP_TRIGGER_CONDITION_AMASK_2(0,0), 0xffffffff);
	writeUINT64(HLP_TRIGGER_ACTION_CFG_1(0,0), 0x1);






























        for(i =  0 ; i < 64; i++) packet[i] = i;

        printf("\n########## Sending %d-byte packet to physPort %d\n", len, physPort);
        rimmonSendPkt(0, physPort, packet, len);

        printf("\n########## Receiving packets...\n");
        unsigned char *packetRcv = NULL;
        unsigned char sbData[32];
        unsigned int lenRcv = -1;
        for(int port = 0 ; port < 4; port++)
        {
            printf("Checking port %d...\n", port);
            /* wait 1 seconds per port */
            recvPacket(port, 1*1000, &packetRcv, &lenRcv, sbData);

            if(packetRcv != NULL)
            {
                printf("\n########## Receiving packet on physPort %d\n", port);
                printPacket(packetRcv, lenRcv);
            }
            if(packetRcv != NULL) free(packetRcv);
        }
    }

    cleanup();

    exit(1);
}


