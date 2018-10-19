/*
 * duke_proxy.cpp
 *
 *  Created on: Jan 22, 2015
 *      Author: hdanial-l
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "duke_proxy.h"
#ifdef __linux__
#include <unistd.h>
#else
#include <Windows.h>
#endif
#ifdef __linux__
#include "ftd2xx.h"
#else
#include ".\..\ftdi_driver\ftd2xx_win.h"
//#include "ftd2xx_win.h"
#endif

#define MAX_DEVICES		10
#define MAX_REGISTERS 4096

int silent = 1;

#ifndef _WINDOWS_

#define	MY_PRINTF(fmt, arg...)	do {\
if (!silent)\
	printf("%s:%d "fmt, __func__, __LINE__, ## arg); \
} while (0)

#define min(a,b) (((a) < (b)) ? (a) : (b))
#define duke_sleep(seconds) sleep(seconds)
#define duke_usleep(useconds) usleep(useconds)
#else
#define duke_sleep(seconds) Sleep((seconds)*1000)
#define duke_usleep(useconds) Sleep((useconds)/1000)
#define MY_PRINTF printf

#endif

#define DEBUG_PRINT_BYTES(buffer,size)\
	do {\
		int i=0;\
		char* _cbuff = (char*)buffer;\
		for(i=0;i<size;i++)\
			MY_PRINTF("%s[%d] = %x\n",#buffer,i,(unsigned int)_cbuff[i]);\
	} while(0)

#define HANDLE_ERR(ret_code,desc,line)\
	do {\
		duke_status _val = (ret_code);\
		if(_val) {\
			MY_PRINTF("%s[%d]: ret_code %d\n",desc,line,_val);\
			return _val;\
		}\
	} while(0)

#define HANDLE_ERR_PRINT_LINE(ret_code)\
		HANDLE_ERR(ret_code,#ret_code,__LINE__)

#define GET32_BYTE_VAL(val,byte_offset)\
	((((unsigned int)val) & ((unsigned int)0xff << ((byte_offset)*8))) >> ((byte_offset) * 8))


#define DOUBLE_FPGA_ON_CARD 2
#define MIN_FPGA_ON_CARD 0

/* sbios */
#define DUKE_MIN_SW_VERSION_NEED 0x0013
#define SBIOS_OFFSET 0

/* sbios & cbios */
#define DUKE_CARD_ID 0x0000
#define DUKE_CARD_OFFSET 0x0001
#define DUKE_NO_OF_CARDS 0x0003
#define DUKE_BIOS_RTL_VERSION 0x0010

/* cbios */
#define DUKE_FPGA_ON_CARD 0x0020

#define DUKE_CONF_DONE 0x0050
#define DUKE_CONF_DONE_IDLE_MASK 0x0
#define DUKE_CONF_DONE_FPGA0_MASK 0x1

#define DUKE_CONF_ERROR 0x0051
#define DUKE_CONF_ERROR_STATUS_GOOD 0x0

#define DUKE_FPGA_RECONFIG 0x0052
#define DUKE_FPGA_RECONFIG_FPGA0_MASK 0x1
#define DUKE_FPGA_RECONFIG_FPGA1_MASK 0x2
#define DUKE_FPGA_RECONFIG_FPGA_MASK 0x3

#define DUKE_CONF_IN_PROGRESS 0x0053
#define DUKE_CONF_IN_PROGRESS_IDLE_MASK 0x0

#define DUKE_FPGA0_IMAGE_SYNC 0x0054
#define DUKE_FPGA1_IMAGE_SYNC 0x0055

/* EEPROM */
#define EEPROM_SYNC 0x1010
#define EEPROM_SYNC_COPY 0xC
#define EEPROM_SYNC_DONE 0xD

#define DUKE_FPGA0_IMAGE_NAME 0x2300
//#define DUKE_FPGA1_IMAGE_NAME 0x2322
#define DUKE_FPGA0_IMAGE_TIME_STAMP 0x2320
//#define DUKE_FPGA1_IMAGE_TIME_STAMP 0x2342

#define DUKE_SW_USE 0x2500
#define DUKE_SW_USE_DOWNLOAD_GOOD_SHIFT 0
#define DUKE_SW_USE_FPGA_CONF_SYNC_SHIFT 1


#define DUKE_MAX_TIMEOUT 1000

// Don't plan to use Windows
// struct duke_handle_t {
//  #ifdef	_WINDOWS_
// 	unsigned long long dummy;
//  #endif
// };


duke_status DukePurge(DukeHandle handle,int purge_rx,int purge_tx)
{
	int ret_code = DUKE_STATUS_OK;
	int purge = 0;

	if(purge_rx)
		purge |= FT_PURGE_RX;
	if(purge_tx)
		purge |= FT_PURGE_TX;

	if(purge)
		ret_code = FT_Purge(handle,purge);


	if(ret_code)
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_GENERAL);

	return DUKE_STATUS_OK;
}


duke_status DukeFTWriteData(DukeHandle handle,char* data,unsigned int size,int purge_rx,int purge_tx)
{
	int ret_code = DUKE_STATUS_OK;
	unsigned int bytes_written = 0;

	HANDLE_ERR_PRINT_LINE(DukePurge(handle,purge_rx,purge_tx));
	MY_PRINTF("trying to FtWrite %d bytes\n",size);

	ret_code = FT_Write(handle,data,size,&bytes_written);

	if(ret_code)
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_FTDI_WRITE);


	if(bytes_written != size) {
		MY_PRINTF("wrote %d bytes instead of %d\n",size,bytes_written);
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_FTDI_WRITE);
	}

	return DUKE_STATUS_OK;

}

duke_status DukeFTReadData(DukeHandle handle,char* buffer,unsigned int bytes,int purge_rx,int purge_tx,unsigned int* bytes_read)
{
	int ret_code;
	int i =0;
	HANDLE_ERR_PRINT_LINE(DukePurge(handle,purge_rx,purge_tx));
for(i =0;i<10000;i++) {
	ret_code = FT_GetQueueStatus(handle,bytes_read);

	if(ret_code)
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_GENERAL);
}
	MY_PRINTF("user asked for %d bytes\n",bytes);
	MY_PRINTF("Bytes available = %d\n",*bytes_read);

	if((*bytes_read) == 0)
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_FTDI_READ);

	ret_code = FT_Read(handle,buffer,bytes,bytes_read);
	MY_PRINTF("read %d bytes\n",*bytes_read);

	if(bytes != *bytes_read) {
		MY_PRINTF("read only %d bytes instead of %d\n",*bytes_read,bytes);
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_FTDI_READ);
	}

	return DUKE_STATUS_OK;
}



/**
 *
 * @param handle
 * @param fpga_offset
 * @param start_offset
 * @param val array of DWORDS of size n to hold values
 * @param n number of DOWRDS to read
 * @return
 */
duke_status DukeReadRegister(DukeHandle handle,unsigned char card_offset,unsigned int start_offset,unsigned int* val,unsigned int n)
{
	int ret_code= 0;
	unsigned char command[12];
	unsigned int bytes_written;
	unsigned int bytes_read;
	unsigned int bytes_available;
	unsigned int total_bytes;
	unsigned char buffer[MAX_REGISTERS];
	unsigned int i;


	command[0] = 0x1; /* opcode */

	command[1] = 0x0;
	command[2] = 0x0; /* body length without first 4 bytes high */
	command[3] = 0x8; /* body length without first 4 bytes low  */

	command[4] = card_offset;
	command[5] = GET32_BYTE_VAL(start_offset,3);

	command[6] = GET32_BYTE_VAL(start_offset,2);
	command[7] = GET32_BYTE_VAL(start_offset,1);
	command[8] = GET32_BYTE_VAL(start_offset,0);
	command[9] = 0x01; /* increment*/
	command[10] = GET32_BYTE_VAL(n,1);
	command[11] = GET32_BYTE_VAL(n,0);


//	DEBUG_PRINT_BYTES(command,12);



	ret_code = FT_Write(handle,command,12,&bytes_written);
	if(FT_OK != ret_code) {
		MY_PRINTF("ReadRegister: Write Error %d\n",ret_code);
		return DUKE_ERROR_FTDI_WRITE;
	}
	if(bytes_written != 12) {
		MY_PRINTF("ReadRegister: wrote %d bytes only\n",bytes_written);
		return DUKE_ERROR_FTDI_WRITE;
	}

	total_bytes = n * 4 + 2; /* num of DWORDS + 2 for ACK */
	i=0;
	do {
		ret_code = FT_GetQueueStatus(handle,&bytes_available);
		if(FT_OK != ret_code) {
			MY_PRINTF("ReadRegister: FT_GetQueueStatus %d\n",ret_code);
			return DUKE_ERROR_FTDI_WRITE;
		}
		if(bytes_available == total_bytes)
			break;

	} while(++i < 100000);




	if(bytes_available != total_bytes) {
		MY_PRINTF("only %d bytes available instead of %d\n",bytes_available,total_bytes);
		if(bytes_available) {
			if(FT_OK == FT_Read(handle,buffer,bytes_available,&bytes_read)) {
				MY_PRINTF("bytes[%d-%d] = %x%x\n",bytes_available-1,bytes_available-2,buffer[bytes_available-2],buffer[bytes_available-1]);
			}
		}
		return DUKE_ERROR_READ_REGISTER;
	}

	ret_code = FT_Read(handle,buffer,bytes_available,&bytes_read);
	if(FT_OK != ret_code) {
		MY_PRINTF("ReadRegister: FT_GetQueueStatus %d\n",ret_code);
		return DUKE_ERROR_FTDI_READ;
	}

	if(bytes_read != bytes_available) {
		MY_PRINTF("only %d bytes available instead of %d\n",bytes_read,bytes_available);
		return DUKE_ERROR_FTDI_READ;
	}

	if(buffer[total_bytes-1] != (unsigned char)0xaa ||
			buffer[total_bytes-2] != (unsigned char)0xaa) {
		MY_PRINTF("ack %x%x\n",buffer[total_bytes-1],buffer[total_bytes-2]);
		return DUKE_ERROR_READ_REGISTER;
	}

	/* return all DWORDS without the ACK */
	for(i=0;i<(total_bytes-2)/4;i++) {
		val[i] = ((unsigned int*)buffer)[i];
	}

	MY_PRINTF("read 0x%x address 0x%x card %d\n",val[0]/*,val[0]*/,start_offset,card_offset);
	return ret_code;
}

duke_status DukeWriteRegister(DukeHandle handle, unsigned char card_offset, unsigned int start_offset, unsigned int* val, unsigned int n)
{
	unsigned char buffer[1024];
	unsigned short total_length = n * 4 + 6; /* csr bytes + write header not include first 4 bytes of general header */
	unsigned int i;
	int ret_code;
	unsigned int bytes_written;
	unsigned int bytes_to_write;
	unsigned int bytes_available;
	unsigned int bytes_read;


	MY_PRINTF("write 0x%x to 0x%x card %d\n",(int)val[0]/*,val[0]*/,start_offset,card_offset);
	if(n > 1) {
            MY_PRINTF("write 0x%x to 0x%x total %d\n",(int)val[1],/*val[1],*/start_offset+1,n);
        }


	buffer[0] = 0x2;				/* opcode */
	buffer[1] = 0x0;
	buffer[2] = total_length >> 8; /* body length */
	buffer[3] = total_length & 0xff;
	buffer[4] = card_offset;
	buffer[5] = GET32_BYTE_VAL(start_offset, 3);
	buffer[6] = GET32_BYTE_VAL(start_offset, 2);
	buffer[7] = GET32_BYTE_VAL(start_offset, 1);
	buffer[8] = GET32_BYTE_VAL(start_offset, 0);
	buffer[9] = 1; /* increment */
	for (i = 0; i < n; i++) {
		buffer[4*i + 10] = GET32_BYTE_VAL(val[i], 3);
		buffer[4*i + 11] = GET32_BYTE_VAL(val[i], 2);
		buffer[4*i + 12] = GET32_BYTE_VAL(val[i], 1);
		buffer[4*i + 13] = GET32_BYTE_VAL(val[i], 0);
	}

	bytes_to_write = 10 + 4 * n;

	ret_code = FT_Write(handle, buffer, bytes_to_write,&bytes_written);
	if (FT_OK != ret_code) {
		MY_PRINTF("Write Error %d\n", ret_code);
		return DUKE_ERROR_FTDI_WRITE;
	}
	if (bytes_written != bytes_to_write) {
		MY_PRINTF("wrote %d bytes only\n", bytes_written);
		return DUKE_ERROR_FTDI_WRITE;
	}

	i=0;
	total_length = 2;
	do {
		ret_code = FT_GetQueueStatus(handle, &bytes_available);
		if (FT_OK != ret_code) {
			MY_PRINTF("ReadRegister: FT_GetQueueStatus %d\n", ret_code);
			return -1;
		}
		if(bytes_available == total_length)
			break;

	}	while(++i < 100000);



	if (bytes_available != total_length) { /* size of ack */
		MY_PRINTF("only %d bytes available instead of %d\n", bytes_available, total_length);
		if (bytes_available) {
			if (FT_OK == FT_Read(handle, buffer, bytes_available, &bytes_read)) {
				MY_PRINTF("bytes[%d-%d] = %x%x\n", bytes_available - 1, bytes_available - 2, buffer[bytes_available - 2], buffer[bytes_available - 1]);
			}
		}
		return DUKE_ERROR_WRITE_REGISTER;
	}

	ret_code = FT_Read(handle, buffer, bytes_available, &bytes_read);
	if (FT_OK != ret_code) {
		MY_PRINTF("ReadRegister: FT_GetQueueStatus %d\n", ret_code);
		return DUKE_ERROR_FTDI_READ;
	}

	if (bytes_read != bytes_available) {
		MY_PRINTF("only %d bytes available instead of %d\n", bytes_read, bytes_available);
		return -1;
	}

	if (buffer[bytes_read - 1] != (unsigned char)0xaa ||
		buffer[bytes_read - 2] != (unsigned char)0xaa) {
		MY_PRINTF("ack %x%x\n", buffer[bytes_read - 1], buffer[bytes_read - 2]);
		return DUKE_ERROR_WRITE_REGISTER;
	}

	return 0;

}




/**
 *
 * @param max_timeout timeout in seconds
 * @return DUKE_STATUS_OK - done | DUKE_STATUS_TIMEOUT - not done
 */
duke_status DukeGetHandle(DukeHandle* handle)
{
	FT_STATUS ftStatus;
	unsigned int	iNumDevs;
	char * 	pcBufLD[MAX_DEVICES + 1];
	char 	cBufLD[MAX_DEVICES][64];
	unsigned int	i;
	DukeHandle duke_handle;
	char* serial_number = "99999999B";
#ifdef SIMULATION
	return 0;
#endif

	for(i = 0; i < MAX_DEVICES; i++) {
		pcBufLD[i] = cBufLD[i];
	}
	pcBufLD[MAX_DEVICES] = NULL;

	iNumDevs = 0;
	ftStatus = FT_ListDevices(pcBufLD, &iNumDevs, FT_LIST_ALL | FT_OPEN_BY_SERIAL_NUMBER);

	if(ftStatus != FT_OK) {
		MY_PRINTF("Error: FT_ListDevices(%d)\n", (int)ftStatus);
		return DUKE_ERROR_OPEN;
	}

	ftStatus = FT_OpenEx(serial_number, FT_OPEN_BY_SERIAL_NUMBER,(FT_HANDLE*)&duke_handle);
	if(ftStatus != FT_OK) {
		MY_PRINTF("unable to open device %u\n",ftStatus);
		return DUKE_ERROR_OPEN;
	}

	*handle = duke_handle;

	for(i = 0; ( (i <MAX_DEVICES) && (i < iNumDevs) ); i++) {
		MY_PRINTF("Device %d Serial Number - %s\n", i, cBufLD[i]);
	}

	ftStatus = FT_ListDevices(pcBufLD, &iNumDevs, FT_LIST_ALL | FT_OPEN_BY_DESCRIPTION);

	if(ftStatus != FT_OK) {
		MY_PRINTF("Error: FT_ListDevices(%d)\n", (int)ftStatus);
		return DUKE_ERROR_OPEN;
	}

	for(i = 0; ( (i <MAX_DEVICES) && (i < iNumDevs) ); i++) {
		MY_PRINTF("Device %d Serial Number - %s\n", i, cBufLD[i]);
	}

//	MY_PRINTF("handle = 0x%lld\n",(u64)duke_handle);

	return DUKE_STATUS_OK;


}




duke_status DukeGetDevicesCount(unsigned int* count)
{
	unsigned int iNumDevs;
	unsigned int devCount;
	FT_STATUS ftStatus;

	ftStatus = FT_ListDevices(&devCount, &iNumDevs, FT_LIST_NUMBER_ONLY);
	if(ftStatus != FT_OK) {
		MY_PRINTF("Error: FT_ListDevices(%d)\n", (int)ftStatus);
		return DUKE_ERROR_OPEN;
	}
	*count = devCount;

	MY_PRINTF(" listDevices iNumDevs %d devCount %d\n",iNumDevs,devCount);
	return DUKE_STATUS_OK;
}

duke_status DukeGetDevices(struct duke_ft_device* user_ft_devices,unsigned int len)
{
	unsigned int iNumDevs;
	unsigned int i;
	FT_DEVICE_LIST_INFO_NODE* lib_device;

	 if(FT_OK != FT_CreateDeviceInfoList(&iNumDevs))
		 HANDLE_ERR_PRINT_LINE(DUKE_ERROR_OPEN);

	 MY_PRINTF("devices count %d\n",iNumDevs);

	 lib_device = malloc(sizeof(FT_DEVICE_LIST_INFO_NODE) * iNumDevs);
	 if(NULL == lib_device)
		 HANDLE_ERR_PRINT_LINE(DUKE_ERROR_GENERAL);


	if(FT_OK != FT_GetDeviceInfoList(lib_device,&iNumDevs))
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_OPEN);

	MY_PRINTF("devices count %d\n",iNumDevs);
	if(len != iNumDevs)
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_GENERAL);



	for(i=0;i<iNumDevs;i++) {

		user_ft_devices[i].Flags = lib_device[i].Flags;
		user_ft_devices[i].ID = lib_device[i].ID;
		user_ft_devices[i].LocId = lib_device[i].LocId;
		user_ft_devices[i].Type = lib_device[i].Type;
		user_ft_devices[i].handle = (DukeHandle)lib_device[i].ftHandle;
		strcpy(user_ft_devices[i].Description,lib_device[i].Description);
		strcpy(user_ft_devices[i].SerialNumber,lib_device[i].SerialNumber);
	}



	if(lib_device)
		free(lib_device);
	return DUKE_STATUS_OK;
}

duke_status DukeGetHandleBy(struct duke_ft_device* device,DukeOpenType type, DukeHandle* handle)
{
	int status;
	if(type == DukeOpenByLocID)
		return DUKE_ERROR_GENERAL;

	MY_PRINTF("%s %s %lu %d %lu open by %d\n",
			device->Description, device->SerialNumber, device->ID, device->LocId, device->Type,
			type);

	if(type == DukeOpenByID) {
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_NOT_SUPPORTED);
	}
	else if(type == DukeOpenBySerial) {
		status = FT_OpenEx(device->SerialNumber, FT_OPEN_BY_SERIAL_NUMBER, (FT_HANDLE*)handle);
	}
	else if(type == DukeOpenByDescription) {
		// Maybe this is wrong, it should be device->Description
		status = FT_OpenEx(device->SerialNumber, FT_OPEN_BY_DESCRIPTION,(FT_HANDLE*)handle);
	}
	else
		HANDLE_ERR_PRINT_LINE(DUKE_ERROR_NOT_SUPPORTED);

	if(status) {
		MY_PRINTF("error %d in opening dev by %d\n",status,type);
		return DUKE_ERROR_OPEN;
	}

	HANDLE_ERR_PRINT_LINE(FT_ResetDevice(*handle));
	HANDLE_ERR_PRINT_LINE(FT_SetBaudRate(*handle, 1000000.0));
    HANDLE_ERR_PRINT_LINE(FT_SetDataCharacteristics(*handle, FT_BITS_8, FT_STOP_BITS_1, FT_PARITY_NONE));
    HANDLE_ERR_PRINT_LINE(FT_SetFlowControl(*handle, FT_FLOW_RTS_CTS, 0x11, 0x13)); // Set data characteristics - Data bits, Stop bits, Parity
    HANDLE_ERR_PRINT_LINE(FT_SetTimeouts(*handle, 5000, 5000));
    HANDLE_ERR_PRINT_LINE(FT_SetLatencyTimer(*handle,0));
//    HANDLE_ERR_PRINT_LINE(FT_SetChars(*handle,0xaa,1,0x0,0x0));


	return DUKE_STATUS_OK;


}

void DukeReleaseHandle(DukeHandle handle)
{
#ifdef SIMULATION
	return;
#endif
	FT_Close(handle);
}













