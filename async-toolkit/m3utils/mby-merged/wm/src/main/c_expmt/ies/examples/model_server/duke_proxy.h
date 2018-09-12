/*
 * duke_proxy.h
 *
 *  Created on: Jan 22, 2015
 *      Author: hdanial-l
 */

#ifndef DUKE_PROXY_H_
#define DUKE_PROXY_H_
/*#ifdef __linux__
#include "ftd2xx.h"
#else
#include "ftd2xx_win.h"
#endif*/

typedef enum {
	DUKE_STATUS_OK = 0,
	DUKE_ERROR_READ_REGISTER = -1,
	DUKE_ERROR_WRITE_REGISTER = -2,
	DUKE_ERROR_DOWNLOAD = -3,
	DUKE_ERROR_CONF = -4,
	DUKE_ERROR_TIMEOUT = -5,
	DUKE_ERROR_FILE_ACCESS = -6,
	DUKE_ERROR_FTDI_READ = -7,
	DUKE_ERROR_FTDI_WRITE = -8,
	DUKE_ERROR_SBIOS = -9,
	DUKE_ERROR_OPEN = -10,
	DUKE_ERROR_SIZE = -11,
	DUKE_ERROR_CBIOS_BUSY = -12,
	DUKE_ERROR_CBIOS = -14,
	DUKE_ERROR_CONF_CBIOS = -15,
	DUKE_ERROR_NOT_SUPPORTED = -16,
	DUKE_ERROR_EEPROM_NOT_DONE = -17,
	DUKE_ERROR_GENERAL

} duke_status;

typedef void *DukeHandle;

#ifdef __cplusplus
extern "C" {
#endif


duke_status DukeGetHandle(DukeHandle* handle);
void		DukeReleaseHandle(DukeHandle handle);
duke_status DukeFTReadData(DukeHandle handle,char* buffer,unsigned int bytes,int purge_rx,int purge_tx,unsigned int* bytes_read);
duke_status DukeFTWriteData(DukeHandle handle,char* data,unsigned int size,int purge_rx,int purge_tx);
duke_status DukeReadRegister(DukeHandle handle,unsigned char card_offset,unsigned int start_offset,unsigned int* val,unsigned int n);
duke_status DukeWriteRegister(DukeHandle handle, unsigned char card_offset, unsigned int start_offset, unsigned int* val, unsigned int n);
duke_status DukePurge(DukeHandle handle,int purge_rx,int purge_tx);

struct duke_ft_device {
	unsigned long Flags;
	unsigned long Type;
	unsigned long ID;
	unsigned int LocId;
	char SerialNumber[16];
	char Description[64];
	DukeHandle handle;
};
typedef enum {
	DukeOpenByID,
	DukeOpenByLocID,
	DukeOpenBySerial,
	DukeOpenByDescription
} DukeOpenType;

duke_status DukeGetDevices(struct duke_ft_device* user_ft_devices,unsigned int len);
duke_status DukeGetDevicesCount(unsigned int* count);
duke_status DukeGetHandleBy(struct duke_ft_device* device,DukeOpenType type,DukeHandle* handle);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* DUKE_PROXY_H_ */
