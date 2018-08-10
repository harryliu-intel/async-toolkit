// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_common.h"

fm_status mbyModelReadCSR(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                          const fm_uint32 byte_addr,
                          fm_uint32 *value)
{
    fm_uint32 word_addr = (byte_addr / 4);
    fm_bool addr_vld = (word_addr < MBY_REGISTER_ARRAY_SIZE);
    *value = (addr_vld) ? regs[word_addr] : 0xBaadDeed;
    fm_status status = (addr_vld) ? FM_OK : FM_FAIL;
    return status;
}

fm_status mbyModelReadCSRMult(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                              const fm_uint32 byte_addr,
                              const fm_int len,
                              fm_uint32 *value) {
    fm_status status = FM_OK;
    for (fm_int i = 0; i < len; i++) {
        status = mbyModelReadCSR(regs, byte_addr + (4*i), &value[i]);
        if (status != FM_OK)
            break;
    }
    return status;
}

fm_status mbyModelWriteCSR(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                          const fm_uint32 byte_addr,
                          fm_uint32 value)
{
    fm_uint32 word_addr = (byte_addr / 4);
    fm_bool addr_vld = (word_addr < MBY_REGISTER_ARRAY_SIZE);
    regs[word_addr] = value;
    fm_status status = (addr_vld) ? FM_OK : FM_FAIL;
    return status;
}

fm_status mbyModelWriteCSRMult(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                              const fm_uint32 byte_addr,
                              const fm_int len,
                              const fm_uint32 *value) {
    fm_status status = FM_OK;
    for (fm_int i = 0; i < len; i++) {
        status = mbyModelWriteCSR(regs, byte_addr + (4*i), value[i]);
        if (status != FM_OK)
            break;
    }
    return status;
}

fm_status mbyModelReadCSR64(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                            const fm_uint32 byte_addr,
                            fm_uint64 *value)
{
    fm_uint32 word_addr = (byte_addr / 4);
    fm_bool addr_vld = (word_addr < MBY_REGISTER_ARRAY_SIZE);
	if (addr_vld)
	{
		*value = regs[word_addr + 1];
		*value = (*value << 32) | regs[word_addr];
	}
	else
		*value = 0xBaadDeedBaadDeed;

    return (addr_vld) ? FM_OK : FM_FAIL;
}

fm_status mbyModelWriteCSR64(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                            const fm_uint32 byte_addr,
                            fm_uint64 value)
{
    fm_uint32 word_addr = (byte_addr / 4);
    fm_bool addr_vld = (word_addr < MBY_REGISTER_ARRAY_SIZE);
	if (addr_vld)
	{
		regs[word_addr + 1] = value >> 32;
		regs[word_addr] = value & 0xffffffff;
	}

    return (addr_vld) ? FM_OK : FM_FAIL;
}
