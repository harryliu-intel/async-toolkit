#!/usr/bin/perl
# vim:ts=4:sw=4:expandtab

###############################################################################
# File:             Applications/TestPoint/Common/regdump2eeprom.pl
# Creation Date:    09/26/07
# Description:      
#
# INTEL CONFIDENTIAL
# Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
#
# The source code contained or described herein and all documents related
# to the source code ("Material") are owned by Intel Corporation or its
# suppliers or licensors. Title to the Material remains with Intel
# Corporation or its suppliers and licensors. The Material contains trade
# secrets and proprietary and confidential information of Intel or its
# suppliers and licensors. The Material is protected by worldwide copyright
# and trade secret laws and treaty provisions. No part of the Material may
# be used, copied, reproduced, modified, published, uploaded, posted,
# transmitted, distributed, or disclosed in any way without Intel's prior
# express written permission.
#
# No license under any patent, copyright, trade secret or other intellectual
# property right is granted to or conferred upon you by disclosure or 
# delivery of the Materials, either expressly, by implication, inducement,
# estoppel or otherwise. Any license under such intellectual property rights
# must be express and approved by Intel in writing.
#
# Generate a Tahoe, Bali or Alta EEPROM image based on a
# register dump.
#
# Output format default is:
#   0000: 00 01 00 0c 08 00 00 19 00 10 00 00 00 01 00 0c
#   0010: 54 00 00 1a 10 10 04 e4 00 10 04 e4 00 01 00 0c
#   0020: 04 00 00 00 04 01 00 0c 00 00 00 00 00 01 00 0c
#   0030: 5c 00 00 00 00 01 00 0c 60 00 00 00 00 01 00 83
#
# Output format with -x command-line option is an xml file
# suitable for use with the Aardvark USB EEPROM programmer.
#
# Output format with -i command-line option is an intel
# hex file suitable for an EEPROM programmer.

# TODO:
#  - some registers that are written more than once need to
#    be compared against the current value, not just the
#    default value, when deciding whether to skip the write
###############################################################################

use Getopt::Std;

use constant {
    UNDEF => 0,
    TAHOE => 1,
    BALI  => 2,
    ALTA  => 3
};

my %chipDef_hash = (
     "tahoe" => TAHOE,
     "bali"  => BALI,
     "alta"  => ALTA
);

use strict 'vars';

# Bus clock period in ns (affects EEPROM delay commands)
my $bus_clock_period_ns = 30;

# SPI clock divisor config (affects EEPROM delay commands)
my $spi_div_cfg = 0x19;

# Debug options
my $debugOption = 0;

# Which ports are we configuring?
my $configure_portmask = 0;

my $adjust_scheduler_tokens = 0;

# EEPROM commands, default values are for BALI
my %eeprom_command = (
     "write" => 0x01,
     "poll"  => undef,
     "wait"  => 0x10,
     "boot"  => 0x11,
     "lcnt"  => undef,
     "loop"  => undef,
     "end"   => 0xFF
);

# SPI mode, used by Alta's eeprom header
# See Alta specification, chapter 25
use constant {
    SPI_MODE_SINGLE      => 0,
    SPI_MODE_DUAL        => 1,
    SPI_MODE_QUAD        => 2,
    SPI_MODE_SINGLE_FAST => 3
};

# SPI speed, used by Alta eeprom header
# See Alta specification, chapter 25
use constant {
    SPI_SPEED_00_5      => 0,   # 0.5 MHz
    SPI_SPEED_01_0      => 1,   # 1.0 MHz
    SPI_SPEED_02_0      => 2,   # 2.0 MHz
    SPI_SPEED_03_9      => 3,   # 3.9 MHz
    SPI_SPEED_07_8      => 4,   # 7.8 MHz
    SPI_SPEED_15_6      => 5,   # 15.6 MHz
    SPI_SPEED_31_2      => 6,   # 31.2 MHz
    SPI_SPEED_62_5      => 7    # 62.5 MHz
};

use constant {
    INCLUDE_OUTPUT_FILE_START       => 1,
    INCLUDE_OUTPUT_FILE_END         => 2,
    INCLUDE_OUTPUT_FILE_START_END   => 3
};


use constant {
    SBUS_OP_RESET   => 0x20,
    SBUS_OP_WRITE   => 0x21,
    SBUS_OP_READ    => 0x22
};

use constant {
    SBUS_DEV_ADDR_SPICO         => 0x65,
    SBUS_DEV_BROAD_ADDR_SPICO   => 0xFD,
};

use constant {
    BIST_MARCH_WRITE_NORMAL_DESCENDING       => 0,
    BIST_MARCH_WRITE_NORMAL_DESCENDING_END   => 1,
    BIST_MARCH_READ_NORMAL_DESCENDING        => 2,
    BIST_MARCH_READ_NORMAL_DESCENDING_END    => 3,
    BIST_MARCH_WRITE_INVERTED_DESCENDING     => 4,
    BIST_MARCH_WRITE_INVERTED_DESCENDING_END => 5,
    BIST_MARCH_READ_INVERTED_DESCENDING      => 6,
    BIST_MARCH_READ_INVERTED_DESCENDING_END  => 7,
    BIST_MARCH_WRITE_NORMAL_ASCENDING        => 8,
    BIST_MARCH_WRITE_NORMAL_ASCENDING_END    => 9,
    BIST_MARCH_READ_NORMAL_ASCENDING         => 10,
    BIST_MARCH_READ_NORMAL_ASCENDING_END     => 11,
    BIST_MARCH_WRITE_INVERTED_ASCENDING      => 12,
    BIST_MARCH_WRITE_INVERTED_ASCENDING_END  => 13,
    BIST_MARCH_READ_INVERTED_ASCENDING       => 14,
    BIST_MARCH_READ_INVERTED_ASCENDING_END   => 15,
    BIST_MARCH_MAX_ELEMENTS                  => 32,

    BIST_MARCH_CONFIG_VT_CONSTANT            => 0,
    BIST_MARCH_CONFIG_VT_ADDRFOLD            => 1,
    BIST_MARCH_CONFIG_VT_DFLTCHK             => 2,
    BIST_MARCH_CONFIG_VT_USERCHK             => 3,
 
    SPDP_TEST_CORE_0                         => 2,
    SPDP_TEST_CORE_1                         => 3,
    SPDP_TEST_BOTH_CORES                     => 0
};

use constant {
    SPDP_TEST_CORE_0     => 2,
    SPDP_TEST_CORE_1     => 3,
    SPDP_TEST_BOTH_CORES => 0
};

my @marchTable = (
    BIST_MARCH_WRITE_NORMAL_ASCENDING_END,
    BIST_MARCH_READ_NORMAL_ASCENDING,
    BIST_MARCH_WRITE_INVERTED_ASCENDING_END,
    BIST_MARCH_READ_INVERTED_ASCENDING,
    BIST_MARCH_WRITE_NORMAL_ASCENDING_END,
    BIST_MARCH_READ_NORMAL_DESCENDING,
    BIST_MARCH_WRITE_INVERTED_DESCENDING_END,
    BIST_MARCH_READ_INVERTED_DESCENDING,
    BIST_MARCH_WRITE_NORMAL_DESCENDING_END,
    BIST_MARCH_READ_NORMAL_ASCENDING_END,

    BIST_MARCH_WRITE_NORMAL_ASCENDING_END,
    BIST_MARCH_WRITE_INVERTED_ASCENDING_END,
    BIST_MARCH_READ_INVERTED_ASCENDING,
    BIST_MARCH_WRITE_NORMAL_ASCENDING,
    BIST_MARCH_READ_NORMAL_ASCENDING_END,

    BIST_MARCH_WRITE_NORMAL_ASCENDING_END,
    BIST_MARCH_READ_NORMAL_ASCENDING_END,
    BIST_MARCH_READ_NORMAL_DESCENDING,
    BIST_MARCH_WRITE_INVERTED_DESCENDING_END,
    BIST_MARCH_WRITE_NORMAL_ASCENDING_END,
    BIST_MARCH_READ_NORMAL_ASCENDING,
    BIST_MARCH_WRITE_INVERTED_ASCENDING,
    BIST_MARCH_READ_INVERTED_ASCENDING_END,
    BIST_MARCH_READ_INVERTED_ASCENDING,
    BIST_MARCH_WRITE_NORMAL_ASCENDING_END,

    BIST_MARCH_WRITE_NORMAL_ASCENDING_END,
    BIST_MARCH_READ_NORMAL_ASCENDING,
    BIST_MARCH_WRITE_INVERTED_ASCENDING,
    BIST_MARCH_READ_INVERTED_ASCENDING_END
);

# SBus write access delay = 15 us
# TBD: adjust this in order to use the lower possible value.
my $sbus_wr_delay = 15;

# SBus device reset access delay
# TBD: adjust this in order to use the lower possible value.
# current spico access delay = 100 * wr_delay
my $sbus_reset_delay = 100 * $sbus_wr_delay;

# EEPROM Boot-command mask (default value is for Tahoe and Bali)
my $eepromBootCmmdMask      = 0x03;

# @clocks_per_port
#   - undef = use value from register dump (default)
#   - 0 = REFCLKA
#   - 1 = REFCLKB
#   - 2 = REFCLKA for 4-lane-ports and REFCLKB for 1-lane ports
my @clocks_per_port      = (undef) x 25;

# @wiring_info_per_port
#   - undef means do not modify reg values from register dump
#   - otherwise, a 10-bit value with the following meaning:
#     bit 9:    RX-lane-rev
#     bit 8:    TX-lane-rev
#     bit 7..4: RX-polarity-D..A (ignored on tahoe)
#     bit 3..0: TX-polarity-D..A (ignored on tahoe)
# Default is no lane or polarity reversal (a value of 0).
my @wiring_info_per_port = (0) x 25;

my %opt_r_char_to_cpp = (
        "a" => 0,
        "b" => 1,
        "s" => 2,
        "r" => undef,
);

my $eeprom_size_kb = 8;
my $chip_is = UNDEF;

# Bali boot command constants.
my $FM_BOOT_COMMAND_SCHED_INIT    = 0;
my $FM_BOOT_COMMAND_ASYNC_REPAIR  = 1;
my $FM_BOOT_COMMAND_INIT_MEM      = 2;
my $FM_BOOT_COMMAND_SYNC_REPAIR   = 3;


# Alta boot command constants
use constant {
    FM_ALTA_BOOT_CMMD_NONE                                  => 0,
    FM_ALTA_BOOT_CMMD_INIT_FFU_SLICE_NUMBERS                => 1,
    FM_ALTA_BOOT_CMMD_APPLY_BANK_MEMORY_REPAIRS             => 2,
    FM_ALTA_BOOT_CMMD_INITIALIZE_ALL_SCHEDULER_FREELISTS    => 3,
    FM_ALTA_BOOT_CMMD_INITIALIZE_ONLY_ARRAY_FREELIST        => 4,
    FM_ALTA_BOOT_CMMD_INITIALIZE_ONLY_HEAD_STORAGE_FREELIST => 5,
    FM_ALTA_BOOT_CMMD_INITIALIZE_ONLY_TXQ_FREELIST          => 6,
    FM_ALTA_BOOT_CMMD_INITIALIZE_ONLY_RXQ_FREELIST          => 7
};


# Alta CRM configuration
my @alta_crm_register_size = (
    0,          # 32 bits
    1 << 22,    # 64 bits
    2 << 22,    # 96 bits
    3 << 22     # 128 bits
);

# Alta PCIe lane reveral configs
my @alta_pcie_lane_reversal = (
    0,
    1<<29,
    2<<29,
    3<<29
);

# Alta: PCIE_REFCLK clock period is 8 ns.
my $pcie_refclk_period = 8;

# Alta: SPICO code
# Avago/65nm/dfe_2011_03_24_0x113.rom
my @alta_spico_code = (
    0x146, 0x000, 0x004, 0x000, 0x044, 0x047, 0x000, 0x347,
    0x037, 0x036, 0x362, 0x036, 0x001, 0x009, 0x005, 0x04e,
    0x000, 0x3fa, 0x362, 0x036, 0x002, 0x009, 0x003, 0x04f,
    0x362, 0x036, 0x003, 0x009, 0x003, 0x04e, 0x362, 0x036,
    0x004, 0x009, 0x003, 0x04e, 0x362, 0x036, 0x005, 0x009,
    0x00d, 0x347, 0x06e, 0x044, 0x347, 0x06e, 0x00e, 0x044,
    0x156, 0x005, 0x000, 0x3f2, 0x307, 0x038, 0x044, 0x0d7,
    0x004, 0x3c0, 0x001, 0x038, 0x307, 0x06d, 0x302, 0x038,
    0x00b, 0x3ca, 0x3c7, 0x001, 0x038, 0x000, 0x3c2, 0x3c4,
    0x1ff, 0x00f, 0x3c5, 0x200, 0x00f, 0x3c4, 0x1ff, 0x00f,
    0x3c7, 0x01e, 0x010, 0x3c7, 0x001, 0x011, 0x3c7, 0x002,
    0x012, 0x3c7, 0x3fc, 0x013, 0x3c7, 0x000, 0x01b, 0x3c7,
    0x014, 0x01a, 0x3c7, 0x014, 0x015, 0x349, 0x01a, 0x041,
    0x34b, 0x01b, 0x042, 0x3c7, 0x009, 0x014, 0x3c7, 0x00a,
    0x016, 0x3c7, 0x00f, 0x017, 0x3c7, 0x001, 0x018, 0x3c7,
    0x020, 0x019, 0x3c7, 0x000, 0x01c, 0x3c7, 0x004, 0x01d,
    0x3c7, 0x003, 0x01e, 0x3c7, 0x00d, 0x01f, 0x3c7, 0x020,
    0x020, 0x3c7, 0x005, 0x021, 0x3c7, 0x000, 0x022, 0x3c7,
    0x000, 0x023, 0x3c7, 0x000, 0x024, 0x3c7, 0x200, 0x025,
    0x3c7, 0x1ff, 0x026, 0x3c7, 0x000, 0x027, 0x3c7, 0x3ff,
    0x028, 0x3c7, 0x000, 0x029, 0x3c7, 0x002, 0x02a, 0x3c7,
    0x001, 0x02b, 0x3c7, 0x000, 0x02c, 0x3c7, 0x000, 0x02d,
    0x3c7, 0x000, 0x02e, 0x3c7, 0x03f, 0x02f, 0x3c7, 0x030,
    0x030, 0x3c7, 0x030, 0x031, 0x3c7, 0x000, 0x039, 0x3c7,
    0x000, 0x03a, 0x3c7, 0x000, 0x03b, 0x3c7, 0x000, 0x03c,
    0x3c7, 0x000, 0x03d, 0x3c7, 0x000, 0x03e, 0x3c7, 0x000,
    0x03f, 0x3c7, 0x000, 0x040, 0x3c7, 0x0d3, 0x08c, 0x387,
    0x000, 0x247, 0x2d5, 0x247, 0x05e, 0x247, 0x05f, 0x247,
    0x060, 0x247, 0x06c, 0x247, 0x033, 0x247, 0x032, 0x3c7,
    0x001, 0x038, 0x3c7, 0x000, 0x037, 0x3c7, 0x0fe, 0x044,
    0x3c7, 0x002, 0x045, 0x044, 0x2e4, 0x005, 0x3c2, 0x001,
    0x059, 0x347, 0x059, 0x06d, 0x3c7, 0x0fd, 0x044, 0x3c7,
    0x014, 0x045, 0x044, 0x2e4, 0x005, 0x347, 0x059, 0x043,
    0x3c7, 0x254, 0x04b, 0x3c7, 0x380, 0x04c, 0x044, 0x11e,
    0x000, 0x3c7, 0x000, 0x0d6, 0x3c7, 0x10f, 0x04b, 0x3c7,
    0x300, 0x04c, 0x044, 0x11e, 0x000, 0x045, 0x387, 0x000,
    0x3a7, 0x000, 0x3c7, 0x000, 0x04a, 0x307, 0x04a, 0x300,
    0x04b, 0x04d, 0x347, 0x04c, 0x04d, 0x340, 0x04a, 0x04d,
    0x001, 0x007, 0x3c0, 0x001, 0x04a, 0x000, 0x3f0, 0x045,
    0x3a7, 0x1ff, 0x387, 0x1ff, 0x04e, 0x382, 0x001, 0x009,
    0x3fd, 0x3a2, 0x001, 0x009, 0x3f7, 0x045, 0x347, 0x000,
    0x062, 0x347, 0x001, 0x063, 0x3c7, 0x000, 0x008, 0x3c7,
    0x000, 0x009, 0x307, 0x062, 0x384, 0x03f, 0x229, 0x220,
    0x387, 0x000, 0x3a0, 0x166, 0x381, 0x000, 0x041, 0x040,
    0x045, 0x3c7, 0x001, 0x008, 0x04e, 0x047, 0x020, 0x1db,
    0x000, 0x020, 0x1e1, 0x000, 0x020, 0x1e7, 0x000, 0x020,
    0x2bf, 0x000, 0x020, 0x256, 0x000, 0x020, 0x25f, 0x000,
    0x020, 0x265, 0x000, 0x020, 0x26b, 0x000, 0x020, 0x271,
    0x000, 0x020, 0x277, 0x000, 0x020, 0x27d, 0x000, 0x020,
    0x283, 0x000, 0x020, 0x289, 0x000, 0x020, 0x28f, 0x000,
    0x020, 0x295, 0x000, 0x020, 0x29b, 0x000, 0x020, 0x2a4,
    0x000, 0x020, 0x2aa, 0x000, 0x020, 0x2b0, 0x000, 0x020,
    0x2b4, 0x000, 0x020, 0x2c6, 0x000, 0x020, 0x2ca, 0x000,
    0x020, 0x1d5, 0x000, 0x020, 0x1d5, 0x000, 0x020, 0x2d5,
    0x000, 0x020, 0x2db, 0x000, 0x020, 0x2e1, 0x000, 0x020,
    0x2e7, 0x000, 0x020, 0x1d5, 0x000, 0x020, 0x1d5, 0x000,
    0x020, 0x1d5, 0x000, 0x020, 0x1d5, 0x000, 0x020, 0x2ed,
    0x000, 0x020, 0x304, 0x000, 0x020, 0x1d5, 0x000, 0x020,
    0x30d, 0x000, 0x020, 0x319, 0x000, 0x3c7, 0x3ff, 0x008,
    0x020, 0x164, 0x000, 0x3c7, 0x001, 0x009, 0x020, 0x161,
    0x000, 0x3c7, 0x113, 0x009, 0x020, 0x161, 0x000, 0x387,
    0x000, 0x227, 0x247, 0x068, 0x247, 0x069, 0x247, 0x06a,
    0x04a, 0x062, 0x340, 0x062, 0x068, 0x3c1, 0x000, 0x069,
    0x3c1, 0x000, 0x06a, 0x3c6, 0x0d8, 0x068, 0x349, 0x068,
    0x068, 0x34b, 0x069, 0x069, 0x34b, 0x06a, 0x06a, 0x00a,
    0x00b, 0x3c0, 0x0e5, 0x068, 0x3c1, 0x000, 0x069, 0x3c1,
    0x000, 0x06a, 0x2e2, 0x36c, 0x001, 0x008, 0x3a0, 0x001,
    0x381, 0x000, 0x000, 0x3d6, 0x263, 0x005, 0x009, 0x3f8,
    0x3a0, 0x001, 0x381, 0x000, 0x04a, 0x062, 0x346, 0x068,
    0x062, 0x362, 0x062, 0x2ef, 0x009, 0x024, 0x3a0, 0x001,
    0x381, 0x000, 0x04a, 0x062, 0x346, 0x069, 0x062, 0x362,
    0x062, 0x36f, 0x009, 0x016, 0x3a0, 0x001, 0x381, 0x000,
    0x04a, 0x062, 0x346, 0x06a, 0x062, 0x362, 0x062, 0x1ea,
    0x009, 0x008, 0x3c7, 0x001, 0x009, 0x020, 0x161, 0x000,
    0x3c7, 0x000, 0x009, 0x020, 0x161, 0x000, 0x3c7, 0x001,
    0x029, 0x3c7, 0x000, 0x016, 0x020, 0x161, 0x000, 0x347,
    0x063, 0x066, 0x020, 0x161, 0x000, 0x347, 0x063, 0x067,
    0x020, 0x161, 0x000, 0x347, 0x063, 0x068, 0x020, 0x161,
    0x000, 0x347, 0x063, 0x069, 0x020, 0x161, 0x000, 0x347,
    0x063, 0x06a, 0x020, 0x161, 0x000, 0x347, 0x063, 0x06b,
    0x020, 0x161, 0x000, 0x347, 0x068, 0x009, 0x020, 0x161,
    0x000, 0x347, 0x069, 0x009, 0x020, 0x161, 0x000, 0x347,
    0x06a, 0x009, 0x020, 0x161, 0x000, 0x347, 0x06b, 0x009,
    0x020, 0x161, 0x000, 0x347, 0x063, 0x037, 0x347, 0x037,
    0x036, 0x020, 0x161, 0x000, 0x347, 0x063, 0x036, 0x020,
    0x161, 0x000, 0x347, 0x063, 0x061, 0x020, 0x161, 0x000,
    0x307, 0x061, 0x000, 0x007, 0x307, 0x061, 0x3c0, 0x001,
    0x061, 0x327, 0x063, 0x04d, 0x020, 0x161, 0x000, 0x347,
    0x063, 0x061, 0x307, 0x061, 0x000, 0x00b, 0x307, 0x061,
    0x000, 0x007, 0x307, 0x061, 0x3c0, 0x001, 0x061, 0x04c,
    0x2c7, 0x009, 0x020, 0x161, 0x000, 0x347, 0x06c, 0x009,
    0x020, 0x161, 0x000, 0x347, 0x05e, 0x009, 0x020, 0x161,
    0x000, 0x347, 0x05f, 0x009, 0x020, 0x161, 0x000, 0x347,
    0x060, 0x009, 0x020, 0x161, 0x000, 0x307, 0x063, 0x380,
    0x10f, 0x04c, 0x2c7, 0x009, 0x3c4, 0x0ff, 0x009, 0x287,
    0x3a7, 0x008, 0x044, 0x2b4, 0x005, 0x384, 0x003, 0x247,
    0x008, 0x020, 0x164, 0x000, 0x347, 0x063, 0x037, 0x347,
    0x037, 0x036, 0x020, 0x161, 0x000, 0x3c7, 0x005, 0x037,
    0x347, 0x063, 0x06e, 0x3c7, 0x0ff, 0x009, 0x020, 0x164,
    0x000, 0x3c7, 0x003, 0x068, 0x3c7, 0x0f0, 0x009, 0x020,
    0x164, 0x000, 0x3c7, 0x000, 0x08d, 0x360, 0x247, 0x000,
    0x009, 0x010, 0x360, 0x029, 0x000, 0x001, 0x005, 0x044,
    0x2e2, 0x004, 0x347, 0x017, 0x076, 0x347, 0x01e, 0x08d,
    0x044, 0x024, 0x001, 0x347, 0x016, 0x071, 0x3c4, 0x00f,
    0x071, 0x347, 0x019, 0x070, 0x347, 0x08d, 0x072, 0x347,
    0x08d, 0x073, 0x347, 0x08d, 0x074, 0x347, 0x08d, 0x075,
    0x044, 0x2cd, 0x003, 0x044, 0x001, 0x004, 0x360, 0x07c,
    0x000, 0x001, 0x00c, 0x3c7, 0x001, 0x033, 0x347, 0x00e,
    0x032, 0x3c7, 0x003, 0x241, 0x045, 0x3c7, 0x00d, 0x045,
    0x3c7, 0x011, 0x059, 0x044, 0x343, 0x005, 0x04e, 0x3c0,
    0x000, 0x022, 0x009, 0x00b, 0x3c7, 0x026, 0x045, 0x3c7,
    0x005, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x000, 0x06f,
    0x044, 0x001, 0x004, 0x044, 0x3f8, 0x003, 0x006, 0x012,
    0x360, 0x070, 0x3c1, 0x001, 0x00d, 0x3c0, 0x001, 0x070,
    0x044, 0x2cd, 0x003, 0x044, 0x001, 0x004, 0x000, 0x3ed,
    0x347, 0x08d, 0x073, 0x347, 0x073, 0x073, 0x347, 0x073,
    0x074, 0x347, 0x073, 0x075, 0x3c7, 0x001, 0x072, 0x044,
    0x2cd, 0x003, 0x044, 0x001, 0x004, 0x044, 0x3e6, 0x003,
    0x00b, 0x007, 0x044, 0x01a, 0x001, 0x000, 0x041, 0x044,
    0x3ed, 0x000, 0x00b, 0x005, 0x044, 0x01a, 0x001, 0x3c0,
    0x001, 0x073, 0x307, 0x01f, 0x302, 0x073, 0x00b, 0x3d5,
    0x362, 0x06f, 0x002, 0x00f, 0x00a, 0x3c0, 0x001, 0x071,
    0x362, 0x071, 0x00f, 0x00b, 0x3c5, 0x044, 0x201, 0x003,
    0x360, 0x018, 0x000, 0x009, 0x005, 0x347, 0x019, 0x070,
    0x044, 0x2cd, 0x003, 0x04e, 0x360, 0x247, 0x000, 0x001,
    0x005, 0x044, 0x14a, 0x005, 0x045, 0x307, 0x078, 0x327,
    0x079, 0x302, 0x07a, 0x323, 0x07b, 0x045, 0x347, 0x070,
    0x04a, 0x3c2, 0x03f, 0x04a, 0x003, 0x004, 0x000, 0x017,
    0x044, 0x3e6, 0x003, 0x003, 0x004, 0x000, 0x010, 0x044,
    0x01a, 0x001, 0x3c0, 0x001, 0x070, 0x044, 0x2cd, 0x003,
    0x044, 0x001, 0x004, 0x000, 0x3e3, 0x3c0, 0x001, 0x06f,
    0x000, 0x3a7, 0x347, 0x078, 0x07a, 0x347, 0x079, 0x07b,
    0x044, 0x1eb, 0x003, 0x045, 0x3c7, 0x0df, 0x08c, 0x044,
    0x07e, 0x004, 0x3c7, 0x0d3, 0x08c, 0x3c7, 0x001, 0x08b,
    0x3c7, 0x00d, 0x045, 0x3c7, 0x091, 0x059, 0x044, 0x343,
    0x005, 0x3c7, 0x026, 0x045, 0x3c7, 0x005, 0x059, 0x044,
    0x343, 0x005, 0x3c7, 0x029, 0x045, 0x3c7, 0x000, 0x059,
    0x044, 0x343, 0x005, 0x044, 0x138, 0x000, 0x3c7, 0x030,
    0x045, 0x3c7, 0x030, 0x059, 0x044, 0x343, 0x005, 0x3c7,
    0x030, 0x045, 0x3c7, 0x000, 0x059, 0x044, 0x343, 0x005,
    0x044, 0x138, 0x000, 0x044, 0x101, 0x002, 0x044, 0x176,
    0x002, 0x044, 0x15e, 0x002, 0x3c7, 0x026, 0x045, 0x3c7,
    0x007, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x000, 0x20f,
    0x044, 0x18e, 0x002, 0x3c7, 0x3ff, 0x07a, 0x3c7, 0x1ff,
    0x07b, 0x045, 0x044, 0x0f2, 0x002, 0x3c7, 0x000, 0x099,
    0x044, 0x243, 0x003, 0x044, 0x217, 0x003, 0x3c0, 0x000,
    0x022, 0x001, 0x00a, 0x3c7, 0x001, 0x08b, 0x3c7, 0x007,
    0x059, 0x000, 0x008, 0x3c7, 0x000, 0x08b, 0x3c7, 0x005,
    0x059, 0x3c7, 0x026, 0x045, 0x360, 0x024, 0x000, 0x001,
    0x005, 0x3c5, 0x020, 0x059, 0x347, 0x059, 0x098, 0x044,
    0x343, 0x005, 0x3c7, 0x0d3, 0x08c, 0x044, 0x2cd, 0x003,
    0x3c7, 0x000, 0x08e, 0x307, 0x08f, 0x302, 0x08e, 0x00f,
    0x090, 0x307, 0x08e, 0x384, 0x018, 0x262, 0x018, 0x009,
    0x00c, 0x044, 0x167, 0x001, 0x3c7, 0x002, 0x20f, 0x044,
    0x18e, 0x002, 0x04e, 0x360, 0x02c, 0x000, 0x001, 0x071,
    0x307, 0x070, 0x040, 0x044, 0x2cd, 0x003, 0x044, 0x001,
    0x004, 0x044, 0x3e6, 0x003, 0x006, 0x018, 0x307, 0x02f,
    0x302, 0x070, 0x001, 0x021, 0x3c0, 0x001, 0x070, 0x044,
    0x2cd, 0x003, 0x044, 0x001, 0x004, 0x044, 0x3e6, 0x003,
    0x006, 0x004, 0x000, 0x3ec, 0x307, 0x019, 0x302, 0x070,
    0x001, 0x00b, 0x3c0, 0x3ff, 0x070, 0x044, 0x2cd, 0x003,
    0x044, 0x001, 0x004, 0x307, 0x070, 0x040, 0x307, 0x245,
    0x040, 0x3c7, 0x000, 0x245, 0x044, 0x1b6, 0x004, 0x042,
    0x247, 0x245, 0x042, 0x247, 0x070, 0x3c4, 0x0ff, 0x096,
    0x34d, 0x096, 0x02d, 0x20a, 0x2ac, 0x20a, 0x2ac, 0x20a,
    0x2ac, 0x20a, 0x2ac, 0x2c2, 0x070, 0x042, 0x227, 0x322,
    0x070, 0x001, 0x010, 0x007, 0x009, 0x247, 0x070, 0x3c0,
    0x001, 0x070, 0x000, 0x007, 0x247, 0x070, 0x3c0, 0x3ff,
    0x070, 0x044, 0x2cd, 0x003, 0x044, 0x001, 0x004, 0x044,
    0x0b4, 0x002, 0x044, 0x1da, 0x001, 0x000, 0x36e, 0x044,
    0x2cd, 0x003, 0x3c7, 0x005, 0x059, 0x360, 0x024, 0x000,
    0x001, 0x005, 0x3c7, 0x025, 0x059, 0x3c7, 0x026, 0x045,
    0x044, 0x343, 0x005, 0x044, 0x0e3, 0x002, 0x045, 0x360,
    0x247, 0x000, 0x009, 0x06f, 0x360, 0x012, 0x000, 0x001,
    0x06a, 0x307, 0x076, 0x300, 0x013, 0x003, 0x064, 0x3c7,
    0x000, 0x0a0, 0x307, 0x072, 0x260, 0x3f1, 0x009, 0x005,
    0x3c0, 0x001, 0x0a0, 0x307, 0x073, 0x260, 0x3f1, 0x009,
    0x005, 0x3c0, 0x001, 0x0a0, 0x307, 0x074, 0x260, 0x3f1,
    0x009, 0x005, 0x3c0, 0x001, 0x0a0, 0x307, 0x075, 0x260,
    0x3f1, 0x009, 0x005, 0x3c0, 0x001, 0x0a0, 0x307, 0x0a0,
    0x302, 0x012, 0x00e, 0x037, 0x30a, 0x017, 0x302, 0x076,
    0x006, 0x031, 0x307, 0x019, 0x302, 0x070, 0x001, 0x005,
    0x3c0, 0x3ff, 0x070, 0x360, 0x072, 0x000, 0x001, 0x005,
    0x3c0, 0x3ff, 0x072, 0x360, 0x073, 0x000, 0x001, 0x005,
    0x3c0, 0x3ff, 0x073, 0x360, 0x074, 0x000, 0x001, 0x005,
    0x3c0, 0x3ff, 0x074, 0x360, 0x075, 0x000, 0x001, 0x005,
    0x3c0, 0x3ff, 0x075, 0x340, 0x013, 0x076, 0x044, 0x2cd,
    0x003, 0x045, 0x3c0, 0x001, 0x039, 0x3c1, 0x000, 0x03a,
    0x3c0, 0x001, 0x099, 0x347, 0x071, 0x086, 0x3c2, 0x001,
    0x086, 0x00b, 0x005, 0x3c7, 0x000, 0x086, 0x360, 0x027,
    0x000, 0x001, 0x017, 0x360, 0x247, 0x000, 0x001, 0x012,
    0x347, 0x072, 0x087, 0x3c2, 0x001, 0x087, 0x360, 0x087,
    0x00f, 0x00b, 0x012, 0x3c7, 0x3f1, 0x087, 0x000, 0x00d,
    0x347, 0x072, 0x087, 0x3c2, 0x001, 0x087, 0x00b, 0x005,
    0x3c7, 0x000, 0x087, 0x347, 0x073, 0x088, 0x3c2, 0x001,
    0x088, 0x00b, 0x005, 0x3c7, 0x000, 0x088, 0x347, 0x074,
    0x089, 0x3c2, 0x001, 0x089, 0x00b, 0x005, 0x3c7, 0x000,
    0x089, 0x347, 0x075, 0x08a, 0x3c2, 0x001, 0x08a, 0x00b,
    0x005, 0x3c7, 0x000, 0x08a, 0x347, 0x071, 0x081, 0x3c0,
    0x001, 0x081, 0x362, 0x081, 0x00f, 0x00b, 0x005, 0x3c7,
    0x00f, 0x081, 0x347, 0x072, 0x082, 0x3c0, 0x001, 0x082,
    0x362, 0x082, 0x00f, 0x00b, 0x005, 0x3c7, 0x00f, 0x082,
    0x347, 0x073, 0x083, 0x3c0, 0x001, 0x083, 0x362, 0x083,
    0x00f, 0x00b, 0x005, 0x3c7, 0x00f, 0x083, 0x347, 0x074,
    0x084, 0x3c0, 0x001, 0x084, 0x362, 0x084, 0x00f, 0x00b,
    0x005, 0x3c7, 0x00f, 0x084, 0x347, 0x075, 0x085, 0x3c0,
    0x001, 0x085, 0x362, 0x085, 0x00f, 0x00b, 0x005, 0x3c7,
    0x00f, 0x085, 0x307, 0x016, 0x384, 0x200, 0x001, 0x00e,
    0x347, 0x016, 0x081, 0x347, 0x016, 0x086, 0x3c4, 0x00f,
    0x081, 0x3c4, 0x00f, 0x086, 0x307, 0x011, 0x001, 0x069,
    0x307, 0x08e, 0x20a, 0x20a, 0x20a, 0x384, 0x003, 0x260,
    0x000, 0x009, 0x016, 0x347, 0x073, 0x083, 0x347, 0x073,
    0x088, 0x347, 0x074, 0x084, 0x347, 0x074, 0x089, 0x347,
    0x075, 0x085, 0x347, 0x075, 0x08a, 0x000, 0x04a, 0x260,
    0x3ff, 0x009, 0x016, 0x347, 0x071, 0x081, 0x347, 0x071,
    0x086, 0x347, 0x074, 0x084, 0x347, 0x074, 0x089, 0x347,
    0x075, 0x085, 0x347, 0x075, 0x08a, 0x000, 0x032, 0x260,
    0x3fe, 0x009, 0x016, 0x347, 0x071, 0x081, 0x347, 0x071,
    0x086, 0x347, 0x072, 0x082, 0x347, 0x072, 0x087, 0x347,
    0x075, 0x085, 0x347, 0x075, 0x08a, 0x000, 0x01a, 0x260,
    0x3fd, 0x009, 0x016, 0x347, 0x071, 0x081, 0x347, 0x071,
    0x086, 0x347, 0x072, 0x082, 0x347, 0x072, 0x087, 0x347,
    0x073, 0x083, 0x347, 0x073, 0x088, 0x000, 0x002, 0x044,
    0x1eb, 0x003, 0x3c7, 0x3ff, 0x090, 0x3c7, 0x1ff, 0x091,
    0x3c7, 0x000, 0x092, 0x3c7, 0x200, 0x093, 0x347, 0x081,
    0x071, 0x347, 0x082, 0x072, 0x347, 0x083, 0x073, 0x347,
    0x084, 0x074, 0x347, 0x085, 0x075, 0x044, 0x0b2, 0x002,
    0x044, 0x2cd, 0x003, 0x044, 0x001, 0x004, 0x347, 0x01a,
    0x09f, 0x347, 0x01b, 0x09e, 0x34a, 0x09e, 0x09e, 0x34c,
    0x09f, 0x09f, 0x307, 0x078, 0x327, 0x079, 0x302, 0x09f,
    0x323, 0x09e, 0x006, 0x073, 0x3c7, 0x001, 0x09a, 0x347,
    0x02b, 0x09b, 0x3c7, 0x000, 0x09c, 0x3c7, 0x000, 0x09d,
    0x3c7, 0x001, 0x20f, 0x044, 0x18e, 0x002, 0x340, 0x096,
    0x09c, 0x341, 0x097, 0x09d, 0x3c0, 0x3ff, 0x09b, 0x009,
    0x3f1, 0x347, 0x02b, 0x09b, 0x34a, 0x09b, 0x09b, 0x001,
    0x00d, 0x34a, 0x09d, 0x09d, 0x34c, 0x09c, 0x09c, 0x34a,
    0x09b, 0x09b, 0x009, 0x3f7, 0x347, 0x09c, 0x096, 0x347,
    0x09d, 0x097, 0x307, 0x096, 0x327, 0x097, 0x302, 0x092,
    0x323, 0x093, 0x00e, 0x049, 0x387, 0x000, 0x327, 0x025,
    0x3a4, 0x0ff, 0x320, 0x094, 0x301, 0x095, 0x322, 0x096,
    0x303, 0x097, 0x00e, 0x018, 0x364, 0x025, 0x200, 0x001,
    0x013, 0x307, 0x08e, 0x20a, 0x20a, 0x20a, 0x384, 0x003,
    0x260, 0x000, 0x009, 0x008, 0x307, 0x081, 0x302, 0x071,
    0x001, 0x023, 0x347, 0x096, 0x092, 0x347, 0x097, 0x093,
    0x044, 0x1eb, 0x003, 0x000, 0x018, 0x3c7, 0x000, 0x09a,
    0x044, 0x3ef, 0x003, 0x006, 0x010, 0x360, 0x093, 0x200,
    0x009, 0x00b, 0x347, 0x078, 0x090, 0x347, 0x079, 0x091,
    0x044, 0x1eb, 0x003, 0x3c2, 0x001, 0x075, 0x307, 0x075,
    0x302, 0x08a, 0x00b, 0x353, 0x3c2, 0x001, 0x074, 0x307,
    0x074, 0x302, 0x089, 0x00b, 0x347, 0x3c2, 0x001, 0x073,
    0x307, 0x073, 0x302, 0x088, 0x00b, 0x33b, 0x3c2, 0x001,
    0x072, 0x307, 0x072, 0x302, 0x087, 0x00b, 0x32f, 0x3c2,
    0x001, 0x071, 0x307, 0x071, 0x302, 0x086, 0x00b, 0x323,
    0x360, 0x02e, 0x000, 0x001, 0x00b, 0x044, 0x201, 0x003,
    0x044, 0x037, 0x002, 0x044, 0x1eb, 0x003, 0x3c7, 0x001,
    0x08b, 0x044, 0x201, 0x003, 0x044, 0x0b2, 0x002, 0x044,
    0x2cd, 0x003, 0x044, 0x001, 0x004, 0x3c0, 0x000, 0x022,
    0x009, 0x005, 0x3c7, 0x000, 0x08b, 0x347, 0x092, 0x094,
    0x347, 0x093, 0x095, 0x362, 0x093, 0x200, 0x001, 0x01d,
    0x3c0, 0x008, 0x08e, 0x360, 0x245, 0x000, 0x009, 0x015,
    0x360, 0x023, 0x000, 0x009, 0x010, 0x327, 0x092, 0x2aa,
    0x2aa, 0x3a4, 0x0ff, 0x3a5, 0x000, 0x307, 0x00e, 0x380,
    0x10f, 0x04d, 0x045, 0x3c0, 0x008, 0x08e, 0x045, 0x3c7,
    0x000, 0x075, 0x3c7, 0x1ff, 0x0a3, 0x3c7, 0x3ff, 0x0a2,
    0x044, 0x2cd, 0x003, 0x044, 0x001, 0x004, 0x307, 0x078,
    0x327, 0x079, 0x302, 0x0a2, 0x323, 0x0a3, 0x006, 0x00b,
    0x347, 0x078, 0x0a2, 0x347, 0x079, 0x0a3, 0x347, 0x075,
    0x0a1, 0x044, 0x3e6, 0x003, 0x006, 0x012, 0x362, 0x070,
    0x03f, 0x001, 0x00d, 0x3c0, 0x001, 0x070, 0x3c7, 0x1ff,
    0x0a3, 0x3c7, 0x3ff, 0x0a2, 0x000, 0x3d4, 0x362, 0x075,
    0x00f, 0x001, 0x007, 0x3c0, 0x001, 0x075, 0x000, 0x3ca,
    0x347, 0x0a1, 0x075, 0x044, 0x2cd, 0x003, 0x044, 0x001,
    0x004, 0x347, 0x078, 0x0a2, 0x347, 0x079, 0x0a3, 0x307,
    0x070, 0x040, 0x044, 0x22d, 0x003, 0x042, 0x247, 0x070,
    0x044, 0x2cd, 0x003, 0x044, 0x001, 0x004, 0x342, 0x0a2,
    0x078, 0x343, 0x0a3, 0x079, 0x00e, 0x00f, 0x044, 0x201,
    0x003, 0x347, 0x0a1, 0x075, 0x044, 0x2cd, 0x003, 0x044,
    0x217, 0x003, 0x045, 0x044, 0x22d, 0x003, 0x044, 0x2cd,
    0x003, 0x045, 0x04e, 0x045, 0x044, 0x001, 0x004, 0x044,
    0x3e6, 0x003, 0x00e, 0x028, 0x044, 0x101, 0x002, 0x347,
    0x07d, 0x0a4, 0x3c0, 0x280, 0x0a4, 0x3c0, 0x001, 0x07d,
    0x044, 0x15e, 0x002, 0x307, 0x0a4, 0x302, 0x07d, 0x001,
    0x013, 0x044, 0x001, 0x004, 0x007, 0x3f1, 0x3c0, 0x001,
    0x07d, 0x044, 0x15e, 0x002, 0x3c0, 0x001, 0x07d, 0x044,
    0x15e, 0x002, 0x045, 0x360, 0x024, 0x000, 0x001, 0x00b,
    0x3c7, 0x030, 0x045, 0x3c7, 0x003, 0x059, 0x044, 0x343,
    0x005, 0x045, 0x360, 0x024, 0x000, 0x001, 0x00b, 0x3c7,
    0x030, 0x045, 0x3c7, 0x000, 0x059, 0x044, 0x343, 0x005,
    0x045, 0x3c7, 0x028, 0x045, 0x044, 0x353, 0x005, 0x347,
    0x059, 0x047, 0x3c4, 0x07f, 0x047, 0x347, 0x047, 0x059,
    0x360, 0x043, 0x3fd, 0x009, 0x041, 0x3e0, 0x000, 0x000,
    0x34a, 0x047, 0x044, 0x34a, 0x044, 0x045, 0x34a, 0x045,
    0x046, 0x34a, 0x046, 0x046, 0x346, 0x047, 0x044, 0x34a,
    0x044, 0x045, 0x34a, 0x045, 0x045, 0x346, 0x044, 0x045,
    0x34a, 0x045, 0x046, 0x34a, 0x046, 0x046, 0x34a, 0x046,
    0x046, 0x34a, 0x046, 0x046, 0x346, 0x045, 0x046, 0x347,
    0x046, 0x059, 0x307, 0x047, 0x384, 0x070, 0x009, 0x00e,
    0x347, 0x045, 0x059, 0x307, 0x047, 0x384, 0x00c, 0x009,
    0x005, 0x347, 0x044, 0x059, 0x347, 0x059, 0x07d, 0x3c4,
    0x07f, 0x07d, 0x3c7, 0x000, 0x07e, 0x045, 0x3c7, 0x029,
    0x045, 0x307, 0x07d, 0x384, 0x07f, 0x227, 0x360, 0x043,
    0x3fd, 0x009, 0x004, 0x22a, 0x226, 0x2c9, 0x059, 0x3c5,
    0x001, 0x059, 0x044, 0x343, 0x005, 0x045, 0x3c7, 0x029,
    0x045, 0x307, 0x07d, 0x384, 0x07f, 0x227, 0x360, 0x043,
    0x3fd, 0x009, 0x004, 0x22a, 0x226, 0x2c9, 0x059, 0x3c4,
    0x0fe, 0x059, 0x044, 0x343, 0x005, 0x045, 0x3c0, 0x001,
    0x03d, 0x3c1, 0x000, 0x03e, 0x044, 0x101, 0x002, 0x347,
    0x07d, 0x04b, 0x3c7, 0x210, 0x04a, 0x347, 0x07d, 0x04c,
    0x3c0, 0x040, 0x04c, 0x3c7, 0x001, 0x04d, 0x3c7, 0x001,
    0x04e, 0x044, 0x04f, 0x003, 0x347, 0x07d, 0x21a, 0x347,
    0x04b, 0x04c, 0x3c7, 0x3ff, 0x04d, 0x3c7, 0x000, 0x04e,
    0x044, 0x04f, 0x003, 0x3c7, 0x21b, 0x04a, 0x347, 0x07d,
    0x04c, 0x3c0, 0x3c0, 0x04c, 0x3c7, 0x3ff, 0x04d, 0x3c7,
    0x001, 0x04e, 0x044, 0x04f, 0x003, 0x347, 0x07d, 0x225,
    0x307, 0x21a, 0x327, 0x225, 0x282, 0x20a, 0x300, 0x225,
    0x247, 0x04f, 0x247, 0x04c, 0x307, 0x20f, 0x382, 0x001,
    0x009, 0x005, 0x347, 0x04b, 0x04c, 0x307, 0x20f, 0x382,
    0x002, 0x009, 0x01a, 0x307, 0x04f, 0x302, 0x04b, 0x007,
    0x006, 0x00e, 0x00c, 0x000, 0x010, 0x347, 0x04b, 0x04c,
    0x3c0, 0x001, 0x04c, 0x000, 0x008, 0x347, 0x04b, 0x04c,
    0x3c0, 0x3ff, 0x04c, 0x3c7, 0x001, 0x04d, 0x3c7, 0x000,
    0x04e, 0x044, 0x04f, 0x003, 0x307, 0x20f, 0x382, 0x001,
    0x001, 0x003, 0x045, 0x044, 0x26f, 0x002, 0x045, 0x307,
    0x219, 0x327, 0x218, 0x302, 0x217, 0x323, 0x216, 0x00e,
    0x049, 0x307, 0x217, 0x327, 0x216, 0x302, 0x215, 0x323,
    0x214, 0x00e, 0x03f, 0x307, 0x215, 0x327, 0x214, 0x302,
    0x213, 0x323, 0x212, 0x00e, 0x035, 0x307, 0x213, 0x327,
    0x212, 0x302, 0x211, 0x323, 0x210, 0x00e, 0x02b, 0x307,
    0x224, 0x327, 0x223, 0x302, 0x222, 0x323, 0x221, 0x00e,
    0x021, 0x307, 0x222, 0x327, 0x221, 0x302, 0x220, 0x323,
    0x21f, 0x00e, 0x017, 0x307, 0x220, 0x327, 0x21f, 0x302,
    0x21e, 0x323, 0x21d, 0x00e, 0x00d, 0x307, 0x21e, 0x327,
    0x21d, 0x302, 0x21c, 0x323, 0x21b, 0x00e, 0x003, 0x045,
    0x3c0, 0x001, 0x03f, 0x3c1, 0x000, 0x040, 0x045, 0x044,
    0x217, 0x002, 0x347, 0x210, 0x0a9, 0x347, 0x211, 0x0aa,
    0x349, 0x0aa, 0x0aa, 0x34b, 0x0a9, 0x0a9, 0x347, 0x218,
    0x0ad, 0x347, 0x219, 0x0ae, 0x349, 0x0ae, 0x0ae, 0x34b,
    0x0ad, 0x0ad, 0x347, 0x218, 0x0af, 0x347, 0x219, 0x0b0,
    0x340, 0x219, 0x0b0, 0x341, 0x218, 0x0af, 0x340, 0x219,
    0x0b0, 0x341, 0x218, 0x0af, 0x340, 0x219, 0x0b0, 0x341,
    0x218, 0x0af, 0x340, 0x219, 0x0b0, 0x341, 0x218, 0x0af,
    0x340, 0x219, 0x0b0, 0x341, 0x218, 0x0af, 0x347, 0x217,
    0x0b2, 0x347, 0x216, 0x0b1, 0x349, 0x0b2, 0x0b2, 0x34b,
    0x0b1, 0x0b1, 0x349, 0x0b2, 0x0b2, 0x34b, 0x0b1, 0x0b1,
    0x347, 0x215, 0x0ac, 0x347, 0x214, 0x0ab, 0x349, 0x0ac,
    0x0ac, 0x34b, 0x0ab, 0x0ab, 0x347, 0x0ae, 0x0a5, 0x347,
    0x0ad, 0x0a6, 0x340, 0x217, 0x0a5, 0x341, 0x216, 0x0a6,
    0x342, 0x213, 0x0a5, 0x343, 0x212, 0x0a6, 0x342, 0x0aa,
    0x0a5, 0x343, 0x0a9, 0x0a6, 0x347, 0x0a5, 0x0b4, 0x347,
    0x0a6, 0x0b3, 0x347, 0x21c, 0x0ba, 0x347, 0x21b, 0x0b9,
    0x349, 0x0ba, 0x0ba, 0x34b, 0x0b9, 0x0b9, 0x347, 0x224,
    0x0be, 0x347, 0x223, 0x0bd, 0x349, 0x0be, 0x0be, 0x34b,
    0x0bd, 0x0bd, 0x347, 0x224, 0x0c0, 0x347, 0x223, 0x0bf,
    0x340, 0x224, 0x0c0, 0x341, 0x223, 0x0bf, 0x340, 0x224,
    0x0c0, 0x341, 0x223, 0x0bf, 0x340, 0x224, 0x0c0, 0x341,
    0x223, 0x0bf, 0x340, 0x224, 0x0c0, 0x341, 0x223, 0x0bf,
    0x340, 0x224, 0x0c0, 0x341, 0x223, 0x0bf, 0x347, 0x222,
    0x0c2, 0x347, 0x221, 0x0c1, 0x349, 0x0c2, 0x0c2, 0x34b,
    0x0c1, 0x0c1, 0x349, 0x0c2, 0x0c2, 0x34b, 0x0c1, 0x0c1,
    0x347, 0x220, 0x0bc, 0x347, 0x21f, 0x0bb, 0x349, 0x0bc,
    0x0bc, 0x34b, 0x0bb, 0x0bb, 0x347, 0x0be, 0x0a5, 0x347,
    0x0bd, 0x0a6, 0x340, 0x222, 0x0a5, 0x341, 0x221, 0x0a6,
    0x342, 0x21e, 0x0a5, 0x343, 0x21d, 0x0a6, 0x342, 0x0ba,
    0x0a5, 0x343, 0x0b9, 0x0a6, 0x347, 0x0a5, 0x0c4, 0x347,
    0x0a6, 0x0c3, 0x347, 0x0b0, 0x0a5, 0x347, 0x0af, 0x0a6,
    0x340, 0x0b2, 0x0a5, 0x341, 0x0b1, 0x0a6, 0x340, 0x0ac,
    0x0a5, 0x341, 0x0ab, 0x0a6, 0x342, 0x0aa, 0x0a5, 0x343,
    0x0a9, 0x0a6, 0x342, 0x01c, 0x0a5, 0x343, 0x01d, 0x0a6,
    0x347, 0x0a5, 0x0b6, 0x347, 0x0a6, 0x0b5, 0x347, 0x0c0,
    0x0a5, 0x347, 0x0bf, 0x0a6, 0x340, 0x0c2, 0x0a5, 0x341,
    0x0c1, 0x0a6, 0x340, 0x0bc, 0x0a5, 0x341, 0x0bb, 0x0a6,
    0x342, 0x0ba, 0x0a5, 0x343, 0x0b9, 0x0a6, 0x342, 0x01c,
    0x0a5, 0x343, 0x01d, 0x0a6, 0x347, 0x0a5, 0x0c6, 0x347,
    0x0a6, 0x0c5, 0x340, 0x0b4, 0x0b6, 0x341, 0x0b3, 0x0b5,
    0x347, 0x0b6, 0x078, 0x347, 0x0b5, 0x079, 0x044, 0x0d4,
    0x003, 0x347, 0x078, 0x0b6, 0x347, 0x079, 0x0b5, 0x347,
    0x0b4, 0x078, 0x347, 0x0b3, 0x079, 0x044, 0x0d4, 0x003,
    0x347, 0x078, 0x0b4, 0x347, 0x079, 0x0b3, 0x347, 0x0b6,
    0x059, 0x347, 0x0b5, 0x05a, 0x342, 0x0b4, 0x059, 0x343,
    0x0b3, 0x05a, 0x044, 0x0a1, 0x003, 0x3cd, 0x040, 0x21a,
    0x247, 0x0a6, 0x2c7, 0x0a5, 0x342, 0x059, 0x0a5, 0x343,
    0x05a, 0x0a6, 0x3c0, 0x040, 0x0a5, 0x3c1, 0x000, 0x0a6,
    0x347, 0x0a5, 0x0b7, 0x347, 0x0a6, 0x0b8, 0x340, 0x0c4,
    0x0c6, 0x341, 0x0c3, 0x0c5, 0x347, 0x0c6, 0x078, 0x347,
    0x0c5, 0x079, 0x044, 0x0d4, 0x003, 0x347, 0x078, 0x0c6,
    0x347, 0x079, 0x0c5, 0x347, 0x0c4, 0x078, 0x347, 0x0c3,
    0x079, 0x044, 0x0d4, 0x003, 0x347, 0x078, 0x0c4, 0x347,
    0x079, 0x0c3, 0x347, 0x0c6, 0x059, 0x347, 0x0c5, 0x05a,
    0x342, 0x0c4, 0x059, 0x343, 0x0c3, 0x05a, 0x044, 0x0a1,
    0x003, 0x3cd, 0x040, 0x225, 0x247, 0x0a6, 0x2c7, 0x0a5,
    0x340, 0x059, 0x0a5, 0x341, 0x05a, 0x0a6, 0x3c2, 0x040,
    0x0a5, 0x3c3, 0x000, 0x0a6, 0x347, 0x0b7, 0x0a7, 0x347,
    0x0b8, 0x0a8, 0x342, 0x0a5, 0x0b7, 0x343, 0x0a6, 0x0b8,
    0x347, 0x0b7, 0x096, 0x347, 0x0b8, 0x097, 0x045, 0x044,
    0x15e, 0x002, 0x3c0, 0x000, 0x04e, 0x001, 0x03f, 0x044,
    0x001, 0x004, 0x044, 0x3e6, 0x003, 0x00a, 0x037, 0x3c7,
    0x004, 0x23c, 0x044, 0x0d4, 0x003, 0x307, 0x04a, 0x327,
    0x079, 0x04d, 0x3c0, 0x001, 0x04a, 0x307, 0x04a, 0x327,
    0x078, 0x04d, 0x3c0, 0x001, 0x04a, 0x340, 0x04d, 0x07d,
    0x044, 0x15e, 0x002, 0x044, 0x001, 0x004, 0x044, 0x0d4,
    0x003, 0x3c2, 0x001, 0x23c, 0x009, 0x3e1, 0x307, 0x04a,
    0x327, 0x079, 0x04d, 0x3c0, 0x001, 0x04a, 0x307, 0x04a,
    0x327, 0x078, 0x04d, 0x045, 0x340, 0x04d, 0x07d, 0x307,
    0x04c, 0x302, 0x07d, 0x009, 0x3b4, 0x044, 0x15e, 0x002,
    0x045, 0x327, 0x059, 0x3a4, 0x03f, 0x387, 0x000, 0x3a0,
    0x1ab, 0x381, 0x003, 0x049, 0x041, 0x307, 0x059, 0x327,
    0x05a, 0x384, 0x3c0, 0x2aa, 0x20c, 0x2aa, 0x20c, 0x2aa,
    0x20c, 0x2aa, 0x20c, 0x2aa, 0x20c, 0x2aa, 0x20c, 0x3c7,
    0x000, 0x05a, 0x043, 0x3a5, 0x040, 0x380, 0x000, 0x001,
    0x00a, 0x2a9, 0x34b, 0x05a, 0x05a, 0x380, 0x3ff, 0x009,
    0x3fa, 0x2c7, 0x059, 0x045, 0x347, 0x078, 0x053, 0x347,
    0x079, 0x052, 0x3c7, 0x000, 0x054, 0x3c7, 0x010, 0x056,
    0x044, 0x13e, 0x003, 0x044, 0x13e, 0x003, 0x044, 0x13e,
    0x003, 0x044, 0x13e, 0x003, 0x044, 0x13e, 0x003, 0x347,
    0x078, 0x053, 0x347, 0x079, 0x052, 0x307, 0x054, 0x380,
    0x004, 0x3a7, 0x000, 0x34a, 0x052, 0x052, 0x34c, 0x053,
    0x053, 0x2ac, 0x380, 0x3ff, 0x009, 0x3f7, 0x3a4, 0x03f,
    0x387, 0x000, 0x3a0, 0x16b, 0x381, 0x003, 0x049, 0x3c7,
    0x000, 0x055, 0x349, 0x054, 0x054, 0x34b, 0x055, 0x055,
    0x349, 0x054, 0x054, 0x34b, 0x055, 0x055, 0x349, 0x054,
    0x054, 0x34b, 0x055, 0x055, 0x349, 0x054, 0x054, 0x34b,
    0x055, 0x055, 0x349, 0x054, 0x054, 0x34b, 0x055, 0x055,
    0x349, 0x054, 0x054, 0x34b, 0x055, 0x055, 0x325, 0x054,
    0x2c7, 0x078, 0x347, 0x055, 0x079, 0x045, 0x307, 0x056,
    0x347, 0x052, 0x057, 0x347, 0x053, 0x058, 0x34a, 0x052,
    0x052, 0x34c, 0x053, 0x053, 0x380, 0x3ff, 0x009, 0x3f8,
    0x3c0, 0x000, 0x053, 0x009, 0x011, 0x3c0, 0x000, 0x052,
    0x009, 0x00c, 0x347, 0x057, 0x052, 0x347, 0x058, 0x053,
    0x34a, 0x056, 0x056, 0x045, 0x340, 0x056, 0x054, 0x34a,
    0x056, 0x056, 0x045, 0x000, 0x001, 0x003, 0x004, 0x006,
    0x007, 0x008, 0x00a, 0x00b, 0x00c, 0x00d, 0x00f, 0x010,
    0x011, 0x012, 0x013, 0x015, 0x016, 0x017, 0x018, 0x019,
    0x01a, 0x01b, 0x01c, 0x01d, 0x01e, 0x01f, 0x020, 0x022,
    0x023, 0x023, 0x024, 0x025, 0x026, 0x027, 0x028, 0x029,
    0x02a, 0x02b, 0x02c, 0x02d, 0x02e, 0x02f, 0x02f, 0x030,
    0x031, 0x032, 0x033, 0x034, 0x034, 0x035, 0x036, 0x037,
    0x038, 0x038, 0x039, 0x03a, 0x03b, 0x03c, 0x03c, 0x03d,
    0x03e, 0x03f, 0x03f, 0x000, 0x001, 0x001, 0x002, 0x003,
    0x004, 0x004, 0x005, 0x006, 0x007, 0x007, 0x008, 0x009,
    0x00a, 0x00a, 0x00b, 0x00c, 0x00d, 0x00e, 0x00f, 0x00f,
    0x010, 0x011, 0x012, 0x013, 0x014, 0x015, 0x016, 0x017,
    0x018, 0x019, 0x01a, 0x01b, 0x01b, 0x01c, 0x01d, 0x01f,
    0x020, 0x021, 0x022, 0x023, 0x024, 0x025, 0x026, 0x027,
    0x028, 0x029, 0x02a, 0x02c, 0x02d, 0x02e, 0x02f, 0x030,
    0x032, 0x033, 0x034, 0x035, 0x037, 0x038, 0x039, 0x03b,
    0x03c, 0x03d, 0x03f, 0x347, 0x070, 0x0c7, 0x347, 0x071,
    0x0c8, 0x347, 0x075, 0x0cc, 0x347, 0x074, 0x0cb, 0x347,
    0x073, 0x0ca, 0x347, 0x072, 0x0c9, 0x347, 0x076, 0x0cd,
    0x045, 0x347, 0x0c7, 0x070, 0x347, 0x0c8, 0x071, 0x347,
    0x0cc, 0x075, 0x347, 0x0cb, 0x074, 0x347, 0x0ca, 0x073,
    0x347, 0x0c9, 0x072, 0x347, 0x0cd, 0x076, 0x045, 0x347,
    0x070, 0x0ce, 0x347, 0x071, 0x0cf, 0x347, 0x075, 0x0d3,
    0x347, 0x074, 0x0d2, 0x347, 0x073, 0x0d1, 0x347, 0x072,
    0x0d0, 0x347, 0x076, 0x0d4, 0x045, 0x347, 0x0ce, 0x070,
    0x347, 0x0cf, 0x071, 0x347, 0x0d3, 0x075, 0x347, 0x0d2,
    0x074, 0x347, 0x0d1, 0x073, 0x347, 0x0d0, 0x072, 0x347,
    0x0d4, 0x076, 0x045, 0x3c7, 0x023, 0x045, 0x044, 0x353,
    0x005, 0x347, 0x059, 0x0d5, 0x3c4, 0x001, 0x0d5, 0x32a,
    0x059, 0x3a4, 0x00f, 0x044, 0x0bf, 0x004, 0x247, 0x076,
    0x32a, 0x059, 0x3a4, 0x070, 0x044, 0x2c7, 0x005, 0x2c7,
    0x072, 0x3c7, 0x022, 0x045, 0x044, 0x353, 0x005, 0x329,
    0x059, 0x3a4, 0x002, 0x2a9, 0x2a9, 0x325, 0x072, 0x044,
    0x0bf, 0x004, 0x360, 0x0d5, 0x000, 0x001, 0x005, 0x38d,
    0x3ff, 0x287, 0x247, 0x072, 0x32a, 0x059, 0x3a4, 0x00f,
    0x044, 0x0bf, 0x004, 0x247, 0x073, 0x32a, 0x059, 0x3a4,
    0x070, 0x044, 0x2c7, 0x005, 0x2c7, 0x074, 0x3c7, 0x021,
    0x045, 0x044, 0x353, 0x005, 0x329, 0x059, 0x3a4, 0x002,
    0x2a9, 0x2a9, 0x325, 0x074, 0x044, 0x0bf, 0x004, 0x247,
    0x074, 0x32a, 0x059, 0x3a4, 0x00f, 0x044, 0x0bf, 0x004,
    0x247, 0x075, 0x32a, 0x059, 0x3a4, 0x070, 0x044, 0x2c7,
    0x005, 0x2c7, 0x071, 0x3c7, 0x020, 0x045, 0x044, 0x353,
    0x005, 0x329, 0x059, 0x3a4, 0x002, 0x2a9, 0x2a9, 0x325,
    0x071, 0x044, 0x0bf, 0x004, 0x247, 0x071, 0x34a, 0x059,
    0x070, 0x3c4, 0x03f, 0x070, 0x045, 0x360, 0x029, 0x000,
    0x001, 0x007, 0x360, 0x08b, 0x000, 0x009, 0x006, 0x044,
    0x302, 0x003, 0x045, 0x3c7, 0x000, 0x08b, 0x044, 0x302,
    0x003, 0x3c7, 0x016, 0x045, 0x3c7, 0x001, 0x059, 0x309,
    0x024, 0x245, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x001,
    0x08b, 0x044, 0x302, 0x003, 0x3c7, 0x016, 0x045, 0x3c7,
    0x000, 0x059, 0x309, 0x024, 0x245, 0x059, 0x044, 0x343,
    0x005, 0x045, 0x36d, 0x0d6, 0x007, 0x287, 0x380, 0x0d7,
    0x327, 0x070, 0x04d, 0x380, 0x001, 0x327, 0x071, 0x04d,
    0x380, 0x001, 0x327, 0x072, 0x04d, 0x380, 0x001, 0x327,
    0x073, 0x04d, 0x380, 0x001, 0x327, 0x074, 0x04d, 0x380,
    0x001, 0x327, 0x075, 0x04d, 0x380, 0x001, 0x327, 0x076,
    0x04d, 0x3c0, 0x001, 0x0d6, 0x3c4, 0x007, 0x0d6, 0x327,
    0x071, 0x044, 0x0a1, 0x004, 0x247, 0x07f, 0x20a, 0x20a,
    0x24a, 0x059, 0x309, 0x070, 0x245, 0x059, 0x307, 0x08b,
    0x001, 0x008, 0x3c7, 0x01f, 0x045, 0x044, 0x343, 0x005,
    0x3c7, 0x023, 0x045, 0x044, 0x343, 0x005, 0x309, 0x07f,
    0x044, 0x2bf, 0x005, 0x247, 0x059, 0x327, 0x075, 0x044,
    0x0a1, 0x004, 0x209, 0x245, 0x059, 0x327, 0x074, 0x044,
    0x0a1, 0x004, 0x247, 0x07f, 0x20a, 0x20a, 0x20a, 0x245,
    0x059, 0x307, 0x08b, 0x001, 0x008, 0x3c7, 0x020, 0x045,
    0x044, 0x343, 0x005, 0x3c7, 0x024, 0x045, 0x044, 0x343,
    0x005, 0x309, 0x07f, 0x044, 0x2bf, 0x005, 0x247, 0x059,
    0x327, 0x073, 0x044, 0x0a1, 0x004, 0x209, 0x245, 0x059,
    0x327, 0x072, 0x044, 0x0a1, 0x004, 0x247, 0x07f, 0x20a,
    0x20a, 0x20a, 0x245, 0x059, 0x307, 0x08b, 0x001, 0x008,
    0x3c7, 0x021, 0x045, 0x044, 0x343, 0x005, 0x3c7, 0x025,
    0x045, 0x044, 0x343, 0x005, 0x309, 0x07f, 0x044, 0x2bf,
    0x005, 0x247, 0x059, 0x327, 0x076, 0x044, 0x0a1, 0x004,
    0x209, 0x245, 0x059, 0x360, 0x072, 0x000, 0x00b, 0x005,
    0x3c5, 0x001, 0x059, 0x307, 0x08b, 0x001, 0x008, 0x3c7,
    0x02e, 0x045, 0x044, 0x343, 0x005, 0x3c7, 0x02f, 0x045,
    0x044, 0x343, 0x005, 0x045, 0x327, 0x071, 0x044, 0x0a1,
    0x004, 0x20a, 0x20a, 0x24a, 0x059, 0x309, 0x070, 0x245,
    0x059, 0x3c7, 0x01f, 0x045, 0x044, 0x343, 0x005, 0x3c7,
    0x023, 0x045, 0x044, 0x343, 0x005, 0x045, 0x307, 0x078,
    0x327, 0x079, 0x302, 0x01a, 0x323, 0x01b, 0x045, 0x307,
    0x078, 0x327, 0x079, 0x302, 0x090, 0x323, 0x091, 0x045,
    0x307, 0x078, 0x327, 0x079, 0x302, 0x041, 0x323, 0x042,
    0x045, 0x3c0, 0x001, 0x03b, 0x3c1, 0x000, 0x03c, 0x347,
    0x08c, 0x059, 0x3c7, 0x004, 0x045, 0x044, 0x343, 0x005,
    0x3c4, 0x0bf, 0x059, 0x044, 0x343, 0x005, 0x347, 0x24a,
    0x046, 0x340, 0x014, 0x046, 0x387, 0x001, 0x3a7, 0x000,
    0x209, 0x2ab, 0x3c0, 0x3ff, 0x046, 0x009, 0x3fb, 0x382,
    0x001, 0x3a3, 0x000, 0x00b, 0x3fc, 0x3c4, 0x03f, 0x059,
    0x044, 0x343, 0x005, 0x3c7, 0x007, 0x045, 0x044, 0x353,
    0x005, 0x307, 0x059, 0x009, 0x039, 0x3c7, 0x008, 0x045,
    0x044, 0x353, 0x005, 0x309, 0x059, 0x264, 0x1e0, 0x009,
    0x02d, 0x044, 0x2bf, 0x005, 0x249, 0x079, 0x3c7, 0x009,
    0x045, 0x044, 0x353, 0x005, 0x307, 0x059, 0x384, 0x003,
    0x044, 0x2bc, 0x005, 0x249, 0x078, 0x30a, 0x059, 0x20a,
    0x245, 0x079, 0x3c7, 0x00a, 0x045, 0x044, 0x353, 0x005,
    0x345, 0x059, 0x078, 0x009, 0x00f, 0x307, 0x079, 0x009,
    0x00b, 0x247, 0x07c, 0x045, 0x3c7, 0x3ff, 0x079, 0x3c7,
    0x3ff, 0x078, 0x3c7, 0x001, 0x07c, 0x045, 0x3c7, 0x000,
    0x24a, 0x044, 0x001, 0x004, 0x327, 0x015, 0x362, 0x078,
    0x000, 0x323, 0x079, 0x00a, 0x00c, 0x360, 0x24a, 0x3f9,
    0x001, 0x007, 0x3c0, 0x001, 0x24a, 0x000, 0x3ec, 0x347,
    0x078, 0x034, 0x347, 0x079, 0x035, 0x044, 0x222, 0x004,
    0x045, 0x2e0, 0x000, 0x00b, 0x004, 0x3ad, 0x3ff, 0x387,
    0x000, 0x3a0, 0x0af, 0x381, 0x004, 0x048, 0x045, 0x000,
    0x001, 0x003, 0x002, 0x006, 0x007, 0x005, 0x004, 0x00c,
    0x00d, 0x00f, 0x00e, 0x00a, 0x00b, 0x009, 0x008, 0x387,
    0x000, 0x3a0, 0x0c7, 0x381, 0x004, 0x048, 0x045, 0x000,
    0x001, 0x003, 0x002, 0x007, 0x006, 0x004, 0x005, 0x00f,
    0x00e, 0x00c, 0x00d, 0x008, 0x009, 0x00b, 0x00a, 0x247,
    0x044, 0x347, 0x044, 0x00e, 0x3c7, 0x0ff, 0x045, 0x044,
    0x353, 0x005, 0x360, 0x059, 0x3ff, 0x009, 0x0d0, 0x3c7,
    0x00f, 0x045, 0x044, 0x353, 0x005, 0x364, 0x059, 0x001,
    0x001, 0x0c5, 0x044, 0x267, 0x004, 0x360, 0x249, 0x000,
    0x001, 0x0b5, 0x360, 0x242, 0x000, 0x001, 0x01c, 0x360,
    0x241, 0x000, 0x009, 0x009, 0x360, 0x244, 0x000, 0x009,
    0x004, 0x000, 0x010, 0x3c7, 0x000, 0x241, 0x3c7, 0x000,
    0x244, 0x3c7, 0x000, 0x249, 0x044, 0x222, 0x004, 0x000,
    0x09e, 0x3c7, 0x001, 0x243, 0x044, 0x222, 0x004, 0x3c7,
    0x000, 0x243, 0x044, 0x222, 0x004, 0x360, 0x241, 0x3fd,
    0x001, 0x08d, 0x360, 0x244, 0x3fd, 0x001, 0x088, 0x360,
    0x241, 0x3fe, 0x001, 0x01e, 0x3c7, 0x001, 0x241, 0x044,
    0x222, 0x004, 0x360, 0x247, 0x000, 0x001, 0x005, 0x044,
    0x332, 0x004, 0x044, 0x322, 0x000, 0x360, 0x241, 0x3ff,
    0x009, 0x005, 0x3c7, 0x002, 0x241, 0x044, 0x222, 0x004,
    0x360, 0x246, 0x000, 0x009, 0x00f, 0x360, 0x240, 0x000,
    0x001, 0x046, 0x3c7, 0x000, 0x240, 0x044, 0x222, 0x004,
    0x000, 0x03e, 0x360, 0x246, 0x3fd, 0x001, 0x007, 0x360,
    0x240, 0x3ff, 0x001, 0x034, 0x347, 0x010, 0x08f, 0x349,
    0x08f, 0x08f, 0x349, 0x08f, 0x08f, 0x349, 0x08f, 0x08f,
    0x360, 0x246, 0x3fe, 0x001, 0x007, 0x3c7, 0x020, 0x08f,
    0x000, 0x007, 0x360, 0x240, 0x3ff, 0x001, 0x019, 0x3c7,
    0x001, 0x244, 0x044, 0x222, 0x004, 0x044, 0x082, 0x001,
    0x360, 0x246, 0x3fd, 0x001, 0x005, 0x3c7, 0x001, 0x240,
    0x3c7, 0x002, 0x244, 0x044, 0x222, 0x004, 0x360, 0x023,
    0x000, 0x001, 0x00c, 0x360, 0x247, 0x000, 0x001, 0x007,
    0x044, 0x10e, 0x005, 0x000, 0x00a, 0x360, 0x245, 0x000,
    0x001, 0x005, 0x044, 0x1b6, 0x004, 0x045, 0x044, 0x0f2,
    0x002, 0x044, 0x243, 0x003, 0x307, 0x070, 0x040, 0x347,
    0x020, 0x070, 0x044, 0x2cd, 0x003, 0x044, 0x0b4, 0x002,
    0x3c7, 0x000, 0x20f, 0x044, 0x18e, 0x002, 0x360, 0x245,
    0x3fe, 0x009, 0x014, 0x307, 0x21a, 0x327, 0x225, 0x282,
    0x247, 0x096, 0x3c0, 0x3f8, 0x096, 0x3c4, 0x0ff, 0x096,
    0x3c5, 0x200, 0x096, 0x000, 0x029, 0x3c7, 0x000, 0x070,
    0x3c7, 0x000, 0x096, 0x044, 0x2cd, 0x003, 0x044, 0x001,
    0x004, 0x044, 0x3e6, 0x003, 0x00b, 0x00a, 0x362, 0x096,
    0x0ff, 0x001, 0x010, 0x3c0, 0x001, 0x096, 0x307, 0x02f,
    0x302, 0x070, 0x001, 0x007, 0x3c0, 0x001, 0x070, 0x000,
    0x3e4, 0x3c5, 0x100, 0x096, 0x042, 0x247, 0x070, 0x044,
    0x2cd, 0x003, 0x360, 0x245, 0x000, 0x001, 0x009, 0x327,
    0x096, 0x307, 0x00e, 0x380, 0x10f, 0x04d, 0x044, 0x0e3,
    0x002, 0x045, 0x3c7, 0x000, 0x059, 0x307, 0x240, 0x3a7,
    0x007, 0x044, 0x2ac, 0x005, 0x245, 0x059, 0x307, 0x241,
    0x3a7, 0x005, 0x044, 0x2ac, 0x005, 0x245, 0x059, 0x307,
    0x244, 0x3a7, 0x003, 0x044, 0x2ac, 0x005, 0x245, 0x059,
    0x307, 0x249, 0x3a7, 0x001, 0x044, 0x2ac, 0x005, 0x245,
    0x059, 0x307, 0x243, 0x3a7, 0x002, 0x044, 0x2ac, 0x005,
    0x245, 0x059, 0x3c7, 0x02b, 0x045, 0x044, 0x343, 0x005,
    0x3c7, 0x000, 0x059, 0x307, 0x24a, 0x209, 0x245, 0x059,
    0x3c7, 0x034, 0x045, 0x044, 0x343, 0x005, 0x045, 0x3c7,
    0x01d, 0x045, 0x044, 0x353, 0x005, 0x347, 0x059, 0x23d,
    0x3c7, 0x01e, 0x045, 0x044, 0x353, 0x005, 0x347, 0x059,
    0x23e, 0x3c7, 0x01f, 0x045, 0x044, 0x353, 0x005, 0x347,
    0x059, 0x23f, 0x307, 0x23d, 0x384, 0x007, 0x247, 0x24a,
    0x307, 0x23d, 0x3a7, 0x003, 0x044, 0x2b4, 0x005, 0x384,
    0x001, 0x247, 0x242, 0x307, 0x23f, 0x3a7, 0x002, 0x044,
    0x2b4, 0x005, 0x384, 0x020, 0x247, 0x247, 0x307, 0x23f,
    0x3a7, 0x006, 0x044, 0x2b4, 0x005, 0x384, 0x001, 0x247,
    0x240, 0x307, 0x23f, 0x3a7, 0x004, 0x044, 0x2b4, 0x005,
    0x384, 0x003, 0x247, 0x241, 0x307, 0x23f, 0x3a7, 0x002,
    0x044, 0x2b4, 0x005, 0x384, 0x003, 0x247, 0x244, 0x307,
    0x23e, 0x3a7, 0x004, 0x044, 0x2b4, 0x005, 0x384, 0x003,
    0x247, 0x248, 0x307, 0x23e, 0x3a7, 0x002, 0x044, 0x2b4,
    0x005, 0x384, 0x003, 0x247, 0x245, 0x307, 0x23e, 0x384,
    0x003, 0x247, 0x246, 0x307, 0x23f, 0x384, 0x001, 0x247,
    0x249, 0x045, 0x3c7, 0x018, 0x045, 0x3c7, 0x0aa, 0x059,
    0x044, 0x343, 0x005, 0x3c7, 0x019, 0x045, 0x3c7, 0x0a8,
    0x059, 0x044, 0x343, 0x005, 0x3c7, 0x015, 0x045, 0x3c7,
    0x000, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x016, 0x045,
    0x3c7, 0x000, 0x059, 0x309, 0x024, 0x245, 0x059, 0x044,
    0x343, 0x005, 0x3c7, 0x017, 0x045, 0x3c7, 0x0f0, 0x059,
    0x345, 0x02a, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x01a,
    0x045, 0x3c7, 0x001, 0x059, 0x044, 0x343, 0x005, 0x3c7,
    0x032, 0x045, 0x3c7, 0x00f, 0x059, 0x044, 0x343, 0x005,
    0x3c7, 0x033, 0x045, 0x3c7, 0x00f, 0x059, 0x044, 0x343,
    0x005, 0x045, 0x3c7, 0x006, 0x045, 0x3c7, 0x008, 0x059,
    0x044, 0x343, 0x005, 0x044, 0x2e2, 0x004, 0x3c7, 0x017,
    0x045, 0x3c7, 0x0f0, 0x059, 0x3c5, 0x003, 0x059, 0x044,
    0x343, 0x005, 0x044, 0x024, 0x001, 0x387, 0x000, 0x247,
    0x070, 0x247, 0x071, 0x247, 0x072, 0x247, 0x073, 0x247,
    0x074, 0x247, 0x075, 0x247, 0x076, 0x247, 0x077, 0x044,
    0x0cc, 0x005, 0x044, 0x001, 0x004, 0x360, 0x07c, 0x000,
    0x001, 0x006, 0x3c7, 0x003, 0x241, 0x045, 0x3c7, 0x00d,
    0x045, 0x3c7, 0x011, 0x059, 0x044, 0x343, 0x005, 0x347,
    0x030, 0x24d, 0x347, 0x030, 0x24c, 0x347, 0x031, 0x24e,
    0x3c7, 0x1ff, 0x250, 0x3c7, 0x1ff, 0x251, 0x3c7, 0x000,
    0x252, 0x3c7, 0x000, 0x253, 0x3c7, 0x000, 0x24b, 0x3c7,
    0x000, 0x070, 0x044, 0x0c2, 0x005, 0x2c7, 0x077, 0x3c7,
    0x000, 0x076, 0x044, 0x0cc, 0x005, 0x044, 0x0b0, 0x005,
    0x360, 0x24b, 0x000, 0x009, 0x011, 0x364, 0x059, 0x040,
    0x009, 0x00c, 0x3c7, 0x080, 0x033, 0x347, 0x00e, 0x032,
    0x3c7, 0x003, 0x241, 0x045, 0x044, 0x09c, 0x005, 0x044,
    0x0b2, 0x002, 0x360, 0x24b, 0x000, 0x009, 0x018, 0x307,
    0x070, 0x302, 0x24c, 0x00e, 0x012, 0x347, 0x070, 0x24d,
    0x360, 0x070, 0x3c1, 0x009, 0x00a, 0x3c7, 0x00e, 0x252,
    0x3c7, 0x000, 0x253, 0x000, 0x0b0, 0x347, 0x070, 0x04a,
    0x342, 0x24c, 0x04a, 0x00b, 0x008, 0x348, 0x04a, 0x04a,
    0x3c0, 0x001, 0x04a, 0x347, 0x250, 0x04b, 0x342, 0x24c,
    0x04b, 0x00b, 0x008, 0x348, 0x04b, 0x04b, 0x3c0, 0x001,
    0x04b, 0x342, 0x04a, 0x04b, 0x00e, 0x008, 0x347, 0x070,
    0x250, 0x347, 0x077, 0x252, 0x347, 0x24d, 0x24c, 0x360,
    0x24b, 0x3f9, 0x001, 0x007, 0x3c0, 0x001, 0x24b, 0x000,
    0x388, 0x3c7, 0x000, 0x077, 0x3c7, 0x000, 0x070, 0x3c7,
    0x000, 0x076, 0x3c7, 0x000, 0x24f, 0x044, 0x0b2, 0x002,
    0x044, 0x0cc, 0x005, 0x3c7, 0x000, 0x070, 0x044, 0x0cc,
    0x005, 0x044, 0x0b0, 0x005, 0x360, 0x076, 0x000, 0x009,
    0x011, 0x364, 0x059, 0x040, 0x009, 0x00c, 0x3c7, 0x081,
    0x033, 0x347, 0x00e, 0x032, 0x3c7, 0x003, 0x241, 0x045,
    0x044, 0x09c, 0x005, 0x044, 0x0b2, 0x002, 0x347, 0x070,
    0x04a, 0x342, 0x24e, 0x04a, 0x00b, 0x008, 0x348, 0x04a,
    0x04a, 0x3c0, 0x001, 0x04a, 0x347, 0x251, 0x04b, 0x342,
    0x24e, 0x04b, 0x00b, 0x008, 0x348, 0x04b, 0x04b, 0x3c0,
    0x001, 0x04b, 0x342, 0x04a, 0x04b, 0x00e, 0x01c, 0x307,
    0x24e, 0x302, 0x070, 0x00f, 0x00d, 0x347, 0x070, 0x251,
    0x347, 0x076, 0x253, 0x347, 0x070, 0x24f, 0x000, 0x00b,
    0x347, 0x24f, 0x251, 0x347, 0x076, 0x253, 0x347, 0x070,
    0x24f, 0x360, 0x076, 0x3f1, 0x001, 0x007, 0x3c0, 0x001,
    0x076, 0x000, 0x39a, 0x347, 0x252, 0x077, 0x347, 0x253,
    0x076, 0x044, 0x0cc, 0x005, 0x044, 0x0b2, 0x002, 0x3c7,
    0x017, 0x045, 0x3c7, 0x0f0, 0x059, 0x345, 0x02a, 0x059,
    0x044, 0x343, 0x005, 0x045, 0x3c0, 0x001, 0x070, 0x044,
    0x0cc, 0x005, 0x044, 0x0b0, 0x005, 0x364, 0x059, 0x040,
    0x001, 0x007, 0x360, 0x070, 0x3c1, 0x009, 0x3ef, 0x045,
    0x3c7, 0x014, 0x045, 0x044, 0x353, 0x005, 0x3c4, 0x0c0,
    0x059, 0x045, 0x000, 0x002, 0x006, 0x004, 0x00c, 0x00e,
    0x00a, 0x008, 0x327, 0x24b, 0x387, 0x000, 0x3a0, 0x0ba,
    0x381, 0x005, 0x049, 0x045, 0x327, 0x077, 0x307, 0x00e,
    0x20a, 0x00a, 0x006, 0x2a9, 0x2a9, 0x2a9, 0x2a9, 0x380,
    0x254, 0x04d, 0x307, 0x077, 0x3a7, 0x004, 0x044, 0x2ac,
    0x005, 0x3c7, 0x015, 0x045, 0x247, 0x059, 0x044, 0x343,
    0x005, 0x044, 0x2cd, 0x003, 0x045, 0x044, 0x243, 0x003,
    0x387, 0x000, 0x247, 0x08b, 0x247, 0x071, 0x247, 0x072,
    0x247, 0x073, 0x247, 0x074, 0x247, 0x075, 0x044, 0x2cd,
    0x003, 0x3c7, 0x017, 0x045, 0x3c7, 0x0f1, 0x059, 0x044,
    0x343, 0x005, 0x044, 0x09c, 0x005, 0x045, 0x044, 0x0ed,
    0x005, 0x307, 0x00e, 0x380, 0x10f, 0x04c, 0x322, 0x070,
    0x00b, 0x004, 0x3ad, 0x3ff, 0x322, 0x023, 0x00e, 0x010,
    0x360, 0x023, 0x000, 0x001, 0x00b, 0x3c7, 0x000, 0x241,
    0x3c7, 0x000, 0x240, 0x044, 0x222, 0x004, 0x3c7, 0x017,
    0x045, 0x3c7, 0x0f0, 0x059, 0x345, 0x02a, 0x059, 0x044,
    0x343, 0x005, 0x044, 0x243, 0x003, 0x044, 0x2cd, 0x003,
    0x3c7, 0x006, 0x045, 0x3c7, 0x088, 0x059, 0x044, 0x343,
    0x005, 0x045, 0x044, 0x0ed, 0x005, 0x327, 0x070, 0x307,
    0x00e, 0x380, 0x10f, 0x04d, 0x000, 0x3da, 0x3c7, 0x0ff,
    0x045, 0x044, 0x353, 0x005, 0x360, 0x059, 0x3fa, 0x009,
    0x145, 0x3c7, 0x0fe, 0x045, 0x044, 0x353, 0x005, 0x360,
    0x059, 0x000, 0x009, 0x13a, 0x3c7, 0x001, 0x045, 0x044,
    0x353, 0x005, 0x360, 0x059, 0x3f9, 0x001, 0x130, 0x3c7,
    0x00e, 0x045, 0x044, 0x2e4, 0x005, 0x3c4, 0x001, 0x059,
    0x001, 0x124, 0x3c7, 0x00f, 0x045, 0x3c7, 0x082, 0x059,
    0x044, 0x2cc, 0x005, 0x044, 0x138, 0x000, 0x3c0, 0x3fe,
    0x00e, 0x3c7, 0x00d, 0x045, 0x3c7, 0x0c1, 0x059, 0x044,
    0x343, 0x005, 0x3c7, 0x000, 0x045, 0x3c7, 0x037, 0x059,
    0x044, 0x343, 0x005, 0x3c7, 0x036, 0x045, 0x3c7, 0x040,
    0x059, 0x044, 0x343, 0x005, 0x3c7, 0x03b, 0x045, 0x3c7,
    0x040, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x022, 0x045,
    0x3c7, 0x000, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x05a,
    0x045, 0x3c7, 0x003, 0x059, 0x044, 0x343, 0x005, 0x3c7,
    0x063, 0x045, 0x3c7, 0x006, 0x059, 0x044, 0x343, 0x005,
    0x3c7, 0x01b, 0x045, 0x3c7, 0x001, 0x059, 0x044, 0x343,
    0x005, 0x3c7, 0x01c, 0x045, 0x3c7, 0x001, 0x059, 0x044,
    0x343, 0x005, 0x3c7, 0x019, 0x045, 0x3c7, 0x0a8, 0x059,
    0x044, 0x343, 0x005, 0x3c7, 0x01a, 0x045, 0x3c7, 0x001,
    0x059, 0x044, 0x343, 0x005, 0x3c7, 0x022, 0x045, 0x3c7,
    0x003, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x015, 0x045,
    0x3c7, 0x000, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x016,
    0x045, 0x3c7, 0x000, 0x059, 0x044, 0x343, 0x005, 0x3c7,
    0x017, 0x045, 0x3c7, 0x010, 0x059, 0x044, 0x343, 0x005,
    0x3c7, 0x003, 0x045, 0x3c7, 0x001, 0x059, 0x044, 0x343,
    0x005, 0x3c7, 0x009, 0x045, 0x3c7, 0x001, 0x059, 0x044,
    0x343, 0x005, 0x3c7, 0x007, 0x045, 0x3c7, 0x080, 0x059,
    0x044, 0x343, 0x005, 0x04e, 0x044, 0x138, 0x000, 0x044,
    0x138, 0x000, 0x044, 0x138, 0x000, 0x044, 0x138, 0x000,
    0x3c7, 0x053, 0x045, 0x3c7, 0x080, 0x059, 0x044, 0x343,
    0x005, 0x3c7, 0x026, 0x045, 0x3c7, 0x000, 0x059, 0x044,
    0x343, 0x005, 0x3c7, 0x004, 0x045, 0x3c7, 0x000, 0x059,
    0x044, 0x343, 0x005, 0x3c7, 0x029, 0x045, 0x3c7, 0x000,
    0x059, 0x044, 0x343, 0x005, 0x3c7, 0x03e, 0x045, 0x3c7,
    0x000, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x00a, 0x045,
    0x3c7, 0x000, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x063,
    0x045, 0x3c7, 0x002, 0x059, 0x044, 0x343, 0x005, 0x3c7,
    0x00d, 0x045, 0x3c7, 0x010, 0x059, 0x044, 0x343, 0x005,
    0x3c0, 0x001, 0x00e, 0x3c7, 0x001, 0x045, 0x3c7, 0x001,
    0x059, 0x044, 0x343, 0x005, 0x3c7, 0x000, 0x045, 0x3c7,
    0x001, 0x059, 0x044, 0x343, 0x005, 0x3c7, 0x000, 0x037,
    0x3c7, 0x000, 0x036, 0x045, 0x045, 0x3c7, 0x000, 0x037,
    0x3c7, 0x000, 0x036, 0x045, 0x3a0, 0x3ff, 0x003, 0x005,
    0x209, 0x000, 0x3fb, 0x045, 0x3a0, 0x3ff, 0x003, 0x005,
    0x20a, 0x000, 0x3fb, 0x045, 0x209, 0x209, 0x209, 0x209,
    0x209, 0x209, 0x209, 0x045, 0x2aa, 0x2aa, 0x2aa, 0x2aa,
    0x2aa, 0x2aa, 0x2aa, 0x045, 0x364, 0x005, 0x020, 0x009,
    0x3fd, 0x044, 0x32c, 0x005, 0x3c7, 0x041, 0x00f, 0x044,
    0x304, 0x005, 0x384, 0x01c, 0x262, 0x004, 0x001, 0x005,
    0x3c0, 0x001, 0x05e, 0x045, 0x364, 0x005, 0x020, 0x009,
    0x3fd, 0x309, 0x045, 0x209, 0x247, 0x00d, 0x347, 0x044,
    0x00e, 0x3c7, 0x042, 0x00f, 0x044, 0x304, 0x005, 0x044,
    0x31d, 0x005, 0x384, 0x01c, 0x262, 0x010, 0x001, 0x005,
    0x3c0, 0x001, 0x05f, 0x045, 0x3c5, 0x200, 0x00f, 0x364,
    0x005, 0x020, 0x001, 0x3fd, 0x3c4, 0x1ff, 0x00f, 0x364,
    0x005, 0x040, 0x001, 0x3fd, 0x3c5, 0x100, 0x00f, 0x3c4,
    0x2ff, 0x00f, 0x307, 0x005, 0x045, 0x347, 0x002, 0x059,
    0x347, 0x003, 0x05b, 0x347, 0x004, 0x05c, 0x247, 0x05d,
    0x3c4, 0x003, 0x05d, 0x045, 0x347, 0x059, 0x00a, 0x347,
    0x05b, 0x00b, 0x347, 0x05c, 0x00c, 0x307, 0x05d, 0x384,
    0x003, 0x329, 0x045, 0x2a9, 0x285, 0x247, 0x00d, 0x347,
    0x044, 0x00e, 0x045, 0x364, 0x005, 0x020, 0x009, 0x3fd,
    0x347, 0x059, 0x00a, 0x329, 0x045, 0x2a9, 0x2c7, 0x00d,
    0x020, 0x2d4, 0x005, 0x364, 0x005, 0x020, 0x009, 0x3fd,
    0x309, 0x045, 0x209, 0x247, 0x00d, 0x3c7, 0x042, 0x00f,
    0x044, 0x304, 0x005, 0x347, 0x002, 0x059, 0x3c4, 0x0ff,
    0x059, 0x020, 0x2fa, 0x005, 0x04e, 0x3f3, 0x12c, 0x090
);

# Alta: alta_mrlFixScanTable
my @alta_mrlFixScanTable = (
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x00000000 ],
    [ 0x90, 0x80000001 ],
    [ 0x90, 0x8400001e ],
    [ 0x90, 0x8500001e ],
    [ 0x90, 0x860000c8 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000001 ],
    [ 0x90, 0xbf000002 ],
    [ 0x90, 0xbf000003 ],
    [ 0x90, 0xbf000004 ],
    [ 0x90, 0xbf000005 ],
    [ 0x90, 0xbf000006 ],
    [ 0x90, 0xbf000007 ],
    [ 0x90, 0xbf000008 ],
    [ 0x90, 0xbf000009 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00048000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00048000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00048000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00048000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00050000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00058000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00050000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00058000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00050000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00058000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00050000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00058000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00048000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00048000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00048000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00030000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00028000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00050000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00058000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00050000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00058000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00050000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00058000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00040000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00010000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00020000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00078000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00050000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00018000 ],
    [ 0x14, 0x00060000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00008000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x14, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0x00000000 ],
    [ 0x10, 0xfffffff8 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x10, 0xfffffff8 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x10, 0xfffffff8 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x10, 0xfffffff8 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
    [ 0x90, 0xbf000000 ],
);

# Registers not handled explicitly in the
# script.  Processed last.  This is just a temporary
# debugging/verification feature.
my %not_handled_regs = ();

# Register values loaded from reg dump.
my @reg_vals = ();

my @eeprom_contents = ();

# Alta only: the alta eeprom may have until 4 images, 
# the eeprom header defines the image offsets.
my @eeprom_header = ();
my $eeprom_include_header = 0;
my $eeprom_target_image = 0;
# Eeprom Upper Linear Base Address, used to generate hexIntel-32
my $eepromULBA = 0;
# eeprom highest address
my $eepromHighAddr = 0;
# boot error jump address (used by the POLL boot command)
my $eepromBootErrJmpAddr1 = 0x20;
my $eepromBootErrJmpAddr2 = 0x28;
my $eepromBootErrJmpAddr3 = 0x30;

# Alta image type selection
#  0: minimal-PCIe
#  1: FIBM          (default)
my $altaFIBMconfig = 1;

# Alta: init memory via BIST
#  0: disable
#  1: enable         (default)
my $altaInitMemBist = 1;

# Alta: MRL init fix. Only for A0
#  0: disable        (default)
#  1: enable
my $altaMrlFix = 0;

# Alta: PCIe endianness
my $altaPcieEndianness = 0;

# Alta: register to be initialized via command line
# leave as not defined if MGMT_SCRATCH must not be initialized
my $altaMgmtScratchReg;
my $altaPcieIdReg;

# Alta: L2F_TABLE_256 definitions
# FIXME: this is defined by uc, and should be exported to this tool
my $altaL2fTablePortMask = 3;

my @alta_image_offsets = (
    0x100,       # image #0
    0x80000,     # image #1
    0x100000,    # image #2
    0x180000     # image #3
);

my @alta_eeprom_image_mode =(
    SPI_MODE_SINGLE,    # default mode image #0 
    SPI_MODE_SINGLE,    # default mode image #1
    SPI_MODE_SINGLE,    # default mode image #2 
    SPI_MODE_SINGLE     # default mode image #3 
);

my @alta_eeprom_image_speed = (
    SPI_SPEED_00_5,     # default speed image #0 
    SPI_SPEED_00_5,     # default speed image #1
    SPI_SPEED_00_5,     # default speed image #2 
    SPI_SPEED_00_5      # default speed image #3 
);

my $eeprom_addr = 0;

# alta global port mask
my $alta_mask_lw = 0;
my $alta_mask_mw = 0;
my $alta_mask_hw = 0;

# alta port specification
# port modes
use constant {
    PORT_MODE_UNDEFINED             => 0xff,    # 00:port mode not defined
    PORT_MODE_DISABLED              => 0x00,    # 00:port disabled
    # Non 40G, single lane modes
    PORT_MODE_SGMII                 => 0x11,    # 01: SGMII:                1G,     1 lane, 8b/10b encoding
    PORT_MODE_1000BASE_X            => 0x12,    # 02: 1000BASE-X:           1G,     1 lane, 8b/10b encoding
    PORT_MODE_1000BASE_KX           => 0x13,    # 03: 1000BASE-KX:          1G,     1 lane, 8b/10b encoding
    PORT_MODE_10GBASE_KR            => 0x14,    # 04: 10GBASE-KR:           10G,    1 lane, 64b/66b encoding
    PORT_MODE_10GBASE_CR            => 0x15,    # 05: 10GBASE-CR (SFP+):    10G,    1 lane, 64b/66b encoding
    PORT_MODE_10GBASE_SR            => 0x16,    # 06: 10BASE-SR (SFP+, SFI):10G,    1 lane, 64b/66b encoding
    # Non 40G, 4-lane modes
    PORT_MODE_XAUI                  => 0x27,    # 07: XAUI:                 1G/10G, 4 lanes, 8b/10b encoding
    PORT_MODE_10GBASE_KX4           => 0x28,    # 08: 10GBASE-KX4:          1G,     4 lanes, 8b/10b encoding
    PORT_MODE_10GBASE_CX4           => 0x29,    # 09: 10GBASE-CX4:          1G,     4 lanes, 8b/10b encoding
    # 40G, 4-lane modes
    PORT_MODE_40GBASE_KR4           => 0x4a,    # 10: 40GBASE-KR4:          40G,    4 lane, 64b/66b encoding
    PORT_MODE_XLAUI                 => 0x4b,    # 11: XLAUI:                40G,    4 lane, 64b/66b encoding
    PORT_MODE_40GBASE_CR4           => 0x4c,    # 12: 40GBASE-CR4 (QSFP 5M Direct Attach): 40G, 4 lane, 64b/66b encod.
    PORT_MODE_40GBASE_SR4           => 0x4d,    # 13: 40GBASE-SR4 (QSFP PMD Serv Interf.): 40G, 4 lane, 64b/66b encod.

    PORT_MODE_AN_73                 => 0x8e     # 14: AN_73
};

# alta port DFE modes
use constant {
    DFE_MODE_STATIC                 => 0,
    DFE_MODE_ONE_SHOT               => 1,
    DFE_MODE_CONTINUOUS             => 2,
    DFE_MODE_KR                     => 3
};

# alta port speed constants
use constant {
    PORT_SPEED_NONE                 => 0x00,
    PORT_SPEED_10M                  => 0x01,
    PORT_SPEED_100M                 => 0x02,
    PORT_SPEED_1G                   => 0x03,
    PORT_SPEED_2_5G                 => 0x04,
    PORT_SPEED_10G                  => 0x05,
    PORT_SPEED_40G                  => 0x06
};

# alta SBUS config sets
use constant {
    SBUS_CONFIG_NONE                => 0x00,
    SBUS_CONFIG_1000BASEX           => 0x01,
    SBUS_CONFIG_10GBASER            => 0x02,
    SBUS_CONFIG_10GBASEX            => 0x03,
    SBUS_CONFIG_40BASER             => 0x04,
};

# alta port mode hash.
# Defines the mode, port speed, DFE mode and the number of lanes.
my %alta_portMode_hash = (
    "DISABLED"                      => [PORT_MODE_DISABLED,    PORT_SPEED_NONE, 0, DFE_MODE_STATIC,     SBUS_CONFIG_NONE],
    "SGMII"                         => [PORT_MODE_SGMII,       PORT_SPEED_NONE, 1, DFE_MODE_STATIC,     SBUS_CONFIG_1000BASEX],
    "1000BASE_X"                    => [PORT_MODE_1000BASE_X,  PORT_SPEED_1G,   1, DFE_MODE_STATIC,     SBUS_CONFIG_1000BASEX],
    "1000BASE_KX"                   => [PORT_MODE_1000BASE_KX, PORT_SPEED_1G,   1, DFE_MODE_STATIC,     SBUS_CONFIG_1000BASEX],
    "10GBASE_KR"                    => [PORT_MODE_10GBASE_KR,  PORT_SPEED_10G,  1, DFE_MODE_KR,         SBUS_CONFIG_10GBASER],
    "10GBASE_CR"                    => [PORT_MODE_10GBASE_CR,  PORT_SPEED_10G,  1, DFE_MODE_CONTINUOUS, SBUS_CONFIG_10GBASER],
    "10GBASE_SR"                    => [PORT_MODE_10GBASE_SR,  PORT_SPEED_10G,  1, DFE_MODE_CONTINUOUS, SBUS_CONFIG_10GBASER],
    "XAUI"                          => [PORT_MODE_XAUI,        PORT_SPEED_10G,  4, DFE_MODE_CONTINUOUS, SBUS_CONFIG_10GBASEX],
    "10GBASE_KX4"                   => [PORT_MODE_10GBASE_KX4, PORT_SPEED_10G,  4, DFE_MODE_CONTINUOUS, SBUS_CONFIG_10GBASEX],
    "10GBASE_CX4"                   => [PORT_MODE_10GBASE_CX4, PORT_SPEED_10G,  4, DFE_MODE_CONTINUOUS, SBUS_CONFIG_10GBASEX],
    "40GBASE_KR4"                   => [PORT_MODE_40GBASE_KR4, PORT_SPEED_40G,  4, DFE_MODE_KR,         SBUS_CONFIG_40BASER],
    "XLAUI"                         => [PORT_MODE_XLAUI,       PORT_SPEED_40G,  4, DFE_MODE_CONTINUOUS, SBUS_CONFIG_40BASER],
    "40GBASE_CR4"                   => [PORT_MODE_40GBASE_CR4, PORT_SPEED_40G,  4, DFE_MODE_CONTINUOUS, SBUS_CONFIG_40BASER],
    "40GBASE_SR4"                   => [PORT_MODE_40GBASE_SR4, PORT_SPEED_40G,  4, DFE_MODE_CONTINUOUS, SBUS_CONFIG_40BASER],
    # TBD: verify AN_73 entry
    "AN_73"                         => [PORT_MODE_AN_73,       PORT_SPEED_NONE, 1, FM_DFE_MODE_STATIC,  SBUS_CONFIG_NONE]
);

my %on_off_hash = (
    "off"                           => 0,
    "on"                            => 1
);

my %alta_portSpeed_hash = (
    "10M"                           => PORT_SPEED_10M,
    "100M"                          => PORT_SPEED_100M,
    "1G"                            => PORT_SPEED_1G,
    "2.5G"                          => PORT_SPEED_2_5G,
    "10G"                           => PORT_SPEED_10G,
    "40G"                           => PORT_SPEED_40G
);

use constant {
    LANE_REVERSAL_NONE              => 0x00,
    LANE_REVERSAL_RX                => 0x01,
    LANE_REVERSAL_TX                => 0x02,
    LANE_REVERSAL_RXTX              => 0x03
};

my %alta_laneReversal_hash = (
    "none"                          => LANE_REVERSAL_NONE,
    "rx"                            => LANE_REVERSAL_RX,
    "tx"                            => LANE_REVERSAL_TX,
    "rxtx"                          => LANE_REVERSAL_RXTX
);

my @laneOrderingTable = (
    # invert order       epl
    0,                  # 0
    1,                  # 1
    1,                  # 2
    0,                  # 3
    1,                  # 4
    0,                  # 5
    0,                  # 6
    0,                  # 7
    1,                  # 8
    0,                  # 9 
    0,                  # 10
    0,                  # 11
    1,                  # 12
    1,                  # 13
    0,                  # 14
    0,                  # 15
    0,                  # 16
    1,                  # 17
    0,                  # 18
    0,                  # 19
    1,                  # 20
    0,                  # 21
    1,                  # 22
    0,                  # 23
    1                   # 24
);


use constant {
    LANE_POLARITY_NORMAL            => 0x00,
    LANE_POLARITY_INV_RX            => 0x01,
    LANE_POLARITY_INV_TX            => 0x02,
    LANE_POLARITY_INV_RXTX          => 0x03
};

my %alta_lanePolarity_hash = (
    "normal"                        => LANE_POLARITY_NORMAL,
    "rx"                            => LANE_POLARITY_INV_RX,
    "tx"                            => LANE_POLARITY_INV_TX,
    "rxtx"                          => LANE_POLARITY_INV_RXTX
);


my %alta_serdesCfg_1000BaseX = (
    "refSel"                        => 0x13,
    "xxRateSel"                     => 0x63,
    "rxFifoWatermark"               => 0x5,
    "xx20b10bEn"                    => 0,
    "detectK28_7"                   => 1,
    "rx8b10bDecodeEn"               => 0,
    "tx8b10bDecodeEn"               => 0,
    "dfeMode"                       => 0
);

my %alta_serdesCfg_10GBaseR = (
    "refSel"                        => 0x1B,
    "xxRateSel"                     => 0x40,
    "rxFifoWatermark"               => 0x5,
    "xx20b10bEn"                    => 1,
    "detectK28_7"                   => 0,
    "rx8b10bDecodeEn"               => 0,
    "tx8b10bDecodeEn"               => 0,
    "dfeMode"                       => 0
);

my %alta_serdesCfg_10GBaseX = (
    "refSel"                        => 0x1,
    "xxRateSel"                     => 0x6,
    "rxFifoWatermark"               => 0x5,
    "xx20b10bEn"                    => 0,
    "detectK28_7"                   => 1,
    "rx8b10bDecodeEn"               => 1,
    "tx8b10bDecodeEn"               => 1,
    "dfeMode"                       => 0
);

my %alta_serdesCfg_40GBaseR = (
    "refSel"                        => 0x1B,
    "xxRateSel"                     => 0x40,
    "rxFifoWatermark"               => 0x2,
    "xx20b10bEn"                    => 1,
    "detectK28_7"                   => 0,
    "rx8b10bDecodeEn"               => 0,
    "tx8b10bDecodeEn"               => 0,
    "dfeMode"                       => 0
);

# Alta: list of 40G capable ports
# if $listXXGCapablePorts_are_logicals is 0, the ports are physical (default)
my $listXXGCapablePorts_are_logicals = 0;
my @alta_list40GCapablePorts = ();

# Alta: list of 10G capable ports
my @alta_list10GCapablePorts = ();

# Alta: logical to physical  port mapping
# usage: physical_port = $alta_portMappingTable[logical_port]
# -1 is returned if the port is undefined
my @alta_portMappingTable = ((-1) x 76);

# Alta: maps an EPL-channel pair to a Lane
# usage:  lane = $alta_EplChnlToLaneMapTable[epl][channel]
#
my @alta_EplChnlToLaneMapTable = (
    [ 0, 0, 0, 0 ],     # EPL 0  (non-existing)
    [ 3, 2, 1, 0 ],     # EPL 1  (flipped)
    [ 3, 2, 1, 0 ],     # EPL 2  (flipped)
    [ 0, 1, 2, 3 ],     # EPL 3
    [ 3, 2, 1, 0 ],     # EPL 4  (flipped)
    [ 0, 1, 2, 3 ],     # EPL 5
    [ 0, 1, 2, 3 ],     # EPL 6
    [ 0, 1, 2, 3 ],     # EPL 7
    [ 3, 2, 1, 0 ],     # EPL 8  (flipped)
    [ 0, 1, 2, 3 ],     # EPL 9        
    [ 0, 1, 2, 3 ],     # EPL 10                
    [ 0, 1, 2, 3 ],     # EPL 11                
    [ 3, 2, 1, 0 ],     # EPL 12 (flipped)      
    [ 3, 2, 1, 0 ],     # EPL 13 (flipped)      
    [ 0, 1, 2, 3 ],     # EPL 14                
    [ 0, 1, 2, 3 ],     # EPL 15                
    [ 0, 1, 2, 3 ],     # EPL 16                
    [ 3, 2, 1, 0 ],     # EPL 17 (flipped)      
    [ 0, 1, 2, 3 ],     # EPL 18                
    [ 0, 1, 2, 3 ],     # EPL 19                
    [ 3, 2, 1, 0 ],     # EPL 20 (flipped)      
    [ 0, 1, 2, 3 ],     # EPL 21                
    [ 3, 2, 1, 0 ],     # EPL 22 (flipped)      
    [ 0, 1, 2, 3 ],     # EPL 23                
    [ 3, 2, 1, 0 ]      # EPL 24 (flipped)      
);

# This table maps EPLs to SerDes for Lane_0;
# In order to get the SerDes for the other lanes:
#  SerDes[lane_n] = SerDes[lane_0] + lane_n;  lane_n = [0..3]   
my @alta_SerdesToEplMapTable = (
    -1,         # EPL 0  (non-existing)
     0,         # EPL 1
    92,         # EPL 2
     4,         # EPL 3
    88,         # EPL 4
     8,         # EPL 5
    84,         # EPL 6
    12,         # EPL 7
    80,         # EPL 8
    16,         # EPL 9        
    76,         # EPL 10                
    20,         # EPL 11
    72,         # EPL 12
    24,         # EPL 13
    68,         # EPL 14
    28,         # EPL 15
    64,         # EPL 16
    40,         # EPL 17
    52,         # EPL 18
    32,         # EPL 19
    60,         # EPL 20
    36,         # EPL 21
    56,         # EPL 22
    44,         # EPL 23
    48,         # EPL 24
);

# constants used to access the alta port table.
use constant {
    ALTA_PORT_TABLE_ENTRY_PHYSICAL_PORT         => 0x00,
    ALTA_PORT_TABLE_ENTRY_LOGICAL_PORT          => 0x01,
    ALTA_PORT_TABLE_ENTRY_ACTIVE_MAC            => 0x02,
    ALTA_PORT_TABLE_ENTRY_PORT_MODE             => 0x03,
    ALTA_PORT_TABLE_ENTRY_PORT_SPEED            => 0x04,
    ALTA_PORT_TABLE_ENTRY_DFE_MODE              => 0x05,
    ALTA_PORT_TABLE_ENTRY_LANE_REVERSAL         => 0x06,
    ALTA_PORT_TABLE_ENTRY_LANE_POLARITY         => 0x07,
    ALTA_PORT_TABLE_ENTRY_EPL                   => 0x08,
    ALTA_PORT_TABLE_ENTRY_CHANNEL               => 0x09,
    ALTA_PORT_TABLE_ENTRY_LANE                  => 0x0a,
    ALTA_PORT_TABLE_ENTRY_SERDES                => 0x0b,
    ALTA_PORT_TABLE_ENTRY_LANE_NUM              => 0x0c,
    ALTA_PORT_TABLE_ENTRY_SBUS_CFG_SET          => 0x0d,
    ALTA_PORT_TABLE_ENTRY_LOOPBACK              => 0x0e,
    ALTA_PORT_TABLE_ENTRY_MASK                  => 0x0f,
};

# alta port table.
# access: $alta_port_def_table[physical_port][port_def_field]
#         for intance, to access to get the eth mode associated to the
#         physical port #2:
#           $alta_port_def_table[2][ALTA_PORT_TABLE_ENTRY_PORT_MODE]
my @alta_port_def_table = (
#         phy log  act     ethernet            port          DFE           lane                 lane             EPL    LANE LANE_N     SBUS      loop  mask
#        port port MAC      MODE               speed         mode        reversal             polarity               CHNL  SERDES      CFG SET    back 
        [  0, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  0,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 00
        [  1, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  0,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 01
        [  2, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  0,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 02
        [  3, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  0,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 03
        [  4, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  1,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 04
        [  5, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  1,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 05
        [  6, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  1,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 06
        [  7, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  1,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 07
        [  8, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  3,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 08
        [  9, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  3,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 09
        [ 10, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  3,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 10
        [ 11, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  3,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 11
        [ 12, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  5,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 12
        [ 13, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  5,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 13
        [ 14, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  5,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 14
        [ 15, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  5,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 15
        [ 16, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  7,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 16
        [ 17, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  7,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 17
        [ 18, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  7,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 18
        [ 19, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  7,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 19
        [ 20, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 16,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 20
        [ 21, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 16,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 21
        [ 22, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 16,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 22
        [ 23, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 16,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 23
        [ 24, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 17,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 24
        [ 25, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 17,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 25
        [ 26, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 17,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 26
        [ 27, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 17,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 27
        [ 28, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 18,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 28
        [ 29, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 18,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 29
        [ 30, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 18,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 30
        [ 31, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 18,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 31
        [ 32, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 19,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 32
        [ 33, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 19,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 33
        [ 34, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 19,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 34
        [ 35, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 19,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 35
        [ 36, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 20,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 36
        [ 37, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 20,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 37
        [ 38, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 20,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 38
        [ 39, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 20,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 39
        [ 40, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 14,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 40
        [ 41, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 14,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 41
        [ 42, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 14,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 42
        [ 43, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 14,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 43
        [ 44, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  9,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 44
        [ 45, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  9,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 45
        [ 46, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  9,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 46
        [ 47, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL,  9,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 47
        [ 48, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 11,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 48
        [ 49, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 11,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 49
        [ 50, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 11,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 50
        [ 51, -1,  0, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 11,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 51
        [ 52, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 13,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 52
        [ 53, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 13,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 53
        [ 54, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 13,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 54
        [ 55, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 13,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 55
        [ 56, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 15,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 56
        [ 57, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 15,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 57
        [ 58, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 15,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 58
        [ 59, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 15,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 59
        [ 60, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 21,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 60
        [ 61, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 21,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 61
        [ 62, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 21,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 62
        [ 63, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 21,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 63
        [ 64, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 22,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 64
        [ 65, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 22,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 65
        [ 66, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 22,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 66
        [ 67, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 22,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 67
        [ 68, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 23,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 68
        [ 69, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 23,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 69
        [ 70, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 23,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 70
        [ 71, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 23,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 71
        [ 72, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 24,  0, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 72
        [ 73, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 24,  1, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 73
        [ 74, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 24,  2, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""],        # port 74
        [ 75, -1, -1, PORT_MODE_DISABLED, PORT_SPEED_10G, DFE_MODE_KR, LANE_REVERSAL_NONE, LANE_POLARITY_NORMAL, 24,  3, 0, 0, 0, SBUS_CONFIG_NONE, 0, ""]         # port 75
);

my $process_multiword_regs_ref = \&bali_process_multiword_regs;
my $process_multiword_regs_insrt_writes = \&bali_process_multiword_regs_insrt_writes;
# Register definition
my %regs_hash = ();


# %tahoe_regs_hash
#
# Entry format:
#   [ word-count, index-count, port-index, ordering,
#     addr-sub, default-sub, bounds-list ]
#
# word-count:  number of 32-bit words that comprise each
#              register value (1-4)
# index-count: number of indices that must be provided to
#              fully specify a register (size of bounds-list)
# port-index:  which index is a port number (-1 for none)
#              (used to filter out writes to unused ports)
# ordering:    indicates whether this register is handled
#              as a special case ("s") or as part of the
#              unordered ("u") automated loop.
# addr-sub:    subroutine that returns a register address
#              given appropriate index arguments
# default-sub: subroutine that returns the register default
#              value given appropriate index arguments
# bounds-list: list of list references.  Each list reference
#              represents one index argument to the subroutine
#              and gives the min and max values allowed.

my %tahoe_regs_hash = (
#       "FM_STAT_EPL_ERROR1"           => [ 1, 1,  0, "u", sub { return 0x8025 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_BOOT_STATUS"               => [ 1, 0, -1, "u", sub { return 0x0; }, sub { return (0x00000000); } ],
        "FM_CHIP_MODE"                 => [ 1, 0, -1, "s", sub { return 0x301; }, sub { return (0x00002000); } ],
        "FM_CLK_MULT_1"                => [ 1, 0, -1, "s", sub { return 0x302; }, sub { return (0x00001900); } ],
        "FM_CM_PRI_MAP_1"              => [ 1, 0, -1, "u", sub { return 0x64000; }, sub { return (0xDDDDDDDD); } ],
        "FM_CM_PRI_MAP_2"              => [ 1, 0, -1, "u", sub { return 0x64001; }, sub { return (0xDDDDDDDD); } ],
#       "FM_CONTEPL_CTRLSTAT"          => [ 1, 0, -1, "u", sub { return 0x311; }, sub { return (0x00000000); } ],
        "FM_EGRESS_SCHEDULE_1"         => [ 1, 1,  0, "u", sub { return 0x2040 + (2 * $_[0]); }, sub { return (0x00000000); }, [ 0, 24 ] ],
        "FM_EGRESS_SCHEDULE_2"         => [ 1, 1,  0, "u", sub { return 0x2041 + (2 * $_[0]); }, sub { return (0x0F070301); }, [ 0, 24 ] ],
#       "FM_STAT_TX_BYTECOUNT"         => [ 2, 1,  0, "u", sub { return 0x802c + (0x400 * ($_[0]-1)); }, sub { return (0x00000000, 0x00000000); }, [ 1, 24 ] ],
#       "FM_EPL_INT_DETECT"            => [ 1, 1,  0, "u", sub { return 0x802b + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_EPL_LED_STATUS"            => [ 1, 1,  0, "u", sub { return 0x802a + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_PACING_RATE"               => [ 1, 1,  0, "u", sub { return 0x8018 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_PACING_STAT"               => [ 1, 1,  0, "u", sub { return 0x8019 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_PACING_PRI_WM"             => [ 1, 2,  1, "u", sub { return (0x8010 + $_[0]) + (0x400 * ($_[1]-1)); }, sub { return (0x00000000); }, [ 0,  7 ], [ 1, 24 ] ],
        "FM_FID_TABLE"                 => [ 2, 1, -1, "s", sub { return 0x52000 + ($_[0] * 2); }, sub { return (0x00000000, 0x00000000); }, [ 0, 4095 ] ],
        "FM_FRAME_CTRL_IM"             => [ 1, 0, -1, "u", sub { return 0x30e; }, sub { return (0x000007FF); } ],
#       "FM_FRAME_CTRL_IP"             => [ 1, 0, -1, "u", sub { return 0x30d; }, sub { return (0x00000000); } ],
        "FM_FRAME_TIME_OUT"            => [ 1, 0, -1, "u", sub { return 0x303; }, sub { return (0x00000000); } ],
        "FM_FUSEBOX_1"                 => [ 1, 0, -1, "s", sub { return 0x305; }, sub { return (0xFFFFFFFF); } ],
        "FM_FUSEBOX_2"                 => [ 1, 0, -1, "s", sub { return 0x306; }, sub { return (0xFFFFFFFF); } ],
        "FM_FUSEBOX_3"                 => [ 1, 0, -1, "s", sub { return 0x307; }, sub { return (0xFFFFFFFF); } ],
        "FM_FUSEBOX_4"                 => [ 1, 0, -1, "s", sub { return 0x308; }, sub { return (0xC020018C); } ],
#       "FM_GLOBAL_EPL_INT_DETECT"     => [ 1, 0, -1, "u", sub { return 0x30a; }, sub { return (0x00000000); } ],
        "FM_GLOBAL_PAUSE_WM"           => [ 1, 1,  0, "u", sub { return 0x64080 + $_[0]; }, sub { return (0x01200144); }, [ 0, 24 ] ],
        "FM_HEADER_MASK"               => [ 1, 1, -1, "u", sub { return 0x58110 + $_[0]; }, sub { return (0xFFFFFFFF); }, [ 0,  3 ] ],
#       "FM_INTERRUPT_DETECT"          => [ 1, 0, -1, "u", sub { return 0x309; }, sub { return (0x00000000); } ],
        "FM_JITTER_WATERMARK"          => [ 1, 0, -1, "u", sub { return 0x20fc; }, sub { return (0x00202020); } ],
        "FM_LCI_CFG"                   => [ 1, 0, -1, "u", sub { return 0x4005; }, sub { return (0x00000004); } ],
        "FM_LCI_IM"                    => [ 1, 0, -1, "u", sub { return 0x4003; }, sub { return (0x000000FF); } ],
#       "FM_LCI_IP"                    => [ 1, 0, -1, "u", sub { return 0x4002; }, sub { return (0x00000000); } ],
        "FM_LCI_RX_FIFO"               => [ 1, 0, -1, "s", sub { return 0x4000; }, sub { return (0x00000000); } ],
#       "FM_LCI_STATUS"                => [ 1, 0, -1, "u", sub { return 0x4004; }, sub { return (0x00000000); } ],
        "FM_LCI_TX_FIFO"               => [ 1, 0, -1, "s", sub { return 0x4001; }, sub { return (0x00000000); } ],
        "FM_LFSR_CFG"                  => [ 1, 0, -1, "u", sub { return 0x64002; }, sub { return (0x00000000); } ],
        "FM_MAC_CFG_1"                 => [ 1, 1,  0, "u", sub { return 0x801a + (0x400 * ($_[0]-1)); }, sub { return (0x10180000); }, [ 1, 24 ] ],
        "FM_MAC_CFG_2"                 => [ 1, 1,  0, "u", sub { return 0x801b + (0x400 * ($_[0]-1)); }, sub { return (0x810000FC); }, [ 1, 24 ] ],
        "FM_MAC_CFG_3"                 => [ 1, 1,  0, "u", sub { return 0x801c + (0x400 * ($_[0]-1)); }, sub { return (0x0000FFFF); }, [ 1, 24 ] ],
        "FM_MAC_CFG_4"                 => [ 1, 1,  0, "u", sub { return 0x801d + (0x400 * ($_[0]-1)); }, sub { return (0x0000FFFF); }, [ 1, 24 ] ],
        "FM_MAC_CFG_5"                 => [ 1, 1,  0, "u", sub { return 0x801e + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_MAC_CFG_6"                 => [ 1, 1,  0, "u", sub { return 0x801f + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_MAC_IM"                    => [ 1, 1,  0, "u", sub { return 0x8024 + (0x400 * ($_[0]-1)); }, sub { return (0x000007FF); }, [ 1, 24 ] ],
#       "FM_MAC_IP"                    => [ 1, 1,  0, "u", sub { return 0x8023 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_MAC_STATUS"                => [ 1, 1,  0, "u", sub { return 0x8022 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_MA_TABLE"                  => [ 3, 1, -1, "s", sub { return 0x10000 + ($_[0] * 4); }, sub { return (0x00000000, 0x00000000, 0x00000000); }, [ 0, 16383 ] ],
        "FM_MA_TABLE_CFG"              => [ 1, 0, -1, "u", sub { return 0x58120; }, sub { return (0x00000000); } ],
#       "FM_MA_TABLE_STATUS_1"         => [ 1, 0, -1, "u", sub { return 0x58000; }, sub { return (0x00000000); } ],
#       "FM_MA_TABLE_STATUS_2"         => [ 1, 0, -1, "u", sub { return 0x58001; }, sub { return (0x00000000); } ],
#       "FM_MA_TABLE_STATUS_3"         => [ 1, 0, -1, "u", sub { return 0x310; }, sub { return (0x00000000); } ],
        "FM_MGR_IM"                    => [ 1, 0, -1, "u", sub { return 0x30c; }, sub { return (0x000000FF); } ],
#       "FM_MGR_IP"                    => [ 1, 0, -1, "u", sub { return 0x30b; }, sub { return (0x00000000); } ],
        "FM_PCS_CFG_1"                 => [ 1, 1,  0, "u", sub { return 0x8009 + (0x400 * ($_[0]-1)); }, sub { return (0x10000C0A); }, [ 1, 24 ] ],
        "FM_PCS_CFG_2"                 => [ 1, 1,  0, "u", sub { return 0x800a + (0x400 * ($_[0]-1)); }, sub { return (0x00000001); }, [ 1, 24 ] ],
        "FM_PCS_CFG_3"                 => [ 1, 1,  0, "u", sub { return 0x800b + (0x400 * ($_[0]-1)); }, sub { return (0x00000002); }, [ 1, 24 ] ],
        "FM_PCS_CFG_4"                 => [ 1, 1,  0, "u", sub { return 0x800c + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_PCS_CFG_5"                 => [ 1, 1,  0, "u", sub { return 0x800d + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_PCS_IM"                    => [ 1, 1,  0, "u", sub { return 0x800f + (0x400 * ($_[0]-1)); }, sub { return (0x00007FFF); }, [ 1, 24 ] ],
#       "FM_PCS_IP"                    => [ 1, 1,  0, "u", sub { return 0x800e + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_PERR_DEBUG"                => [ 1, 0, -1, "u", sub { return 0x314; }, sub { return (0x00000000); } ],
        "FM_PERR_IM"                   => [ 1, 0, -1, "u", sub { return 0x313; }, sub { return (0x0000FFFF); } ],
#       "FM_PERR_IP"                   => [ 1, 0, -1, "u", sub { return 0x312; }, sub { return (0x00000000); } ],
        "FM_PLL_FH_CTRL"               => [ 1, 0, -1, "s", sub { return 0x315; }, sub { return (0x00002202); } ],
#       "FM_PLL_FH_STAT"               => [ 1, 0, -1, "u", sub { return 0x316; }, sub { return (0x00000000); } ],
        "FM_PORT_CFG_1"                => [ 1, 1,  0, "u", sub { return 0x54000 + $_[0]; }, sub { return (($_[0] == 0)?0x00000000:0x00100001); }, [ 0, 24 ] ],
        "FM_PORT_CFG_2"                => [ 1, 1,  0, "s", sub { return 0x60060 + $_[0]; }, sub { return (0x01FFFFFF); }, [ 0, 24 ] ],
        "FM_PORT_CLK_SEL"              => [ 1, 0, -1, "s", sub { return 0x317; }, sub { return (0x00000000); } ],
        "FM_PORT_MAC_SEC_IM"           => [ 1, 0, -1, "u", sub { return 0x640c5; }, sub { return (0x01FFFFFE); } ],
#       "FM_PORT_MAC_SEC_IP"           => [ 1, 0, -1, "u", sub { return 0x640c4; }, sub { return (0x00000000); } ],
        "FM_PORT_RESET"                => [ 1, 0, -1, "s", sub { return 0x318; }, sub { return (0x01FFFFFE); } ],
        "FM_PORT_VLAN_IM_1"            => [ 1, 0, -1, "u", sub { return 0x640c1; }, sub { return (0x01FFFFFE); } ],
        "FM_PORT_VLAN_IM_2"            => [ 1, 0, -1, "u", sub { return 0x640c3; }, sub { return (0x01FFFFFE); } ],
#       "FM_PORT_VLAN_IP_1"            => [ 1, 0, -1, "u", sub { return 0x640c0; }, sub { return (0x00000000); } ],
#       "FM_PORT_VLAN_IP_2"            => [ 1, 0, -1, "u", sub { return 0x640c2; }, sub { return (0x00000000); } ],
        "FM_QUEUE_CFG_1"               => [ 1, 1,  0, "u", sub { return 0x64020 + $_[0]; }, sub { return (0x00FF00FF); }, [ 0, 24 ] ],
        "FM_QUEUE_CFG_2"               => [ 1, 1,  0, "u", sub { return 0x64040 + $_[0]; }, sub { return (0x00000010); }, [ 0, 24 ] ],
        "FM_QUEUE_CFG_3"               => [ 1, 0, -1, "u", sub { return 0x65000; }, sub { return (0x55555555); } ],
        "FM_QUEUE_CFG_4"               => [ 1, 0, -1, "u", sub { return 0x64003; }, sub { return (0x021C021C); } ],
        "FM_QUEUE_CFG_5"               => [ 1, 0, -1, "u", sub { return 0x64004; }, sub { return (0x000003D0); } ],
#       "FM_RMON_RX_JABBER"            => [ 1, 1,  0, "u", sub { return 0x8029 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_RMON_TX_CRC"               => [ 1, 1,  0, "u", sub { return 0x8027 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_RMON_TX_PAUSE"             => [ 1, 1,  0, "u", sub { return 0x8026 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_RX_PAUSE_WM"               => [ 1, 1,  0, "u", sub { return 0x640a0 + $_[0]; }, sub { return (0x00F500FF); }, [ 0, 24 ] ],
        "FM_RX_PRI_MAP"                => [ 1, 1,  0, "u", sub { return 0x60040 + $_[0]; }, sub { return (0x76543210); }, [ 0, 24 ] ],
        "FM_SAF_MATRIX"                => [ 1, 1,  0, "u", sub { return 0x650c0 + $_[0]; }, sub { return (($_[0] == 0)?0xFFFFFFFF:0xFE000001); }, [ 0, 24 ] ],
        "FM_SCAN_CTRL"                 => [ 1, 0, -1, "s", sub { return 0x101; }, sub { return (0x00000000); } ],
        "FM_SCAN_DATA_IN"              => [ 1, 0, -1, "s", sub { return 0x103; }, sub { return (0x00000000); } ],
        "FM_SCAN_DATA_OUT"             => [ 1, 0, -1, "s", sub { return 0x104; }, sub { return (0x00000000); } ],
        "FM_SCAN_FREQ_MULT"            => [ 1, 0, -1, "s", sub { return 0x100; }, sub { return (0x00000000); } ],
        "FM_SCAN_SEL"                  => [ 1, 0, -1, "s", sub { return 0x102; }, sub { return (0x00000000); } ],
        "FM_SCHED_PRI_MAP"             => [ 1, 0, -1, "u", sub { return 0x65001; }, sub { return (0xFA41FA41); } ],
#       "FM_SERDES_BIST_CNT"           => [ 1, 1,  0, "u", sub { return 0x8007 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_SERDES_BIST_ERR_CNT"       => [ 1, 1,  0, "u", sub { return 0x8008 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_SERDES_CTRL_1"             => [ 1, 1,  0, "u", sub { return 0x8000 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_SERDES_CTRL_2"             => [ 1, 1,  0, "s", sub { return 0x8001 + (0x400 * ($_[0]-1)); }, sub { return (0x0003FF00); }, [ 1, 24 ] ],
        "FM_SERDES_CTRL_3"             => [ 1, 1,  0, "u", sub { return 0x8002 + (0x400 * ($_[0]-1)); }, sub { return (0x0001312D); }, [ 1, 24 ] ],
        "FM_SERDES_IM"                 => [ 1, 1,  0, "u", sub { return 0x8006 + (0x400 * ($_[0]-1)); }, sub { return (0x00000FFF); }, [ 1, 24 ] ],
#       "FM_SERDES_IP"                 => [ 1, 1,  0, "u", sub { return 0x8005 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
#       "FM_SERDES_STATUS"             => [ 1, 1,  0, "u", sub { return 0x8004 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_SERDES_TEST_MODE"          => [ 1, 1,  0, "s", sub { return 0x8003 + (0x400 * ($_[0]-1)); }, sub { return (0x00000060); }, [ 1, 24 ] ],
        "FM_SHADOW_FUSEBOX_1"          => [ 1, 0, -1, "s", sub { return 0x319; }, sub { return (0x00000000); } ],
        "FM_SHADOW_FUSEBOX_2"          => [ 1, 0, -1, "s", sub { return 0x31a; }, sub { return (0x00000000); } ],
        "FM_SHADOW_FUSEBOX_3"          => [ 1, 0, -1, "s", sub { return 0x31b; }, sub { return (0x00000000); } ],
        "FM_SHADOW_FUSEBOX_4"          => [ 1, 0, -1, "s", sub { return 0x31c; }, sub { return (0x00000000); } ],
        "FM_SOFT_RESET"                => [ 1, 0, -1, "s", sub { return 0x300; }, sub { return (0x00000002); } ],
        "FM_STATS_CFG"                 => [ 1, 0, -1, "u", sub { return 0x66200; }, sub { return (0x0000036F); } ],
#       "FM_STATS_DROP_COUNT"          => [ 1, 0, -1, "u", sub { return 0x66202; }, sub { return (0x00000000); } ],
#       "FM_STAT_BROADCAST_DROPS"      => [ 2, 1,  0, "u", sub { return 0x70116 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_CMRX_DROPS"           => [ 2, 1,  0, "u", sub { return 0x70118 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_DLFDROPS"             => [ 2, 1,  0, "u", sub { return 0x70114 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_FIDFORWARDED"         => [ 2, 1,  0, "u", sub { return 0x70100 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_FLOOD_FORWARDED"      => [ 2, 1,  0, "u", sub { return 0x70102 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_GLOBAL_HIGH_DROP"     => [ 2, 0, -1, "u", sub { return 0x66002; }, sub { return (0x00000000, 0x00000000); },  ],
#       "FM_STAT_GLOBAL_LOW_DROP"      => [ 2, 0, -1, "u", sub { return 0x66000; }, sub { return (0x00000000, 0x00000000); },  ],
#       "FM_STAT_GLOBAL_PRIV_DROP"     => [ 2, 0, -1, "u", sub { return 0x66004; }, sub { return (0x00000000, 0x00000000); },  ],
#       "FM_STAT_RESERVED_TRAPS"       => [ 2, 1,  0, "u", sub { return 0x70106 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_10240_TO_MAX"      => [ 2, 1,  0, "u", sub { return 0x70096 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_1024_TO_1522"      => [ 2, 1,  0, "u", sub { return 0x7008c + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_128_TO_255"        => [ 2, 1,  0, "u", sub { return 0x70086 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_1523_TO_2047"      => [ 2, 1,  0, "u", sub { return 0x7008e + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_2048_TO_4095"      => [ 2, 1,  0, "u", sub { return 0x70090 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_256_TO_511"        => [ 2, 1,  0, "u", sub { return 0x70088 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_4096_TO_8191"      => [ 2, 1,  0, "u", sub { return 0x70092 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_512_TO_1023"       => [ 2, 1,  0, "u", sub { return 0x7008a + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_64_PKTS"           => [ 2, 1,  0, "u", sub { return 0x70082 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_65_TO_127"         => [ 2, 1,  0, "u", sub { return 0x70084 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_8192_TO_10239"     => [ 2, 1,  0, "u", sub { return 0x70094 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_BAD_OCTETS"        => [ 2, 1,  0, "u", sub { return 0x700a2 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_BCST_PKTS"         => [ 2, 1,  0, "u", sub { return 0x70002 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_FCSERRORS"         => [ 2, 1,  0, "u", sub { return 0x70008 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_FRAGMENTS"         => [ 2, 1,  0, "u", sub { return 0x7009c + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_GOOD_OCTETS"       => [ 2, 1,  0, "u", sub { return 0x700a0 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_JABBERS"           => [ 2, 1,  0, "u", sub { return 0x7009e + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_MCST_PKTS"         => [ 2, 1,  0, "u", sub { return 0x70004 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_MINTO_63"          => [ 2, 1,  0, "u", sub { return 0x70080 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OCTETS_P0"         => [ 2, 1,  0, "u", sub { return 0x70120 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OCTETS_P1"         => [ 2, 1,  0, "u", sub { return 0x70122 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OCTETS_P2"         => [ 2, 1,  0, "u", sub { return 0x70124 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OCTETS_P3"         => [ 2, 1,  0, "u", sub { return 0x70126 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OCTETS_P4"         => [ 2, 1,  0, "u", sub { return 0x70128 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OCTETS_P5"         => [ 2, 1,  0, "u", sub { return 0x7012a + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OCTETS_P6"         => [ 2, 1,  0, "u", sub { return 0x7012c + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OCTETS_P7"         => [ 2, 1,  0, "u", sub { return 0x7012e + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_OVERSIZED"         => [ 2, 1,  0, "u", sub { return 0x7009a + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_P0"                => [ 2, 1,  0, "u", sub { return 0x70010 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_P1"                => [ 2, 1,  0, "u", sub { return 0x70012 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_P2"                => [ 2, 1,  0, "u", sub { return 0x70014 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_P3"                => [ 2, 1,  0, "u", sub { return 0x70016 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_P4"                => [ 2, 1,  0, "u", sub { return 0x70018 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_P5"                => [ 2, 1,  0, "u", sub { return 0x7001a + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_P6"                => [ 2, 1,  0, "u", sub { return 0x7001c + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_P7"                => [ 2, 1,  0, "u", sub { return 0x7001e + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_PAUSE_PKTS"        => [ 2, 1,  0, "u", sub { return 0x70006 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_SYMBOL_ERRORS"     => [ 2, 1,  0, "u", sub { return 0x7000a + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_UCST_PKTS"         => [ 2, 1,  0, "u", sub { return 0x70000 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_RX_UNDERSIZED"        => [ 2, 1,  0, "u", sub { return 0x70098 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_SECURITY_VIOLATIONS"  => [ 2, 1,  0, "u", sub { return 0x70108 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_STPDROPS"             => [ 2, 1,  0, "u", sub { return 0x70104 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TRIG"                 => [ 2, 1,  0, "u", sub { return 0x660c0 + ($_[0] * 2); }, sub { return (0x00000000, 0x00000000); }, [ 0, 16 ] ],
#       "FM_STAT_TRIGGER_DROPS"        => [ 2, 1,  0, "u", sub { return 0x70110 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TRIGGER_MIRRORED"     => [ 2, 1,  0, "u", sub { return 0x70112 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_10240_TO_MAX"      => [ 2, 1,  0, "u", sub { return 0x7014a + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_1024_TO_1522"      => [ 2, 1,  0, "u", sub { return 0x70140 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_128_TO_255"        => [ 2, 1,  0, "u", sub { return 0x7013a + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_1523_TO_2047"      => [ 2, 1,  0, "u", sub { return 0x70142 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_2048_TO_4095"      => [ 2, 1,  0, "u", sub { return 0x70144 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_256_TO_511"        => [ 2, 1,  0, "u", sub { return 0x7013c + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_4096_TO_8191"      => [ 2, 1,  0, "u", sub { return 0x70146 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_512_TO_1023"       => [ 2, 1,  0, "u", sub { return 0x7013e + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_64_PKTS"           => [ 2, 1,  0, "u", sub { return 0x70136 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_65_TO_127"         => [ 2, 1,  0, "u", sub { return 0x70138 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_8192_TO_10239"     => [ 2, 1,  0, "u", sub { return 0x70148 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_BCST_PKTS"         => [ 2, 1,  0, "u", sub { return 0x70022 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_DROP_PORT"         => [ 2, 1,  0, "u", sub { return 0x66080 + ($_[0] * 2); }, sub { return (0x00000000, 0x00000000); }, [ 1, 24 ] ],
#       "FM_STAT_TX_ERROR_DROP"        => [ 2, 1,  0, "u", sub { return 0x70028 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_MCST_PKTS"         => [ 2, 1,  0, "u", sub { return 0x70024 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_MINTO_63"          => [ 2, 1,  0, "u", sub { return 0x70134 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_OCTETS"            => [ 2, 1,  0, "u", sub { return 0x70130 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_TIME_OUT_DROP"     => [ 2, 1,  0, "u", sub { return 0x70026 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_TX_UCST_PKTS"         => [ 2, 1,  0, "u", sub { return 0x70020 + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_VLAN_EGRESS_BV"       => [ 2, 1,  0, "u", sub { return 0x7010e + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_VLAN_INGRESS_BV"      => [ 2, 1,  0, "u", sub { return 0x7010c + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_VLAN_TAG_DROPS"       => [ 2, 1,  0, "u", sub { return 0x7010a + (0x200 * $_[0]); }, sub { return (0x00000000, 0x00000000); }, [ 0, 24 ] ],
#       "FM_STAT_VLAN_UCST_OCTETS"     => [ 2, 1,  0, "u", sub { return 0x66180 + ($_[0] * 2); }, sub { return (0x00000000, 0x00000000); }, [ 0, 31 ] ],
#       "FM_STAT_VLAN_UCST_PKTS"       => [ 2, 1,  0, "u", sub { return 0x66100 + ($_[0] * 2); }, sub { return (0x00000000, 0x00000000); }, [ 0, 31 ] ],
#       "FM_STAT_VLAN_XCST_OCTETS"     => [ 2, 1,  0, "u", sub { return 0x661c0 + ($_[0] * 2); }, sub { return (0x00000000, 0x00000000); }, [ 0, 31 ] ],
#       "FM_STAT_VLAN_XCST_PKTS"       => [ 2, 1,  0, "u", sub { return 0x66140 + ($_[0] * 2); }, sub { return (0x00000000, 0x00000000); }, [ 0, 31 ] ],
#       "FM_STREAM_STATUS_1"           => [ 1, 1,  0, "u", sub { return 0x64060 + $_[0]; }, sub { return (0x00000000); }, [ 0, 24 ] ],
#       "FM_STREAM_STATUS_2"           => [ 1, 0, -1, "u", sub { return 0x64008; }, sub { return (0x00000000); } ],
#       "FM_STAT_EPL_ERROR2"           => [ 1, 1,  0, "u", sub { return 0x8028 + (0x400 * ($_[0]-1)); }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_SYS_CFG_1"                 => [ 1, 0, -1, "u", sub { return 0x60001; }, sub { return (0x000004FF); } ],
        "FM_SYS_CFG_2"                 => [ 1, 0, -1, "u", sub { return 0x58121; }, sub { return (0x00000000); } ],
        "FM_SYS_CFG_3"                 => [ 1, 0, -1, "u", sub { return 0x60002; }, sub { return (0x00000000); } ],
        "FM_SYS_CFG_4"                 => [ 1, 0, -1, "u", sub { return 0x60003; }, sub { return (0x00000000); } ],
        "FM_SYS_CFG_6"                 => [ 1, 0, -1, "u", sub { return 0x60004; }, sub { return (0x0000888E); } ],
        "FM_SYS_CFG_7"                 => [ 1, 0, -1, "u", sub { return 0x30f; }, sub { return (0x80007530); } ],
        "FM_TRIGGER_CFG"               => [ 1, 1, -1, "u", sub { return 0x62020 + $_[0]; }, sub { return (0x00000003); }, [ 0, 15 ] ],
        "FM_TRIGGER_IM"                => [ 1, 0, -1, "u", sub { return 0x640c7; }, sub { return (0x0000FFFF); } ],
#       "FM_TRIGGER_IP"                => [ 1, 0, -1, "u", sub { return 0x640c6; }, sub { return (0x00000000); } ],
        "FM_TRIGGER_PRI"               => [ 1, 1, -1, "u", sub { return 0x62040 + $_[0]; }, sub { return (0x00000000); }, [ 0, 15 ] ],
        "FM_TRIGGER_RX"                => [ 1, 1, -1, "u", sub { return 0x62060 + $_[0]; }, sub { return (0x00000000); }, [ 0, 15 ] ],
        "FM_TRIGGER_TX"                => [ 1, 1, -1, "u", sub { return 0x62080 + $_[0]; }, sub { return (0x00000000); }, [ 0, 15 ] ],
        "FM_TRUNK_CANONICAL"           => [ 1, 1,  0, "u", sub { return 0x60020 + $_[0]; }, sub { return ($_[0]); }, [ 1, 24 ] ],
        "FM_TRUNK_GROUP_1"             => [ 1, 1, -1, "u", sub { return 0x63020 + $_[0]; }, sub { return (0x00000000); }, [ 0, 11 ] ],
        "FM_TRUNK_GROUP_2"             => [ 1, 1, -1, "u", sub { return 0x63040 + $_[0]; }, sub { return (0x00000000); }, [ 0, 11 ] ],
        "FM_TRUNK_GROUP_3"             => [ 1, 1, -1, "u", sub { return 0x63060 + $_[0]; }, sub { return (0x00000000); }, [ 0, 11 ] ],
        "FM_TRUNK_HASH_MASK"           => [ 1, 0, -1, "u", sub { return 0x61000; }, sub { return (0x00000033); } ],
        "FM_TRUNK_PORT_MAP"            => [ 1, 1,  0, "u", sub { return 0x63000 + $_[0]; }, sub { return (0x00000000); }, [ 1, 24 ] ],
        "FM_TX_PRI_MAP_1"              => [ 1, 1,  0, "u", sub { return 0x8020 + (0x400 * ($_[0]-1)); }, sub { return (0x76543210); }, [ 1, 24 ] ],
        "FM_TX_PRI_MAP_2"              => [ 1, 1,  0, "u", sub { return 0x8021 + (0x400 * ($_[0]-1)); }, sub { return (0x76543210); }, [ 1, 24 ] ],
        "FM_VID_TABLE"                 => [ 2, 1, -1, "s", sub { return 0x50000 + ($_[0] * 2); }, sub { return (0x00000000, 0x00000000); }, [ 0, 4095 ] ],
#       "FM_VITAL_PRODUCT_DATA"        => [ 1, 0, -1, "u", sub { return 0x304; }, sub { return (0x0AE1842B); } ],
);


# %bali_regs_hash
#
# Entry format:
#   [ word-count, index-count, port-index, ordering,
#     addr-sub, default-sub, bounds-list ]
#
# word-count:  number of 32-bit words that comprise each
#              register value (1-4)
# index-count: number of indices that must be provided to
#              fully specify a register (size of bounds-list)
# port-index:  which index is a port number (-1 for none)
#              (used to filter out writes to unused ports)
# ordering:    indicates whether this register is handled
#              as a special case ("s") or as part of the
#              unordered ("u") automated loop.
# addr-sub:    subroutine that returns a register address
#              given appropriate index arguments
# default-sub: subroutine that returns the register default
#              value given appropriate index arguments
# bounds-list: list of list references.  Each list reference
#              represents one index argument to the subroutine
#              and gives the min and max values allowed.

my %bali_regs_hash = (

# Block: HSM
        "FM_LCI_CFG"                     => [ 1, 0, -1, "u", sub { return 0x1; }, sub { return (0x00000004); } ],
        "FM_LCI_RX_FIFO"                 => [ 1, 0, -1, "s", sub { return 0x2; }, sub { return (0x00000000); } ],
        "FM_LCI_TX_FIFO"                 => [ 1, 0, -1, "s", sub { return 0x3; }, sub { return (0x00000000); } ],
#       "FM_LCI_IP"                      => [ 1, 0, -1, "u", sub { return 0x4; }, sub { return (0x00000000); } ],
        "FM_LCI_IM"                      => [ 1, 0, -1, "u", sub { return 0x5; }, sub { return (0x00000006); } ],
#       "FM_LCI_STATUS"                  => [ 1, 0, -1, "u", sub { return 0x6; }, sub { return (0x00000001); } ],
#       "FM_LAST_FATAL_CODE"             => [ 1, 0, -1, "u", sub { return 0x200; }, sub { return (0x00000000); } ],
#       "FM_INTERRUPT_DETECT"            => [ 1, 0, -1, "u", sub { return 0x201; }, sub { return (0x00000000); } ],
#       "FM_FATAL_COUNT"                 => [ 1, 0, -1, "u", sub { return 0x202; }, sub { return (0x00000000); } ],
        "FM_SOFT_RESET"                  => [ 1, 0, -1, "s", sub { return 0x203; }, sub { return (0x00000008); } ],
        "FM_WATCHDOG_CFG"                => [ 1, 0, -1, "u", sub { return 0x204; }, sub { return (0x00000000); } ],
#       "FM_PLL_FH_STAT"                 => [ 1, 0, -1, "u", sub { return 0x205; }, sub { return (0x00000000); } ],
        "FM_PLL_FH_CTRL"                 => [ 1, 0, -1, "s", sub { return 0x206; }, sub { return (0x00002142); } ],
        "FM_PREFETCH_CFG"                => [ 1, 0, -1, "u", sub { return 0x401; }, sub { return (0x00000008); } ],
#       "FM_VITAL_PRODUCT_DATA"          => [ 1, 0, -1, "u", sub { return 0x304; }, sub { return (0x0ae19000); } ],

# Block: LSM
#       "FM_LSM_INT_DETECT"              => [ 1, 0, -1, "u", sub { return 0x40000; }, sub { return (0x00000000); } ],
#       "FM_GLOBAL_EPL_INT_DETECT"       => [ 1, 0, -1, "u", sub { return 0x40001; }, sub { return (0x00000000); } ],
#       "FM_PERR_IP"                     => [ 1, 0, -1, "u", sub { return 0x40002; }, sub { return (0x00000000); } ],
        "FM_PERR_IM"                     => [ 1, 0, -1, "u", sub { return 0x40003; }, sub { return (0x0000ffff); } ],
#       "FM_SW_IP"                       => [ 1, 0, -1, "u", sub { return 0x40004; }, sub { return (0x00000000); } ],
        "FM_SW_IM"                       => [ 1, 0, -1, "u", sub { return 0x40005; }, sub { return (0xffffffff); } ],
        "FM_FRAME_TIME_OUT"              => [ 1, 0, -1, "u", sub { return 0x40101; }, sub { return (0x000088b8); } ],
        "FM_BOOT_CTRL"                   => [ 1, 0, -1, "s", sub { return 0x40102; }, sub { return (0x00000000); } ],
#       "FM_BOOT_STATUS"                 => [ 1, 0, -1, "u", sub { return 0x40103; }, sub { return (0x00000000); } ],
        "FM_CLK_MULT_1"                  => [ 1, 0, -1, "s", sub { return 0x40104; }, sub { return (0x00000019); } ],
        "FM_FUSEBOX_CFG"                 => [ 1, 0, -1, "s", sub { return 0x40105; }, sub { return (0x00000040); } ],
        "FM_VPD_INFO_1"                  => [ 2, 0, -1, "s", sub { return 0x40108; }, sub { return (0x00000000, 0x00900000); } ],
        "FM_VPD_INFO_2"                  => [ 2, 0, -1, "s", sub { return 0x4010a; }, sub { return (0x00000215, 0x00000000); } ],
        "FM_VPD_ASYNC_RAM_REPAIR"        => [ 1, 1, -1, "s", sub { return 0x4010c + $_[0]; }, sub { return (0xffffffff); }, [0, 3] ],
        "FM_VPD_SYNC_RAM_REPAIR"         => [ 2, 1, -1, "s", sub { return 0x40110 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 11] ],
        "FM_GPIO_CFG"                    => [ 1, 0, -1, "u", sub { return 0x40134; }, sub { return (0x00000000); } ],
        "FM_GPIO_DATA"                   => [ 1, 0, -1, "s", sub { return 0x40135; }, sub { return (0x00000000); } ],
#       "FM_GPIO_IP"                     => [ 1, 0, -1, "u", sub { return 0x40136; }, sub { return (0x00000000); } ],
        "FM_GPIO_IM"                     => [ 1, 0, -1, "u", sub { return 0x40137; }, sub { return (0xffffffff); } ],
        "FM_I2C_CFG"                     => [ 1, 0, -1, "u", sub { return 0x40138; }, sub { return (0x29010081); } ],
        "FM_I2C_DATA"                    => [ 1, 1, -1, "s", sub { return 0x40139 + $_[0]; }, sub { return (0x00000000); }, [0, 1] ],
        "FM_I2C_CTRL"                    => [ 1, 0, -1, "u", sub { return 0x4013b; }, sub { return (0x00780000); } ],
        "FM_MDIO_CFG"                    => [ 1, 0, -1, "u", sub { return 0x4013c; }, sub { return (0x00003010); } ],
        "FM_MDIO_DATA"                   => [ 1, 0, -1, "s", sub { return 0x4013d; }, sub { return (0x00000000); } ],
        "FM_MDIO_CTRL"                   => [ 1, 0, -1, "u", sub { return 0x4013e; }, sub { return (0x00000000); } ],
        "FM_LED_CFG"                     => [ 1, 0, -1, "u", sub { return 0x4013f; }, sub { return (0x00000000); } ],
        "FM_EPL_PORT_CTRL"               => [ 1, 1,  0, "s", sub { return 0x40160 + $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_FUSEBOX"                     => [ 1, 1, -1, "s", sub { return 0x40180 + $_[0]; }, sub { return (0x00000000); }, [0, 127] ],
        "FM_ASYNC_SCAN_CFG"              => [ 1, 0, -1, "s", sub { return 0x40200; }, sub { return (0x00010000); } ],
        "FM_ASYNC_SCAN_CMD"              => [ 1, 0, -1, "s", sub { return 0x40201; }, sub { return (0x00000000); } ],
        "FM_ASYNC_SCAN_IN"               => [ 1, 0, -1, "s", sub { return 0x40202; }, sub { return (0x00000000); } ],
        "FM_ASYNC_SCAN_OUT"              => [ 1, 0, -1, "s", sub { return 0x40203; }, sub { return (0x00000000); } ],
        "FM_CRM_CFG_COUNTER"             => [ 1, 1, -1, "u", sub { return 0x41200 + $_[0]; }, sub { return (0x00000000); }, [0, 255] ],
        "FM_CRM_CFG_WINDOW"              => [ 1, 1, -1, "u", sub { return 0x41300 + $_[0]; }, sub { return (0x00000000); }, [0, 255] ],
        "FM_CRM_CFG_LIMIT"               => [ 1, 1, -1, "u", sub { return 0x41400 + $_[0]; }, sub { return (0x00000000); }, [0, 255] ],
#       "FM_CRM_LAST_COUNT"              => [ 1, 1, -1, "u", sub { return 0x41500 + $_[0]; }, sub { return (0x00000000); }, [0, 255] ],
#       "FM_CRM_EXCEED_COUNT"            => [ 1, 1, -1, "u", sub { return 0x41600 + $_[0]; }, sub { return (0x00000000); }, [0, 255] ],
        "FM_CRM_CFG"                     => [ 1, 0, -1, "u", sub { return 0x41700; }, sub { return (0x00000001); } ],
#       "FM_CRM_INT_DETECT"              => [ 1, 0, -1, "u", sub { return 0x41701; }, sub { return (0x00000000); } ],
#       "FM_CRM_IP"                      => [ 1, 1, -1, "u", sub { return 0x41710 + $_[0]; }, sub { return (0x00000000); }, [0, 7] ],
        "FM_CRM_IM"                      => [ 1, 1, -1, "u", sub { return 0x41720 + $_[0]; }, sub { return (0xffffffff); }, [0, 7] ],
        "FM_SOT_CFG"                     => [ 1, 0, -1, "s", sub { return 0x41800; }, sub { return (0x00000000); } ],

# Block: EPL
        "FM_SERDES_CTRL_1"               => [ 1, 1,  0, "u", sub { return 0x50000 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_SERDES_CTRL_2"               => [ 1, 1,  0, "s", sub { return 0x50001 + 0x400 * $_[0]; }, sub { return (0x0003ff00); }, [0, 24] ],
        "FM_SERDES_CTRL_3"               => [ 1, 1,  0, "s", sub { return 0x50002 + 0x400 * $_[0]; }, sub { return (0x1321312d); }, [0, 24] ],
        "FM_SERDES_TEST_MODE"            => [ 1, 1,  0, "s", sub { return 0x50003 + 0x400 * $_[0]; }, sub { return (0x00000060); }, [0, 24] ],
#       "FM_SERDES_STATUS"               => [ 1, 1,  0, "u", sub { return 0x50004 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_SERDES_IP"                   => [ 1, 1,  0, "u", sub { return 0x50005 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_SERDES_IM"                   => [ 1, 1,  0, "u", sub { return 0x50006 + 0x400 * $_[0]; }, sub { return (0x00000fff); }, [0, 24] ],
        "FM_SERDES_BIST_ERR_CNT"         => [ 1, 1,  0, "u", sub { return 0x50007 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_PCS_CFG_1"                   => [ 1, 1,  0, "u", sub { return 0x50009 + 0x400 * $_[0]; }, sub { return (0x80000c0a); }, [0, 24] ],
        "FM_PCS_CFG_2"                   => [ 1, 1,  0, "u", sub { return 0x5000a + 0x400 * $_[0]; }, sub { return (0x00000001); }, [0, 24] ],
        "FM_PCS_CFG_3"                   => [ 1, 1,  0, "u", sub { return 0x5000b + 0x400 * $_[0]; }, sub { return (0x00000002); }, [0, 24] ],
        "FM_PCS_CFG_4"                   => [ 1, 1,  0, "u", sub { return 0x5000c + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#        "FM_PCS_CFG_5"                   => [ 1, 1,  0, "u", sub { return 0x5000d + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_PCS_IP"                      => [ 1, 1,  0, "u", sub { return 0x5000e + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_PCS_IM"                      => [ 1, 1,  0, "u", sub { return 0x5000f + 0x400 * $_[0]; }, sub { return (0x0003ffff); }, [0, 24] ],
        "FM_SYNCBUF_CFG"                 => [ 1, 1,  0, "u", sub { return 0x50018 + 0x400 * $_[0]; }, sub { return (0x00002800); }, [0, 24] ],
        "FM_MAC_CFG_1"                   => [ 1, 1,  0, "u", sub { return 0x5001a + 0x400 * $_[0]; }, sub { return (0x10180000); }, [0, 24] ],
        "FM_MAC_CFG_2"                   => [ 1, 1,  0, "u", sub { return 0x5001b + 0x400 * $_[0]; }, sub { return (0x0007217c); }, [0, 24] ],
        "FM_MAC_CFG_3"                   => [ 1, 1,  0, "u", sub { return 0x5001c + 0x400 * $_[0]; }, sub { return (0x4010ffff); }, [0, 24] ],
        "FM_MAC_CFG_5"                   => [ 1, 1,  0, "u", sub { return 0x5001e + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_MAC_CFG_6"                   => [ 1, 1,  0, "u", sub { return 0x5001f + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_TX_VPRI_MAP_1"               => [ 1, 1,  0, "u", sub { return 0x50020 + 0x400 * $_[0]; }, sub { return (0xeca86420); }, [0, 24] ],
        "FM_TX_VPRI_MAP_2"               => [ 1, 1,  0, "u", sub { return 0x50021 + 0x400 * $_[0]; }, sub { return (0xeca86420); }, [0, 24] ],
#       "FM_MAC_STATUS"                  => [ 1, 1,  0, "u", sub { return 0x50022 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_MAC_IP"                      => [ 1, 1,  0, "u", sub { return 0x50023 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_MAC_IM"                      => [ 1, 1,  0, "u", sub { return 0x50024 + 0x400 * $_[0]; }, sub { return (0x00000fff); }, [0, 24] ],
#       "FM_EPL_INT_DETECT"              => [ 1, 1,  0, "u", sub { return 0x5002b + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_EPL_LED_STATUS"              => [ 1, 1,  0, "u", sub { return 0x5002a + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_STAT_EPL_ERROR1"             => [ 1, 1,  0, "u", sub { return 0x50025 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_STAT_EPL_ERROR2"             => [ 1, 1,  0, "u", sub { return 0x50028 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_STAT_TX_BYTECOUNT"           => [ 2, 1,  0, "u", sub { return 0x5002c + 0x400 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RX_JABBER"              => [ 1, 1,  0, "u", sub { return 0x50029 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_STAT_TX_CRC"                 => [ 1, 1,  0, "u", sub { return 0x50027 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_STAT_TX_PAUSE"               => [ 1, 1,  0, "u", sub { return 0x50026 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_DEBUG_RX_MAC"                => [ 1, 1,  0, "u", sub { return 0x5002e + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_DEBUG_RX_RS"                 => [ 1, 1,  0, "u", sub { return 0x5002f + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_DEBUG_SYNCBUF"               => [ 1, 1,  0, "u", sub { return 0x50030 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_DEBUG_TX_MAC"                => [ 1, 1,  0, "u", sub { return 0x50031 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_SRC_MAC_LO"                  => [ 1, 1,  0, "u", sub { return 0x50033 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_SRC_MAC_HI"                  => [ 1, 1,  0, "u", sub { return 0x50034 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_SRC_MAC_VIRTUAL_LO"          => [ 1, 1,  0, "u", sub { return 0x50035 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_SRC_MAC_VIRTUAL_HI"          => [ 1, 1,  0, "u", sub { return 0x50036 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_JITTER_TIMER"                => [ 1, 1,  0, "u", sub { return 0x5003d + 0x400 * $_[0]; }, sub { return (0x001c1000); }, [0, 24] ],
        "FM_PARSE_CFG"                   => [ 1, 1,  0, "u", sub { return 0x5003e + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_MAC_VLAN_ETYPE_1"            => [ 1, 1,  0, "u", sub { return 0x50066 + 0x400 * $_[0]; }, sub { return (0xf3208100); }, [0, 24] ],
        "FM_MAC_VLAN_ETYPE_2"            => [ 1, 1,  0, "u", sub { return 0x5003f + 0x400 * $_[0]; }, sub { return (0x910088a8); }, [0, 24] ],
        "FM_PARSE_RLT_1"                 => [ 1, 1,  0, "u", sub { return 0x50040 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_PARSE_RLT_2"                 => [ 1, 1,  0, "u", sub { return 0x50041 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_TX_TRUNC"                    => [ 1, 1,  0, "u", sub { return 0x50042 + 0x400 * $_[0]; }, sub { return (0x00160016); }, [0, 24] ],
        "FM_CPID_0"                      => [ 2, 1,  0, "u", sub { return 0x50050 + 0x400 * $_[0]; }, sub { return (0x00000016, 0x00000000); }, [0, 24] ],
        "FM_CPID_1"                      => [ 2, 1,  0, "u", sub { return 0x50052 + 0x400 * $_[0]; }, sub { return (0x00000016, 0x00000000); }, [0, 24] ],
        "FM_CPID_2"                      => [ 2, 1,  0, "u", sub { return 0x50054 + 0x400 * $_[0]; }, sub { return (0x00000016, 0x00000000); }, [0, 24] ],
        "FM_CPID_3"                      => [ 2, 1,  0, "u", sub { return 0x50056 + 0x400 * $_[0]; }, sub { return (0x00000016, 0x00000000); }, [0, 24] ],
        "FM_CPID_4"                      => [ 2, 1,  0, "u", sub { return 0x50058 + 0x400 * $_[0]; }, sub { return (0x00000016, 0x00000000); }, [0, 24] ],
        "FM_CPID_5"                      => [ 2, 1,  0, "u", sub { return 0x5005a + 0x400 * $_[0]; }, sub { return (0x00000016, 0x00000000); }, [0, 24] ],
        "FM_CPID_6"                      => [ 2, 1,  0, "u", sub { return 0x5005c + 0x400 * $_[0]; }, sub { return (0x00000016, 0x00000000); }, [0, 24] ],
        "FM_CPID_7"                      => [ 2, 1,  0, "u", sub { return 0x5005e + 0x400 * $_[0]; }, sub { return (0x00000016, 0x00000000); }, [0, 24] ],
        "FM_DI_CFG"                      => [ 1, 1,  0, "u", sub { return 0x50067 + 0x400 * $_[0]; }, sub { return (0x00800000); }, [0, 24] ],
        "FM_TCP_WD_MASK_LO"              => [ 1, 1,  0, "u", sub { return 0x50068 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_TCP_WD_MASK_HI"              => [ 1, 1,  0, "u", sub { return 0x50069 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_UDP_WD_MASK_LO"              => [ 1, 1,  0, "u", sub { return 0x5006a + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_UDP_WD_MASK_HI"              => [ 1, 1,  0, "u", sub { return 0x5006b + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_L4PROT1_WD_MASK_LO"          => [ 1, 1,  0, "u", sub { return 0x5006c + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_L4PROT1_WD_MASK_HI"          => [ 1, 1,  0, "u", sub { return 0x5006d + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_L4PROT2_WD_MASK_LO"          => [ 1, 1,  0, "u", sub { return 0x5006e + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_L4PROT2_WD_MASK_HI"          => [ 1, 1,  0, "u", sub { return 0x5006f + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_AN_TX_MSG0"                  => [ 2, 1,  0, "u", sub { return 0x50070 + 0x400 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
        "FM_AN_TX_MSG1"                  => [ 2, 1,  0, "u", sub { return 0x50072 + 0x400 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_AN_RX_MSG0"                  => [ 2, 1,  0, "u", sub { return 0x50074 + 0x400 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_AN_RX_MSG1"                  => [ 2, 1,  0, "u", sub { return 0x50076 + 0x400 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
        "FM_AN_CTL"                      => [ 1, 1,  0, "u", sub { return 0x50078 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_AN_STATUS"                   => [ 1, 1,  0, "u", sub { return 0x50079 + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_AN_TIMEOUT"                  => [ 1, 1,  0, "u", sub { return 0x5007a + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_AN_TX_TIMER"                 => [ 1, 1,  0, "u", sub { return 0x5007b + 0x400 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_VLANTAG_TABLE"               => [ 1, 2,  0, "u", sub { return 0x50080 + 0x400 * $_[0] + $_[1]; }, sub { return (0x00000000); }, [0, 24], [0, 127] ],

# Block: SCHED2
        "FM_SCHED_GROUP_CFG"             => [ 1, 1,  0, "u", sub { return 0x60040 + 0x2 * $_[0]; }, sub { return (0x00ffffff); }, [0, 24] ],
        "FM_FUSE_SEG"                    => [ 1, 0, -1, "s", sub { return 0x600ff; }, sub { return (0x00000000); } ],
        "FM_FUSE_PORT"                   => [ 1, 0, -1, "s", sub { return 0x600fe; }, sub { return (0x00000000); } ],

# Block: MTABLE
        "FM_TX_MIRROR"                   => [ 1, 0, -1, "u", sub { return 0xc0000; }, sub { return (0x00000000); } ],
        "FM_LOG_MASK"                    => [ 1, 0, -1, "u", sub { return 0xc0001; }, sub { return (0x00000000); } ],
        "FM_MIRROR_GLORTS"               => [ 1, 0, -1, "u", sub { return 0xc4019; }, sub { return (0x0000ff00); } ],
        "FM_LOOPBACK_SUPPRESS"           => [ 1, 1,  0, "u", sub { return 0xc4000 + $_[0]; }, sub { return (0x0000ffff); }, [0, 24] ],
        "FM_IP_MULTICAST_TABLE"          => [ 1, 1, -1, "u", sub { return 0xc8000 + $_[0]; }, sub { return (0x00000000); }, [0, 16383] ],

# Block: PORT_STATS

# Block: STATS_RX_TYPE
#       "FM_STAT_RxUcstPktsNonIP"        => [ 2, 1,  0, "u", sub { return 0x90000 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxMcstPktsNonIP"        => [ 2, 1,  0, "u", sub { return 0x90070 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxBcstPktsNonIP"        => [ 2, 1,  0, "u", sub { return 0x90002 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxUcstPktsIPv4"         => [ 2, 1,  0, "u", sub { return 0x90072 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxMcstPktsIPv4"         => [ 2, 1,  0, "u", sub { return 0x900e2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxBcstPktsIPv4"         => [ 2, 1,  0, "u", sub { return 0x900e0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxUcstPktsIPv6"         => [ 2, 1,  0, "u", sub { return 0x90150 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxMcstPktsIPv6"         => [ 2, 1,  0, "u", sub { return 0x901c0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxBcstPktsIPv6"         => [ 2, 1,  0, "u", sub { return 0x90152 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxPausePkts"            => [ 2, 1,  0, "u", sub { return 0x901c2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxCBPausePkts"          => [ 2, 1,  0, "u", sub { return 0x90230 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxSymbolErrors"         => [ 2, 1,  0, "u", sub { return 0x90232 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxFCSErrors"            => [ 2, 1,  0, "u", sub { return 0x902a0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxFrameSizeErrors"      => [ 2, 1,  0, "u", sub { return 0x902a2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_RX_BIN
#       "FM_STAT_RxMinto63"              => [ 2, 1,  0, "u", sub { return 0x91000 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx64Pkts"               => [ 2, 1,  0, "u", sub { return 0x91002 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx65to127"              => [ 2, 1,  0, "u", sub { return 0x91070 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx128to255"             => [ 2, 1,  0, "u", sub { return 0x91072 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx256to511"             => [ 2, 1,  0, "u", sub { return 0x910e0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx512to1023"            => [ 2, 1,  0, "u", sub { return 0x910e2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx1024to1522"           => [ 2, 1,  0, "u", sub { return 0x91150 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx1523to2047"           => [ 2, 1,  0, "u", sub { return 0x91152 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx2048to4095"           => [ 2, 1,  0, "u", sub { return 0x911c0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx4096to8191"           => [ 2, 1,  0, "u", sub { return 0x911c2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx8192to10239"          => [ 2, 1,  0, "u", sub { return 0x91230 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Rx10240toMax"           => [ 2, 1,  0, "u", sub { return 0x91232 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxFragments"            => [ 2, 1,  0, "u", sub { return 0x912a0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxUndersized"           => [ 2, 1,  0, "u", sub { return 0x912a2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOversized"            => [ 2, 1,  0, "u", sub { return 0x91310 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_RX_OCTET
#       "FM_STAT_RxOctetsNonIP"          => [ 2, 1,  0, "u", sub { return 0x91380 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsIPv4"           => [ 2, 1,  0, "u", sub { return 0x91382 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsIPv6"           => [ 2, 1,  0, "u", sub { return 0x913f0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsError"          => [ 2, 1,  0, "u", sub { return 0x913f2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_RX_PKT_PRI
#       "FM_STAT_RxP0"                   => [ 2, 1,  0, "u", sub { return 0x90310 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxP1"                   => [ 2, 1,  0, "u", sub { return 0x90312 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxP2"                   => [ 2, 1,  0, "u", sub { return 0x90380 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxP3"                   => [ 2, 1,  0, "u", sub { return 0x90382 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxP4"                   => [ 2, 1,  0, "u", sub { return 0x903f0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxP5"                   => [ 2, 1,  0, "u", sub { return 0x903f2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxP6"                   => [ 2, 1,  0, "u", sub { return 0x90460 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxP7"                   => [ 2, 1,  0, "u", sub { return 0x90462 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_RX_OCT_PRI
#       "FM_STAT_RxOctetsP0"             => [ 2, 1,  0, "u", sub { return 0x92700 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsP1"             => [ 2, 1,  0, "u", sub { return 0x92702 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsP2"             => [ 2, 1,  0, "u", sub { return 0x92770 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsP3"             => [ 2, 1,  0, "u", sub { return 0x92772 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsP4"             => [ 2, 1,  0, "u", sub { return 0x927e0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsP5"             => [ 2, 1,  0, "u", sub { return 0x927e2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsP6"             => [ 2, 1,  0, "u", sub { return 0x92850 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxOctetsP7"             => [ 2, 1,  0, "u", sub { return 0x92852 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_RX_ACTION
#       "FM_STAT_FIDForwarded"           => [ 2, 1,  0, "u", sub { return 0x92000 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_FloodForwarded"         => [ 2, 1,  0, "u", sub { return 0x92002 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_SpeciallyHandled"       => [ 2, 1,  0, "u", sub { return 0x92070 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_ParseErrDrops"          => [ 2, 1,  0, "u", sub { return 0x92072 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_ParityError"            => [ 2, 1,  0, "u", sub { return 0x920e0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Trapped"                => [ 2, 1,  0, "u", sub { return 0x920e2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_PauseDrops"             => [ 2, 1,  0, "u", sub { return 0x92150 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_STPDrops"               => [ 2, 1,  0, "u", sub { return 0x92152 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_SecurityViolations"     => [ 2, 1,  0, "u", sub { return 0x921c0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_VlanTagDrops"           => [ 2, 1,  0, "u", sub { return 0x921c2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_VlanIngressDrops"       => [ 2, 1,  0, "u", sub { return 0x92230 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_VlanEgressDrops"        => [ 2, 1,  0, "u", sub { return 0x92232 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_GlortMissDrops"         => [ 2, 1,  0, "u", sub { return 0x922a0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_FFUDrops"               => [ 2, 1,  0, "u", sub { return 0x922a2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TriggerDrops"           => [ 2, 1,  0, "u", sub { return 0x92310 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_PolicerDrops"           => [ 2, 1,  0, "u", sub { return 0x92312 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TTLDrops"               => [ 2, 1,  0, "u", sub { return 0x92380 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_CMPrivDrops"            => [ 2, 1,  0, "u", sub { return 0x92382 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_SMP0Drops"              => [ 2, 1,  0, "u", sub { return 0x923f0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_SMP1Drops"              => [ 2, 1,  0, "u", sub { return 0x923f2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxHog0Drops"            => [ 2, 1,  0, "u", sub { return 0x92460 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RxHog1Drops"            => [ 2, 1,  0, "u", sub { return 0x92462 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxHog0Drops"            => [ 2, 1,  0, "u", sub { return 0x924d0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxHog1Drops"            => [ 2, 1,  0, "u", sub { return 0x924d2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RateLimit0Drops"        => [ 2, 1,  0, "u", sub { return 0x92540 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_RateLimit1Drops"        => [ 2, 1,  0, "u", sub { return 0x92542 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_BadSMPDrops"            => [ 2, 1,  0, "u", sub { return 0x925b0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TriggerRedirects"       => [ 2, 1,  0, "u", sub { return 0x925b2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Others"                 => [ 2, 1,  0, "u", sub { return 0x92620 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_TX_TYPE
#       "FM_STAT_TxUcstPkts"             => [ 2, 1,  0, "u", sub { return 0x904d0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxBcstPkts"             => [ 2, 1,  0, "u", sub { return 0x904d2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxMcstPkts"             => [ 2, 1,  0, "u", sub { return 0x90540 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxTimeOutDrop"          => [ 2, 1,  0, "u", sub { return 0x90542 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxErrorDrop"            => [ 2, 1,  0, "u", sub { return 0x905b0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxLoopbackDrop"         => [ 2, 1,  0, "u", sub { return 0x905b2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_TX_BIN
#       "FM_STAT_TxMinto63"              => [ 2, 1,  0, "u", sub { return 0x91460 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx64Pkts"               => [ 2, 1,  0, "u", sub { return 0x91462 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx65to127"              => [ 2, 1,  0, "u", sub { return 0x914d0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx128to255"             => [ 2, 1,  0, "u", sub { return 0x914d2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx256to511"             => [ 2, 1,  0, "u", sub { return 0x91540 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx512to1023"            => [ 2, 1,  0, "u", sub { return 0x91542 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx1024to1522"           => [ 2, 1,  0, "u", sub { return 0x915b0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx1523to2047"           => [ 2, 1,  0, "u", sub { return 0x915b2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx2048to4095"           => [ 2, 1,  0, "u", sub { return 0x91620 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx4096to8191"           => [ 2, 1,  0, "u", sub { return 0x91622 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx8192to10239"          => [ 2, 1,  0, "u", sub { return 0x91690 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_Tx10240toMax"           => [ 2, 1,  0, "u", sub { return 0x91692 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxErrors"               => [ 2, 1,  0, "u", sub { return 0x91700 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_TX_OCTET
#       "FM_STAT_TxOctets"               => [ 2, 1,  0, "u", sub { return 0x928c0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxErrorOctets"          => [ 2, 1,  0, "u", sub { return 0x928c2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_TX_INGRESS
#       "FM_STAT_TxPort0"                => [ 2, 1,  0, "u", sub { return 0x90620 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort1"                => [ 2, 1,  0, "u", sub { return 0x90622 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort2"                => [ 2, 1,  0, "u", sub { return 0x90690 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort3"                => [ 2, 1,  0, "u", sub { return 0x90692 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort4"                => [ 2, 1,  0, "u", sub { return 0x90700 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort5"                => [ 2, 1,  0, "u", sub { return 0x90702 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort6"                => [ 2, 1,  0, "u", sub { return 0x90770 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort7"                => [ 2, 1,  0, "u", sub { return 0x90772 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort8"                => [ 2, 1,  0, "u", sub { return 0x907e0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort9"                => [ 2, 1,  0, "u", sub { return 0x907e2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort10"               => [ 2, 1,  0, "u", sub { return 0x90850 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort11"               => [ 2, 1,  0, "u", sub { return 0x90852 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort12"               => [ 2, 1,  0, "u", sub { return 0x908c0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort13"               => [ 2, 1,  0, "u", sub { return 0x908c2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort14"               => [ 2, 1,  0, "u", sub { return 0x90930 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort15"               => [ 2, 1,  0, "u", sub { return 0x90932 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort16"               => [ 2, 1,  0, "u", sub { return 0x909a0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort17"               => [ 2, 1,  0, "u", sub { return 0x909a2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort18"               => [ 2, 1,  0, "u", sub { return 0x90a10 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort19"               => [ 2, 1,  0, "u", sub { return 0x90a12 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort20"               => [ 2, 1,  0, "u", sub { return 0x90a80 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort21"               => [ 2, 1,  0, "u", sub { return 0x90a82 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort22"               => [ 2, 1,  0, "u", sub { return 0x90af0 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort23"               => [ 2, 1,  0, "u", sub { return 0x90af2 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_TxPort24"               => [ 2, 1,  0, "u", sub { return 0x90b60 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: MSB
        "FM_MSB_CFG"                     => [ 1, 0, -1, "u", sub { return 0x80000; }, sub { return (0x0000000f); } ],
        "FM_MSB_IBM_GLORT"               => [ 1, 0, -1, "u", sub { return 0x80001; }, sub { return (0x00000000); } ],
        "FM_MSB_IBM_INT"                 => [ 1, 0, -1, "u", sub { return 0x80002; }, sub { return (0x00ff0000); } ],
        "FM_MSB_INT_FRAME"               => [ 1, 0, -1, "u", sub { return 0x80003; }, sub { return (0x000fffff); } ],
#       "FM_MSB_STATS_0"                 => [ 1, 0, -1, "u", sub { return 0x80004; }, sub { return (0x00000000); } ],
#       "FM_MSB_STATS_1"                 => [ 1, 0, -1, "u", sub { return 0x80005; }, sub { return (0x00000000); } ],
#       "FM_MSB_STATS_2"                 => [ 1, 0, -1, "u", sub { return 0x80006; }, sub { return (0x00000000); } ],
#       "FM_MSB_INTR_CTR_0"              => [ 1, 0, -1, "u", sub { return 0x80007; }, sub { return (0x00000000); } ],
#       "FM_MSB_INTR_CTR_1"              => [ 1, 0, -1, "u", sub { return 0x80008; }, sub { return (0x00000000); } ],
#       "FM_MSB_INTR_CTR_2"              => [ 1, 0, -1, "u", sub { return 0x80009; }, sub { return (0x00000000); } ],
#       "FM_MSB_INTR_CTR_3"              => [ 1, 0, -1, "u", sub { return 0x8000a; }, sub { return (0x00000000); } ],
#       "FM_MSB_INTR_CTR_4"              => [ 1, 0, -1, "u", sub { return 0x8000b; }, sub { return (0x00000000); } ],
#       "FM_MSB_INTR_CTR_5"              => [ 1, 0, -1, "u", sub { return 0x8000c; }, sub { return (0x00000000); } ],
#       "FM_MSB_IP"                      => [ 1, 0, -1, "u", sub { return 0x8000d; }, sub { return (0x00000000); } ],
        "FM_MSB_IM"                      => [ 1, 0, -1, "u", sub { return 0x8000e; }, sub { return (0x0000000f); } ],
        "FM_MSB_RX_EPL_RATE"             => [ 1, 0, -1, "u", sub { return 0x8000f; }, sub { return (0x00000605); } ],
        "FM_MSB_SCRATCH_0"               => [ 1, 0, -1, "u", sub { return 0x80010; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_1"               => [ 1, 0, -1, "u", sub { return 0x80011; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_2"               => [ 1, 0, -1, "u", sub { return 0x80012; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_3"               => [ 1, 0, -1, "u", sub { return 0x80013; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_4"               => [ 1, 0, -1, "u", sub { return 0x80014; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_5"               => [ 1, 0, -1, "u", sub { return 0x80015; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_6"               => [ 1, 0, -1, "u", sub { return 0x80016; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_7"               => [ 1, 0, -1, "u", sub { return 0x80017; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_8"               => [ 1, 0, -1, "u", sub { return 0x80018; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_9"               => [ 1, 0, -1, "u", sub { return 0x80019; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_10"              => [ 1, 0, -1, "u", sub { return 0x8001a; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_11"              => [ 1, 0, -1, "u", sub { return 0x8001b; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_12"              => [ 1, 0, -1, "u", sub { return 0x8001c; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_13"              => [ 1, 0, -1, "u", sub { return 0x8001d; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_14"              => [ 1, 0, -1, "u", sub { return 0x8001e; }, sub { return (0x00000000); } ],
        "FM_MSB_SCRATCH_15"              => [ 1, 0, -1, "u", sub { return 0x8001f; }, sub { return (0x00000000); } ],
        "FM_MSB_TS"                      => [ 1, 0, -1, "u", sub { return 0x80020; }, sub { return (0x00000000); } ],
        "FM_MSB_CREDITS"                 => [ 1, 0, -1, "u", sub { return 0x80021; }, sub { return (0x00000003); } ],
        "FM_MSB_SRAM_REPAIR_0"           => [ 1, 0, -1, "s", sub { return 0x80022; }, sub { return (0x00000000); } ],
        "FM_MSB_SRAM_REPAIR_1"           => [ 1, 0, -1, "s", sub { return 0x80023; }, sub { return (0x00000000); } ],

# Block: HANDLER
        "FM_PRE_TO_MID_FIFO_CFG"         => [ 1, 0, -1, "u", sub { return 0x100087; }, sub { return (0x000001c6); } ],
        "FM_HEAD_TAIL_SPACING_3"         => [ 1, 0, -1, "u", sub { return 0x100086; }, sub { return (0x00000284); } ],
        "FM_HEAD_TAIL_SPACING_2"         => [ 1, 0, -1, "u", sub { return 0x100085; }, sub { return (0x00000284); } ],
        "FM_HEAD_TAIL_SPACING_1"         => [ 1, 0, -1, "u", sub { return 0x100084; }, sub { return (0x00000041); } ],
        "FM_MGMT_FFU_CLK_COUNTER"        => [ 1, 0, -1, "u", sub { return 0x100083; }, sub { return (0x00000014); } ],
        "FM_MGMT_CLK_COUNTER"            => [ 1, 0, -1, "u", sub { return 0x100082; }, sub { return (0x00000004); } ],
        "FM_INTERNAL_PORT_MASK"          => [ 1, 0, -1, "u", sub { return 0x100080; }, sub { return (0x01ffffff); } ],
        "FM_FH_LOOPBACK_SUPPRESS"        => [ 1, 1,  0, "u", sub { return 0x100060 + $_[0]; }, sub { return (0x0000ffff); }, [0, 24] ],
        "FM_SAF_MATRIX"                  => [ 1, 1,  0, "u", sub { return 0x100040 + $_[0]; }, sub { return ($_[0]==0?0x01FFFFFF:0x00000001); }, [0, 24] ],
        "FM_PARITY_IM"                   => [ 1, 0, -1, "u", sub { return 0x100033; }, sub { return (0x000001ff); } ],
#       "FM_PARITY_IP"                   => [ 1, 0, -1, "u", sub { return 0x100032; }, sub { return (0x00000000); } ],
        "FM_RX_MIRROR_CFG"               => [ 1, 0, -1, "u", sub { return 0x100031; }, sub { return (0x00000000); } ],
        "FM_TRAP_GLORT"                  => [ 1, 0, -1, "u", sub { return 0x100035; }, sub { return (0x00000000); } ],
        "FM_CPU_LOG_MASK_FH"             => [ 1, 0, -1, "u", sub { return 0x100030; }, sub { return (0x00000001); } ],
        "FM_CPU_TRAP_MASK_FH"            => [ 1, 0, -1, "u", sub { return 0x10002f; }, sub { return (0x00000001); } ],
        "FM_TX_MIRROR_FH"                => [ 1, 0, -1, "u", sub { return 0x10002e; }, sub { return (0x00003f00); } ],
        "FM_L234_HASH_CFG"               => [ 1, 0, -1, "u", sub { return 0x100023; }, sub { return (0x0000e5fb); } ],
        "FM_L34_FLOW_HASH_CFG_2"         => [ 1, 0, -1, "u", sub { return 0x100022; }, sub { return (0x00000000); } ],
        "FM_L34_FLOW_HASH_CFG_1"         => [ 1, 0, -1, "u", sub { return 0x100021; }, sub { return (0x00000000); } ],
        "FM_L34_HASH_CFG"                => [ 1, 0, -1, "u", sub { return 0x100020; }, sub { return (0x0101033e); } ],
        "FM_SYS_CFG_ROUTER"              => [ 1, 0, -1, "u", sub { return 0x100019; }, sub { return (0x00000000); } ],
#       "FM_FH_INT_DETECT"               => [ 1, 0, -1, "u", sub { return 0x100018; }, sub { return (0x00000000); } ],
        "FM_PORT_MAC_SEC_IM"             => [ 1, 0, -1, "u", sub { return 0x100017; }, sub { return (0x01fffffe); } ],
#       "FM_PORT_MAC_SEC_IP"             => [ 1, 0, -1, "u", sub { return 0x100016; }, sub { return (0x00000000); } ],
        "FM_PORT_VLAN_IM_2"              => [ 1, 0, -1, "u", sub { return 0x100015; }, sub { return (0x01fffffe); } ],
#       "FM_PORT_VLAN_IP_2"              => [ 1, 0, -1, "u", sub { return 0x100014; }, sub { return (0x00000000); } ],
        "FM_PORT_VLAN_IM_1"              => [ 1, 0, -1, "u", sub { return 0x100013; }, sub { return (0x01fffffe); } ],
#       "FM_PORT_VLAN_IP_1"              => [ 1, 0, -1, "u", sub { return 0x100012; }, sub { return (0x00000000); } ],
        "FM_SYS_CFG_8"                   => [ 1, 0, -1, "s", sub { return 0x100007; }, sub { return (0x00000000); } ],
        "FM_SYS_CFG_7"                   => [ 1, 0, -1, "u", sub { return 0x100006; }, sub { return (0x80007530); } ],
        "FM_SYS_CFG_4"                   => [ 1, 0, -1, "u", sub { return 0x100003; }, sub { return (0x00000000); } ],
        "FM_SYS_CFG_3"                   => [ 1, 0, -1, "u", sub { return 0x100002; }, sub { return (0x00000000); } ],
        "FM_SYS_CFG_1"                   => [ 1, 0, -1, "u", sub { return 0x100000; }, sub { return (0x00001cff); } ],

# Block: ASSOC
        "FM_PORT_CFG_1"                  => [ 1, 1,  0, "u", sub { return 0x101000 + $_[0]; }, sub { return (0x29000001); }, [0, 24] ],
        "FM_PORT_CFG_2"                  => [ 1, 1,  0, "s", sub { return 0x101020 + $_[0]; }, sub { return (0x01ffffff); }, [0, 24] ],
        "FM_PORT_CFG_3"                  => [ 1, 1,  0, "u", sub { return 0x101040 + $_[0]; }, sub { return (0x00000022); }, [0, 24] ],
        "FM_PORT_CFG_ISL"                => [ 1, 1,  0, "u", sub { return 0x101060 + $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_RX_VPRI_MAP"                 => [ 2, 1,  0, "u", sub { return 0x101080 + 0x2 * $_[0]; }, sub { return (0x33221100, 0x77665544); }, [0, 24] ],
        "FM_DSCP_PRI_MAP"                => [ 1, 1, -1, "u", sub { return 0x1010c0 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_VPRI_PRI_MAP"                => [ 1, 1, -1, "u", sub { return 0x101100 + $_[0]; }, sub { return ($_[0]); }, [0, 15] ],

# Block: GLORT
        "FM_GLORT_DEST_TABLE"            => [ 2, 1, -1, "u", sub { return 0x108000 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 4095] ],
        "FM_GLORT_CAM"                   => [ 1, 1, -1, "u", sub { return 0x10a000 + $_[0]; }, sub { return (0x00000000); }, [0, 255] ],
        "FM_GLORT_RAM"                   => [ 2, 1, -1, "u", sub { return 0x10a200 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 255] ],
        "FM_GLORT_RAM_TABLE_REPAIR"      => [ 1, 0, -1, "s", sub { return 0x10a500; }, sub { return (0x00000000); } ],
        "FM_GLORT_DEST_TABLE_REPAIR"     => [ 1, 0, -1, "s", sub { return 0x10a501; }, sub { return (0x00000000); } ],

# Block: CM
        "FM_CM_TX_TC_PRIVATE_WM"         => [ 1, 2,  1, "u", sub { return 0x103000 + $_[0] + 0x8 * $_[1]; }, sub { return (0x00001fff); }, [0, 7], [0, 24] ],
        "FM_CM_TX_TC_USAGE"              => [ 1, 2,  1, "u", sub { return 0x103100 + $_[0] + 0x8 * $_[1]; }, sub { return (0x00000000); }, [0, 7], [0, 24] ],
        "FM_CM_TX_SMP_HOG_WM"            => [ 1, 2,  1, "u", sub { return 0x103200 + $_[0] + 0x4 * $_[1]; }, sub { return (0x00001fff); }, [0, 3], [0, 24] ],
        "FM_CM_RX_SMP_PAUSE_WM"          => [ 1, 2,  1, "u", sub { return 0x103280 + $_[0] + 0x2 * $_[1]; }, sub { return (0x1fff1fff); }, [0, 1], [0, 24] ],
        "FM_CM_RX_SMP_PRIVATE_WM"        => [ 1, 2,  1, "u", sub { return 0x1032c0 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00001fff); }, [0, 1], [0, 24] ],
        "FM_CM_RX_SMP_USAGE"             => [ 1, 2,  1, "u", sub { return 0x103300 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00000000); }, [0, 1], [0, 24] ],
        "FM_CM_TX_SMP_USAGE"             => [ 1, 2,  1, "u", sub { return 0x103380 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00000000); }, [0, 1], [0, 24] ],
        "FM_CM_TX_SMP_PRIVATE_WM"        => [ 1, 2,  1, "u", sub { return 0x1033c0 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00001fff); }, [0, 1], [0, 24] ],
        "FM_CM_RX_SMP_HOG_WM"            => [ 1, 2,  1, "u", sub { return 0x103400 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00001fff); }, [0, 1], [0, 24] ],
        "FM_CM_PAUSE_RESEND_INTERVAL"    => [ 1, 1,  0, "u", sub { return 0x103480 + $_[0]; }, sub { return (0x00000100); }, [0, 24] ],
        "FM_CM_PORT_CFG"                 => [ 1, 1,  0, "u", sub { return 0x1034a0 + $_[0]; }, sub { return (0x0000ff00); }, [0, 24] ],
#       "FM_CM_RX_USAGE"                 => [ 1, 1,  0, "u", sub { return 0x1034c0 + $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_CM_SHARED_WM"                => [ 1, 1, -1, "u", sub { return 0x1034e0 + $_[0]; }, sub { return (0x0000021c); }, [0, 15] ],
        "FM_CM_PAUSE_DECIMATION"         => [ 1, 1, -1, "u", sub { return 0x1034f0 + $_[0]; }, sub { return (0x001a4000); }, [0, 7] ],
        "FM_CM_SHARED_SMP_PAUSE_WM"      => [ 1, 1, -1, "u", sub { return 0x1034f8 + $_[0]; }, sub { return (0x1fff1fff); }, [0, 1] ],
#       "FM_CM_SHARED_SMP_USAGE"         => [ 1, 1, -1, "u", sub { return 0x1034fa + $_[0]; }, sub { return (0x00000000); }, [0, 1] ],
#       "FM_CM_GLOBAL_USAGE"             => [ 1, 0, -1, "u", sub { return 0x1034fc; }, sub { return (0x00000000); } ],
        "FM_CM_GLOBAL_WM"                => [ 1, 0, -1, "u", sub { return 0x1034fd; }, sub { return (0x00000fa0); } ],
        "FM_CM_SMP_MEMBERSHIP"           => [ 1, 0, -1, "u", sub { return 0x1034fe; }, sub { return (0x00000000); } ],
        "FM_CM_TX_HOG_MAP"               => [ 1, 0, -1, "u", sub { return 0x1034ff; }, sub { return (0x00000000); } ],
        "FM_CN_CPID_MASK"                => [ 1, 0, -1, "u", sub { return 0x103500; }, sub { return (0x00000000); } ],
        "FM_CN_VCN_DMAC_2"               => [ 1, 1,  0, "u", sub { return 0x103540 + $_[0]; }, sub { return (0x0000eeff); }, [0, 24] ],
        "FM_CN_RATE_LIM_CPID"            => [ 1, 2,  1, "u", sub { return 0x103580 + $_[0] + 0x2 * $_[1]; }, sub { return (0x0000000f); }, [0, 1], [0, 24] ],
        "FM_CN_SMP_CFG"                  => [ 1, 2,  1, "u", sub { return 0x1035c0 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00000000); }, [0, 1], [0, 24] ],
        "FM_CN_SMP_THRESHOLD"            => [ 1, 2,  1, "u", sub { return 0x103600 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00000000); }, [0, 1], [0, 24] ],
        "FM_CN_SAMPLE_CFG"               => [ 1, 0, -1, "u", sub { return 0x103640; }, sub { return (0x000f0064); } ],
        "FM_CN_RATE_LIM_CFG"             => [ 1, 1,  0, "u", sub { return 0x103660 + $_[0]; }, sub { return (0x15151501); }, [0, 24] ],
        "FM_CN_CPID_TABLE"               => [ 2, 1, -1, "u", sub { return 0x103680 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 7] ],
        "FM_CN_GLOBAL_CFG_1"             => [ 1, 0, -1, "u", sub { return 0x103692; }, sub { return (0x10010028); } ],
        "FM_CN_GLOBAL_CFG_2"             => [ 1, 0, -1, "u", sub { return 0x103693; }, sub { return (0x04080000); } ],
        "FM_CN_FB_CFG"                   => [ 1, 0, -1, "u", sub { return 0x103694; }, sub { return (0x0008010c); } ],
        "FM_CN_FORWARD_MASK"             => [ 1, 0, -1, "u", sub { return 0x103695; }, sub { return (0x00000000); } ],
        "FM_CN_RATE_ACTION_MASK"         => [ 1, 0, -1, "u", sub { return 0x103696; }, sub { return (0x00000000); } ],
        "FM_CN_BACKOFF_BYTETIME"         => [ 1, 0, -1, "u", sub { return 0x103697; }, sub { return (0x00001388); } ],
        "FM_CN_FRAME_CFG_1"              => [ 1, 0, -1, "u", sub { return 0x103698; }, sub { return (0x00000000); } ],
        "FM_CN_FRAME_CFG_2"              => [ 1, 0, -1, "u", sub { return 0x103699; }, sub { return (0x00000000); } ],
#       "FM_CM_CPU_PORT_STATUS"          => [ 1, 0, -1, "u", sub { return 0x10369c; }, sub { return (0x00000000); } ],
        "FM_CN_VCN_DMAC_1"               => [ 1, 0, -1, "u", sub { return 0x10369d; }, sub { return (0x00effeff); } ],
        "FM_TX_RATE_LIM_CFG"             => [ 1, 2,  1, "u", sub { return 0x103700 + $_[0] + 0x8 * $_[1]; }, sub { return (0xdffe0100); }, [0, 7], [0, 24] ],
#       "FM_TX_RATE_LIM_USAGE"           => [ 1, 2,  1, "u", sub { return 0x103800 + $_[0] + 0x8 * $_[1]; }, sub { return (0x00400000); }, [0, 7], [0, 24] ],
        "FM_RX_RATE_LIM_CFG"             => [ 1, 2,  1, "u", sub { return 0x103900 + $_[0] + 0x2 * $_[1]; }, sub { return (0xdffe0100); }, [0, 1], [0, 24] ],
#       "FM_RX_RATE_LIM_USAGE"           => [ 1, 2,  1, "u", sub { return 0x103940 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00400000); }, [0, 1], [0, 24] ],
        "FM_RX_RATE_LIM_THRESHOLD"       => [ 1, 2,  1, "u", sub { return 0x103980 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00000000); }, [0, 1], [0, 24] ],
        "FM_CM_PORT_LIST"                => [ 1, 1,  0, "u", sub { return 0x1039c0 + $_[0]; }, sub { return (($_[0]==24)?0x38:$_[0]); }, [0, 24] ],
        "FM_SCHED_DRR_Q"                 => [ 1, 2,  1, "u", sub { return 0x103a00 + $_[0] + 0x8 * $_[1]; }, sub { return (0x00000800); }, [0, 7], [0, 24] ],
        "FM_SCHED_SHAPING_GROUP_CFG"     => [ 1, 1,  0, "u", sub { return 0x103b00 + $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_SWITCH_PRI_TO_CLASS_1"       => [ 1, 0, -1, "u", sub { return 0x103b20; }, sub { return (0x00fac688); } ],
        "FM_SWITCH_PRI_TO_CLASS_2"       => [ 1, 0, -1, "u", sub { return 0x103b21; }, sub { return (0x00fac688); } ],
        "FM_CM_GLOBAL_CFG"               => [ 1, 0, -1, "u", sub { return 0x103b22; }, sub { return (0x00000014); } ],
        "FM_TRAFFIC_CLASS_TO_SCHED_PRI"  => [ 1, 0, -1, "u", sub { return 0x103b23; }, sub { return (0x76543210); } ],
        "FM_CN_STATS_CFG"                => [ 1, 1, -1, "u", sub { return 0x103c00 + $_[0]; }, sub { return (0x00000000); }, [0, 1] ],
#       "FM_CN_STATS"                    => [ 1, 2, -1, "u", sub { return 0x103d00 + $_[0] + 0x8 * $_[1]; }, sub { return (0x00000000); }, [0, 7], [0, 1] ],
        "FM_SCHED_FH_GROUP_CFG"          => [ 1, 1,  0, "u", sub { return 0x103f20 + $_[0]; }, sub { return (0x00ff00ff); }, [0, 24] ],
        "FM_QDM_CFG"                     => [ 1, 1, -1, "u", sub { return 0x103f40 + $_[0]; }, sub { return (0x00000401); }, [0, 1] ],
        "FM_QDM_FRAME_CNT"               => [ 1, 1, -1, "u", sub { return 0x103f42 + $_[0]; }, sub { return (0x00000000); }, [0, 1] ],
        "FM_QDM_INSTRUMENT"              => [ 1, 1, -1, "u", sub { return 0x103f44 + $_[0]; }, sub { return (0x00000000); }, [0, 1] ],

# Block: LAG
        "FM_LAG_CFG"                     => [ 1, 1,  0, "u", sub { return 0x104000 + $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_CANONICAL_GLORT_CAM"         => [ 1, 1, -1, "u", sub { return 0x104020 + $_[0]; }, sub { return (0x00000000); }, [0, 15] ],

# Block: POLICER
        "FM_POLICER_TABLE"               => [ 4, 2, -1, "u", sub { return 0x10c000 + 0x4 * $_[0] + 0x800 * $_[1]; }, sub { return (0x00000000, 0x00000000, 0x00000000, 0x00000000); }, [0, 511], [0, 3] ],
#       "FM_POLICER_REPAIR"              => [ 1, 2, -1, "u", sub { return 0x10e000 + $_[0] + 0x2 * $_[1]; }, sub { return (0x00000000); }, [0, 1], [0, 3] ],
        "FM_POLICER_CFG"                 => [ 1, 1, -1, "u", sub { return 0x10e008 + $_[0]; }, sub { return (0x00000000); }, [0, 3] ],
#       "FM_POLICER_IP"                  => [ 1, 0, -1, "u", sub { return 0x10e00c; }, sub { return (0x00000000); } ],
        "FM_POLICER_IM"                  => [ 1, 0, -1, "u", sub { return 0x10e00d; }, sub { return (0x0000000f); } ],
#       "FM_POLICER_STATUS"              => [ 1, 0, -1, "u", sub { return 0x10e00e; }, sub { return (0x00000000); } ],
        "FM_POLICER_TS_DIV"              => [ 1, 0, -1, "u", sub { return 0x10e00f; }, sub { return (0x00000000); } ],
        "FM_POLICER_DSCP_DOWN_MAP"       => [ 1, 1, -1, "u", sub { return 0x10e100 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_POLICER_SWPRI_DOWN_MAP"      => [ 1, 1, -1, "u", sub { return 0x10e140 + $_[0]; }, sub { return (0x00000000); }, [0, 15] ],

# Block: TRIG
        "FM_TRIGGER_CONDITION_CFG"       => [ 1, 1, -1, "u", sub { return 0x106100 + $_[0]; }, sub { return (0x0c4aaaaa); }, [0, 63] ],
        "FM_TRIGGER_CONDITION_PARAM"     => [ 1, 1, -1, "u", sub { return 0x106140 + $_[0]; }, sub { return (0x1fc00000); }, [0, 63] ],
        "FM_TRIGGER_CONDITION_FFU"       => [ 1, 1, -1, "u", sub { return 0x106180 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_CONDITION_TYPE"      => [ 1, 1, -1, "u", sub { return 0x1061c0 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_CONDITION_GLORT"     => [ 1, 1, -1, "u", sub { return 0x106200 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_CONDITION_RX"        => [ 1, 1, -1, "u", sub { return 0x106240 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_CONDITION_TX"        => [ 1, 1, -1, "u", sub { return 0x106280 + $_[0]; }, sub { return (0x01ffffff); }, [0, 63] ],
        "FM_TRIGGER_CONDITION_AMASK_1"   => [ 1, 1, -1, "u", sub { return 0x1062c0 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_CONDITION_AMASK_2"   => [ 1, 1, -1, "u", sub { return 0x106300 + $_[0]; }, sub { return (0x00000030); }, [0, 63] ],
        "FM_TRIGGER_ACTION_CFG_1"        => [ 1, 1, -1, "u", sub { return 0x106340 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_ACTION_CFG_2"        => [ 1, 1, -1, "u", sub { return 0x106380 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_ACTION_GLORT"        => [ 1, 1, -1, "u", sub { return 0x1063c0 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_ACTION_DMASK"        => [ 1, 1, -1, "u", sub { return 0x106400 + $_[0]; }, sub { return (0x02000000); }, [0, 63] ],
        "FM_TRIGGER_ACTION_MIRROR"       => [ 1, 1, -1, "u", sub { return 0x106440 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_ACTION_DROP"         => [ 1, 1, -1, "u", sub { return 0x106480 + $_[0]; }, sub { return (0x00000000); }, [0, 63] ],
        "FM_TRIGGER_RATE_LIM_CFG_1"      => [ 1, 1, -1, "u", sub { return 0x1064c0 + $_[0]; }, sub { return (0x00000000); }, [0, 15] ],
        "FM_TRIGGER_RATE_LIM_CFG_2"      => [ 1, 1, -1, "u", sub { return 0x1064d0 + $_[0]; }, sub { return (0x00000000); }, [0, 15] ],
        "FM_TRIGGER_RATE_LIM_USAGE"      => [ 1, 1, -1, "u", sub { return 0x106580 + $_[0]; }, sub { return (0x00000000); }, [0, 15] ],
#       "FM_TRIGGER_IP"                  => [ 1, 1, -1, "u", sub { return 0x1064e0 + $_[0]; }, sub { return (0x00000000); }, [0, 1] ],
        "FM_TRIGGER_IM"                  => [ 1, 1, -1, "u", sub { return 0x1064e4 + $_[0]; }, sub { return (0xffffffff); }, [0, 1] ],

# Block: ARP
        "FM_ARP_TABLE"                   => [ 3, 1, -1, "u", sub { return 0x120000 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000, 0x00000000); }, [0, 16383] ],
        "FM_ARP_USED"                    => [ 1, 1, -1, "u", sub { return 0x130000 + $_[0]; }, sub { return (0x00000000); }, [0, 511] ],
#       "FM_ARP_IP"                      => [ 1, 0, -1, "u", sub { return 0x130201; }, sub { return (0x00000000); } ],
        "FM_ARP_IM"                      => [ 1, 0, -1, "u", sub { return 0x130202; }, sub { return (0x00000001); } ],
        "FM_ARP_REDIRECT_SIP"            => [ 4, 0, -1, "u", sub { return 0x130204; }, sub { return (0x00000000, 0x00000000, 0x00000000, 0x00000000); } ],
        "FM_ARP_REDIRECT_DIP"            => [ 4, 0, -1, "u", sub { return 0x130208; }, sub { return (0x00000000, 0x00000000, 0x00000000, 0x00000000); } ],
        "FM_ARP_TABLE_REPAIR_0"          => [ 1, 0, -1, "s", sub { return 0x130210; }, sub { return (0x00000000); } ],
        "FM_ARP_TABLE_REPAIR_1"          => [ 1, 0, -1, "s", sub { return 0x130211; }, sub { return (0x00000000); } ],
        "FM_ARP_TABLE_REPAIR_2"          => [ 1, 0, -1, "s", sub { return 0x130212; }, sub { return (0x00000000); } ],
        "FM_ARP_TABLE_REPAIR_3"          => [ 1, 0, -1, "s", sub { return 0x130213; }, sub { return (0x00000000); } ],
        "FM_ARP_USED_REPAIR"             => [ 1, 0, -1, "s", sub { return 0x130214; }, sub { return (0x00000000); } ],

# Block: FFU
        "FM_FFU_MAP_SRC"                 => [ 1, 1,  0, "u", sub { return 0x17b000 + $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
        "FM_FFU_MAP_MAC"                 => [ 2, 1, -1, "u", sub { return 0x17b100 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 15] ],
        "FM_FFU_MAP_VLAN_REPAIR"         => [ 1, 0, -1, "s", sub { return 0x17b180; }, sub { return (0x00000000); } ],
        "FM_FFU_MAP_VLAN"                => [ 1, 1, -1, "u", sub { return 0x17c000 + $_[0]; }, sub { return (0x00000000); }, [0, 4095] ],
        "FM_FFU_MAP_TYPE"                => [ 1, 1, -1, "u", sub { return 0x17b200 + $_[0]; }, sub { return (0x00000000); }, [0, 15] ],
        "FM_FFU_MAP_LENGTH"              => [ 1, 1, -1, "u", sub { return 0x17b300 + $_[0]; }, sub { return (0x00000000); }, [0, 15] ],
        "FM_FFU_MAP_IP_LO"               => [ 2, 1, -1, "u", sub { return 0x17b400 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 15] ],
        "FM_FFU_MAP_IP_HI"               => [ 2, 1, -1, "u", sub { return 0x17b420 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 15] ],
        "FM_FFU_MAP_IP_CFG"              => [ 1, 1, -1, "u", sub { return 0x17b440 + $_[0]; }, sub { return (0x00000000); }, [0, 15] ],
        "FM_FFU_MAP_PROT"                => [ 1, 1, -1, "u", sub { return 0x17b500 + $_[0]; }, sub { return (0x00000000); }, [0, 7] ],
        "FM_FFU_MAP_L4_SRC"              => [ 2, 1, -1, "u", sub { return 0x17b600 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 63] ],
        "FM_FFU_MAP_L4_DST"              => [ 2, 1, -1, "u", sub { return 0x17b700 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 63] ],
        "FM_FFU_INIT_SLICE"              => [ 1, 0, -1, "s", sub { return 0x17b800; }, sub { return (0x0003ffe0); } ],
        "FM_FFU_MASTER_VALID"            => [ 2, 0, -1, "u", sub { return 0x17a000; }, sub { return (0xffffffff, 0xffffffff); } ],
        "FM_FFU_EGRESS_CHUNK_CFG"        => [ 1, 1, -1, "u", sub { return 0x17d000 + $_[0]; }, sub { return (0x00000000); }, [0, 31] ],
        "FM_FFU_EGRESS_CHUNK_VALID"      => [ 1, 1, -1, "u", sub { return 0x17d100 + $_[0]; }, sub { return (0xffffffff); }, [0, 31] ],
        "FM_FFU_EGRESS_ACTIONS"          => [ 1, 1, -1, "u", sub { return 0x17d200 + $_[0]; }, sub { return (0x00000000); }, [0, 511] ],
        "FM_FFU_SLICE_TCAM"              => [ 3, 2, -1, "u", sub { return 0x140000 + 0x4 * $_[0] + 0x1000 * $_[1]; }, sub { return (0x00000000, 0x00000000, 0x00000200); }, [0, 511], [0, 31] ],
        "FM_FFU_SLICE_SRAM"              => [ 2, 2, -1, "u", sub { return 0x140800 + 0x2 * $_[0] + 0x1000 * $_[1]; }, sub { return (0x00000000, 0x00000000); }, [0, 511], [0, 31] ],
        "FM_FFU_SLICE_VALID"             => [ 1, 1, -1, "u", sub { return 0x140c00 + 0x1000 * $_[0]; }, sub { return (0x00000000); }, [0, 31] ],
        "FM_FFU_SLICE_CASE"              => [ 1, 1, -1, "u", sub { return 0x140c02 + 0x1000 * $_[0]; }, sub { return (0x00000000); }, [0, 31] ],
        "FM_FFU_SLICE_CASCADE_ACTION"    => [ 1, 1, -1, "u", sub { return 0x140c04 + 0x1000 * $_[0]; }, sub { return (0x00000000); }, [0, 31] ],
        "FM_FFU_SLICE_CASE_CFG"          => [ 1, 2, -1, "u", sub { return 0x140c08 + $_[0] + 0x1000 * $_[1]; }, sub { return (0x00000000); }, [0, 1], [0, 31] ],

# Block: STATS

# Block: STATS_RX_CM

# Block: STATS_VLAN_PKT
#       "FM_STAT_VlanUcstPkts"           => [ 2, 1, -1, "u", sub { return 0x1a0100 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 63] ],
#       "FM_STAT_VlanXcstPkts"           => [ 2, 1, -1, "u", sub { return 0x1a0180 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 63] ],

# Block: STATS_VLAN_OCT
#       "FM_STAT_VlanUcstOctets"         => [ 2, 1, -1, "u", sub { return 0x1a0200 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 63] ],
#       "FM_STAT_VlanXcstOctets"         => [ 2, 1, -1, "u", sub { return 0x1a0280 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 63] ],

# Block: STATS_TRIG
#       "FM_STAT_Trigger"                => [ 2, 1, -1, "u", sub { return 0x1a0300 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 63] ],

# Block: STATS_EGRESS_ACL
#       "FM_STAT_EgressACLPkts"          => [ 2, 1,  0, "u", sub { return 0x1a0400 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],
#       "FM_STAT_EgressACLOctets"        => [ 2, 1,  0, "u", sub { return 0x1a0480 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 24] ],

# Block: STATS_CTRL
        "FM_STATS_CFG"                   => [ 1, 1,  0, "u", sub { return 0x1a0500 + 0x2 * $_[0]; }, sub { return (0x000079fe); }, [0, 24] ],
#       "FM_STATS_DROP_COUNT_RX"         => [ 1, 1,  0, "u", sub { return 0x1a0540 + 0x2 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],
#       "FM_STATS_DROP_COUNT_TX"         => [ 1, 1,  0, "u", sub { return 0x1a0580 + 0x2 * $_[0]; }, sub { return (0x00000000); }, [0, 24] ],

# Block: L2LOOKUP
        "FM_MA_TABLE"                    => [ 3, 1, -1, "s", sub { return 0x180000 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000, 0x00000000); }, [0, 16383] ],
        "FM_INGRESS_VID_TABLE"           => [ 3, 1, -1, "s", sub { return 0x190000 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000, 0x00000000); }, [0, 4095] ],
        "FM_EGRESS_VID_TABLE"            => [ 2, 1, -1, "s", sub { return 0x194000 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 4095] ],
        "FM_INGRESS_FID_TABLE"           => [ 2, 1, -1, "s", sub { return 0x196000 + 0x2 * $_[0]; }, sub { return (0x00000000, 0x00000000); }, [0, 4095] ],
        "FM_EGRESS_FID_TABLE"            => [ 1, 1, -1, "s", sub { return 0x198000 + $_[0]; }, sub { return (0x00000000); }, [0, 4095] ],
#       "FM_MA_TCN_FIFO"                 => [ 4, 1, -1, "u", sub { return 0x19e000 + 0x4 * $_[0]; }, sub { return (0x00000000, 0x00000000, 0x00000000, 0x00000000); }, [0, 511] ],
        "FM_MA_TCN_PTR"                  => [ 1, 0, -1, "s", sub { return 0x19e800; }, sub { return (0x00000000); } ],
#       "FM_MA_TCN_IP"                   => [ 1, 0, -1, "u", sub { return 0x19e801; }, sub { return (0x00000000); } ],
        "FM_MA_TCN_IM"                   => [ 1, 0, -1, "u", sub { return 0x19e802; }, sub { return (0x0000003f); } ],
#       "FM_MA_TABLE_STATUS_3"           => [ 1, 0, -1, "u", sub { return 0x19e803; }, sub { return (0x00000000); } ],
        "FM_MA_TABLE_CFG_1"              => [ 1, 0, -1, "s", sub { return 0x19e804; }, sub { return (0x03c11040); } ],
        "FM_MA_TABLE_CFG_2"              => [ 1, 0, -1, "u", sub { return 0x19e805; }, sub { return (0x00000000); } ],
        "FM_MA_TABLE_CFG_3"              => [ 1, 0, -1, "u", sub { return 0x19e806; }, sub { return (0x00000000); } ],
        "FM_MA_TCN_CFG_1"                => [ 1, 0, -1, "u", sub { return 0x19e807; }, sub { return (0x07ffffff); } ],
        "FM_MA_TCN_CFG_2"                => [ 1, 0, -1, "u", sub { return 0x19e808; }, sub { return (0x0007ffff); } ],
        "FM_MA_PURGE"                    => [ 1, 0, -1, "s", sub { return 0x19e809; }, sub { return (0x00000000); } ],
        "FM_MTU_TABLE"                   => [ 1, 1, -1, "u", sub { return 0x19e810 + $_[0]; }, sub { return (0x00000000); }, [0, 7] ],
        "FM_INGRESS_VID_TABLE_REPAIR"    => [ 1, 0, -1, "s", sub { return 0x19e820; }, sub { return (0x00000000); } ],
        "FM_EGRESS_VID_TABLE_REPAIR"     => [ 1, 0, -1, "s", sub { return 0x19e821; }, sub { return (0x00000000); } ],
        "FM_INGRESS_FID_TABLE_REPAIR"    => [ 1, 0, -1, "s", sub { return 0x19e822; }, sub { return (0x00000000); } ],
        "FM_EGRESS_FID_TABLE_REPAIR"     => [ 1, 0, -1, "s", sub { return 0x19e823; }, sub { return (0x00000000); } ],
        "FM_TCN_FIFO_REPAIR"             => [ 1, 1, -1, "s", sub { return 0x19e824 + $_[0]; }, sub { return (0x00000000); }, [0, 3] ],
);


# %alta_regs_hash
#
# Entry format:
#   [ word-count, index-count, port-index, ordering,
#     addr-sub, default-sub, bounds-list ]
#
# word-count:  number of 32-bit words that comprise each
#              register value (1-4)
# index-count: number of indices that must be provided to
#              fully specify a register (size of bounds-list)
# port-index:  indicates which index is either a EPL or a port number
#              (-1 for none) the indicated index refers an EPL number if 
#              the index size is 25 or a port number if the index size is 75.
# ordering:    indicates how this register should be handled:
#              "s": handle as a special case
#              "u": handle in an unordered automated loop (normal case)
#              "o": read only register
#              "i": ignore register
# addr-sub:    subroutine that returns a register address
#              given appropriate index arguments
# default-sub: subroutine that returns the register default
#              value given appropriate index arguments
#              Read only registers return 'undef'.
# bounds-list: list of list references.  Each list reference
#              represents one index argument to the subroutine
#              and gives the min and max values allowed.

# NOTE:
#
# This table is automatically generated from the XML used to generate the chip
# design. This table should not be edited directly. Instead, changes should be
# implemented in the XSLT stylesheet, codegen_fm6000_regs.xsl or to the tool
# used to generate the perl hash table (xml2perlhash).
#
# Generated: 11/16/2011

my %alta_regs_hash = (

# Block: EPL, base address = 0x0e0000 (85 registers) 
        "FM_EPL_IP"                             => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000300 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_CFG_A"                          => [ 01, 01, 0, "s", sub { return (0x0e0000 + 0x000301 + 0x400 * $_[0]) }, sub { return (0x0c7d7899); } , [0, 24] ],
        "FM_EPL_CFG_B"                          => [ 01, 01, 0, "s", sub { return (0x0e0000 + 0x000302 + 0x400 * $_[0]) }, sub { return (0x00080000); } , [0, 24] ],
        "FM_EPL_LED_STATUS"                     => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000303 + 0x400 * $_[0]) }, sub { return (0x00); } , [0, 24] ],
        "FM_EPL_FIFO_ERROR_STATUS"              => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000304 + 0x400 * $_[0]) }, sub { return (0x00); } , [0, 24] ],
        "FM_EPL_TX_FIFO_READ_PTR_STATUS"        => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000305 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_TX_FIFO_WRITE_PTR_STATUS"       => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000306 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_TX_FIFO_A_PTR_STATUS"           => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000307 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_TX_FIFO_B_PTR_STATUS"           => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000308 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_TX_FIFO_C_PTR_STATUS"           => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000309 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_TX_FIFO_D_PTR_STATUS"           => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x00030a + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_RX_FIFO_READ_PTR_STATUS"        => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x00030b + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_RX_FIFO_WRITE_PTR_STATUS"       => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x00030c + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_RX_FIFO_A_PTR_STATUS"           => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x00030d + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_RX_FIFO_B_PTR_STATUS"           => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x00030e + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_RX_FIFO_C_PTR_STATUS"           => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x00030f + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_RX_FIFO_D_PTR_STATUS"           => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000310 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_PCS_10GBASEX_CFG"                   => [ 01, 01,-1, "s", sub { return (0x0e0000 + 0x000311 + 0x400 * $_[0]) }, sub { return (0x00); } , [0, 24] ],
        "FM_PCS_10GBASEX_RX_STATUS"             => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000312 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_PCS_10GBASEX_TX_STATUS"             => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000313 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_PCS_40GBASER_CFG"                   => [ 01, 01,-1, "s", sub { return (0x0e0000 + 0x000314 + 0x400 * $_[0]) }, sub { return (0x000001ff); } , [0, 24] ],
        "FM_PCS_40GBASER_RX_STATUS"             => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000315 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_EPL_1588_TIMER_STATUS"              => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000316 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_PORT_STATUS"                        => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000000 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_AN_IM"                              => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000001 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x0007ffff); } , [0, 3], [0, 24] ],
        "FM_LINK_IM"                            => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000002 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x07ffffff); } , [0, 3], [0, 24] ],
        "FM_AN_IP"                              => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000003 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_LINK_IP"                            => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000004 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_EPL_PORT_MULTI_PURPOSE_CFG_A"       => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000006 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_AN_37_PAGE_RX_STATUS"               => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000008 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_73_PAGE_RX_STATUS"               => [ 02, 02, 1, "o", sub { return (0x0e0000 + 0x00000a + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_LINK_RULES"                         => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x00000c + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_CFG"                            => [ 04, 02, 1, "s", sub { return (0x0e0000 + 0x000010 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x40000000, 0x00, 0x00, 0x00005381); } , [0, 3], [0, 24] ],
        "FM_TX_SEQUENCE"                        => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000014 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_RX_SEQUENCE"                        => [ 02, 02, 1, "o", sub { return (0x0e0000 + 0x000016 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_MAC_1588_STATUS"                    => [ 02, 02, 1, "o", sub { return (0x0e0000 + 0x000018 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_OVERSIZE_COUNTER"               => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x00001a + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_JABBER_COUNTER"                 => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x00001b + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_UNDERSIZE_COUNTER"              => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x00001c + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_RUNT_COUNTER"                   => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x00001d + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_OVERRUN_COUNTER"                => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x00001e + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_UNDERRUN_COUNTER"               => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x00001f + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_CODE_ERROR_COUNTER"             => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x000020 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_MAC_LINK_COUNTER"                   => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x000021 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_PCS_1000BASEX_CFG"                  => [ 01, 02, 1, "s", sub { return (0x0e0000 + 0x000022 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_PCS_1000BASEX_RX_STATUS"            => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000023 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_PCS_1000BASEX_TX_STATUS"            => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000024 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_PCS_10GBASER_CFG"                   => [ 01, 02, 1, "s", sub { return (0x0e0000 + 0x000025 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_PCS_10GBASER_RX_STATUS"             => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000026 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_PCS_10GBASER_TX_STATUS"             => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000027 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_37_CFG"                          => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000028 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_AN_37_TIMER_CFG"                    => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x00002c + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_AN_37_BASE_PAGE_TX"                 => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000006 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_AN_37_BASE_PAGE_RX"                 => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000008 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_37_NEXT_PAGE_TX"                 => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000006 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_AN_37_NEXT_PAGE_RX"                 => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000008 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_SGMII_AN_TIMER_CFG"                 => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x00002c + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_SGMII_AN_TX_CONFIG"                 => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000006 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_SGMII_AN_TX_CONFIG_LOOPBACK"        => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000006 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_SGMII_AN_RX_CONFIG"                 => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000008 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_37_STATUS"                       => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x00002a + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_73_CFG"                          => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x00002b + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_AN_73_TIMER_CFG"                    => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x00002c + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_AN_73_BASE_PAGE_TX"                 => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000006 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_AN_73_BASE_PAGE_RX"                 => [ 02, 02, 1, "o", sub { return (0x0e0000 + 0x00000a + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_73_NEXT_PAGE_TX"                 => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000006 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_AN_73_NEXT_PAGE_RX"                 => [ 02, 02, 1, "o", sub { return (0x0e0000 + 0x00000a + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_73_STATUS"                       => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x00002e + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_73_TX_LCW_DEBUG"                 => [ 02, 02, 1, "o", sub { return (0x0e0000 + 0x000030 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG"   => [ 02, 02, 1, "o", sub { return (0x0e0000 + 0x000032 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_SERDES_CFG"                         => [ 02, 02, 1, "s", sub { return (0x0e0000 + 0x000034 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x0aaaa005, 0x00); } , [0, 3], [0, 24] ],
        "FM_MULTI_PURPOSE_ERROR_COUNTER"        => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x000036 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_PCS_40GBASER_RX_BIP_STATUS"         => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000036 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_PCS_10GBASER_RX_BER_STATUS"         => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000036 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_DISPARITY_ERROR_8B10B"              => [ 01, 02, 1, "i", sub { return (0x0e0000 + 0x000036 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_LANE_CFG"                           => [ 01, 02, 1, "s", sub { return (0x0e0000 + 0x000037 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00000001); } , [0, 3], [0, 24] ],
        "FM_LANE_STATUS"                        => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000038 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_SERDES_RX_CFG"                      => [ 01, 02, 1, "s", sub { return (0x0e0000 + 0x000039 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_SERDES_TX_CFG"                      => [ 02, 02, 1, "s", sub { return (0x0e0000 + 0x00003a + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_SERDES_SIGNAL_DETECT"               => [ 01, 02, 1, "s", sub { return (0x0e0000 + 0x00003c + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_SERDES_STATUS"                      => [ 02, 02, 1, "o", sub { return (0x0e0000 + 0x00003e + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_SERDES_IM"                          => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000040 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00003fff); } , [0, 3], [0, 24] ],
        "FM_SERDES_IP"                          => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000041 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_LANE_DEBUG"                         => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000042 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],

# Block: PARSER, base address = 0x100000 (4 registers) 
        "FM_PARSER_CAM"                         => [ 04, 02,-1, "u", sub { return (0x100000 + 0x000000 + 0x4 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 127], [0, 27] ],
        "FM_PARSER_RAM"                         => [ 04, 02,-1, "u", sub { return (0x100000 + 0x000200 + 0x4 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 127], [0, 27] ],
        "FM_PARSER_INIT_STATE"                  => [ 02, 01, 0, "u", sub { return (0x100000 + 0x008000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 75] ],
        "FM_PARSER_INIT_FIELDS"                 => [ 02, 02, 1, "u", sub { return (0x100000 + 0x008200 + 0x2 * $_[0]+ 0x4 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 1], [0, 75] ],

# Block: MAPPER, base address = 0x120000 (50 registers) 
        "FM_MAPPER_VID_PROFILE_TABLE"           => [ 01, 01,-1, "u", sub { return (0x120000 + 0x000000 + 0x1 * $_[0]) }, sub { return (0x00000002); } , [0, 3] ],
        "FM_MAPPER_VID1_TABLE"                  => [ 01, 01,-1, "u", sub { return (0x120000 + 0x001000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],
        "FM_MAPPER_VID2_TABLE"                  => [ 01, 01,-1, "u", sub { return (0x120000 + 0x002000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],
        "FM_MAPPER_SRC_PORT_TABLE"              => [ 02, 01, 0, "u", sub { return (0x120000 + 0x003000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 75] ],
        "FM_MAPPER_DMAC_CAM1"                   => [ 03, 01,-1, "u", sub { return (0x120000 + 0x003100 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 15] ],
        "FM_MAPPER_DMAC_CAM2"                   => [ 03, 01,-1, "u", sub { return (0x120000 + 0x003140 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 15] ],
        "FM_MAPPER_DMAC_CAM3"                   => [ 03, 01,-1, "u", sub { return (0x120000 + 0x003180 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 31] ],
        "FM_MAPPER_DMAC_RAM1"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003200 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_DMAC_RAM2"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003210 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_DMAC_RAM3"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003220 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_MAPPER_SMAC_CAM1"                   => [ 03, 01,-1, "u", sub { return (0x120000 + 0x003300 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 15] ],
        "FM_MAPPER_SMAC_CAM3"                   => [ 03, 01,-1, "u", sub { return (0x120000 + 0x003380 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 31] ],
        "FM_MAPPER_SMAC_RAM1"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003400 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_SMAC_RAM3"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003420 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_MAPPER_TYPE_CAM1"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003440 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_TYPE_CAM2"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003450 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_TYPE_RAM1"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003460 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_TYPE_RAM2"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003470 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_DIP_CAM1"                    => [ 04, 02,-1, "u", sub { return (0x120000 + 0x003800 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 15] ],
        "FM_MAPPER_DIP_CAM2"                    => [ 04, 02,-1, "u", sub { return (0x120000 + 0x003880 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 15] ],
        "FM_MAPPER_DIP_CAM3"                    => [ 04, 02,-1, "u", sub { return (0x120000 + 0x003900 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 31] ],
        "FM_MAPPER_SIP_CAM1"                    => [ 04, 02,-1, "u", sub { return (0x120000 + 0x003a00 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 15] ],
        "FM_MAPPER_SIP_CAM2"                    => [ 04, 02,-1, "u", sub { return (0x120000 + 0x003a80 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 15] ],
        "FM_MAPPER_SIP_CAM3"                    => [ 04, 02,-1, "u", sub { return (0x120000 + 0x003b00 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 31] ],
        "FM_MAPPER_DIP_RAM1"                    => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003c00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_DIP_RAM2"                    => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003c10 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_DIP_RAM3"                    => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003c20 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_MAPPER_SIP_RAM1"                    => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003c40 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_SIP_RAM2"                    => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003c50 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_SIP_RAM3"                    => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003c60 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_MAPPER_PROT_CAM1"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003c80 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_PROT_CAM2"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003c90 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_PROT_RAM1"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003ca0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_PROT_RAM2"                   => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003cb0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_LENGTH_COMPARE"              => [ 01, 01,-1, "u", sub { return (0x120000 + 0x003cc0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_L4_SRC_COMPARE"              => [ 02, 01,-1, "u", sub { return (0x120000 + 0x003d00 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 63] ],
        "FM_MAPPER_L4_DST_COMPARE"              => [ 02, 01,-1, "u", sub { return (0x120000 + 0x003d80 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 63] ],
        "FM_MAPPER_SCENARIO_FLAGS_CFG"          => [ 04, 00,-1, "u", sub { return (0x120000 + 0x003e00 ) }, sub { return (0x03020100, 0x07060504, 0x0b0a0908, 0x0f0e0d0c); }  ],
        "FM_MAPPER_FFU_INIT"                    => [ 01, 00,-1, "u", sub { return (0x120000 + 0x003e04 ) }, sub { return (0x00); }  ],
        "FM_MAPPER_QOS_PER_PORT_VPRI1"          => [ 02, 01, 0, "u", sub { return (0x120000 + 0x003f00 + 0x2 * $_[0]) }, sub { return (0x33221100, 0x77665544); } , [0, 75] ],
        "FM_MAPPER_QOS_PER_PORT_VPRI2"          => [ 02, 01, 0, "u", sub { return (0x120000 + 0x004000 + 0x2 * $_[0]) }, sub { return (0x33221100, 0x77665544); } , [0, 75] ],
        "FM_MAPPER_QOS_PER_PORT_W4"             => [ 02, 01, 0, "u", sub { return (0x120000 + 0x004100 + 0x2 * $_[0]) }, sub { return (0x33221100, 0x77665544); } , [0, 75] ],
        "FM_MAPPER_QOS_L2_VPRI1_TO_ISL"         => [ 01, 01,-1, "u", sub { return (0x120000 + 0x004200 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_QOS_L2_VPRI2_TO_ISL"         => [ 01, 01,-1, "u", sub { return (0x120000 + 0x004210 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_QOS_W4_TO_ISL"               => [ 01, 01,-1, "u", sub { return (0x120000 + 0x004220 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_QOS_L3_PRI_TO_ISL"           => [ 01, 01,-1, "u", sub { return (0x120000 + 0x004230 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_MAPPER_QOS_CAM1"                    => [ 02, 01,-1, "u", sub { return (0x120000 + 0x004280 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_MAPPER_QOS_CAM2"                    => [ 02, 01,-1, "u", sub { return (0x120000 + 0x0042c0 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_MAPPER_QOS_RAM1"                    => [ 01, 01,-1, "u", sub { return (0x120000 + 0x004300 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_MAPPER_QOS_RAM2"                    => [ 01, 01,-1, "u", sub { return (0x120000 + 0x004320 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],

# Block: FFU, base address = 0x300000 (22 registers) 
        "FM_FFU_BST_ACTION"                     => [ 02, 03,-1, "u", sub { return (0x300000 + 0x000000 + 0x2 * $_[0]+ 0x800 * $_[1]+ 0x10000 * $_[2]) }, sub { return (0x00, 0x00); } , [0, 1023], [0, 15], [0, 3] ],
        "FM_FFU_BST_ACTION_ROUTE"               => [ 02, 03,-1, "u", sub { return (0x300000 + 0x000000 + 0x2 * $_[0]+ 0x800 * $_[1]+ 0x10000 * $_[2]) }, sub { return (0x00, 0x00); } , [0, 1023], [0, 15], [0, 3] ],
        "FM_FFU_BST_ACTION_MISC"                => [ 02, 03,-1, "u", sub { return (0x300000 + 0x000000 + 0x2 * $_[0]+ 0x800 * $_[1]+ 0x10000 * $_[2]) }, sub { return (0x00, 0x00); } , [0, 1023], [0, 15], [0, 3] ],
        "FM_FFU_BST_KEY"                        => [ 01, 03,-1, "u", sub { return (0x300000 + 0x008000 + 0x1 * $_[0]+ 0x400 * $_[1]+ 0x10000 * $_[2]) }, sub { return (0x00); } , [0, 1023], [0, 15], [0, 3] ],
        "FM_FFU_BST_SCENARIO_CAM"               => [ 02, 02,-1, "u", sub { return (0x300000 + 0x00c000 + 0x2 * $_[0]+ 0x10000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 31], [0, 3] ],
        "FM_FFU_BST_SCENARIO_CFG1"              => [ 01, 02,-1, "u", sub { return (0x300000 + 0x00c040 + 0x1 * $_[0]+ 0x10000 * $_[1]) }, sub { return (0x00); } , [0, 31], [0, 3] ],
        "FM_FFU_BST_SCENARIO_CFG2"              => [ 01, 02,-1, "u", sub { return (0x300000 + 0x00c060 + 0x1 * $_[0]+ 0x10000 * $_[1]) }, sub { return (0x00); } , [0, 31], [0, 3] ],
        "FM_FFU_BST_ROOT_KEYS"                  => [ 02, 02,-1, "u", sub { return (0x300000 + 0x00c080 + 0x2 * $_[0]+ 0x10000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 15], [0, 3] ],
        "FM_FFU_BST_MASTER_VALID"               => [ 01, 01,-1, "u", sub { return (0x300000 + 0x00c0a0 + 0x10000 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_FFU_SLICE_CAM"                      => [ 04, 02,-1, "u", sub { return (0x300000 + 0x080000 + 0x4 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1023], [0, 23] ],
        "FM_FFU_SLICE_ACTION"                   => [ 02, 02,-1, "u", sub { return (0x300000 + 0x081000 + 0x2 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 1023], [0, 23] ],
        "FM_FFU_SLICE_ACTION_ROUTE"             => [ 02, 02,-1, "u", sub { return (0x300000 + 0x081000 + 0x2 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 1023], [0, 23] ],
        "FM_FFU_SLICE_ACTION_MISC"              => [ 02, 02,-1, "u", sub { return (0x300000 + 0x081000 + 0x2 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 1023], [0, 23] ],
        "FM_FFU_SLICE_SCENARIO_CAM"             => [ 02, 02,-1, "u", sub { return (0x300000 + 0x081800 + 0x2 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 31], [0, 23] ],
        "FM_FFU_SLICE_SCENARIO_CFG"             => [ 02, 02,-1, "u", sub { return (0x300000 + 0x081840 + 0x2 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 31], [0, 23] ],
        "FM_FFU_SLICE_MASTER_VALID"             => [ 01, 01,-1, "u", sub { return (0x300000 + 0x081880 + 0x4000 * $_[0]) }, sub { return (0x00); } , [0, 23] ],
        "FM_FFU_REMAP_SCENARIO_CAM"             => [ 02, 01,-1, "u", sub { return (0x300000 + 0x0f8000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_FFU_REMAP_PROFILE"                  => [ 01, 01,-1, "u", sub { return (0x300000 + 0x0f8040 + 0x1 * $_[0]) }, sub { return (0x0c000040); } , [0, 31] ],
        "FM_FFU_EACL_CFG"                       => [ 02, 00,-1, "u", sub { return (0x300000 + 0x0fc000 ) }, sub { return (0x00, 0x00); }  ],
        "FM_HASH_LAYER3_PROFILE"                => [ 04, 01,-1, "u", sub { return (0x300000 + 0x0fc040 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 15] ],
        "FM_HASH_LAYER3_PTABLE"                 => [ 01, 01,-1, "u", sub { return (0x300000 + 0x0fc400 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1023] ],
        "FM_FFU_ATOMIC_APPLY"                   => [ 01, 00,-1, "u", sub { return (0x300000 + 0x0f0000 ) }, sub { return (0x00); }  ],

# Block: NEXTHOP, base address = 0x160000 (4 registers) 
        "FM_NEXTHOP_WIDE"                       => [ 04, 01,-1, "u", sub { return (0x160000 + 0x000000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 32767] ],
        "FM_NEXTHOP_TABLE"                      => [ 02, 01,-1, "u", sub { return (0x160000 + 0x000000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 65535] ],
        "FM_NEXTHOP_NARROW_UNICAST"             => [ 02, 01,-1, "u", sub { return (0x160000 + 0x000000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 65535] ],
        "FM_NEXTHOP_NARROW_MULTICAST"           => [ 02, 01,-1, "u", sub { return (0x160000 + 0x000000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 65535] ],

# Block: HASH, base address = 0x00b000 (4 registers) 
        "FM_HASH_LAYER2_KEY_PROFILE"            => [ 05, 01,-1, "u", sub { return (0x00b000 + 0x000000 + 0x8 * $_[0]) }, sub { return (0x00, 0x00, 0x00, 0x00, 0x00); } , [0, 15] ],
        "FM_HASH_LAYER2_FUNC_PROFILE"           => [ 01, 01,-1, "u", sub { return (0x00b000 + 0x000080 + 0x1 * $_[0]) }, sub { return (0x00000001); } , [0, 15] ],
        "FM_HASH_LAYER2_ROTA_PTABLE"            => [ 01, 01,-1, "u", sub { return (0x00b000 + 0x000400 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1023] ],
        "FM_HASH_LAYER2_ROTB_PTABLE"            => [ 01, 01,-1, "u", sub { return (0x00b000 + 0x000800 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1023] ],

# Block: L3AR, base address = 0x010000 (33 registers) 
        "FM_L3AR_CAM"                           => [ 04, 03,-1, "u", sub { return (0x010000 + 0x000000 + 0x4 * $_[0]+ 0x10 * $_[1]+ 0x200 * $_[2]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 3], [0, 31], [0, 4] ],
        "FM_L3AR_SLICE_CFG"                     => [ 01, 00,-1, "u", sub { return (0x010000 + 0x001000 ) }, sub { return (0x00); }  ],
        "FM_L3AR_KEY_CFG"                       => [ 01, 00,-1, "u", sub { return (0x010000 + 0x001001 ) }, sub { return (0x0000000f); }  ],
        "FM_L3AR_ACTION_CFG"                    => [ 01, 01,-1, "u", sub { return (0x010000 + 0x001008 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_L3AR_RAM1"                          => [ 02, 02,-1, "u", sub { return (0x010000 + 0x001200 + 0x2 * $_[0]+ 0x40 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 31], [0, 4] ],
        "FM_L3AR_RAM2"                          => [ 02, 02,-1, "u", sub { return (0x010000 + 0x001400 + 0x2 * $_[0]+ 0x40 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 31], [0, 4] ],
        "FM_L3AR_RAM3"                          => [ 01, 02,-1, "u", sub { return (0x010000 + 0x001600 + 0x1 * $_[0]+ 0x20 * $_[1]) }, sub { return (0x00); } , [0, 31], [0, 4] ],
        "FM_L3AR_RAM4"                          => [ 02, 02,-1, "u", sub { return (0x010000 + 0x001800 + 0x2 * $_[0]+ 0x40 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 31], [0, 4] ],
        "FM_L3AR_RAM5"                          => [ 02, 02,-1, "u", sub { return (0x010000 + 0x001a00 + 0x2 * $_[0]+ 0x40 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 31], [0, 4] ],
        "FM_L3AR_DGLORT_PROFILE_TABLE"          => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001c00 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_SGLORT_PROFILE_TABLE"          => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001c40 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_W8ABCD_PROFILE_TABLE"          => [ 03, 01,-1, "u", sub { return (0x010000 + 0x001c80 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_W8E_PROFILE_TABLE"             => [ 01, 01,-1, "u", sub { return (0x010000 + 0x001d00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L3AR_W8F_PROFILE_TABLE"             => [ 01, 01,-1, "u", sub { return (0x010000 + 0x001d20 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L3AR_MA1_MAC_PROFILE_TABLE"         => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001d40 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_MA2_MAC_PROFILE_TABLE"         => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001d80 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_VID_PROFILE_TABLE"             => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001dc0 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_MA_FID_PROFILE_TABLE"          => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001e00 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_CSGLORT_PROFILE_TABLE"         => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001e40 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_W16ABC_PROFILE_TABLE"          => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001e80 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_W16DEF_PROFILE_TABLE"          => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001ec0 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_W16GH_PROFILE_TABLE"           => [ 02, 01,-1, "u", sub { return (0x010000 + 0x001f00 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_HASH_ROT_PROFILE_TABLE"        => [ 01, 01,-1, "u", sub { return (0x010000 + 0x001f40 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_L3AR_ALU13_OP_PROFILE_TABLE"        => [ 04, 01,-1, "u", sub { return (0x010000 + 0x001f80 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_ALU46_OP_PROFILE_TABLE"        => [ 04, 01,-1, "u", sub { return (0x010000 + 0x002000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_POL1_IDX_PROFILE_TABLE"        => [ 01, 01,-1, "u", sub { return (0x010000 + 0x002080 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_L3AR_POL2_IDX_PROFILE_TABLE"        => [ 01, 01,-1, "u", sub { return (0x010000 + 0x002090 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_L3AR_POL3_IDX_PROFILE_TABLE"        => [ 01, 01,-1, "u", sub { return (0x010000 + 0x0020a0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_L3AR_QOS_PROFILE_TABLE"             => [ 02, 01,-1, "u", sub { return (0x010000 + 0x0020c0 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L3AR_TRAP_HEADER_RULE"              => [ 01, 01,-1, "o", sub { return (0x010000 + 0x002100 + 0x1 * $_[0]) }, sub { } , [0, 1] ],
        "FM_L3AR_TRAP_HEADER_DATA"              => [ 11, 01,-1, "o", sub { return (0x010000 + 0x002120 + 0x10 * $_[0]) }, sub { } , [0, 1] ],
        "FM_L3AR_IP"                            => [ 01, 01,-1, "o", sub { return (0x010000 + 0x002140 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_L3AR_IM"                            => [ 01, 01,-1, "u", sub { return (0x010000 + 0x002148 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4] ],

# Block: L2L_MAC, base address = 0x280000 (2 registers) 
        "FM_L2L_MAC_TABLE"                      => [ 04, 02,-1, "u", sub { return (0x280000 + 0x000000 + 0x4 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 4095], [0, 15] ],
        "FM_L2L_MAC_TABLE_SWEEPER"              => [ 04, 02,-1, "u", sub { return (0x280000 + 0x040000 + 0x4 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 4095], [0, 15] ],

# Block: L2L, base address = 0x030000 (7 registers) 
        "FM_L2L_MAC_TABLE_CFG"                  => [ 01, 00,-1, "u", sub { return (0x030000 + 0x000000 ) }, sub { return (0x00003000); }  ],
        "FM_L2L_CMD_PROFILE_TABLE"              => [ 01, 01,-1, "u", sub { return (0x030000 + 0x000010 + 0x1 * $_[0]) }, sub { return (0x00088000); } , [0, 15] ],
        "FM_L2L_LOCK_TABLE"                     => [ 02, 01,-1, "u", sub { return (0x030000 + 0x000800 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 1023] ],
        "FM_L2L_EVID1_TABLE"                    => [ 02, 01,-1, "u", sub { return (0x030000 + 0x002000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 4095] ],
        "FM_L2L_IVID1_TABLE"                    => [ 02, 01,-1, "u", sub { return (0x030000 + 0x004000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 4095] ],
        "FM_L2L_EVID2_TABLE"                    => [ 01, 01,-1, "u", sub { return (0x030000 + 0x006000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],
        "FM_L2L_IVID2_TABLE"                    => [ 01, 01,-1, "u", sub { return (0x030000 + 0x007000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],

# Block: L2F, base address = 0x180000 (3 registers) 
        "FM_L2F_TABLE_4K"                       => [ 03, 02,-1, "u", sub { return (0x180000 + 0x000000 + 0x4 * $_[0]+ 0x4000 * $_[1]) }, sub { return (0x00, 0x00, 0x00); } , [0, 4095], [0, 7] ],
        "FM_L2F_TABLE_256"                      => [ 03, 02,-1, "s", sub { return (0x180000 + 0x020000 + 0x4 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00, 0x00); } , [0, 255], [0, 3] ],
        "FM_L2F_PROFILE_TABLE"                  => [ 01, 02,-1, "u", sub { return (0x180000 + 0x021000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x00); } , [0, 15], [0, 11] ],

# Block: L2L_SWEEPER, base address = 0x00d000 (11 registers) 
        "FM_L2L_SWEEPER_TIMER_CFG"              => [ 02, 01,-1, "u", sub { return (0x00d000 + 0x000000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 1] ],
        "FM_L2L_SWEEPER_TIMER_STATUS"           => [ 02, 01,-1, "o", sub { return (0x00d000 + 0x000004 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 1] ],
        "FM_L2L_SWEEPER_CAM"                    => [ 04, 02,-1, "i", sub { return (0x00d000 + 0x000080 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 15] ],
        "FM_L2L_SWEEPER_RAM"                    => [ 03, 01,-1, "i", sub { return (0x00d000 + 0x000100 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 15] ],
        "FM_L2L_SWEEPER_FIFO"                   => [ 01, 01,-1, "i", sub { return (0x00d000 + 0x000200 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 511] ],
        "FM_L2L_SWEEPER_FIFO_HEAD"              => [ 01, 00,-1, "i", sub { return (0x00d000 + 0x000400 ) }, sub { return (0x00); }  ],
        "FM_L2L_SWEEPER_FIFO_TAIL"              => [ 01, 00,-1, "i", sub { return (0x00d000 + 0x000401 ) }, sub { return (0x00); }  ],
        "FM_L2L_SWEEPER_IP"                     => [ 01, 00,-1, "o", sub { return (0x00d000 + 0x000402 ) }, sub { return (0x00); }  ],
        "FM_L2L_SWEEPER_IM"                     => [ 01, 00,-1, "i", sub { return (0x00d000 + 0x000403 ) }, sub { return (0x00); }  ],
        "FM_L2L_SWEEPER_WRITE_COMMAND"          => [ 01, 00,-1, "u", sub { return (0x00d000 + 0x000404 ) }, sub { return (0x00); }  ],
        "FM_L2L_SWEEPER_WRITE_DATA"             => [ 04, 00,-1, "u", sub { return (0x00d000 + 0x000408 ) }, sub { return (0x00, 0x00, 0x00, 0x00); }  ],

# Block: GLORT, base address = 0x00e000 (2 registers) 
        "FM_GLORT_CAM"                          => [ 01, 01,-1, "u", sub { return (0x00e000 + 0x000000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1023] ],
        "FM_GLORT_RAM"                          => [ 02, 01,-1, "u", sub { return (0x00e000 + 0x000800 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 1023] ],

# Block: ALU, base address = 0x00c000 (2 registers) 
        "FM_ALU_CMD_TABLE"                      => [ 02, 02,-1, "u", sub { return (0x00c000 + 0x000000 + 0x2 * $_[0]+ 0x40 * $_[1]) }, sub { return (0x00, 0x00000800); } , [0, 31], [0, 5] ],
        "FM_ALU_Y_TABLE"                        => [ 01, 02,-1, "u", sub { return (0x00c000 + 0x000200 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x00); } , [0, 15], [0, 5] ],

# Block: POLICERS, base address = 0x130000 (6 registers) 
        "FM_POLICER_CFG_4K"                     => [ 02, 02,-1, "u", sub { return (0x130000 + 0x000000 + 0x2 * $_[0]+ 0x2000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 4095], [0, 1] ],
        "FM_POLICER_CFG_1K"                     => [ 01, 01,-1, "u", sub { return (0x130000 + 0x004000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1023] ],
        "FM_POLICER_STATE_4K"                   => [ 02, 02,-1, "i", sub { return (0x130000 + 0x008000 + 0x2 * $_[0]+ 0x2000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 4095], [0, 1] ],
        "FM_POLICER_STATE_1K"                   => [ 01, 01,-1, "i", sub { return (0x130000 + 0x00c000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1023] ],
        "FM_POLICER_QOS_MAP1"                   => [ 01, 01,-1, "u", sub { return (0x130000 + 0x00c400 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_POLICER_QOS_MAP2"                   => [ 01, 01,-1, "u", sub { return (0x130000 + 0x00c440 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 63] ],

# Block: EACL, base address = 0x006000 (4 registers) 
        "FM_EACL_CAM1"                          => [ 03, 02,-1, "u", sub { return (0x006000 + 0x000000 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00); } , [0, 1], [0, 31] ],
        "FM_EACL_CAM2"                          => [ 02, 01, 0, "u", sub { return (0x006000 + 0x000100 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 75] ],
        "FM_EACL_ACTION_RAM"                    => [ 01, 02,-1, "u", sub { return (0x006000 + 0x000400 + 0x1 * $_[0]+ 0x20 * $_[1]) }, sub { return (0x00); } , [0, 31], [0, 31] ],
        "FM_EACL_PROFILE_TABLE"                 => [ 01, 01,-1, "u", sub { return (0x006000 + 0x000800 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],

# Block: LAG, base address = 0x007000 (2 registers) 
        "FM_LAG_PORT_TABLE"                     => [ 01, 01, 0, "u", sub { return (0x007000 + 0x000000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_LAG_PROFILE_TABLE"                  => [ 01, 01,-1, "u", sub { return (0x007000 + 0x000080 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],

# Block: LBS, base address = 0x014000 (2 registers) 
        "FM_LBS_CAM"                            => [ 01, 01, 0, "u", sub { return (0x014000 + 0x000000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_LBS_PROFILE_TABLE"                  => [ 01, 01,-1, "u", sub { return (0x014000 + 0x000080 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],

# Block: L2AR, base address = 0x140000 (46 registers) 
        "FM_L2AR_CAM"                           => [ 04, 03,-1, "u", sub { return (0x140000 + 0x000000 + 0x4 * $_[0]+ 0x20 * $_[1]+ 0x800 * $_[2]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 5], [0, 63], [0, 7] ],
        "FM_L2AR_CAM_DMASK"                     => [ 04, 03,-1, "u", sub { return (0x140000 + 0x004000 + 0x4 * $_[0]+ 0x8 * $_[1]+ 0x200 * $_[2]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 63], [0, 7] ],
        "FM_L2AR_SLICE_CFG"                     => [ 01, 00,-1, "u", sub { return (0x140000 + 0x005000 ) }, sub { return (0x00); }  ],
        "FM_L2AR_EACL_EXT"                      => [ 02, 01,-1, "u", sub { return (0x140000 + 0x005010 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 7] ],
        "FM_L2AR_RAM"                           => [ 02, 02,-1, "u", sub { return (0x140000 + 0x005400 + 0x2 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 63], [0, 7] ],
        "FM_L2AR_FLAGS_CAM_POLARITY"            => [ 03, 01,-1, "u", sub { return (0x140000 + 0x005800 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 1] ],
        "FM_L2AR_FLAGS_CAM_VALUE"               => [ 03, 01,-1, "u", sub { return (0x140000 + 0x005808 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 1] ],
        "FM_L2AR_FLAGS_CAM"                     => [ 04, 02, 0, "u", sub { return (0x140000 + 0x005c00 + 0x4 * $_[0]+ 0x200 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 75], [0, 1] ],
        "FM_L2AR_ACTION_DMT"                    => [ 03, 02,-1, "u", sub { return (0x140000 + 0x006000 + 0x4 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00, 0x00, 0x00); } , [0, 31], [0, 2] ],
        "FM_L2AR_ACTION_CPU_CODE"               => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006200 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_TRAP_HEADER"            => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006280 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_MIRROR"                 => [ 01, 02,-1, "u", sub { return (0x140000 + 0x006400 + 0x1 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 127], [0, 3] ],
        "FM_L2AR_ACTION_QOS"                    => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006600 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_MA_WRITEBACK"           => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006680 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_DGLORT"                 => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006700 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_W16AB"                  => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006780 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_W16CDEF"                => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006800 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_W8ABCDE"                => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006880 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_W4"                     => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006900 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_VID"                    => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006980 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_DMASK_IDX"              => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006a00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_STATS_IDX5AB"           => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006a80 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_STATS_IDX5C"            => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006b00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_STATS_IDX12A"           => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006b80 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_STATS_IDX12B"           => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006c00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_STATS_IDX16A"           => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006c80 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_ACTION_STATS_IDX16B"           => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006d00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_L2AR_DGLORT_PROFILE_TABLE"          => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006d80 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L2AR_W16AB_PROFILE_TABLE"           => [ 02, 01,-1, "u", sub { return (0x140000 + 0x006dc0 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L2AR_W16CDEF_PROFILE_TABLE"         => [ 03, 01,-1, "u", sub { return (0x140000 + 0x006e00 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 31] ],
        "FM_L2AR_W8ABCDE_PROFILE_TABLE"         => [ 03, 01,-1, "u", sub { return (0x140000 + 0x006e80 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 31] ],
        "FM_L2AR_W4_PROFILE_TABLE"              => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006f00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_L2AR_VID_PROFILE_TABLE"             => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006f20 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L2AR_DMASK_IDX_PROFILE_TABLE"       => [ 01, 01,-1, "u", sub { return (0x140000 + 0x006f40 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L2AR_QOS_PROFILE_TABLE"             => [ 02, 01,-1, "u", sub { return (0x140000 + 0x006f80 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 31] ],
        "FM_L2AR_MA_WRITEBACK_PROFILE_TABLE"    => [ 03, 01,-1, "u", sub { return (0x140000 + 0x007000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 31] ],
        "FM_L2AR_STATS_IDX5AB_PROFILE_TABLE"    => [ 01, 01,-1, "u", sub { return (0x140000 + 0x007080 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L2AR_STATS_IDX5C_PROFILE_TABLE"     => [ 01, 01,-1, "u", sub { return (0x140000 + 0x0070a0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L2AR_STATS_IDX12A_PROFILE_TABLE"    => [ 01, 01,-1, "u", sub { return (0x140000 + 0x0070c0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L2AR_STATS_IDX12B_PROFILE_TABLE"    => [ 01, 01,-1, "u", sub { return (0x140000 + 0x0070e0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_L2AR_STATS_IDX16A_PROFILE_TABLE"    => [ 01, 01,-1, "u", sub { return (0x140000 + 0x007100 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_L2AR_STATS_IDX16B_PROFILE_TABLE"    => [ 01, 01,-1, "u", sub { return (0x140000 + 0x007110 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_L2AR_TRAP_HEADER_RULE"              => [ 01, 01,-1, "o", sub { return (0x140000 + 0x007120 + 0x1 * $_[0]) }, sub { } , [0, 1] ],
        "FM_L2AR_TRAP_HEADER_DATA"              => [ 04, 01,-1, "o", sub { return (0x140000 + 0x007128 + 0x4 * $_[0]) }, sub { } , [0, 1] ],
        "FM_L2AR_IP"                            => [ 02, 01,-1, "u", sub { return (0x140000 + 0x007130 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 7] ],
        "FM_L2AR_IM"                            => [ 02, 01,-1, "u", sub { return (0x140000 + 0x007140 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 7] ],

# Block: CM, base address = 0x110000 (36 registers) 
        "FM_CM_RXMP_MAP"                        => [ 02, 00,-1, "u", sub { return (0x110000 + 0x000000 ) }, sub { return (0x00, 0x00); }  ],
        "FM_CM_TXMP_MAP"                        => [ 02, 00,-1, "u", sub { return (0x110000 + 0x000002 ) }, sub { return (0x00, 0x00); }  ],
        "FM_CM_TC_MAP"                          => [ 02, 00,-1, "u", sub { return (0x110000 + 0x000004 ) }, sub { return (0x00, 0x00); }  ],
        "FM_CM_BSG_MAP"                         => [ 02, 01, 0, "u", sub { return (0x110000 + 0x000100 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 75] ],
        "FM_CM_GLOBAL_USAGE"                    => [ 02, 00,-1, "o", sub { return (0x110000 + 0x000200 ) }, sub { }  ],
        "FM_CM_RXMP_USAGE"                      => [ 01, 01,-1, "o", sub { return (0x110000 + 0x000210 + 0x1 * $_[0]) }, sub { } , [0, 11] ],
        "FM_CM_SHARED_RXMP_USAGE"               => [ 02, 01,-1, "o", sub { return (0x110000 + 0x000220 + 0x2 * $_[0]) }, sub { } , [0, 11] ],
        "FM_CM_PORT_RXMP_USAGE"                 => [ 02, 02, 1, "o", sub { return (0x110000 + 0x001000 + 0x2 * $_[0]+ 0x20 * $_[1]) }, sub { } , [0, 11], [0, 75] ],
        "FM_CM_GLOBAL_TXMP_USAGE"               => [ 01, 01,-1, "o", sub { return (0x110000 + 0x002000 + 0x1 * $_[0]) }, sub { } , [0, 11] ],
        "FM_CM_PORT_TX_DROP_COUNT"              => [ 02, 01, 0, "u", sub { return (0x110000 + 0x002100 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 79] ],
        "FM_CM_GLOBAL_WM"                       => [ 01, 00,-1, "u", sub { return (0x110000 + 0x002200 ) }, sub { return (0x3e00bcc0); }  ],
        "FM_CM_SHARED_RXMP_WM"                  => [ 01, 01,-1, "u", sub { return (0x110000 + 0x002210 + 0x1 * $_[0]) }, sub { return (0xffffffff); } , [0, 15] ],
        "FM_CM_PORT_RXMP_PRIVATE_WM"            => [ 01, 02, 1, "u", sub { return (0x110000 + 0x002800 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0xffffffff); } , [0, 11], [0, 75] ],
        "FM_CM_PORT_RXMP_HOG_WM"                => [ 01, 02, 1, "u", sub { return (0x110000 + 0x003000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0xffffffff); } , [0, 15], [0, 75] ],
        "FM_CM_PORT_TXMP_PRIVATE_WM"            => [ 01, 02, 1, "u", sub { return (0x110000 + 0x003800 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x00003fff); } , [0, 15], [0, 79] ],
        "FM_CM_PORT_TXMP_HOG_WM"                => [ 01, 02, 1, "u", sub { return (0x110000 + 0x004000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x00003fff); } , [0, 15], [0, 79] ],
        "FM_CM_RXMP_SOFT_DROP_WM"               => [ 01, 01,-1, "u", sub { return (0x110000 + 0x004800 + 0x1 * $_[0]) }, sub { return (0x7ffe3fff); } , [0, 15] ],
        "FM_CM_PORT_RXMP_PAUSE_ON_WM"           => [ 01, 02, 1, "u", sub { return (0x110000 + 0x005000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0xffffffff); } , [0, 11], [0, 75] ],
        "FM_CM_PORT_RXMP_PAUSE_OFF_WM"          => [ 01, 02, 1, "u", sub { return (0x110000 + 0x005800 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0xffffffff); } , [0, 11], [0, 75] ],
        "FM_CM_SHARED_RXMP_PAUSE_ON_WM"         => [ 01, 01,-1, "u", sub { return (0x110000 + 0x006000 + 0x1 * $_[0]) }, sub { return (0xffffffff); } , [0, 11] ],
        "FM_CM_SHARED_RXMP_PAUSE_OFF_WM"        => [ 01, 01,-1, "u", sub { return (0x110000 + 0x006010 + 0x1 * $_[0]) }, sub { return (0xffffffff); } , [0, 11] ],
        "FM_CM_TC_PC_MAP"                       => [ 02, 01, 0, "u", sub { return (0x110000 + 0x006100 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 75] ],
        "FM_CM_PC_RXMP_MAP"                     => [ 01, 01, 0, "u", sub { return (0x110000 + 0x006200 + 0x1 * $_[0]) }, sub { return (0x88888888); } , [0, 75] ],
        "FM_CM_PAUSE_CFG"                       => [ 04, 01, 0, "u", sub { return (0x110000 + 0x006400 + 0x4 * $_[0]) }, sub { return (0xc1e613ff, 0x003fffff, 0x00, 0x00); } , [0, 75] ],
        "FM_CM_PAUSE_PACING_CFG"                => [ 04, 01, 0, "u", sub { return (0x110000 + 0x006600 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 75] ],
        "FM_CM_PAUSE_RCV_STATE"                 => [ 01, 02, 1, "o", sub { return (0x110000 + 0x006800 + 0x1 * $_[0]+ 0x8 * $_[1]) }, sub { } , [0, 7], [0, 75] ],
        "FM_CM_ESCHED_STATE"                    => [ 01, 01, 0, "o", sub { return (0x110000 + 0x006c00 + 0x1 * $_[0]) }, sub { } , [0, 75] ],
        "FM_CM_PAUSE_GEN_STATE"                 => [ 02, 01, 0, "o", sub { return (0x110000 + 0x006d00 + 0x2 * $_[0]) }, sub { } , [0, 75] ],
        "FM_CM_PAUSE_PACING_STATE"              => [ 01, 01, 0, "o", sub { return (0x110000 + 0x006e00 + 0x1 * $_[0]) }, sub { } , [0, 75] ],
        "FM_CM_TX_MIRROR_DEST"                  => [ 03, 01,-1, "u", sub { return (0x110000 + 0x006e80 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 3] ],
        "FM_CM_TX_MIRROR_SRC"                   => [ 03, 01,-1, "u", sub { return (0x110000 + 0x006e90 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 3] ],
        "FM_CM_SAMPLING_MIRROR_CFG"             => [ 01, 01,-1, "u", sub { return (0x110000 + 0x006ea0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 11] ],
        "FM_ERL_CFG"                            => [ 01, 02, 1, "u", sub { return (0x110000 + 0x007000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x20001000); } , [0, 11], [0, 75] ],
        "FM_ERL_CFG_IFG"                        => [ 01, 01, 0, "u", sub { return (0x110000 + 0x007800 + 0x1 * $_[0]) }, sub { return (0x00000014); } , [0, 75] ],
        "FM_ERL_USAGE"                          => [ 01, 02, 1, "o", sub { return (0x110000 + 0x008000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { } , [0, 11], [0, 75] ],
        "FM_CM_QUEUE_STATE_INIT"                => [ 01, 02,-1, "u", sub { return (0x110000 + 0x008800 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x00); } , [0, 11], [0, 79] ],

# Block: CMM, base address = 0x020000 (9 registers) 
        "FM_CM_PORT_TXMP_USAGE"                 => [ 01, 02, 1, "o", sub { return (0x020000 + 0x000000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { } , [0, 11], [0, 79] ],
        "FM_CM_PORT_TXMP_IP_WM"                 => [ 01, 02, 1, "i", sub { return (0x020000 + 0x000800 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0xffffffff); } , [0, 11], [0, 79] ],
        "FM_CM_PORT_TXMP_SAMPLING_PERIOD"       => [ 01, 02, 1, "u", sub { return (0x020000 + 0x001000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x00); } , [0, 11], [0, 79] ],
        "FM_CM_PORT_TXMP_SAMPLING_STATE"        => [ 01, 02, 1, "o", sub { return (0x020000 + 0x001800 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { } , [0, 11], [0, 79] ],
        "FM_CM_PORT_TXMP_ABOVE_IP"              => [ 03, 01,-1, "i", sub { return (0x020000 + 0x002000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 11] ],
        "FM_CM_PORT_TXMP_BELOW_IP"              => [ 03, 01,-1, "i", sub { return (0x020000 + 0x002040 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 11] ],
        "FM_CM_PORT_TXMP_ABOVE_IM"              => [ 03, 01,-1, "u", sub { return (0x020000 + 0x002080 + 0x4 * $_[0]) }, sub { return (0xffffffff, 0xffffffff, 0x0000ffff); } , [0, 11] ],
        "FM_CM_PORT_TXMP_BELOW_IM"              => [ 03, 01,-1, "u", sub { return (0x020000 + 0x0020c0 + 0x4 * $_[0]) }, sub { return (0xffffffff, 0xffffffff, 0x0000ffff); } , [0, 11] ],
        "FM_CM_INTERRUPT_DETECT"                => [ 01, 00,-1, "o", sub { return (0x020000 + 0x002100 ) }, sub { }  ],

# Block: SAF, base address = 0x0a0000 (1 registers) 
        "FM_SAF_MATRIX"                         => [ 03, 01, 0, "u", sub { return (0x0a0000 + 0x000000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 75] ],

# Block: STATS_AR, base address = 0x018000 (12 registers) 
        "FM_STATS_AR_IDX_CAM"                   => [ 04, 03,-1, "u", sub { return (0x018000 + 0x000000 + 0x4 * $_[0]+ 0x8 * $_[1]+ 0x100 * $_[2]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 31], [0, 5] ],
        "FM_STATS_AR_IDX_RAM"                   => [ 01, 02,-1, "u", sub { return (0x018000 + 0x000800 + 0x1 * $_[0]+ 0x20 * $_[1]) }, sub { return (0x00); } , [0, 31], [0, 5] ],
        "FM_STATS_AR_FLAGS_CAM1"                => [ 04, 02,-1, "u", sub { return (0x018000 + 0x000a00 + 0x4 * $_[0]+ 0x8 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 1], [0, 63] ],
        "FM_STATS_AR_FLAGS_CAM2"                => [ 04, 01,-1, "u", sub { return (0x018000 + 0x000c00 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 89] ],
        "FM_STATS_AR_FLAGS_CAM2_POLARITY"       => [ 03, 00,-1, "u", sub { return (0x018000 + 0x000e00 ) }, sub { return (0x00, 0x00, 0x00); }  ],
        "FM_STATS_AR_FLAGS_CAM2_VALUE"          => [ 03, 00,-1, "u", sub { return (0x018000 + 0x000e04 ) }, sub { return (0x00, 0x00, 0x00); }  ],
        "FM_STATS_AR_RX_PORT_MAP"               => [ 01, 01, 0, "u", sub { return (0x018000 + 0x000e80 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_STATS_AR_TX_PORT_MAP"               => [ 01, 01, 0, "u", sub { return (0x018000 + 0x000f00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_STATS_AR_RX_LENGTH_COMPARE"         => [ 01, 01,-1, "u", sub { return (0x018000 + 0x000f80 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_STATS_AR_TX_LENGTH_COMPARE"         => [ 01, 01,-1, "u", sub { return (0x018000 + 0x000f90 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_STATS_AR_BANK_CFG1"                 => [ 01, 01,-1, "u", sub { return (0x018000 + 0x000fa0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_STATS_AR_BANK_CFG2"                 => [ 01, 01,-1, "u", sub { return (0x018000 + 0x000fb0 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],

# Block: STATS_BANK, base address = 0x200000 (1 registers) 
        "FM_STATS_BANK_COUNTER"                 => [ 02, 02,-1, "u", sub { return (0x200000 + 0x000000 + 0x2 * $_[0]+ 0x1000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 2047], [0, 15] ],

# Block: STATS_DISCRETE, base address = 0x01a000 (2 registers) 
        "FM_STATS_DISCRETE_COUNTER_FRAME"       => [ 02, 01,-1, "u", sub { return (0x01a000 + 0x000000 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 63] ],
        "FM_STATS_DISCRETE_COUNTER_BYTE"        => [ 02, 01,-1, "u", sub { return (0x01a000 + 0x000080 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 63] ],

# Block: MCAST_MID, base address = 0x240000 (7 registers) 
        "FM_MCAST_DEST_TABLE"                   => [ 03, 01,-1, "u", sub { return (0x240000 + 0x000000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 4095] ],
        "FM_MCAST_TX_MIRROR_DEST"               => [ 01, 01,-1, "u", sub { return (0x240000 + 0x004000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 9] ],
        "FM_MCAST_MIRROR_CFG"                   => [ 01, 01,-1, "u", sub { return (0x240000 + 0x004010 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_MCAST_TX_TRUNC_MASK"                => [ 01, 01,-1, "u", sub { return (0x240000 + 0x004020 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 9] ],
        "FM_MCAST_PRIVATE_WM"                   => [ 01, 01,-1, "u", sub { return (0x240000 + 0x004030 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 11] ],
        "FM_MCAST_HOG_WM"                       => [ 01, 00,-1, "u", sub { return (0x240000 + 0x004040 ) }, sub { return (0x00003fff); }  ],
        "FM_MCAST_LIMITED_SKEW_MULTICAST"       => [ 01, 00,-1, "u", sub { return (0x240000 + 0x004041 ) }, sub { return (0x00); }  ],

# Block: MCAST_POST, base address = 0x260000 (1 registers) 
        "FM_MCAST_VLAN_TABLE"                   => [ 01, 01,-1, "u", sub { return (0x260000 + 0x000000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 32767] ],

# Block: ESCHED, base address = 0x002000 (3 registers) 
        "FM_ESCHED_CFG_1"                       => [ 01, 01, 0, "u", sub { return (0x002000 + 0x000000 + 0x1 * $_[0]) }, sub { return (0x00ffffff); } , [0, 75] ],
        "FM_ESCHED_CFG_2"                       => [ 01, 01, 0, "u", sub { return (0x002000 + 0x000080 + 0x1 * $_[0]) }, sub { return (0x00ffffff); } , [0, 75] ],
        "FM_ESCHED_CFG_3"                       => [ 01, 01, 0, "u", sub { return (0x002000 + 0x000100 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],

# Block: MONITOR, base address = 0x003000 (3 registers) 
        "FM_ESCHED_DRR_Q"                       => [ 01, 02, 1, "u", sub { return (0x003000 + 0x000000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x00); } , [0, 11], [0, 75] ],
        "FM_ESCHED_DRR_CFG"                     => [ 01, 01, 0, "u", sub { return (0x003000 + 0x000800 + 0x1 * $_[0]) }, sub { return (0x00ffffff); } , [0, 75] ],
        "FM_ESCHED_DRR_DC_INIT"                 => [ 01, 01,-1, "u", sub { return (0x003000 + 0x000c00 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1023] ],

# Block: SSCHED, base address = 0x008000 (17 registers) 
        "FM_SSCHED_TX_NEXT_PORT"                => [ 01, 01,-1, "s", sub { return (0x008000 + 0x000000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_SSCHED_TX_INIT_TOKEN"               => [ 01, 00,-1, "s", sub { return (0x008000 + 0x000020 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_TX_INIT_COMPLETE"            => [ 01, 00,-1, "s", sub { return (0x008000 + 0x000021 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_TX_REPLACE_TOKEN"            => [ 01, 00,-1, "s", sub { return (0x008000 + 0x000022 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_RX_NEXT_PORT"                => [ 01, 01,-1, "s", sub { return (0x008000 + 0x000040 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 31] ],
        "FM_SSCHED_RX_INIT_TOKEN"               => [ 01, 00,-1, "s", sub { return (0x008000 + 0x000060 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_RX_INIT_COMPLETE"            => [ 01, 00,-1, "s", sub { return (0x008000 + 0x000061 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_RX_REPLACE_TOKEN"            => [ 01, 00,-1, "s", sub { return (0x008000 + 0x000062 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_RX_SLOW_PORT"                => [ 01, 01,-1, "s", sub { return (0x008000 + 0x000070 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 15] ],
        "FM_SSCHED_RXQ_FREELIST_INIT"           => [ 01, 00,-1, "s", sub { return (0x008000 + 0x0000f0 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_RXQ_FREELIST_INIT_DONE"      => [ 01, 00,-1, "s", sub { return (0x008000 + 0x0000f1 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_TXQ_FREELIST_INIT"           => [ 01, 00,-1, "s", sub { return (0x008000 + 0x0000f4 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_TXQ_FREELIST_INIT_DONE"      => [ 01, 00,-1, "s", sub { return (0x008000 + 0x0000f5 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_HS_FREELIST_INIT"            => [ 01, 00,-1, "s", sub { return (0x008000 + 0x0000f8 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_HS_FREELIST_INIT_DONE"       => [ 01, 00,-1, "s", sub { return (0x008000 + 0x0000f9 ) }, sub { return (0x00); }  ],
        "FM_SSCHED_FREELIST_INIT"               => [ 01, 00,-1, "s", sub { return (0x008000 + 0x0000fc ) }, sub { return (0x00); }  ],
        "FM_SSCHED_FREELIST_INIT_DONE"          => [ 01, 00,-1, "s", sub { return (0x008000 + 0x0000fd ) }, sub { return (0x00); }  ],

# Block: MOD, base address = 0x150000 (27 registers) 
        "FM_MOD_L2_VLAN1_TX_TAGGED"             => [ 03, 01,-1, "u", sub { return (0x150000 + 0x000000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 4095] ],
        "FM_MOD_L2_VLAN2_TX_TAGGED"             => [ 03, 01,-1, "u", sub { return (0x150000 + 0x004000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 4095] ],
        "FM_MOD_MAP_DATA_V2T"                   => [ 03, 01,-1, "u", sub { return (0x150000 + 0x004000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 4095] ],
        "FM_MOD_CAM"                            => [ 04, 02,-1, "u", sub { return (0x150000 + 0x008000 + 0x4 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00, 0x00, 0x00, 0x00); } , [0, 31], [0, 31] ],
        "FM_MOD_COMMAND_RAM"                    => [ 01, 02,-1, "u", sub { return (0x150000 + 0x009000 + 0x1 * $_[0]+ 0x20 * $_[1]) }, sub { return (0x00); } , [0, 31], [0, 19] ],
        "FM_MOD_VALUE_RAM"                      => [ 02, 02,-1, "u", sub { return (0x150000 + 0x009400 + 0x2 * $_[0]+ 0x40 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 31], [0, 14] ],
        "FM_MOD_MAP_IDX12A"                     => [ 01, 01,-1, "u", sub { return (0x150000 + 0x00a000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],
        "FM_MOD_MAP_DATA_W16A"                  => [ 01, 01,-1, "u", sub { return (0x150000 + 0x00b000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],
        "FM_MOD_MAP_DATA_W16B"                  => [ 01, 01,-1, "u", sub { return (0x150000 + 0x00c000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],
        "FM_MOD_MAP_DATA_W16C"                  => [ 01, 01,-1, "u", sub { return (0x150000 + 0x00d000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],
        "FM_MOD_MAP_DATA_W16D"                  => [ 01, 01,-1, "u", sub { return (0x150000 + 0x00e000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 4095] ],
        "FM_MOD_MAP_DATA_W16E"                  => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_MOD_MAP_DATA_W16F"                  => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f080 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_MOD_MAP_DATA_W12A"                  => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f100 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_MOD_MAP_DATA_W8A"                   => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f180 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_MOD_MAP_DATA_W8B"                   => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f200 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_MOD_TX_PORT_TAG"                    => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f280 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_MOD_DST_PORT_TAG"                   => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f300 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 75] ],
        "FM_MOD_L2_VPRI1_TX_MAP"                => [ 02, 01, 0, "u", sub { return (0x150000 + 0x00f400 + 0x2 * $_[0]) }, sub { return (0x76543210, 0xfedcba98); } , [0, 75] ],
        "FM_MOD_L2_VPRI2_TX_MAP"                => [ 02, 01, 0, "u", sub { return (0x150000 + 0x00f500 + 0x2 * $_[0]) }, sub { return (0x76543210, 0xfedcba98); } , [0, 75] ],
        "FM_MOD_TX_PAUSE_QUANTA"                => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f600 + 0x1 * $_[0]) }, sub { return (0xffffffff); } , [0, 75] ],
        "FM_MOD_MIN_LENGTH"                     => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f680 + 0x1 * $_[0]) }, sub { return (0x00000040); } , [0, 75] ],
        "FM_MCAST_LOOPBACK_SUPPRESS"            => [ 01, 01, 0, "u", sub { return (0x150000 + 0x00f700 + 0x1 * $_[0]) }, sub { return (0x0000ffff); } , [0, 75] ],
        "FM_MOD_MAP_DATA_CTRL"                  => [ 01, 00,-1, "u", sub { return (0x150000 + 0x00f800 ) }, sub { return (0x00); }  ],
        "FM_MOD_MAP_DATA_V2T_CTRL"              => [ 01, 00,-1, "u", sub { return (0x150000 + 0x00f801 ) }, sub { return (0x00000006); }  ],
        "FM_MOD_TX_MIRROR_SRC"                  => [ 01, 00,-1, "u", sub { return (0x150000 + 0x00f802 ) }, sub { return (0x00); }  ],
        "FM_MOD_TRANSMIT_MODE"                  => [ 01, 00,-1, "u", sub { return (0x150000 + 0x00f803 ) }, sub { return (0x00); }  ],

# Block: MSB, base address = 0x004000 (9 registers) 
        "FM_MSB_CFG"                            => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000000 ) }, sub { return (0x03000003); }  ],
        "FM_MSB_RX_RATE"                        => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000001 ) }, sub { return (0x00000605); }  ],
        "FM_MSB_TRUNC"                          => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000002 ) }, sub { return (0x00); }  ],
        "FM_MSB_TX_CRC_ERROR"                   => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000003 ) }, sub { return (0x00); }  ],
        "FM_MSB_TX_FRAME_ERROR"                 => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000004 ) }, sub { return (0x00); }  ],
        "FM_MSB_TX_TIMEOUT"                     => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000005 ) }, sub { return (0x00); }  ],
        "FM_MSB_TX_VALID_FRAMES"                => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000006 ) }, sub { return (0x00); }  ],
        "FM_MSB_TX_INVALID_FRAMES"              => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000007 ) }, sub { return (0x00); }  ],
        "FM_MSB_RX_VALID_FRAMES"                => [ 01, 00,-1, "u", sub { return (0x004000 + 0x000008 ) }, sub { return (0x00); }  ],

# Block: FIBM, base address = 0x005000 (27 registers) 
        "FM_FIBM_CFG"                           => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000000 ) }, sub { return (0x00048883); }  ],
        "FM_FIBM_SGLORT"                        => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000001 ) }, sub { return (0x00); }  ],
        "FM_FIBM_INT"                           => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000002 ) }, sub { return (0x00ff0000); }  ],
        "FM_FIBM_INT_FRAME"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000003 ) }, sub { return (0x000fffff); }  ],
        "FM_FIBM_INT_FRAME_DMAC"                => [ 02, 00,-1, "u", sub { return (0x005000 + 0x000004 ) }, sub { return (0x0000ffff, 0x00); }  ],
        "FM_FIBM_INT_FRAME_SMAC"                => [ 02, 00,-1, "u", sub { return (0x005000 + 0x000006 ) }, sub { return (0x0000ffff, 0x00); }  ],
        "FM_FIBM_REQUEST_CTR"                   => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000008 ) }, sub { return (0x00); }  ],
        "FM_FIBM_DROP_CTR"                      => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000009 ) }, sub { return (0x00); }  ],
        "FM_FIBM_RESPONSE_CTR"                  => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00000a ) }, sub { return (0x00); }  ],
        "FM_FIBM_INTR_CTR_0"                    => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00000b ) }, sub { return (0x00); }  ],
        "FM_FIBM_INTR_CTR_1"                    => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00000c ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_0"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00000d ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_1"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00000e ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_2"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00000f ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_3"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000010 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_4"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000011 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_5"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000012 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_6"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000013 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_7"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000014 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_8"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000015 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_9"                     => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000016 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_10"                    => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000017 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_11"                    => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000018 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_12"                    => [ 01, 00,-1, "u", sub { return (0x005000 + 0x000019 ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_13"                    => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00001a ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_14"                    => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00001b ) }, sub { return (0x00); }  ],
        "FM_FIBM_SCRATCH_15"                    => [ 01, 00,-1, "u", sub { return (0x005000 + 0x00001c ) }, sub { return (0x00); }  ],

# Block: MGMT2, base address = 0x01c000 (141 registers) 
        "FM_GLOBAL_INTERRUPT_DETECT"            => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000000 ) }, sub { }  ],
        "FM_INTERRUPT_MASK_INT"                 => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000001 ) }, sub { return (0x00007fff); }  ],
        "FM_INTERRUPT_MASK_PCIE"                => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000002 ) }, sub { return (0x00007fff); }  ],
        "FM_INTERRUPT_MASK_FIBM"                => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000003 ) }, sub { return (0x00007fff); }  ],
        "FM_GLOBAL_EPL_INT_DETECT"              => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000004 ) }, sub { }  ],
        "FM_SRAM_CORRECTED_IP"                  => [ 04, 00,-1, "o", sub { return (0x01c000 + 0x000008 ) }, sub { return (0x00, 0x00, 0x00, 0x00); }  ],
        "FM_SRAM_CORRECTED_IM"                  => [ 04, 00,-1, "i", sub { return (0x01c000 + 0x00000c ) }, sub { return (0x00, 0x00, 0x00, 0x00); }  ],
        "FM_SRAM_UNCORRECTABLE_IP"              => [ 04, 00,-1, "o", sub { return (0x01c000 + 0x000010 ) }, sub { return (0x00, 0x00, 0x00, 0x00); }  ],
        "FM_SRAM_UNCORRECTABLE_IM"              => [ 04, 00,-1, "i", sub { return (0x01c000 + 0x000014 ) }, sub { return (0x00, 0x00, 0x00, 0x00); }  ],
        "FM_SRAM_UNCORRECTABLE_FATAL"           => [ 04, 00,-1, "i", sub { return (0x01c000 + 0x000018 ) }, sub { return (0x00, 0x00, 0x00, 0x00); }  ],
        "FM_SW_IP"                              => [ 01, 00,-1, "i", sub { return (0x01c000 + 0x00001c ) }, sub { return (0x00); }  ],
        "FM_SW_IM"                              => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x00001d ) }, sub { return (0xffffffff); }  ],
        "FM_SW_TEST_AND_SET"                    => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x00001e ) }, sub { return (0x00); }  ],
        "FM_FRAME_TIME_OUT"                     => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x00001f ) }, sub { return (0x00002fa8); }  ],
        "FM_CHIP_VERSION"                       => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000020 ) }, sub { }  ],
        "FM_PIN_STRAP_STAT"                     => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000021 ) }, sub { }  ],
        "FM_BOOT_CTRL"                          => [ 01, 00,-1, "i", sub { return (0x01c000 + 0x000022 ) }, sub { return (0x00); }  ],
        "FM_BOOT_ARGS"                          => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000023 ) }, sub { return (0x00); }  ],
        "FM_GPIO_CFG"                           => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000024 ) }, sub { return (0x00); }  ],
        "FM_GPIO_DATA"                          => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000025 ) }, sub { return (0x00); }  ],
        "FM_GPIO_IP"                            => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000026 ) }, sub { return (0x00); }  ],
        "FM_GPIO_IM"                            => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000027 ) }, sub { return (0xffffffff); }  ],
        "FM_I2C_CFG"                            => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000028 ) }, sub { return (0x02900c81); }  ],
        "FM_I2C_DATA"                           => [ 01, 01,-1, "u", sub { return (0x01c000 + 0x00002c + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 2] ],
        "FM_I2C_CTRL"                           => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000030 ) }, sub { return (0x00); }  ],
        "FM_MDIO_CFG"                           => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000031 ) }, sub { return (0x00003010); }  ],
        "FM_MDIO_DATA"                          => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000032 ) }, sub { return (0x00); }  ],
        "FM_MDIO_CTRL"                          => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000033 ) }, sub { return (0x00); }  ],
        "FM_SPI_TX_DATA"                        => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000034 ) }, sub { return (0x00); }  ],
        "FM_SPI_RX_DATA"                        => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000035 ) }, sub { }  ],
        "FM_SPI_HEADER"                         => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000036 ) }, sub { return (0x00); }  ],
        "FM_SPI_CTRL"                           => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000037 ) }, sub { return (0x0000003e); }  ],
        "FM_LED_CFG"                            => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000038 ) }, sub { return (0x0001e848); }  ],
        "FM_SCAN_CONTROL"                       => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x000039 ) }, sub { return (0x00000010); }  ],
        "FM_SCAN_CONFIG_DATA_IN"                => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00003a ) }, sub { return (0x00); }  ],
        "FM_SCAN_CHAIN_DATA_IN"                 => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00003b ) }, sub { return (0x00); }  ],
        "FM_SCAN_DATA_OUT"                      => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00003c ) }, sub { }  ],
        "FM_SCAN_STATUS"                        => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x00003d ) }, sub { return (0x00); }  ],
        "FM_ETHCLK_CFG"                         => [ 01, 01,-1, "u", sub { return (0x01c000 + 0x00003e + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1] ],
        "FM_ETHCLK_RATIO"                       => [ 01, 01,-1, "u", sub { return (0x01c000 + 0x000040 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1] ],
        "FM_PLL_CTRL"                           => [ 02, 00,-1, "s", sub { return (0x01c000 + 0x000042 ) }, sub { return (0x20841436, 0x00005560); }  ],
        "FM_DLL_CTRL"                           => [ 02, 00,-1, "s", sub { return (0x01c000 + 0x000044 ) }, sub { return (0x08011b05, 0x00); }  ],
        "FM_PLL_STAT"                           => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000046 ) }, sub { }  ],
        "FM_SWEEPER_CFG"                        => [ 05, 00,-1, "s", sub { return (0x01c000 + 0x000048 ) }, sub { return (0x00, 0x00, 0x00, 0x00, 0x00); }  ],
        "FM_S2A_EN_STATUS"                      => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000050 ) }, sub { return (0x00); }  ],
        "FM_RO_CFG"                             => [ 01, 01,-1, "u", sub { return (0x01c000 + 0x000052 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1] ],
        "FM_TESTCTRL_MOD_CFG"                   => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x000054 ) }, sub { return (0x00); }  ],
        "FM_TESTCTRL_TICK_CFG"                  => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x000055 ) }, sub { return (0x00); }  ],
        "FM_TESTCTRL_TICK_CNT"                  => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000056 ) }, sub { }  ],
        "FM_TESTCTRL_CLK_CNT"                   => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x000057 ) }, sub { }  ],
        "FM_TESTCTRL_START"                     => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x000058 ) }, sub { return (0x00); }  ],
        "FM_FUSEBOX"                            => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 127] ],
        "FM_BM_MARCH_SEQUENCE"                  => [ 04, 00,-1, "s", sub { return (0x01c000 + 0x001080 ) }, sub { return (0xfca952b9, 0xa9fca99e, 0xb16529ed, 0x7430fdb9); }  ],
        "FM_BM_GENERAL_CONFIG"                  => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001084 ) }, sub { return (0x00000080); }  ],
        "FM_BM_TXQ_HS_SEGMENTS"                 => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001085 ) }, sub { return (0x00003fff); }  ],
        "FM_BM_RXQ_PAGES"                       => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001086 ) }, sub { return (0x00000400); }  ],
        "FM_BM_MODEL_INFO"                      => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001087 ) }, sub { return (0x0000bfff); }  ],
        "FM_BM_FFU_SLICE_MASK"                  => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001088 ) }, sub { return (0x00); }  ],
        "FM_BM_VRM"                             => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001089 ) }, sub { return (0x00); }  ],
        "FM_BM_MAX_ALLOWED_REPAIRS"             => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00108a ) }, sub { return (0x00000014); }  ],
        "FM_BM_FUSEBOX_SUMMARY"                 => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00108b ) }, sub { }  ],
        "FM_BM_IP"                              => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00108c ) }, sub { return (0x00); }  ],
        "FM_BM_IM"                              => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00108d ) }, sub { return (0x00); }  ],
        "FM_BM_ENGINE_STATUS"                   => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00108e ) }, sub { }  ],
        "FM_BM_FUSEBOX_BURN_TIME"               => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00108f ) }, sub { return (0x00000271); }  ],
        "FM_BM_FUSEBOX_APPEND"                  => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001090 ) }, sub { return (0x00); }  ],
        "FM_BM_START_OPERATION"                 => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001091 ) }, sub { return (0x00); }  ],
        "FM_BM_DEBUG_STATUS_1"                  => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x001092 ) }, sub { }  ],
        "FM_BM_DEBUG_STATUS_2"                  => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x001093 ) }, sub { }  ],
        "FM_TEN_T_BIST_MARCH_CONFIG"            => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001100 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_TEN_T_BIST_GENERAL_CONFIG"          => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001104 ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_STATUS"                  => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x001105 ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_USER_CHECKER_MASK"       => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001106 ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_IP"                      => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x001107 ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_IM"                      => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001108 ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_USER_OP"                 => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001109 ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_PAIRED_READ"             => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00110a ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_CURRENT_ADDR"            => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00110b ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_START_SEQUENCE"          => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00110c ) }, sub { return (0x00); }  ],
        "FM_TEN_T_BIST_SAVED_ADDRESS"           => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00110d ) }, sub { }  ],
        "FM_TEN_T_BIST_DEBUG_STATUS_1"          => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00110e ) }, sub { }  ],
        "FM_TEN_T_BIST_DEBUG_STATUS_2"          => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00110f ) }, sub { }  ],
        "FM_TEN_T_BIST_CHAIN_LATENCY"           => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001140 ) }, sub { return (0x00000002); }  ],
        "FM_TEN_T_BIST_CHAIN_CDC"               => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001141 ) }, sub { return (0x00); }  ],
        "FM_CDP_BIST_REPAIR"                    => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001200 + 0x1 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 9], [0, 3] ],
        "FM_CDP_BIST_MARCH_CONFIG"              => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001210 + 0x1 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 3] ],
        "FM_CDP_BIST_DEFECT_MAP"                => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001214 + 0x1 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 3] ],
        "FM_CDP_BIST_GENERAL_CONFIG"            => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001218 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_STATUS"                    => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x001219 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_USER_CHECKER_MASKS"        => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x00121a + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_IP"                        => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x00121b + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_IM"                        => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x00121c + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_CLEAR_REPAIRS"             => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x00121d + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_ADD_REPAIR"                => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x00121e + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_USER_OP"                   => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x00121f + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_START_SEQUENCE"            => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001220 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_BLOCK_HALFCHUNK"           => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001221 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_SEGMENT_COUNT"             => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x001222 + 0x80 * $_[0]) }, sub { } , [0, 3] ],
        "FM_CDP_BIST_NEXT_SEGMENT"              => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001223 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 3] ],
        "FM_CDP_BIST_NEXT_NEW_REPAIR"           => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x001224 + 0x80 * $_[0]) }, sub { } , [0, 3] ],
        "FM_CDP_BIST_SAVED_ADDRESS"             => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x001225 + 0x80 * $_[0]) }, sub { } , [0, 3] ],
        "FM_CDP_BIST_DEBUG_STATUS_1"            => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x001226 + 0x80 * $_[0]) }, sub { } , [0, 3] ],
        "FM_CDP_BIST_DEBUG_STATUS_2"            => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x001227 + 0x80 * $_[0]) }, sub { } , [0, 3] ],
        "FM_CDP_BIST_CHAIN_GENERAL_CONFIG"      => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001240 + 0x20 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 1], [0, 3] ],
        "FM_CDP_BIST_CHAIN_LATENCY"             => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001241 + 0x20 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00000002); } , [0, 1], [0, 3] ],
        "FM_CDP_BIST_CHAIN_CDC"                 => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001242 + 0x20 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 1], [0, 3] ],
        "FM_SPDP_BIST_MARCH_CONFIG"             => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001400 + 0x1 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 4] ],
        "FM_SPDP_BIST_GENERAL_CONFIG"           => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001404 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_SPDP_BIST_STATUS"                   => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x001405 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_SPDP_BIST_USER_CHECKER_MASKS"       => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001406 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_SPDP_BIST_IP"                       => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x001407 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_SPDP_BIST_IM"                       => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001408 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_SPDP_BIST_MAX_ADDR"                 => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001409 + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_SPDP_BIST_USER_OP"                  => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x00140a + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_SPDP_BIST_START_SEQUENCE"           => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x00140b + 0x80 * $_[0]) }, sub { return (0x00); } , [0, 4] ],
        "FM_SPDP_BIST_SAVED_ADDRESS"            => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x00140c + 0x80 * $_[0]) }, sub { } , [0, 4] ],
        "FM_SPDP_BIST_DEBUG_STATUS_1"           => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x00140d + 0x80 * $_[0]) }, sub { } , [0, 4] ],
        "FM_SPDP_BIST_DEBUG_STATUS_2"           => [ 01, 01,-1, "o", sub { return (0x01c000 + 0x00140e + 0x80 * $_[0]) }, sub { } , [0, 4] ],
        "FM_SPDP_BIST_CHAIN_GENERAL_CONFIG"     => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001440 + 0x20 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 1], [0, 4] ],
        "FM_SPDP_BIST_CHAIN_LATENCY"            => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001441 + 0x20 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00000002); } , [0, 1], [0, 4] ],
        "FM_SPDP_BIST_CHAIN_CDC"                => [ 01, 02,-1, "s", sub { return (0x01c000 + 0x001442 + 0x20 * $_[0]+ 0x80 * $_[1]) }, sub { return (0x00); } , [0, 1], [0, 4] ],
        "FM_SRBM_REPAIR"                        => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x001700 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 7] ],
        "FM_SRBM_MARCH_SEQUENCE"                => [ 04, 00,-1, "s", sub { return (0x01c000 + 0x001708 ) }, sub { return (0xfca952b9, 0xa9fca99e, 0xb16529ed, 0x7430fdb9); }  ],
        "FM_SRBM_GENERAL_CONFIG"                => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00170c ) }, sub { return (0x00000200); }  ],
        "FM_SRBM_STATUS"                        => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00170d ) }, sub { }  ],
        "FM_SRBM_IP"                            => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x00170e ) }, sub { return (0x00); }  ],
        "FM_SRBM_IM"                            => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x00170f ) }, sub { return (0x00); }  ],
        "FM_SRBM_CLEAR_REPAIRS"                 => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001710 ) }, sub { return (0x00); }  ],
        "FM_SRBM_ADD_REPAIR"                    => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001711 ) }, sub { return (0x00); }  ],
        "FM_SRBM_NEXT_NEW_REPAIR"               => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x001712 ) }, sub { }  ],
        "FM_SRBM_LAST_NR_DEFECT"                => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x001713 ) }, sub { return (0x00); }  ],
        "FM_CRM_DATA"                           => [ 01, 02,-1, "u", sub { return (0x01c000 + 0x002000 + 0x1 * $_[0]+ 0x2 * $_[1]) }, sub { return (0x00); } , [0, 1], [0, 2047] ],
        "FM_CRM_CTRL"                           => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x003000 ) }, sub { return (0x00); }  ],
        "FM_CRM_STATUS"                         => [ 01, 00,-1, "o", sub { return (0x01c000 + 0x003001 ) }, sub { return (0x00); }  ],
        "FM_CRM_TIME"                           => [ 01, 00,-1, "s", sub { return (0x01c000 + 0x003002 ) }, sub { return (0x00); }  ],
        "FM_CRM_IP"                             => [ 02, 00,-1, "o", sub { return (0x01c000 + 0x003004 ) }, sub { return (0x00, 0x00); }  ],
        "FM_CRM_IM"                             => [ 02, 00,-1, "s", sub { return (0x01c000 + 0x003006 ) }, sub { return (0xffffffff, 0x00); }  ],
        "FM_CRM_COMMAND"                        => [ 02, 01,-1, "s", sub { return (0x01c000 + 0x003080 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 63] ],
        "FM_CRM_REGISTER"                       => [ 02, 01,-1, "s", sub { return (0x01c000 + 0x003100 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 63] ],
        "FM_CRM_PERIOD"                         => [ 02, 01,-1, "s", sub { return (0x01c000 + 0x003180 + 0x2 * $_[0]) }, sub { return (0x00, 0x00); } , [0, 63] ],
        "FM_CRM_PARAM"                          => [ 01, 01,-1, "s", sub { return (0x01c000 + 0x003200 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 63] ],

# Block: MGMT1, base address = 0x000000 (14 registers) 
        "FM_LCI_CFG"                            => [ 01, 00,-1, "u", sub { return (0x000000 + 0x000000 ) }, sub { return (0x00); }  ],
        "FM_LCI_RX_FIFO"                        => [ 01, 00,-1, "o", sub { return (0x000000 + 0x000001 ) }, sub { }  ],
        "FM_LCI_TX_FIFO"                        => [ 01, 00,-1, "u", sub { return (0x000000 + 0x000002 ) }, sub { return (0x00); }  ],
        "FM_LCI_IP"                             => [ 01, 00,-1, "i", sub { return (0x000000 + 0x000003 ) }, sub { return (0x00); }  ],
        "FM_LCI_IM"                             => [ 01, 00,-1, "u", sub { return (0x000000 + 0x000004 ) }, sub { return (0x00000003); }  ],
        "FM_LCI_STATUS"                         => [ 01, 00,-1, "o", sub { return (0x000000 + 0x000005 ) }, sub { }  ],
        "FM_FATAL_CODE"                         => [ 01, 00,-1, "s", sub { return (0x000000 + 0x000006 ) }, sub { return (0x00); }  ],
        "FM_LAST_FATAL_CODE"                    => [ 01, 00,-1, "i", sub { return (0x000000 + 0x000007 ) }, sub { return (0x00); }  ],
        "FM_FATAL_COUNT"                        => [ 01, 00,-1, "i", sub { return (0x000000 + 0x000008 ) }, sub { return (0x00); }  ],
        "FM_SOFT_RESET"                         => [ 01, 00,-1, "s", sub { return (0x000000 + 0x000009 ) }, sub { return (0x0000001f); }  ],
        "FM_RESET_CFG"                          => [ 01, 00,-1, "u", sub { return (0x000000 + 0x00000a ) }, sub { return (0x00004010); }  ],
        "FM_WATCHDOG_CFG"                       => [ 01, 00,-1, "u", sub { return (0x000000 + 0x00000b ) }, sub { return (0x00); }  ],
        "FM_MGMT_SCRATCH"                       => [ 01, 01,-1, "u", sub { return (0x000000 + 0x00000c + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1] ],
        "FM_VITAL_PRODUCT_DATA"                 => [ 01, 00,-1, "o", sub { return (0x000000 + 0x000304 ) }, sub { }  ],

# Block: PCIE, base address = 0x001000 (85 registers) 
        "FM_PCI_CFG_ID"                         => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000000 ) }, sub { }  ],
        "FM_PCI_CFG_CMD"                        => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000001 ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_1"                          => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000002 ) }, sub { }  ],
        "FM_PCI_CFG_2"                          => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000003 ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_BAR0"                       => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000004 ) }, sub { }  ],
        "FM_PCI_CFG_BAR1"                       => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000005 ) }, sub { }  ],
        "FM_PCI_CFG_BAR2"                       => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000006 ) }, sub { }  ],
        "FM_PCI_CFG_BAR3"                       => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000007 ) }, sub { }  ],
        "FM_PCI_CFG_BAR4"                       => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000008 ) }, sub { }  ],
        "FM_PCI_CFG_BAR5"                       => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000009 ) }, sub { }  ],
        "FM_PCI_CFG_CARDBUS"                    => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00000a ) }, sub { }  ],
        "FM_PCI_CFG_SUBID"                      => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00000b ) }, sub { }  ],
        "FM_PCI_CFG_EXP_ROM"                    => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00000c ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_CAP_PTR"                    => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00000d ) }, sub { }  ],
        "FM_PCI_CFG_RSVD"                       => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00000e ) }, sub { }  ],
        "FM_PCI_CFG_INT"                        => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00000f ) }, sub { return (0x000000ff); }  ],
        "FM_PCI_CFG_PM_CAP"                     => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000010 ) }, sub { return (0xc9c30000); }  ],
        "FM_PCI_CFG_PM_CTRL"                    => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000011 ) }, sub { return (0x00008000); }  ],
        "FM_PCI_CFG_MSI_CAP"                    => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000014 ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_MSI_ADDR_LO"                => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000015 ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_MSI_ADDR_HI"                => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000016 ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_MSI_DATA"                   => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000017 ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_MSI_MASK"                   => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000018 ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_MSI_PENDING"                => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000019 ) }, sub { }  ],
        "FM_PCI_CFG_PCIE_CAP"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00001c ) }, sub { }  ],
        "FM_PCI_CFG_PCI_DEV_CAP"                => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00001d ) }, sub { return (0x00008701); }  ],
        "FM_PCI_CFG_PCIE_DEV"                   => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00001e ) }, sub { return (0x00002010); }  ],
        "FM_PCI_CFG_PCI_LINK_CAP"               => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00001f ) }, sub { }  ],
        "FM_PCI_CFG_PCI_LINK_CTRL"              => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000020 ) }, sub { return (0x10110000); }  ],
        "FM_PCI_CFG_DEV_CAP2"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000025 ) }, sub { }  ],
        "FM_PCI_CFG_DEV_CTRL2"                  => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000026 ) }, sub { return (0x00); }  ],
        "FM_PCI_CFG_PCI_LINK_CAP2"              => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000027 ) }, sub { }  ],
        "FM_PCI_CFG_PCI_LINK_CTRL2"             => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000028 ) }, sub { return (0x00000002); }  ],
        "FM_PCI_ENDIANISM"                      => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000400 ) }, sub { return (0x00); }  ],
        "FM_PCI_COMMAND"                        => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000401 ) }, sub { return (0x00); }  ],
        "FM_PCI_STATUS"                         => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000402 ) }, sub { return (0x00); }  ],
        "FM_PCI_COALESCING"                     => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000403 ) }, sub { return (0x00); }  ],
        "FM_PCI_RX_BD_BASE"                     => [ 02, 00,-1, "s", sub { return (0x001000 + 0x000404 ) }, sub { return (0x00, 0x00); }  ],
        "FM_PCI_RX_BD_END"                      => [ 02, 00,-1, "s", sub { return (0x001000 + 0x000406 ) }, sub { return (0x00, 0x00); }  ],
        "FM_PCI_TX_BD_BASE"                     => [ 02, 00,-1, "s", sub { return (0x001000 + 0x000408 ) }, sub { return (0x00, 0x00); }  ],
        "FM_PCI_TX_BD_END"                      => [ 02, 00,-1, "s", sub { return (0x001000 + 0x00040a ) }, sub { return (0x00, 0x00); }  ],
        "FM_PCI_IP"                             => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00040c ) }, sub { return (0x00); }  ],
        "FM_PCI_IM"                             => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00040d ) }, sub { return (0xffffffff); }  ],
        "FM_PCI_CURRENT_TX_DATA_PTR"            => [ 02, 00,-1, "s", sub { return (0x001000 + 0x00040e ) }, sub { return (0x00, 0x00); }  ],
        "FM_PCI_CURRENT_RX_DATA_PTR"            => [ 02, 00,-1, "s", sub { return (0x001000 + 0x000410 ) }, sub { return (0x00, 0x00); }  ],
        "FM_PCI_CURRENT_TX_BD_PTR"              => [ 02, 00,-1, "s", sub { return (0x001000 + 0x000412 ) }, sub { return (0x00, 0x00); }  ],
        "FM_PCI_CURRENT_RX_BD_PTR"              => [ 02, 00,-1, "s", sub { return (0x001000 + 0x000414 ) }, sub { return (0x00, 0x00); }  ],
        "FM_PCI_TX_FRAME_LEN"                   => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000416 ) }, sub { return (0x00404000); }  ],
        "FM_PCI_SIZE"                           => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000417 ) }, sub { return (0x0000071f); }  ],
        "FM_PCI_DMA_CFG"                        => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000418 ) }, sub { return (0x00000075); }  ],
        "FM_PCI_FRAME_TIMEOUT"                  => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000419 ) }, sub { return (0x003ffff0); }  ],
        "FM_PCI_STAT_COUNTER"                   => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00041a ) }, sub { return (0x00); }  ],
        "FM_PCI_STAT_NUM_PKTS"                  => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00041b ) }, sub { return (0x00); }  ],
        "FM_PCI_DEBUG"                          => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00041c ) }, sub { return (0x00000003); }  ],
        "FM_PCI_CORE_CTRL_1"                    => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00041d ) }, sub { return (0x0000c01f); }  ],
        "FM_PCI_CORE_CTRL_2"                    => [ 01, 00,-1, "s", sub { return (0x001000 + 0x00041e ) }, sub { return (0x00); }  ],
        "FM_PCI_CORE_DEBUG_1"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00041f ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_2"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000420 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_3"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000421 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_4"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000422 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_5"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000423 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_6"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000424 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_7"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000425 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_8"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000426 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_9"                   => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000427 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_10"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000428 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_11"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000429 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_12"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00042a ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_13"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00042b ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_14"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00042c ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_15"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00042d ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_16"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00042e ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_17"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00042f ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_18"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000430 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_19"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000431 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_20"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000432 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_21"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000433 ) }, sub { }  ],
        "FM_PCI_CORE_DEBUG_22"                  => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000434 ) }, sub { }  ],
        "FM_PCI_SERDES_CTRL_1"                  => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000435 ) }, sub { return (0x0f121f34); }  ],
        "FM_PCI_SERDES_CTRL_2"                  => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000436 ) }, sub { return (0x00000008); }  ],
        "FM_PCI_SERDES_CTRL_3"                  => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000437 ) }, sub { return (0x1401aaaa); }  ],
        "FM_PCI_SERDES_CTRL_4"                  => [ 01, 00,-1, "s", sub { return (0x001000 + 0x000438 ) }, sub { return (0x00006000); }  ],
        "FM_PCI_SERDES_DEBUG_1"                 => [ 01, 00,-1, "o", sub { return (0x001000 + 0x000439 ) }, sub { }  ],
        "FM_PCI_SERDES_DEBUG_2"                 => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00043a ) }, sub { }  ],
        "FM_PCI_SERDES_DEBUG_3"                 => [ 01, 00,-1, "o", sub { return (0x001000 + 0x00043b ) }, sub { }  ],

# Block: JSS, base address = 0x00f000 (12 registers) 
        "FM_SBUS_CFG"                           => [ 01, 00,-1, "s", sub { return (0x00f000 + 0x000000 ) }, sub { return (0x00000001); }  ],
        "FM_SBUS_COMMAND"                       => [ 01, 00,-1, "s", sub { return (0x00f000 + 0x000001 ) }, sub { return (0x00); }  ],
        "FM_SBUS_REQUEST"                       => [ 01, 00,-1, "s", sub { return (0x00f000 + 0x000002 ) }, sub { return (0x00); }  ],
        "FM_SBUS_RESPONSE"                      => [ 01, 00,-1, "o", sub { return (0x00f000 + 0x000003 ) }, sub { }  ],
        "FM_SBUS_SPICO"                         => [ 01, 00,-1, "s", sub { return (0x00f000 + 0x000004 ) }, sub { return (0x00000001); }  ],
        "FM_SBUS_IP"                            => [ 01, 00,-1, "o", sub { return (0x00f000 + 0x000005 ) }, sub { return (0x00); }  ],
        "FM_SBUS_IM"                            => [ 01, 00,-1, "s", sub { return (0x00f000 + 0x000006 ) }, sub { return (0x00); }  ],
        "FM_SPICO_BIST"                         => [ 01, 00,-1, "s", sub { return (0x00f000 + 0x000007 ) }, sub { return (0x00); }  ],
        "FM_TAP_FSM_STATE"                      => [ 01, 00,-1, "o", sub { return (0x00f000 + 0x000008 ) }, sub { }  ],
        "FM_TAP_EFUSE_CFG"                      => [ 01, 00,-1, "s", sub { return (0x00f000 + 0x000009 ) }, sub { return (0x00); }  ],
        "FM_TAP_EFUSE"                          => [ 04, 00,-1, "o", sub { return (0x00f000 + 0x00000c ) }, sub { }  ],
        "FM_SSCHED_TICK_CFG"                    => [ 01, 00,-1, "s", sub { return (0x00f000 + 0x000010 ) }, sub { return (0x00); }  ],

# Block: FC_BEM, base address = 0x028000 (8 registers) 
        "FM_FC_MRL_MGMT_CYCLES"                 => [ 01, 01,-1, "s", sub { return (0x028000 + 0x000000 + 0x1 * $_[0]) }, sub { return (0x00008208); } , [0, 15] ],
        "FM_FC_MRL_SWEEP_CYCLES"                => [ 01, 01,-1, "s", sub { return (0x028000 + 0x000010 + 0x1 * $_[0]) }, sub { return (0x00000008); } , [0, 2] ],
        "FM_FC_MRL_SWEEP_PERIOD"                => [ 01, 00,-1, "s", sub { return (0x028000 + 0x000014 ) }, sub { return (0x00000050); }  ],
        "FM_FC_MRL_UNROLL_ITER"                 => [ 01, 00,-1, "s", sub { return (0x028000 + 0x000015 ) }, sub { return (0x00); }  ],
        "FM_FC_MRL_TOKEN_LIMIT"                 => [ 01, 01,-1, "s", sub { return (0x028000 + 0x000018 + 0x1 * $_[0]) }, sub { return (0x00000010); } , [0, 5] ],
        "FM_FC_MRL_FC_TOKEN_LIMIT"              => [ 01, 00,-1, "s", sub { return (0x028000 + 0x000020 ) }, sub { return (0x000003ff); }  ],
        "FM_FC_MRL_ALIGN_TX_STATS"              => [ 01, 00,-1, "s", sub { return (0x028000 + 0x000021 ) }, sub { return (0x00); }  ],
        "FM_FC_MRL_RATE_LIMITER"                => [ 01, 00,-1, "u", sub { return (0x028000 + 0x000022 ) }, sub { return (0x00000128); }  ],
);


#################################################################################
#################################################################################
### Executable code starts here
#################################################################################
#################################################################################


# parse commandline options (pass 1)
getAppOpts(1);

# configure target chip specific parameters
config_app();

# load the register dump file
if ($altaFIBMconfig != 0) {
    load_reg_dump();
}

# generate the target specific configuration sequence
configure_chip();

# create the output file with the specified format
generate_output_file();

# report eeprom use and return.
my $byte_count = 0 + @eeprom_contents;
printf STDERR "\n# Used %.2f KB of EEPROM space\n", $byte_count/1024;
if ($chip_is == ALTA) {
    printf STDERR "# Include SPICO code: %s\n",($altaFIBMconfig && $main::opt_L)?"Enabled":"Disabled";
    printf STDERR "# Initialize Memory via BIST: %s\n",($altaFIBMconfig & $altaInitMemBist)?"Enabled":"Disabled";
    printf STDERR "# Image %d base address:    0x%x\n", $eeprom_target_image,
                                                        $alta_image_offsets[$eeprom_target_image];
    printf STDERR "# Highest EEPROM address:  0x%x\n", $eepromHighAddr;
}
exit;


#################################################################################
#################################################################################
#################################################################################

##@method eeprom_insert_word()
#
# @brief        insert a word command into the eeprom buffer. 
#
# @param[in]    $word: data word to be inserted
#
sub eeprom_insert_word
{
    my ($word) = @_;

    if ($chip_is != ALTA) {
        die "EEPROM size exceeded" if $eeprom_addr >= ($eeprom_size_kb*1024);
    }
    my @rv = ();

    for (my $i=3; $i>=0; $i--) {
        my $byte = ($word>>(8*$i))&0xFF;
        push @eeprom_contents, $byte;
        $eeprom_addr++;
        push @rv, sprintf("%02x", $byte);
    }
    return @rv;
}

##@method eeprom_insert_write_register32_command
#
# @brief        insert a 'WRITE 32 bit register' command into the
#               eeprom buffer.
#               This method only write single-word registers.
#               Alta is not supported by this method, use
#               eeprom_insert_alta_write_register_command() instead.
#
# @param[in]    $addr: register address
#
# @param[in]    $data: register value (single word)
#
sub eeprom_insert_write_register32_command
{
    my ($addr, $data) = @_;
    my $cmdword;

    # This function cannot be used with Alta, 
    # use eeprom_insert_alta_write_register_command() instead.
    die "Alta not supported by this function" if ($chip_is == ALTA);
    if ($chip_is == BALI) {
        $cmdword = ($eeprom_command{"write"}<<24) | ($addr & 0xFFFFFF);
    } else {
        # this is for TAHOE
        $cmdword = ($eeprom_command{"write"}<<24) | (($addr & 0x3FFFFF)<<2);
    }

    my @cmd_bytes = eeprom_insert_word($cmdword);
    push @cmd_bytes, eeprom_insert_word($data);

    log_message(sprintf("WRITE 0x%x 0x%x (@cmd_bytes)", $addr, $data));

    if ($main::opt_t or $main::opt_T) {
        printf "fp_wr(0x%x, 0x%x);\n", $addr, $data;
    }
    delete $not_handled_regs{$addr};
}

##@method eeprom_insert_alta_write_register_command
#
# @brief        insert an alta 'write-register' command into the
#               eeprom buffer. This method supports multiword write commands.
#
# @param[in]    $addr: register base address
#
# @param[in]    @dataw: list of data words, MSW first.
# 
sub eeprom_insert_alta_write_register_command
{
    my ($addr, @dataw) = @_;

    my $wordcount = @dataw + 0;

    die "EEPROM command only supported by ALTA" if ($chip_is != ALTA);
    die "ERROR in WRITE command: Invalid data word list" if ($wordcount == 0 || $wordcount > 14);

    # Option field = nDataWords - 1
    my $cmdword = (($wordcount-1) << 28) |
                  ($eeprom_command{"write"}<<24) |
                  ($addr & 0xFFFFFF);

    my @cmd_bytes = eeprom_insert_word($cmdword);
    foreach my $dataword (@dataw) {
        push @cmd_bytes, eeprom_insert_word($dataword);
    }
    my $logMessg = sprintf("WRITE 0x%x", $addr);
    foreach my $dataword (@dataw) {
        $logMessg .= sprintf("0x%x ",$dataword);
    }
    $logMessg .= "(@cmd_bytes)";

    log_message($logMessg);
}


##@method eeprom_insert_boot_command()
#
# @brief        insert a 'boot' command into the eeprom buffer.
#
# @param[in]    cmdval: boot command.
#
sub eeprom_insert_boot_command
{
    my ($cmdval) = @_;

    my $cmdword = ($eeprom_command{"boot"}<<24) | ($cmdval & $eepromBootCmmdMask);
    my @cmd_bytes = eeprom_insert_word($cmdword);

    log_message ("BOOTCMD $cmdval (@cmd_bytes)\n");
}

##@method eeprom_insert_delay_command()
#
# @brief        insert a 'wait' command into the eeprom buffer.
#
# @param[in]    $usec: delay in usec.
#
sub eeprom_insert_delay_command
{
    my ($usec) = @_;

    # EBI clock period in microseconds.
    my $ebi_period_us = ($bus_clock_period_ns*0.001);

    my $delay_period_us;
    my $delayval;

    if ($chip_is == BALI) {
        # On bali, delay durations are expressed in terms of the LSM clock.
        # EBI max frequency on bali is 133MHz => 7.52ns period (.00752 us)
        # LSM clock period is .00752us * 2 = .01515 us.
        # Max SPI cycles = 0xFFFFFF = 16777215
        # Max usec (133MHz) = 16777215*.01515 = 254,174 = .26 sec

        $delay_period_us = $ebi_period_us*2;
        $delayval = $usec/$delay_period_us;
    } elsif ($chip_is == TAHOE) {
        # On tahoe, delay durations are expressed in terms of the SPI clock.
        # EBI max frequency on tahoe is <100MHz => 10ns period (.010 us)
        # Default SPI clock period is .010us * 2 * (25+1) = 0.52 us.
        # Max SPI cycles = 0xFFFFFF = 16777215
        # Max usec (33MHz, div = 0x19) = 16777215*0.52 = 8,724,151 = 8.7 sec

        $delay_period_us = $ebi_period_us*2*($spi_div_cfg+1);
        $delayval = $usec/$delay_period_us;
    } else {
        # On ALTA, delay durations are expressed in PCIE_REFCLK clock cycles.
        # PCIE_REFCLK clock period is 8 ns.
        # Max delay[us] = 16777215 * 8 /1000 = 134217us = 0.134 sec
        $delay_period_us = $pcie_refclk_period / 1000;
        $delayval = ($usec * 1000) / $pcie_refclk_period;
    }

    # check that required delay is inside the the allowed range
    my $max_usec = 0xFFFFFF * $delay_period_us;

    if ($usec > $max_usec) {
        my $req_sec_str = sprintf "%.3f", $usec/1e6;
        my $max_sec_str = sprintf "%.3f", $max_usec/1e6;
        die "Delay $req_sec_str seconds too long (max $max_sec_str seconds)";
    }

    my $cmdword = ($eeprom_command{"wait"}<<24) | ($delayval & 0xFFFFFF);
    my @cmd_bytes = eeprom_insert_word($cmdword);

    log_message ("DELAY $usec usec (@cmd_bytes)");

    if ($main::opt_t or $main::opt_T) {
        my $sec = ($usec+1e5-1)/1e6;
        printf "select(undef, undef, undef, %.1f);\n", $sec;
    }
}

##@method eeprom_insert_poll_command()
#
# @brief        insert a 'poll' command into the eeprom buffer.
#
# @param[in]    $address: register address to read.
#
# @param[in]    $data:    reference value to compare with
#                         ([register]&[mask]
#
# @param[in]    $maxRetries: max number of polls [0..0xffff]
#
# @param[in]    $retryInterval: poll interval (in us) [0..0xff]
#
# @param[in]    $jumpAddress: [0..0xffffff]
#
#
sub eeprom_insert_poll_command
{
    my ($address, $data, $mask, $maxRetries, $retryUsec, $jumpAddress) = @_;

    die "EEPROM command only supported by ALTA" if ($chip_is != ALTA);

    my $cmdword = ($eeprom_command{"poll"}<<24) | ($address & 0xFFFFFF);
    
    # Convert the time [us] into PCIE_REFCLOCK clock cycles
    my $retryInterval = ($retryUsec * 1000) / $pcie_refclk_period;

    my @cmd_bytes = eeprom_insert_word($cmdword);
    push @cmd_bytes, eeprom_insert_word($data);
    push @cmd_bytes, eeprom_insert_word($mask);
    push @cmd_bytes, eeprom_insert_word( (($maxRetries & 0xffff)<<16) | ($retryInterval & 0xffff));
    push @cmd_bytes, eeprom_insert_word( $jumpAddress & 0xffffff );

    log_message(sprintf("POLL 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x (@cmd_bytes)", 
                        $address, $data, $mask, $maxRetries, $retryInterval, $jumpAddress));
}


##@method eeprom_insert_finish_command()
#
# @brief        insert a 'end' command into the eeprom buffer.
#               (In fact, this method inserts 2 'ends' sequences
#               into the eeprom buffer).
#
# @param[in]    $usec: delay in usec.
#
sub eeprom_insert_finish_command
{
    my $cmdword = 0xFFFFFFFF;
    my @cmd_bytes = eeprom_insert_word($cmdword);
    push @cmd_bytes, eeprom_insert_word($cmdword);

    log_message ("FINISH (@cmd_bytes)");
}

##@method       log_message()
#
# @brief        if the -s option has been included, send 
#               the log message to STDERR. 
#               A '\n' character is appended to the message.
#
# @param[in]    $msg: log message.
#
sub log_message
{
    my ($msg) = @_;

    if ($main::opt_s) {
        print STDERR "${msg}\n";
    }
}


##@method       describe_stage()
#
# @brief        if the -s command line option has been included,
#               send the process avance information to STDERR. 
#               A '\n' character is appended to the message.
#
# @param[in]    $msg: message
#
sub describe_stage
{
    my ($msg) = @_;

    log_message("# ${msg}");

    if ($main::opt_t or $main::opt_T) {
        print $msg;
    }
}

##@method       generate_output_file()
#
# @brief        generate the eeprom image file using the format
#               specified in the command line.
#               Currnet possiblities are:
#               - xml format
#               - intel-hex format
#               - TE commands
#               - standard-byte-format
#
#
sub generate_output_file
{
    my $blockaddr = 0;

    if ($chip_is == ALTA) {
        $blockaddr = $alta_image_offsets[$eeprom_target_image];
    }

    if (@eeprom_header => 0)
    {
        if ($main::opt_x) {
            output_xml_file(\@eeprom_contents, $blockaddr, INCLUDE_OUTPUT_FILE_START_END);
        }
        elsif ($main::opt_i) {
            output_intel_hex_format(\@eeprom_contents, $blockaddr, INCLUDE_OUTPUT_FILE_END);
        }
        elsif ($main::opt_t or $main::opt_T) {
            # TE commands.  Already done, so nothing to do here.
        }
        else {
            output_standard_byte_format(\@eeprom_contents, $blockaddr);
        }
    }
    else
    {
        if ($main::opt_x) {
            output_xml_file(\@eeprom_header, 0,INCLUDE_OUTPUT_FILE_START);
            output_xml_file(\@eeprom_contents,$blockaddr,INCLUDE_OUTPUT_FILE_END);
        }
        elsif ($main::opt_i) {
            output_intel_hex_format(\@eeprom_header, 0);
            output_intel_hex_format(\@eeprom_contents,$blockaddr, INCLUDE_OUTPUT_FILE_END);
        }
        elsif ($main::opt_t or $main::opt_T) {
            # TE commands.  Already done, so nothing to do here.
        }
        else {
            output_standard_byte_format(\@eeprom_header, 0);
            output_standard_byte_format(\@eeprom_contents, $blockaddr);
        }
    }
}

##@method       output_xml_file()
#
# @brief        Generates the eeprom image file in xml format. 
#
# @param[in]    $bufferRef: reference to the data buffer.
# 
# @param[in]    $blockaddr: eeprom starting address. This is an optional
#                parameter. The default value is 0x00.
#                
# @param[in]    $options: allows to control the generation of Start and End file
#                section in order to allow the concatenation of several data
#                segments in the output.
#
sub output_xml_file
{
    my ($bufferRef, $blockaddr, $options) = @_;

    if ($options == INCLUDE_OUTPUT_FILE_START ||
        $options == INCLUDE_OUTPUT_FILE_START_END) {
    print <<EOF;
<aardvark>

<configure i2c="1" spi="1" gpio="0" tpower="1" pullups="0" />

<spi_config polarity="falling/rising" phase="setup/sample" bitorder="msb" ss="active_low" />

<spi_bitrate khz="125" />

EOF
    }

    my $blksize = 64;
    if (defined $main::opt_b) {
        $blksize = $main::opt_b;
    }
    my $blkmax  = $blksize-1;

    my $start_index = 0;

    if (!defined $blockaddr) {
        $blockaddr = 0;
    }
    while ($start_index < @{$bufferRef}) {

        my $end_index = $start_index + $blkmax;
        if ($end_index > $#{$bufferRef}) {
            $end_index = $#{$bufferRef};
        }

        my @block = @{$bufferRef}[$start_index..$end_index];
        $start_index = $end_index + 1;

        unshift @block, $blockaddr&0xFF; # LSB addr
        unshift @block, $blockaddr>>8;   # MSB addr
        unshift @block, 2; # write command

        my $len = @block;

        my @hex = map { sprintf "%02x", $_; } @block;

        # Write Enable
        print "<spi_write count=\"1\" radix=\"16\">06</spi_write>\n";
        # Write 64 bytes
        print "<spi_write count=\"$len\" radix=\"16\">@hex</spi_write>\n\n";

        $blockaddr += $blksize;
    }

    if ($options == INCLUDE_OUTPUT_FILE_END ||
        $options == INCLUDE_OUTPUT_FILE_START_END) {
    # Read back the first few bytes and turn off device power.
    print <<EOF;

<spi_write count="17" radix="16">03 00 00 00 00 00 00 00 00 00 00 00 00 00</spi_write>

<configure i2c="1" spi="1" gpio="0" tpower="0" pullups="0" />

</aardvark>

EOF
    }
}

##@method       output_standard_byte_format()
#
# @brief        Generates the eeprom image file in standard byte format. 
#
# @param[in]    $bufferRef: reference to the data buffer.
#
# @param[in]    $blockaddr: eeprom starting address. This is an optional
#                parameter. The default value is 0x00.
# 
sub output_standard_byte_format
{
    my ($bufferRef, $blockaddr) = @_;

    my $blksize = 16;
    my $blkmax  = $blksize-1;

    my $start_index = 0;

    if (!defined $blockaddr) {
        $blockaddr = 0;
    }
    while ($start_index < @{$bufferRef}) {

        my $end_index = $start_index + $blkmax;
        if ($end_index > $#{$bufferRef}) {
            $end_index = $#{$bufferRef};
        }

        my @block = @{$bufferRef}[$start_index..$end_index];
        $start_index = $end_index + 1;

        my @hex = map { sprintf "%02x", $_; } @block;

        printf "%04x: @hex\n", $blockaddr;

        $blockaddr += $blksize;
    }
}

##@method       output_intel_hex_format()
#
# @brief        Generates the eeprom image file in intelhex 32 bit format. The 
#               generation of the 'End of File' record is controlled by an option.
#               This allows the concatenation of different segments in the output
#               file.
#
# @param[in]    $bufferRef: reference to the data buffer.
# 
# @param[in]    $blockaddr: eeprom starting address. This is an optional
#                parameter. The default value is 0x00.
# 
# @param[in]    $options: if it is defined and equal to INCLUDE_OUTPUT_FILE_END,
#                an 'End of File' record will be appended to the output.
sub output_intel_hex_format
{
    my ($bufferRef, $blockaddr, $options) = @_;

    my $blksize = 16;
    my $blkmax  = $blksize-1;

    my $start_index = 0;

    if (!defined $blockaddr) {
        $blockaddr = 0;
    }
    while ($start_index < @{$bufferRef}) {

        my $end_index = $start_index + $blkmax;
        if ($end_index > $#{$bufferRef}) {
            $end_index = $#{$bufferRef};
        }

        my @block = @{$bufferRef}[$start_index..$end_index];
        $start_index = $end_index + 1;

        my $count = scalar @block;
        # This is for HexIntel-32
        # Check if the current ULBA is OK; otherwise an 'Extended Linear Address Record'
        # must be generated
        if ((($blockaddr >> 16) & 0xffff) != $eepromULBA) {
            # update the Upper Linear Base Address
            $eepromULBA = ($blockaddr >> 16) & 0xffff;
            # generate an Extended Linear Address Record (record type 4)
            # See Intel Hexadecimal Object File Format Specification, Revision A
            my $extRecordCksum = 6 + (($eepromULBA >> 8) & 0xff) + ($eepromULBA & 0xff);
            $extRecordCksum = (-$extRecordCksum) & 0xff;
            printf ":02000004%4.4X%02X\n",$eepromULBA,$extRecordCksum;
        }

        # Byte count, two hex digits.
        # Address, four hex digits.
        # Record type, two hex digits.
        # Data, a sequence of n bytes.
        my @line = ($count, ($blockaddr>>8)&0xFF, $blockaddr&0xFF, 0, @block);
        my @hex = map { sprintf "%02X", $_; } @line;
        my $hexstr = join '', @hex;

        my $sum = 0;
        for my $val (@line) { $sum += $val; }

        my $csum = ((($sum&0xFF)^0xFF)+1)&0xFF;

        die "Bad checksum" if (($sum+$csum)&0xFF) != 0;

        # Start code (":")
        # Body as above (count, addr, type, data)
        # Checksum, two hex digits.
        printf ":${hexstr}%02X\n", $csum;

        $blockaddr += $blksize;
    }
    if ($options == INCLUDE_OUTPUT_FILE_END) {
        print ":00000001FF\n";
    }
    if ($blockaddr > $eepromHighAddr) {
        $eepromHighAddr = $blockaddr;
    }
}

##@method       lookup_addr_and_default()
#
# @brief        Looks for the specified register name in the active chip hash
#               table and, if it is found, returns the register base address and
#               default(s) value(s).
#
# @param[in]    $name: register name.
# 
# @param[in]    $indices: register indices (optional parameter)
# 
# @return       base address and default value(s)
sub lookup_addr_and_default
{
    my ($name, @indicesRev) = @_;

    my $entry = $regs_hash{$name};
    die "Found no entry for reg $name" if not defined $entry;

    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

    my @indices = reverse(@indicesRev);
    my $ic = @indices + 0;
    die "Bad index count ($ic) for $name" if $ic != $indexcount;

    my $bi = 0;
    foreach my $index (@indices) {
        my ($min, $max) = @{$boundlist[$bi]};
        if ($index < $min or $index > $max) {
            die "Index $bi value $index is out of range [$min,$max] for $name";
        }
        $bi++;
    }

    my $addr    = &$addrsub(@indices);
    my @default = &$defaultsub(@indices);

    return ($addr, @default);
}

##@method       lookup_addr()
#
# @brief        Looks for the specified register name in the active chip hash
#               table and, if it is found, returns the register base address.
#
# @param[in]    $name: register name.
# 
# @param[in]    $indices: register indices (optional parameter)
# 
# @return       register base address
sub lookup_addr {
    my ($addr, @default) = lookup_addr_and_default(@_);

    return $addr;
}

##@method       lookup_reg()
#
# @brief        Looks for the specified address in the register value table and
#               returns the associated value. This table is loaded with the 
#               register values coming from the register-dump-file.
#
# @param[in]    $addr: register address.
# 
# @return       the register value (from the register-dump file)
#
sub lookup_reg
{
    my ($addr) = @_;
    my $rv = $reg_vals[$addr];

    # CSRs not handled
    delete $not_handled_regs{$addr};

    if (not defined $rv) {
        if ($main::opt_z) {
            # Treat unspecified registers as 0.
            $rv = 0;
        } else {
            my $addrhex = sprintf "0x%x", $addr;
            #die "Register dump has no value for register at addr $addrhex";
        }
    }
    return $rv;
}

sub configuring_port
{
    my ($port) = @_;

    return (($configure_portmask&(1<<$port)) == 0)?0:1;
}

##@method       bali_process_multiword_regs()
#
# @brief        process multiwords registers in order to generate eeprom WRITE
#               commands.This function works for BALI and TAHOE.
#               In BALI, a eeprom WRITE command is generated for every word to be
#               written into a register.
#               Registers whose values are equal to the default are skipped unless
#               $unconditional is set.
#
# @param[in]    $writesref: reference to the list to push the address and data.
#
# @param[in]    $addr: base address of the register.
#
# @param[in]    $wordcount: register lenght in words.
#
# @param[in]    $mask: data mask.
#
# @param[in]    $defaultref: reference to the list of default values.
#
# @param[in]    $unconditional: when not equal to zero, it forces writing the 
#               specified register even when the data is equal to the default value.
sub bali_process_multiword_regs
{
    my ($writesref, $addr, $wordcount, $mask, $defaultref, $unconditional) = @_;

    my $need_to_write = ($main::opt_a)?1:0;
    my @tmp_writes = ();
MULTIWORD:
    for (my $k = 0; $k < $wordcount; $k++) {
        my $val = lookup_reg($addr+$k);

        # Skip registers with undefined values.
        if (not defined $val) {
            $need_to_write = 0;
            last MULTIWORD;
        }

        # Mask off value if requested.
        if (defined $mask) {
            die "Mask size too small" if not defined $mask->[$k];
            $val &= $mask->[$k];
        }

        push @tmp_writes, [$addr+$k, $val];

        if ($val != $defaultref->[$k] or defined $unconditional) {
            $need_to_write = 1;
        }
    }
    if ($need_to_write) {
        push @{$writesref},@tmp_writes;
    }
}

##@method       bali_process_multiword_regs_insrt_writes()
#
# @brief        generates a sequence of bali WRITE commands for every duple addr-data
#               contained in the list writesref.
#               This method also makes command logging.
#
# @param[in]    $writesref: reference to the list to push the address and data.
#
# @param[in]    $base: register name.
sub bali_process_multiword_regs_insrt_writes
{
    my ($writesref, $base) = @_;

    if (@{$writesref} > 0) {
        describe_stage($base);
        foreach my $write (@{$writesref}) {
            my ($addr, $val) = @$write;
            eeprom_insert_write_register32_command($addr, $val);
        }
    }        
}

##@method       alta_process_multiword_regs()
#
# @brief        process multiwords registers in order to generate eeprom WRITE
#               commands for alta.
#               In ALTA, multiword register may be written using an only WRITE
#               command.
#               Registers whose values are equal to the default are skipped unless
#               $unconditional is set.
#
# @param[in]    $writesref: reference to the list to push the address and data.
#
# @param[in]    $addr: base address of the register.
#
# @param[in]    $wordcount: register lenght in words.
#
# @param[in]    $mask: data mask.
#
# @param[in]    $default: reference to the list of default values.
#
# @param[in]    $unconditional: when not equal to zero, it forces writing the 
#               specified register even when the data is equal to the default value.
sub alta_process_multiword_regs
{
    my ($writesref, $addr, $wordcount, $mask, $defaultref, $unconditional) = @_;

    my $need_to_write = ($main::opt_a)?1:0;
    my @tmp_writes = ();
MULTIWORD:
    for (my $k = $wordcount; $k > 0; $k--) {
        my $val = lookup_reg($addr+$k-1);


        # Skip registers with undefined values.
        if (not defined $val) {
            $need_to_write = 0;
            last MULTIWORD;
        }
        # Mask off value if requested.
        if (defined $mask) {
            die "Mask size too small" if not defined $mask->[$k];
            $val &= $mask->[$k];
        }
        unshift @tmp_writes, $val;

        if ($val != $defaultref->[$k] or defined $unconditional) {
            $need_to_write |= 1;
        }
    }
    if ($need_to_write) {
        push @{$writesref},$addr;
        push @{$writesref},@tmp_writes;
    }
}

##@method       alta_process_multiword_regs_insrt_writes()
#
# @brief        generates an alta WRITE command for the specified register.
#               In Alta, eeprom WRITE commands may be multibyte.
#               This method also makes command logging.
#
# @param[in]    $writesref: reference to the list to push the address and data.
#
# @param[in]    $base: register name.
sub alta_process_multiword_regs_insrt_writes
{
    my ($writesref, $base) = @_;

    # when chip is alta, writesref list has always 1 or more items, the first one
    # is the register address. 
    if (@{$writesref} > 1) {
        describe_stage($base);
        eeprom_insert_alta_write_register_command(shift @{$writesref}, @{$writesref});
    }        
}


##@method       process_single_reg()
#
# @brief        verifies that the register is a single one and looks for the 
#               configuration value(s) in the table loaded from the register dump
#               file. If the register value(s) is(are) not equal to the default value(s),
#               a WRITE command is appending to the eeprom image.
#
# @param[in]    $base:      register name.
# 
# @param[in]    $mask:      register value mask (optional parameter, use "undef"
#                           if it is not specified.
# @param[in]    $unconditional: if it is not zero, the register will be processed
#                           even when its value is equal to the default.
sub process_single_reg
{
    my ($base, $mask, $unconditional) = @_;

    my ($entry) = $regs_hash{$base};
    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

    die "Reg $base is not a single reg" if @boundlist != 0;

    my $addr    = &$addrsub();
    my @default = &$defaultsub();

    #printf "$base => 0x%x\n", $addr;
    my @writes = ();
    &{$process_multiword_regs_ref}(\@writes, $addr, $wordcount, $mask, \@default, $unconditional);
    &{$process_multiword_regs_insrt_writes}(\@writes, $base);
}

##@method       process_array1_reg()
#
# @brief        verifies that the register is a 1 dimensional array and looks for the 
#               configuration value(s) in the table loaded from the register dump
#               file. If the register value(s) is(are) not equal to the default value(s),
#               a WRITE command is appending to the eeprom image.
#
# @param[in]    $base:      array name.
# 
# @param[in]    $override_indices allows to override the array index.
# 
# @param[in]    $mask:      register value mask (optional parameter, use "undef"
#                           if it is not specified.
# @param[in]    $unconditional: if it is not zero, the register will be processed
#                           even when its value is equal to the default.
sub process_array1_reg {
    my ($base, $override_indices, $mask, $unconditional) = @_;

    my ($entry) = $regs_hash{$base};
    die "Reg name (1D array) not found" if !defined $entry;
    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

    die "Reg $base is not a 1D array" if @boundlist != 1;
    my @bounds1 = @{$boundlist[0]};
    my @indices1 = ($bounds1[0]..$bounds1[1]);
    if (defined $override_indices) {
        @indices1 = @$override_indices;
    }

    my @writes = ();
    foreach my $i (@indices1) {
        next if $chip_is != ALTA and $portindex == 0 and not configuring_port($i);

        my $addr    = &$addrsub($i);
        my @default = &$defaultsub($i);

        &{$process_multiword_regs_ref}(\@writes, $addr, $wordcount, $mask, \@default, $unconditional);
        &{$process_multiword_regs_insrt_writes}(\@writes, $base);
        @writes = ();
    }
}

##@method       process_array2_reg()
#
# @brief        verifies that the register is a 2 dimensional array and looks for the 
#               configuration value(s) in the table loaded from the register dump
#               file. If the register value(s) is(are) not equal to the default value(s),
#               a WRITE command is appending to the eeprom image.
#
# @param[in]    $base:      array name.
# 
# @param[in]    $override_indices allows to override the array index.
# 
# @param[in]    $mask:      register value mask (optional parameter, use "undef"
#                           if it is not specified.
# @param[in]    $unconditional: if it is not zero, the register will be processed
#                           even when its value is equal to the default.
sub process_array2_reg {
    my ($base, $override_indices_1, $override_indices_2,
            $mask, $unconditional) = @_;

    my ($entry) = $regs_hash{$base};
    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

    die "Reg $base is not a 2D array" if @boundlist != 2;
    my @bounds1 = @{$boundlist[0]};
    my @bounds2 = @{$boundlist[1]};
    my @indices1 = ($bounds1[0]..$bounds1[1]);
    my @indices2 = ($bounds2[0]..$bounds2[1]);
    if (defined $override_indices_1) {
        @indices1 = @$override_indices_1;
    }
    if (defined $override_indices_2) {
        @indices2 = @$override_indices_2;
    }

    my @writes = ();
    foreach my $i (@indices1) {
        next if $chip_is != ALTA and $portindex == 0 and not configuring_port($i);
        foreach my $j (@indices2) {
            next if $chip_is != ALTA and $portindex == 1 and not configuring_port($j);

            my $addr    = &$addrsub($i,$j);
            my @default = &$defaultsub($i,$j);

            &{$process_multiword_regs_ref}(\@writes, $addr, $wordcount, $mask, \@default, $unconditional);
            &{$process_multiword_regs_insrt_writes}(\@writes, $base);
            @writes = ();
        }
    }
}

##@method       process_array3_reg()
#
# @brief        verifies that the register is a 3 dimensional array and looks for the 
#               configuration value(s) in the table loaded from the register dump
#               file. If the register value(s) is(are) not equal to the default value(s),
#               a WRITE command is appending to the eeprom image.
#
# @param[in]    $base:      array name.
# 
# @param[in]    $override_indices allows to override the array index.
# 
# @param[in]    $mask:      register value mask (optional parameter, use "undef"
#                           if it is not specified.
# @param[in]    $unconditional: if it is not zero, the register will be processed
#                           even when its value is equal to the default.
sub process_array3_reg {
    my ($base, $override_indices_1, $override_indices_2, $override_indices_3,
            $mask, $unconditional) = @_;

    my ($entry) = $regs_hash{$base};
    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

    die "Reg $base is not a 3D array" if @boundlist != 3;
    my @bounds1 = @{$boundlist[0]};
    my @bounds2 = @{$boundlist[1]};
    my @bounds3 = @{$boundlist[2]};
    my @indices1 = ($bounds1[0]..$bounds1[1]);
    my @indices2 = ($bounds2[0]..$bounds2[1]);
    my @indices3 = ($bounds3[0]..$bounds3[1]);
    if (defined $override_indices_1) {
        @indices1 = @$override_indices_1;
    }
    if (defined $override_indices_2) {
        @indices2 = @$override_indices_2;
    }
    if (defined $override_indices_3) {
        @indices3 = @$override_indices_3;
    }

    my @writes = ();
    foreach my $i (@indices1) {
        next if $chip_is != ALTA and $portindex == 0 and not configuring_port($i);

        foreach my $j (@indices2) {
            next if $chip_is != ALTA and $portindex == 1 and not configuring_port($j);

            foreach my $k (@indices3) {
                next if $chip_is != ALTA and $portindex == 2 and not configuring_port($k);

                my $addr    = &$addrsub($i,$j,$k);
                my @default = &$defaultsub($i,$j,$k);
                
                &{$process_multiword_regs_ref}(\@writes, $addr, $wordcount, $mask, \@default, $unconditional);
                # check if there are regs to initialize and push them into the eeprom buffer
                &{$process_multiword_regs_insrt_writes}(\@writes, $base);
                @writes = ();
            }
        }
    }
}

##@method       process_all_matching_registers()
#
# @brief        process all the registers to be handled in an unordered loop (the
#               ones tagged as "u").
#
# @param[in]    $dim:       array dimension
# 
# @param[in]    $is_port_reg defined if it is a port related register/array.
# 
sub process_all_matching_registers {
    my ($dim, $is_port_reg) = @_;

    foreach my $reg (sort keys %regs_hash) {
        my ($entry) = $regs_hash{$reg};
        my ($wordcount, $indexcount, $portindex, $ordering,
                $addrsub, $defaultsub, @boundlist) = @$entry;

        next if $ordering ne "u";
        next if $indexcount != $dim;

        if (($is_port_reg and $portindex >= 0) or
                (not $is_port_reg and $portindex == -1)) {

            if ($dim == 0) {
                process_single_reg($reg);
            } elsif ($dim == 1) {
                process_array1_reg($reg);
            } elsif ($dim == 2) {
                process_array2_reg($reg);
            } elsif ($dim == 3 && $chip_is == ALTA) {
                process_array3_reg($reg);
            } else {
                die "Invalid dim: $dim";
            }
        }
    }
}

sub process_all_single_registers {
    process_all_matching_registers(0, 0);
}

sub process_all_port_registers {
    process_all_matching_registers(1, 1);
    process_all_matching_registers(2, 1);
}

sub process_all_array_registers {
    process_all_matching_registers(1, 0);
    process_all_matching_registers(2, 0);
    if ($chip_is == ALTA) {
        process_all_matching_registers(3, 0);
    }
}


# tahoe_setup_tables
# Configure MA_TABLE, FID_TABLE, and VID_TABLE

sub tahoe_setup_tables {
    # The API initializes all the FID entries.  However, most
    # are not needed.  The @fid_needed table is used to determine
    # which FID entries need to be written.  An entry is written
    # if it satisfies one of the following criteria:
    #
    # - The FID is FID 0, which is used is shared VLAN learning
    #   mode.
    # - The FID corresponds to a valid VID table entry.
    # - The FID is referenced by a valid MA table entry.
    # - The FID corresponds port default VID value.
    my @fid_needed = (0) x 4096;

    # Always write FID 0.
    $fid_needed[0] = 1;

    foreach my $port (1..24) {
        if (configuring_port($port)) {
            my $addr = lookup_addr("FM_PORT_CFG_1", $port);
            my $val = lookup_reg($addr);
            my $default_vid = $val&0xFFF;

            # Always write FID that matches default VID for
            # configured ports.
            $fid_needed[$default_vid] = 1;
        }
    }

    describe_stage("Locked MAC Table Entries.");
    foreach my $entry (0..16383) {
        my $baseaddr = lookup_addr("FM_MA_TABLE", $entry);

        my $word2 = lookup_reg($baseaddr+2);
        if (($word2 & 0x18) == 0x18) {
            # Only valid, locked entries.
            my $word0 = lookup_reg($baseaddr+0);
            my $word1 = lookup_reg($baseaddr+1);
            #die "Bad parity on MA Table entry $entry" if $word0&1;
            eeprom_insert_write_register32_command($baseaddr+0, $word0);
            eeprom_insert_write_register32_command($baseaddr+1, $word1);
            eeprom_insert_write_register32_command($baseaddr+2, $word2);

            # Need to write FID entry referenced by this
            # MA table entry.
            my $fid = ($word1>>18)&0xFFF; # bits 61..50
            $fid_needed[$fid] = 1;
        }
    }

    describe_stage("VID Table.");
    foreach my $entry (0..4095) {
        my $baseaddr = lookup_addr("FM_VID_TABLE", $entry);

        my $word0 = lookup_reg($baseaddr+0);
        my $word1 = lookup_reg($baseaddr+1);
        if ($word0 != 0 or $word1 != 0) {
            #die "Bad parity on VID entry $entry" if $word0&1;
            eeprom_insert_write_register32_command($baseaddr+0, $word0);
            eeprom_insert_write_register32_command($baseaddr+1, $word1);

            # Always write corresponding FID entry.
            $fid_needed[$entry] = 1;
        }
    }

    describe_stage("FID Table.");
    foreach my $entry (0..4095) {
        my $baseaddr = lookup_addr("FM_FID_TABLE", $entry);

        my $word0 = lookup_reg($baseaddr+0);
        my $word1 = lookup_reg($baseaddr+1);
        if ($word0 != 0 or $word1 != 0) {
            if ($main::opt_f or $fid_needed[$entry]) {
                #die "Bad parity on FID entry $entry" if $word0&1;
                eeprom_insert_write_register32_command($baseaddr+0, $word0);
                eeprom_insert_write_register32_command($baseaddr+1, $word1);
            }
        }
    }
}


# bali_setup_tables
# Configure MA_TABLE, FID_TABLE, and VID_TABLE

sub bali_setup_tables {
    # The API initializes all the FID entries.  However, most
    # are not needed.  The @ingress_fid_needed and
    # @egress_fid_needed tables are used to determine
    # which FID entries need to be written.  An entry is written
    # if it satisfies one of the following criteria:
    #
    # - The FID is FID 0, which is used is shared VLAN learning
    #   mode.
    # - The FID corresponds to a valid VID table entry.
    # - The FID is referenced by a valid MA table entry.
    my @ingress_fid_needed = (0) x 4096;
    my @egress_fid_needed  = (0) x 4096;

    # Always write FID 0.
    $ingress_fid_needed[0] = 1;
    $egress_fid_needed[0] = 1;

    describe_stage("Locked MAC Table Entries.");
    foreach my $entry (0..16383) {
        my $baseaddr = lookup_addr("FM_MA_TABLE", $entry);

        my $word1 = lookup_reg($baseaddr+1);
        if (($word1 & 0x30000000) == 0x30000000) {
            # Only valid, locked entries.
            my $word0 = lookup_reg($baseaddr+0);
            my $word2 = lookup_reg($baseaddr+2);
            eeprom_insert_write_register32_command($baseaddr+0, $word0);
            eeprom_insert_write_register32_command($baseaddr+1, $word1);
            eeprom_insert_write_register32_command($baseaddr+2, $word2);

            # Need to write FID entry referenced by this
            # MA table entry.
            my $fid = ($word1>>16)&0xFFF; # bits 59..48
            $ingress_fid_needed[$fid] = 1;
            $egress_fid_needed[$fid] = 1;
        }
    }

    describe_stage("Ingress VID Table.");
    foreach my $entry (0..4095) {
        my $baseaddr = lookup_addr("FM_INGRESS_VID_TABLE", $entry);

        my $word0 = lookup_reg($baseaddr+0);
        my $word1 = lookup_reg($baseaddr+1);
        my $word2 = lookup_reg($baseaddr+2);
        if ($word0 != 0 or $word1 != 0 or $word2 != 0) {
            eeprom_insert_write_register32_command($baseaddr+0, $word0);
            eeprom_insert_write_register32_command($baseaddr+1, $word1);
            eeprom_insert_write_register32_command($baseaddr+2, $word2);

            # Always write corresponding FID entry.
            my $fid = $word2&0xFFF; # bits 75..64
            $ingress_fid_needed[$fid] = 1;
        }
    }

    describe_stage("Ingress FID Table.");
    foreach my $entry (0..4095) {
        my $baseaddr = lookup_addr("FM_INGRESS_FID_TABLE", $entry);

        my $word0 = lookup_reg($baseaddr+0);
        my $word1 = lookup_reg($baseaddr+1);
        if ($word0 != 0 or $word1 != 0) {
            if ($main::opt_f or $ingress_fid_needed[$entry]) {
                eeprom_insert_write_register32_command($baseaddr+0, $word0);
                eeprom_insert_write_register32_command($baseaddr+1, $word1);
            }
        }
    }

    describe_stage("Egress VID Table.");
    foreach my $entry (0..4095) {
        my $baseaddr = lookup_addr("FM_EGRESS_VID_TABLE", $entry);

        my $word0 = lookup_reg($baseaddr+0);
        my $word1 = lookup_reg($baseaddr+1);
        if ($word0 != 0 or $word1 != 0) {
            eeprom_insert_write_register32_command($baseaddr+0, $word0);
            eeprom_insert_write_register32_command($baseaddr+1, $word1);

            # Always write corresponding FID entry.
            my $fid = $word1&0xFFF; # bits 43..32
            $egress_fid_needed[$fid] = 1;
        }
    }

    describe_stage("Egress FID Table.");
    foreach my $entry (0..4095) {
        my $baseaddr = lookup_addr("FM_EGRESS_FID_TABLE", $entry);

        my $word0 = lookup_reg($baseaddr+0);
        if ($word0 != 0) {
            if ($main::opt_f or $egress_fid_needed[$entry]) {
                eeprom_insert_write_register32_command($baseaddr+0, $word0);
            }
        }
    }
}


# serdes_power_up
#
# Power up SERDES only on ports that are out of reset.
# SERDES config registers and PCS_CFG_1 should be
# configured prior to calling this function in order
# to set lane reversal, number of lanes, etc.

sub serdes_power_up {
    my @portlist = ();

    my @stm_addr = (0);
    my @sc2_addr = (0);
    my @final_values = (0);
    my @current_values = (0);

    foreach my $port (1..24) {
        $sc2_addr[$port] = lookup_addr("FM_SERDES_CTRL_2", $port);
        $stm_addr[$port] = lookup_addr("FM_SERDES_TEST_MODE", $port);

        $final_values[$port] = lookup_reg($sc2_addr[$port]);
        $current_values[$port] = 0x3ff00; # chip default
        if (configuring_port($port)) {
            push @portlist, $port;
        }
    }

    eeprom_insert_delay_command(10);

    my @bringup_value_sequence = (
        0x3ff00, # initial state (all reset and power down)
        0x3cf00, # AB: drop power down
        0x2cf00, # AB: drop pll reset
        0x2cc00, # AB: drop lane reset
        0x20c00, # CD: drop power down
        0x00c00, # CD: drop pll reset
        0x00000, # CD: drop lane reset
    );

    # Work around bug 11093 (ports sometimes come up misaligned
    # in loopback mode).
    describe_stage("SERDES_TEST_MODE: Put SYNCBUFs in reset");
    foreach my $port (@portlist) {
        # Put PCS syncbuf in reset
        eeprom_insert_write_register32_command($stm_addr[$port], 0x70);
    }

    describe_stage("SERDES power-up procedure");
    foreach my $value (@bringup_value_sequence) {
        foreach my $port (@portlist) {
            my $regval = $value | $final_values[$port];
            if ($regval != $current_values[$port]) {
                eeprom_insert_write_register32_command($sc2_addr[$port], $regval);
            }
            $current_values[$port] = $regval;
        }
        eeprom_insert_delay_command(10);
    }

    # This will bring the PCS syncbuf back out of reset
    describe_stage("SERDES_TEST_MODE: Final values");
    foreach my $port (@portlist) {
        my $regval = lookup_reg($stm_addr[$port]);
        $regval = 0x60 if not defined $regval;
        eeprom_insert_write_register32_command($stm_addr[$port], $regval);
    }
}


# configure_port_cfg_2
#
# Special handling of PORT_CFG_2 to optionally disable
# sending to the CPU port if requested by the user.

sub configure_port_cfg_2 {
    describe_stage("PORT_CFG_2");

    foreach my $port (0..24) {
        if (configuring_port($port)) {
            my ($addr, $default) =
                lookup_addr_and_default("FM_PORT_CFG_2", $port);

            my $regval = lookup_reg($addr);

            # Optionally disable sending to port 0.
            if ($port != 0 and not $main::opt_0) {
                $regval &= 0xFFFFFFFE;
            }

            if ($regval != $default) {
                eeprom_insert_write_register32_command($addr, $regval);
            }
        }
    }
}


sub bali_execute_eeprom_insert_boot_command {
    my ($cmd) = @_;

    if ($main::opt_t or $main::opt_T) {
        eeprom_insert_write_register32_command(lookup_addr("FM_BOOT_CTRL"), $cmd);

        # FIXME: For TE scripts, we could poll instead of
        # delaying in this case.

        # Wait 100ms
        #eeprom_insert_delay_command(100_000);
        eeprom_insert_delay_command(1_000_000);
    } else {
        # When creating an EEPROM, there is a special
        # command to do boot operations and wait for them
        # to complete.
        eeprom_insert_boot_command($cmd);
    }
}


# tahoe_fh_pll_init
#
# Configure the FH PLL.  Allow time for it to lock and
# then turn off the PLL bypass.  Bring FH out of reset.

sub tahoe_fh_pll_init {
    my ($pll_cfg_val) = @_;

    my $FM_CHIP_MODE   = lookup_addr("FM_CHIP_MODE");
    my $FM_PLL_FH_CTRL = lookup_addr("FM_PLL_FH_CTRL");
    my $FM_SOFT_RESET  = lookup_addr("FM_SOFT_RESET");

    describe_stage("Configuring PLL");
    eeprom_insert_write_register32_command($FM_PLL_FH_CTRL, $pll_cfg_val); 

    # 0.5 second delay for PLL lock (FIXME: should shorten this)
    eeprom_insert_delay_command(500000);

    # turn off PLL bypass if needed
    # leave LEDs disabled during port config stage (bug 7900)
    describe_stage("Configure PLL bypass setting");
    my $chip_mode = lookup_reg($FM_CHIP_MODE);
    eeprom_insert_write_register32_command($FM_CHIP_MODE, $chip_mode&0x2000);

    # Bring FH out of reset.
    describe_stage("Bring FH out of reset");
    eeprom_insert_write_register32_command($FM_SOFT_RESET, 0);
}


# tahoe_port_reset_initial
#
# Configure clock MUXes and bring ports out of reset.
# This must be done before configure the ports.

sub tahoe_port_reset_initial {
    my ($clocksel) = @_;

    my $FM_PORT_CLK_SEL = lookup_addr("FM_PORT_CLK_SEL");
    my $FM_PORT_RESET   = lookup_addr("FM_PORT_RESET");

    describe_stage("port_reset_initial");
    my $str = "X";
    foreach my $port (1..24) {
        if ($clocksel&(1<<$port)) {
            $str = "B$str";
        } else {
            $str = "A$str";
        }
    }
    describe_stage("Current clock selection: $str");
    eeprom_insert_write_register32_command($FM_PORT_CLK_SEL, $clocksel);

    describe_stage("Bringing all EPLs out of reset");
    eeprom_insert_write_register32_command($FM_PORT_RESET, (~$configure_portmask)&0xFE000000);
}



##@method       load_reg_dump()
#
# @brief        load the register dump file into a table. Later, these register values
#               are used to generate the configuration output file.
#
sub load_reg_dump {
    while (<>) {
        chomp;
        if (/^FM/) {
            # output from TestPoint
            # addr (in parenthesis) is the address of the
            #   top of the register (base+size-1)
            # FM2000_BOOT_STATUS (00000000) : 00000000
            # FM4000_MA_TABLE[0] (00180000) : 2001D806C60CEBE9  02000018
            chomp;
            s/:/ /;
            s/[()]//g;

            my ($name, $addr, @vals) = split;
            $addr = hex $addr;
            my $size = @vals + 0;
            my $mask = ~($size-1);
            $addr &= $mask;
            foreach my $val (reverse @vals) {
                $val = hex $val;
                $reg_vals[$addr] = $val;
                $addr++;
            }
        } elsif (/:/ and not /WRITE64/) {
            # simple format (addr: val32 val32 val32)
            chomp;
            s/:/ /;
            my ($addr, @vals) = split;
            $addr = hex $addr;
            foreach my $val (@vals) {
                $val = hex $val;
                $reg_vals[$addr] = $val;
                $addr++;
            }
        } else {
            # hacked up list of register writes
            # WRITE64: 0x0004010a=0x0000000000000215
            # 0x00000206 0x000022d0
            chomp;
            if (/WRITE64/) {
                # WRITE64: 0x00040108=0x0090000000000000
                s/.*WRITE64: //;
                s/=/ /;
                my ($addr, $valstr) = split;
                $addr = hex $addr;

                $valstr =~ s/0x//;
                my $val1 = hex(substr($valstr, 0, 8));
                my $val0 = hex(substr($valstr, 8, 8));

                $reg_vals[$addr] = $val0;
                $reg_vals[$addr+1] = $val1;

                $not_handled_regs{$addr} = $val0;
                $not_handled_regs{$addr+1} = $val1;
            } else {
                my ($addr, $val) = split;
                $addr = hex $addr;
                $val = hex $val;
                next if $val == 0xdeadbeef; # skip
                $reg_vals[$addr] = $val;
                $not_handled_regs{$addr} = $val;
            }
        }
    }
    print STDERR "# Loaded register dump.\n";
}


sub tahoe_configure_chip {
    my $FM_CLK_MULT_1   = lookup_addr("FM_CLK_MULT_1");
    my $FM_PLL_FH_CTRL  = lookup_addr("FM_PLL_FH_CTRL");
    my $FM_PORT_CLK_SEL = lookup_addr("FM_PORT_CLK_SEL");
    my $FM_CHIP_MODE    = lookup_addr("FM_CHIP_MODE");

    if ($main::opt_t or $main::opt_T) {
        # In TE scripts (but not in EEPROM images), we need to
        # do an extra step to boot the chip properly and init the
        # memories.
        if ($main::opt_T) {
            # TestPoint/Manatee version
            print "tp(\"reset chip\");\n";
        } else {
            # te/Kirkwood version
            print "reset_chip 20000;\n";
        }
        eeprom_insert_write_register32_command($FM_CHIP_MODE, 0x2D00);
        eeprom_insert_delay_command(1_000_000);
    }

    # First configure the SPI clock divisor.  This will ensure
    # that our delays have the correct duration.
    my $clk_mult_1 = lookup_reg($FM_CLK_MULT_1);
    $spi_div_cfg = ($clk_mult_1>>8)&0xFF;
    if ($spi_div_cfg == 0) {
        $spi_div_cfg = 0x19;
        $clk_mult_1 &= 0xFF;
        $clk_mult_1 |= ($spi_div_cfg<<8);
    }

    # Set the LED clock frequency.
    eeprom_insert_write_register32_command($FM_CLK_MULT_1, $clk_mult_1);

    my $pll_ctrl_val = lookup_reg($FM_PLL_FH_CTRL);
    my $clocksel = lookup_reg($FM_PORT_CLK_SEL);

    # Initialize FH PLL and make sure the FH gets clocked.
    describe_stage("Initialize the frame handler PLL.");
    tahoe_fh_pll_init($pll_ctrl_val);

    # Bring ports out of reset.
    describe_stage("Bring ports out of reset.");
    tahoe_port_reset_initial($clocksel);

    # Normal 32-bit registers (single address per register)
    describe_stage("Single registers.");
    process_all_single_registers();

    # Per-port registers
    describe_stage("Port registers.");
    process_all_port_registers();
    configure_port_cfg_2();

    # Normal arrays, written one word at a time
    describe_stage("Array registers.");
    process_all_array_registers();

    # VID, FID, MA
    describe_stage("Setup tables.");
    tahoe_setup_tables();

    # Now do the SERDES bringup procedure.
    describe_stage("Serdes bringup.");
    serdes_power_up();

    # enable LEDs only now that all port config is done (bug 7900)
    describe_stage("Enable LEDs.");
    my $chip_mode = lookup_reg($FM_CHIP_MODE);
    eeprom_insert_write_register32_command($FM_CHIP_MODE, $chip_mode&0x200C);

    eeprom_insert_finish_command();
    eeprom_insert_finish_command();
    eeprom_insert_finish_command();
}


sub FM4000_SCAN_SHIFT {
    my ($in) = @_;

    my $SCAN_IN  = lookup_addr("FM_ASYNC_SCAN_IN");

    eeprom_insert_write_register32_command($SCAN_IN, $in);
}

sub FM4000_SCAN_SHIFT_N {
    my ($in, $count) = @_;

    for (my $i=0; $i<$count; $i++) {
        FM4000_SCAN_SHIFT($in);
    }
}

sub FM4000_SCAN_DELAY {
    eeprom_insert_delay_command(1); # 1 us
}

# fm4000PreAdjustSchedulerTokens
#
# Prepares the switch for adjustment of the number of scheduler
# tokens.  This function depends on ASYNC memory repair, while the
# remainder of the boot process depends on this function

sub fm4000PreAdjustSchedulerTokens {
    my $SCAN_CFG = lookup_addr("FM_ASYNC_SCAN_CFG");
    my $SCAN_CMD = lookup_addr("FM_ASYNC_SCAN_CMD");
    my $SCAN_IN  = lookup_addr("FM_ASYNC_SCAN_IN");

    # Switch to SCAN mode.
    eeprom_insert_write_register32_command($SCAN_CFG, 0x8001ff2e);

    # Prepare to shift.
    eeprom_insert_write_register32_command($SCAN_CMD, 0x0);
    eeprom_insert_write_register32_command($SCAN_IN, 0x0);
    eeprom_insert_write_register32_command($SCAN_CMD, 0xff2e);

    # Send in the drain command on the TX_QUEUES FREE1 channel.
    FM4000_SCAN_SHIFT_N(4, 1);
    FM4000_SCAN_SHIFT_N(0, 133);
    FM4000_SCAN_SHIFT_N(4, 1);
    FM4000_SCAN_SHIFT_N(0, 885);

    # Execute the command and leave SCAN mode.
    eeprom_insert_write_register32_command($SCAN_CFG, 0x0)
}


# fm4000AdjustSchedulerTokens
#
# Adjusts the number of scheduler tokens.  Note: this function depends
# on the scheduler being initialized, while the remainder of the boot
# process depends on this function.

sub fm4000AdjustSchedulerTokens {
    my $SCAN_CFG = lookup_addr("FM_ASYNC_SCAN_CFG");
    my $SCAN_CMD = lookup_addr("FM_ASYNC_SCAN_CMD");
    my $SCAN_IN  = lookup_addr("FM_ASYNC_SCAN_IN");

    my $dc = 0;
    my $numDrains = 3 * (4 + $dc) + 4;

    # Modify the number of per-priority reserved credits.
    eeprom_insert_write_register32_command(0x600FD, $dc << 24);

    # Switch to SCAN mode.
    eeprom_insert_write_register32_command($SCAN_CFG, 0x8001ff2e);

    # Prepare to shift.
    eeprom_insert_write_register32_command($SCAN_CMD, 0x0);
    eeprom_insert_write_register32_command($SCAN_IN, 0x0);

    # No need to actually shift because the drain command has already
    # been scanned in.

    # Execute the drain command.
    eeprom_insert_write_register32_command($SCAN_CMD, 0x0);
    for (my $i=0; $i<$numDrains; $i++) {
        eeprom_insert_write_register32_command($SCAN_IN, 0x0);
        FM4000_SCAN_DELAY();
        eeprom_insert_write_register32_command($SCAN_IN, 0x0);
        FM4000_SCAN_DELAY();
    }

    # Prepare to shift.
    eeprom_insert_write_register32_command($SCAN_CMD, 0xff2e);

    # Put the TX_QUEUES FREE 1 channel back in pass through mode by
    # moving the drain commands up by only one bit, into the data
    # portion of the scan buffer.  This will change the command back
    # to pass-through.
    FM4000_SCAN_SHIFT_N(0, 1);

    # Execute the command and leave SCAN mode.
    eeprom_insert_write_register32_command($SCAN_CFG, 0x0);
}


sub bali_configure_chip {
    my $FM_CLK_MULT_1   = lookup_addr("FM_CLK_MULT_1");
    my $FM_PLL_FH_CTRL  = lookup_addr("FM_PLL_FH_CTRL");
    my $FM_SOFT_RESET   = lookup_addr("FM_SOFT_RESET");
    my $FM_SYS_CFG_8    = lookup_addr("FM_SYS_CFG_8");

    if ($main::opt_t or $main::opt_T) {
        # In TE, need to boot chip properly to init memories.
        if ($main::opt_T) {
            # TestPoint/Manatee version
            print "tp(\"reset chip\");\n";
        } else {
            # te/Kirkwood version
            print "reset_chip 20000;\n";
        }

        # Wait 10ms to let the fusebox load
        #eeprom_insert_delay_command(10_000);
        eeprom_insert_delay_command(100_000);

        # Write to fusebox registers
        describe_stage("Fusebox registers");
        process_single_reg("FM_VPD_INFO_1");
        process_single_reg("FM_VPD_INFO_2");
        process_array1_reg("FM_VPD_ASYNC_RAM_REPAIR");
        process_array1_reg("FM_VPD_SYNC_RAM_REPAIR");
    } else {
        # Set up EEPROM clock divisor.
        my $clk_mult_1 = lookup_reg($FM_CLK_MULT_1);

        $spi_div_cfg = $clk_mult_1&0xFF;
        if ($spi_div_cfg == 0) {
            $spi_div_cfg = 0x19;
        }
        $clk_mult_1 = $spi_div_cfg&0xFF;

        eeprom_insert_write_register32_command($FM_CLK_MULT_1, $clk_mult_1);
        eeprom_insert_delay_command(10);
    }

    # Step 2: Initialize the async memory repair.
    describe_stage("2. Repairing async memory errors.");
    bali_execute_eeprom_insert_boot_command($FM_BOOT_COMMAND_ASYNC_REPAIR);

    if ($adjust_scheduler_tokens) {
        fm4000PreAdjustSchedulerTokens();
    }

    # Step 3: Initialize the scheduler.
    describe_stage("3. Initializing the scheduler.");
    bali_execute_eeprom_insert_boot_command($FM_BOOT_COMMAND_SCHED_INIT);

    if ($adjust_scheduler_tokens) {
        fm4000AdjustSchedulerTokens();
    }

    # Step 4: Configure the FH PLL.  Allow time for it to lock.
    describe_stage("4. Initialize the frame handler PLL.");
    process_single_reg("FM_PLL_FH_CTRL"); 
    # 80 microsecond delay for PLL lock
    eeprom_insert_delay_command(80);

    # Step 5: Bring all modules out of reset.
    describe_stage("5. Bring all modules out of reset.");
    eeprom_insert_write_register32_command($FM_SOFT_RESET, 0); 
    eeprom_insert_delay_command(10_000);

    # Step 6: Bring MSB/EPL0 out of reset.
    describe_stage("6. Bring MSB/EPL0 out of reset.");
    # FIXME: what happens when EPL0 is in reset in the
    # register dump file?
    process_array1_reg("FM_EPL_PORT_CTRL", [0], [0x0], 1);
    process_array1_reg("FM_EPL_PORT_CTRL", [0], [0x7], 1);
    process_array1_reg("FM_EPL_PORT_CTRL", [0], undef, 1);

    # Step 7: Enable the FFU.
    describe_stage("7. Enabling the FFU");
    eeprom_insert_write_register32_command($FM_SYS_CFG_8, 1);
    eeprom_insert_delay_command(100_000); # 0.1 second delay

    # Step 8: Initialize the sync memory repair.
    describe_stage("8. Repairing sync memory errors.");
    bali_execute_eeprom_insert_boot_command($FM_BOOT_COMMAND_SYNC_REPAIR);

    # Step 9: Initialize the FFU.
    # Must do this write to trigger the init step.
    describe_stage("9. Initializing the FFU");
    process_single_reg("FM_FFU_INIT_SLICE", undef, 1);

    # Step 10: Bring all ports out of reset.
    #          Note that we need to do this before
    #          memory init so that we initialize the
    #          tag table.
    describe_stage("10. Bring all ports out of reset.");
    # FIXME: what happens when EPLs are in reset in the
    # register dump file?
    process_array1_reg("FM_EPL_PORT_CTRL", [1..24], [0x0], 1);
    process_array1_reg("FM_EPL_PORT_CTRL", [1..24], [0x7], 1);
    process_array1_reg("FM_EPL_PORT_CTRL", [1..24], undef, 1);
    process_array1_reg("FM_SERDES_CTRL_3");

    # Step 11: Initialize the memory.
    describe_stage("11. Initializing memories.");
    bali_execute_eeprom_insert_boot_command($FM_BOOT_COMMAND_INIT_MEM);

    # Step 12: Set final FFU enable/disable state.
    describe_stage("12. Set FFU enable/disable state.");
    # FIXME: what if this disables the FFU but we later go on to
    # configure FFU registers?
    my $sc8 = lookup_reg($FM_SYS_CFG_8);
    if (not defined $sc8 or ($sc8&1) != 1) {
        die "Currently unable to handle FFU disabled case.";
    }
    process_single_reg("FM_SYS_CFG_8");

    # I think we should do this to ensure table partitioning is
    # configured before writing the MAC table.
    describe_stage("Set the learning rate limiter.");
    process_single_reg("FM_MA_TABLE_CFG_1");

    describe_stage("Setup tables.");
    bali_setup_tables();

    # Normal 32-bit registers (single address per register)
    describe_stage("Single registers.");
    process_all_single_registers();

    # Per-port registers
    describe_stage("Port registers.");
    process_all_port_registers();
    configure_port_cfg_2();

    # Normal arrays, written one word at a time
    describe_stage("Array registers.");
    process_all_array_registers();

    # Now do serdes bringup procedure
    describe_stage("Serdes bringup.");
    serdes_power_up();

    describe_stage("Leftovers.");
    foreach my $addr (sort keys %not_handled_regs) {
        my $val = $reg_vals[$addr];
        if ($main::opt_m) {
            # Manatee version.
            printf "tp(\"reg write 0x%x 0x%x\");\n", $addr, $val;
        } else {
            # Kirkwood version.
            printf "fp_wr(0x%x, 0x%x);\n", $addr, $val;
        }
    }

    eeprom_insert_finish_command();
    eeprom_insert_finish_command();
    eeprom_insert_finish_command();
}


##@method       alta_configure_chip()
#
# @brief        Sets up the sequence of eeprom commands that will configure the
#               Alta chip.
#               This sequence follows the steps described in the chapter 25 of 
#               the Alta Functional Specification.
#               There are two possible configuration: minimal-PCIe and full-FIBM.
#               The first one only performs the minimum required steps to boot
#               with processor w/PCIe. The second one performs a full switch init
#               to be usit in FIBM applications.
#
sub alta_configure_chip {
    my $registerAddr;
    my @regDefault;
    my $regValue;

    # step #0 (FIBM configs)
    # Initialize memory via BIST
    if ($altaInitMemBist)
    {
        describe_stage("Initialize memory via BIST.");
        alta_init_bist_mem();

        eeprom_insert_alta_write_register_command(lookup_addr("FM_SCAN_CONFIG_DATA_IN"), 0x88800000);
        eeprom_insert_delay_command(80_000);

        eeprom_insert_alta_write_register_command(lookup_addr("FM_SCAN_CONFIG_DATA_IN"), 0x88008000);
        eeprom_insert_delay_command(80_000);

        eeprom_insert_alta_write_register_command(lookup_addr("FM_SCAN_CONFIG_DATA_IN"), 0x80000040);
    }
    # MRL init fix
    if ($altaMrlFix) {
        eeprom_insert_delay_command(80_000);
        alta_mrlRegisterFix();
    }

    # step #1 (min PCIe & FIBM configs)
    # write 0xFFFFFFFF to FM_SCAN_CHAIN_DATA_IN
    describe_stage("Select normal operating mode.");

    # write 0xffffffff into FM_SCAN_CHAIN_DATA_IN
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SCAN_CHAIN_DATA_IN"), 0xffffffff);

    # step #2 (min PCIe & FIBM configs)
    # setup PLL
    describe_stage("PLL setup.");

    # It is not reliable to check lock status,
    # so just wait for 80ms
    eeprom_insert_delay_command(80_000);

    # step #3 (FIBM config)
    # Perform FFU slice number assignment.
    # - Send command BOOT("Initialize FFU Slice Numbers")
    # (eeprom fetching remains on hold until this operation is completed)
    if ($altaFIBMconfig) {
        describe_stage("Perform FFU slice number assignment.");
        eeprom_insert_boot_command(FM_ALTA_BOOT_CMMD_NONE);
        eeprom_insert_boot_command(FM_ALTA_BOOT_CMMD_INIT_FFU_SLICE_NUMBERS);

        # wait until BOOT_CTRL:CommandDone == 1 (maximun wait time: 500 ms)
        eeprom_insert_poll_command(lookup_addr("FM_BOOT_CTRL"),
                                   0x10,                    # wait until CommandDone is set
                                   0x10,                    # mask
                                   10000,                   # test 10000 times
                                   50,                      # read every 50 us
                                   $eepromBootErrJmpAddr1); # on error jump to ...
    }
    # step #4 (FIBM config)
    # Perform Bank Memory Repair
    # - Send command BOOT("Apply Bank Memory Repairs")
    # (eeprom fetching remains on hold until this operation is completed)
    if ($altaFIBMconfig) {
        describe_stage("Perform Bank Memory Repair.");
        eeprom_insert_boot_command(FM_ALTA_BOOT_CMMD_NONE);
        eeprom_insert_boot_command(FM_ALTA_BOOT_CMMD_APPLY_BANK_MEMORY_REPAIRS);
    
        # wait until BOOT_CTRL:CommandDone == 1 (maximun wait time: 500 ms)
        eeprom_insert_poll_command(lookup_addr("FM_BOOT_CTRL"),
                                   0x10,                    # wait until CommandDone is set
                                   0x10,                    # mask 
                                   10000,                   # test 10000 times
                                   50,                      # read every 50 us
                                   $eepromBootErrJmpAddr1); # on error jump to ...
    }

    # step #5 (FIBM config)
    # Perform Freelist Initialization
    # - Send command BOOT("Initialize All Scheduler Freelist")
    # (eeprom fetching remains on hold until this operation is completed)
    if ($altaFIBMconfig) {
        describe_stage("Perform Freelist Initialization.");
        eeprom_insert_boot_command(FM_ALTA_BOOT_CMMD_NONE);
        eeprom_insert_boot_command(FM_ALTA_BOOT_CMMD_INITIALIZE_ALL_SCHEDULER_FREELISTS);
        
    
        # wait until BOOT_CTRL:CommandDone == 1 (maximun wait time: 500 ms)
        eeprom_insert_poll_command(lookup_addr("FM_BOOT_CTRL"),
                                   0x10,                    # wait until CommandDone is set
                                   0x10,                    # mask
                                   10000,                   # test 10000 times
                                   50,                      # read every 50 us
                                   $eepromBootErrJmpAddr1); # on error jump to ...
    }

    # step #6 (FIBM config)
    # Scheduler Ring initialization
    # Do not move this. See alta_initSchedulersRing() description.
    if ($altaFIBMconfig)
    {
        alta_initSchedulersRing();
    }

    # step #7 (min PCIe & FIBM)
    # take modules out of reset
    describe_stage("Enable modules.");
    if ($altaFIBMconfig) {
        # FIBM config: take all the modules out of reset
        eeprom_insert_alta_write_register_command(lookup_addr("FM_SOFT_RESET"), 0);
    }
    else {
        # min PCIe: keep reset: MSB, FIBM, EPL modules
        #           release reset: PCIe and JSS modules
        eeprom_insert_alta_write_register_command(lookup_addr("FM_SOFT_RESET"), 0x16);
    }
    # delay 1 ms
    eeprom_insert_delay_command(1_000);

    # clear interrupts
    eeprom_insert_alta_write_register_command(lookup_addr("FM_LCI_IM"), 1);

    # release SBUS controller reset. This is required before configuring PCIe.
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SBUS_CFG"), 0);

    # step #8
    # Start PCIe (min PCIe & FIBM)
    # describe_stage("Start PCIe.");
    #  8.1 Overwrite Vendor ID
    if (!$altaFIBMconfig && $main::opt_Z )
    {
        if ( defined $altaPcieIdReg)
        {
            eeprom_insert_alta_write_register_command(lookup_addr("FM_PCI_CFG_ID"), $altaPcieIdReg);
        }
    }

    # eeprom_insert_alta_write_register_command(lookup_addr("FM_PCI_CFG_ID"), 0x17708086);

    #  8.2 Configure PCIe endianness (default is little endian)
    eeprom_insert_alta_write_register_command(lookup_addr("FM_PCI_ENDIANISM"), $altaPcieEndianness);

    #  8.3 Set PCI class
    eeprom_insert_alta_write_register_command(lookup_addr("FM_PCI_CFG_1"), 0x028000);
    #  8.4 Enable DMA
    eeprom_insert_alta_write_register_command(lookup_addr("FM_PCI_DMA_CFG"), 0x035);
    #  8.5 Clear any pending interrupt and set the mask register.
    eeprom_insert_alta_write_register_command(lookup_addr("FM_PCI_IP"), 0xFFFFFFFF);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_PCI_IM"), 0xFFFFFFFF);
    #  8.6 Serdes Lane Reversal & polarity options (if necessary)
    #      FIXME: check if this should be linked to a command line option
    #      (RxLaneReverse, TxLaneReverse) = bits 29,30
    if ($main::opt_X) {
        $regValue = $alta_pcie_lane_reversal[$main::opt_X];
        ($registerAddr, @regDefault) = lookup_addr_and_default(FM_PCI_SERDES_CTRL_1);
        $regValue |= shift @regDefault;
        eeprom_insert_alta_write_register_command($registerAddr, $regValue);
    }

    #  8.7 Enable link setting CoreEnable (bit 16) in FM_PCI_CORE_CTRL_1
    ($registerAddr, @regDefault) = lookup_addr_and_default(FM_PCI_CORE_CTRL_1);
    $regValue = shift @regDefault | (0x1<<16);
    eeprom_insert_alta_write_register_command($registerAddr, $regValue);

    # step #9 (min PCIe & FIBM) ??
    # SBUS initialization.
    # Note: The SBUS controller reset was released in the step #7    
    alta_eeprom_SBUS_init();

    # step #10 (FIBM)
    if ($altaFIBMconfig) {
        # Initialize Memory
        describe_stage("Initialize memory.");
        # init special tables (they are managed using CRM)
        #  9.1 init (fill with 00) memory tables
        alta_initMemTables();
        #  9.2 BEM block init
        alta_init_FC_BEM();
        #  9.3 Configure ports
        alta_configurePorts();
        ##  From now on, registers with  value = default will be ignored
        #  9.4 init single registers w/values coming from register dump.
        process_all_single_registers();
        #  9.5 init port related registers 
        process_all_port_registers();
        #  9.6 init array registers w/values coming from register dump.
        process_all_array_registers();

        # set FFU_ATOMIC_APPLY
        eeprom_insert_alta_write_register_command( lookup_addr("FM_FFU_ATOMIC_APPLY"), 0x03);
    }

    # step #11 (FIBM)
    # if command line option -L was specified, load SPICO controller code.
    #  FIXME: check if SPICO loading is in the correct place in the startup sequence.
    if ($altaFIBMconfig && $main::opt_L) {
        alta_load_spico_code();
    }

    # step #12 (min PCIe)
    # write the eeprom ID into the SCRATCH register
    if (!$altaFIBMconfig && $main::opt_Z )
    {
        if ( defined $altaMgmtScratchReg)
        {
            eeprom_insert_alta_write_register_command(lookup_addr("FM_MGMT_SCRATCH",0), $altaMgmtScratchReg);
        }
    }

    # step #13 (min PCIe & FIBM)
    # Insert an End Record and add the image selection table (eeprom header)
    # if requested. 
    eeprom_insert_finish_command();
    eeprom_insert_finish_command();
    eeprom_insert_finish_command();

    # create eeprom header
    if (defined $main::opt_H ) {
        alta_generate_eeprom_header();
    }
}

##@method       alta_validatePortSpec
#
# @brief        Validates the port parameters. The parameters may be either
#               manadatory or optional. In some cases this clasification 
#               depends of the port mode. Mandatory parameters are checked 
#               first. On the other hand, optional parameters, if they are
#               defined, must be coherent with the specified mode.
#
sub alta_validatePortSpec
{
    my ($logicalPort,
        $physicalPort_ref,
        $portMode_ref,
        $portSpeed_ref,
        $laneNum_ref,
        $laneReversal_ref,
        $lanePolarity_ref,
        $activeMac_ref,
        $dfeMode_ref,
        $defaultPortSpeed_ref)  = @_;

    # Check physical port
    die "ERROR: physical port not defined\n" if ${$physicalPort_ref} == -1;
    die "ERROR: invalid physical port ${$physicalPort_ref} \n" if ${$physicalPort_ref} > 75;
    die "ERROR: invalid logical port $logicalPort \n" if $logicalPort > 75;
    die "ERROR: logical port already defined\n" if $alta_portMappingTable[$logicalPort] != -1;

    # mode
    die "ERROR: mode not defined\n" if ${$portMode_ref} == PORT_MODE_UNDEFINED;

    # port speed is only mandatory for SGMII mode
    # for other modes, if it is defined, it must be equal to the default value.
    if (${$portMode_ref} == PORT_MODE_SGMII) {
        die "ERROR: port speed is mandatory for SGMII mode\n" if (${$physicalPort_ref} == PORT_SPEED_NONE);
    } elsif ( ${$physicalPort_ref} == PORT_SPEED_NONE ){
        ${$portSpeed_ref} = ${$defaultPortSpeed_ref};
    } else {
        die "ERROR: speed incompatible with port mode (Port = $logicalPort)" if (${$portSpeed_ref} != ${$defaultPortSpeed_ref});
    }

    # active mac
    if ($alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_ACTIVE_MAC] == -1) {
        print STDERR "WARNING: no active MAC selection is supported by this port ($logicalPort)\n" if (${$activeMac_ref} != -1);
    } else {
        die "ERROR: invalid \"active MAC\" specification\n" if (${$activeMac_ref} < 0 || ${$activeMac_ref} > 1);
    }

    return 0;
}

##@method       alta_setPortParameters
#
# @brief        Adds the defined port parameters into the alta port table.
#
sub alta_setPortParameters
{
    my ($logicalPort,
        $physicalPort_ref,
        $portMode_ref,
        $portSpeed_ref,
        $laneNum_ref,
        $laneReversal_ref,
        $lanePolarity_ref,
        $activeMac_ref,
        $dfeMode_ref,
        $sbusCfgSet_ref,
        $loopback_ref,
        $mask_ref)          = @_;

    # set the port mapping
    $alta_portMappingTable[$logicalPort] = ${$physicalPort_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_LOGICAL_PORT] = $logicalPort;

    # set port parameters
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_ACTIVE_MAC]    = ${$activeMac_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_PORT_MODE]     = ${$portMode_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_PORT_SPEED]    = ${$portSpeed_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_DFE_MODE]      = ${$portSpeed_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_LANE_REVERSAL] = ${$laneReversal_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_LANE_POLARITY] = ${$lanePolarity_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_LANE_NUM]      = ${$laneNum_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_SBUS_CFG_SET]  = ${$sbusCfgSet_ref};
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_LOOPBACK]      = ${$loopback_ref};
    if (defined $mask_ref) {
        if (${$mask_ref} ne "") {
            $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_MASK]          = ${$mask_ref};
        }
        if (${$physicalPort_ref} < 32) {
            $alta_mask_lw |= 1 << ${$physicalPort_ref};
        }
        elsif (${$physicalPort_ref} < 64) {
            $alta_mask_mw |= 1 << (${$physicalPort_ref} - 32);
        }
        elsif (${$physicalPort_ref} < 76) {
            $alta_mask_hw |= 1 << (${$physicalPort_ref} - 64);
        }
    }
    
    # set the EPL, lane and SerDes
    if ($alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_ACTIVE_MAC] == 1) {
        $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_EPL]++;
    }
    my $epl     = $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_EPL];
    my $channel = $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_CHANNEL];
    my $lane    = $alta_EplChnlToLaneMapTable[$epl][$channel];
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_LANE] = $lane;
    $alta_port_def_table[${$physicalPort_ref}][ALTA_PORT_TABLE_ENTRY_SERDES] = $alta_SerdesToEplMapTable[$epl] + $lane;

    # if it is a 4 lanes port, set the following 3 physical ports in the port table.
    if (${$laneNum_ref} == 4) {
        my $nextPhysicalPort = ${$physicalPort_ref};
        # the mode of the following ports is set to DISABLED
        my $nextPortMode = PORT_MODE_DISABLED;
        # lane number is set to 0 (to avoid an infinite recursion loop)                      
        my $nextPortLaneNum = 0;                                    
        for (my $i = 1; $i <= 4; $i++) {
            $nextPhysicalPort++;                                    # inc physical port
            alta_setPortParameters( $logicalPort,                   # use the same logical port
                                    \$nextPhysicalPort,
                                    \$nextPortMode,
                                    $portSpeed_ref,
                                    \$nextPortLaneNum,
                                    $laneReversal_ref,
                                    $lanePolarity_ref,
                                    $activeMac_ref,
                                    $dfeMode_ref,
                                    $sbusCfgSet_ref,
                                    $loopback_ref);
        }
    }
}

##@method       alta_setEplCfg
#
# @brief        configure FM_EPL_CFG_A and FM_EPL_CFG_B for the specified
#               epl and setting the reverse tx/rx lanes bits.
#
sub alta_setEplCfg
{
    my ($epl, $laneReverse) = @_;

    # fix the $laneReverse order definition
    if ($laneOrderingTable[$epl]) {
        $laneReverse = (~$laneReverse) & 0x03;
    }

    # set EPL_CFG_A
    my $registerAddr = lookup_addr(FM_EPL_CFG_A,$epl);
    my $regValue = lookup_reg($registerAddr) & ~(0x3<<8);

    if ($laneReverse & LANE_REVERSAL_RX) {
        $regValue |= 1 << 9;
    }
    if ($laneReverse & LANE_REVERSAL_TX) {
        $regValue |= 1 << 8;
    }
    eeprom_insert_alta_write_register_command($registerAddr, $regValue);

    # set EPL_CFG_B
    process_array1_reg("FM_EPL_CFG_B",[$epl], undef, 1);
}

##@method       alta_setPortMask
#
# @brief        configure the mask for the given port.
#               The value to be used as mask is controlled by the 'mask' parameter
#               of the port definition in the eeprom-config file.
#               The different possiblities are:
#               1- No mask is defined for the given port ($mask eq "")
#                  The general port mask is used.
#               2- The mask is defined as a serie of logical ports: the ports are
#                  mapped into physical ports and used to set the mask.
#               3- The mask is defined as "tp": in this case the API mask value is
#                  used
#
sub alta_setPortMask
{
    my ($physicalPort, $logicalPort, $mask) = @_;

    my $mask_lw = 0;
    my $mask_mw = 0;
    my $mask_hw = 0;


    if ($mask =~ /tp/) {
        # mask value is 'tp': use the API value
        process_array2_reg("FM_L2F_TABLE_256",[$physicalPort],[$altaL2fTablePortMask], undef, 1);
    } else {
        if ($mask eq "") {
            # no mask was defined, use the general mask
            $mask_lw = $alta_mask_lw;
            $mask_mw = $alta_mask_mw;
            $mask_hw = $alta_mask_hw;

        } else {
            # use the mask defined in the configuration file
            my @maskList = split(/ /,$mask);
            foreach my $destPort (@maskList) {
                my $phyDestPort = $alta_portMappingTable[$destPort];
                if ($phyDestPort < 1) {
                    next;
                }
                elsif ($phyDestPort < 32) {
                    $mask_lw |= 1 << $phyDestPort;
                }
                elsif ($phyDestPort < 64) {
                    $mask_mw |= 1 << $phyDestPort;
                }
                elsif ($phyDestPort < 76) {
                    $mask_hw |= 1 << $phyDestPort;
                }
                else {
                    die "Invalid mask value for port $logicalPort\n";
                }
            }
        }
        my $registerAddr = lookup_addr(FM_L2F_TABLE_256,$altaL2fTablePortMask,$physicalPort);
        eeprom_insert_alta_write_register_command($registerAddr+0, $mask_lw);
        eeprom_insert_alta_write_register_command($registerAddr+1, $mask_mw);
        eeprom_insert_alta_write_register_command($registerAddr+2, $mask_hw);
    }
}

##@method       alta_configurePorts
#
# @brief        configure the ports using the information stored in the
#               alta_port_def_table.
#
sub alta_configurePorts
{
    my @eplList  = ((0) x 25);
    
    # first pass: EPL level configuration
    foreach my $portEntry (@alta_port_def_table)
    {
        my ($phyPort, 
            $logPort, 
            $actMac, 
            $portMode, 
            $portSpeed, 
            $dfeMode, 
            $laneRev, 
            $lanePol, 
            $epl, 
            $channel, 
            $lane, 
            $serdes, 
            $laneNum,
            $sbusCfgSet,
            $loopback,
            $mask)              =  @$portEntry;

        next if ($logPort < 0);

        if ($eplList[$epl] == 0)
        {
            if ($actMac == 0 || $actMac == 1) {
                my $pEpl = $epl - $actMac;
                for (my $i = 0; $i < 2; $i++) {
                    $eplList[$pEpl] = 1;
                    alta_setEplCfg($pEpl,$laneRev);
                    $pEpl++;
                }

            }
            else {
                $eplList[$epl] = 1;
                alta_setEplCfg($epl,$laneRev);
            }
        }

        # set FM_LANE_CFG

        my $registerAddr = lookup_addr(FM_LANE_CFG,$epl,$lane);
        my $regValue = lookup_reg($registerAddr) | (0x3<<23);
        eeprom_insert_alta_write_register_command($registerAddr, $regValue);

        # set MAC_CFG[epl][channel]
        process_array2_reg("FM_MAC_CFG",[$channel],[$epl],undef,1);

        # set FM_LANE_CFG
        process_array2_reg("FM_LANE_CFG",[$lane],[$epl],undef,1);

        # config SERDES
        process_array2_reg("FM_SERDES_SIGNAL_DETECT",[$lane],[$epl], undef, 1);

        # SERDES_RX_CFG
        $registerAddr = lookup_addr(FM_SERDES_RX_CFG,$epl,$lane);
        $regValue = lookup_reg($registerAddr) & ~((0x1<<25) | (1 << 20));

        if ($loopback == 0) {
            $regValue |= 1 << 20;

            if ($lanePol & LANE_POLARITY_INV_RX) {
                $regValue |= 1 << 25;
            }
        }

        eeprom_insert_alta_write_register_command($registerAddr, $regValue);

        # SERDES_TX_CFG
        $registerAddr = lookup_addr(FM_SERDES_TX_CFG,$epl,$lane);
        $regValue = lookup_reg($registerAddr) & ~(0x1<<30);

        if ($lanePol & LANE_POLARITY_INV_TX) {
            $regValue |= 1 << 30;
        }
        eeprom_insert_alta_write_register_command($registerAddr, $regValue);
        $regValue = lookup_reg($registerAddr+1);
        eeprom_insert_alta_write_register_command($registerAddr+1, $regValue);

        # SERDES_CFG
        $registerAddr = lookup_addr(FM_SERDES_CFG,$epl,$lane);
        $regValue = lookup_reg($registerAddr) & ~(0x1<<28);

        if ($loopback) {
            $regValue |= 1 << 28;
        }
        eeprom_insert_alta_write_register_command($registerAddr, $regValue);

        $regValue = lookup_reg($registerAddr+1);
        eeprom_insert_alta_write_register_command($registerAddr+1, $regValue);

        if ($portMode == PORT_MODE_SGMII) {

            # SGMII_AN_TX_CONFIG_LOOPBACK
            $registerAddr = lookup_addr(FM_SGMII_AN_TX_CONFIG_LOOPBACK,$epl,$channel);
            $regValue = 0x4001;
            eeprom_insert_alta_write_register_command($registerAddr, $regValue);

            $regValue = 0;
            eeprom_insert_alta_write_register_command($registerAddr+1, $regValue);

            # SGMII_AN_TIMER_CFG
            $registerAddr = lookup_addr(FM_SGMII_AN_TIMER_CFG,$epl,$channel);
            $regValue = 0x103;
            eeprom_insert_alta_write_register_command($registerAddr, $regValue);

            # AN_37_CFG
            $registerAddr = lookup_addr(FM_AN_37_CFG,$epl,$channel);
            $regValue = 0x1;
            eeprom_insert_alta_write_register_command($registerAddr, $regValue);
        }

        # Set port mask
        alta_setPortMask($phyPort,$logPort,$mask);

        # configure SBUS 
        my $sbusAddr = $serdes + 5;
        my $sbusCfgSet_ref;

        if ($sbusCfgSet == SBUS_CONFIG_1000BASEX) {
            $sbusCfgSet_ref = \%alta_serdesCfg_1000BaseX;
            # configure scrambler/desrambler and link status mask
            process_array2_reg("FM_PCS_1000BASEX_CFG",[$channel],[$epl], undef, 1);
        } elsif ($sbusCfgSet == SBUS_CONFIG_10GBASER) {
            $sbusCfgSet_ref = \%alta_serdesCfg_10GBaseR;
            # configure scrambler/desrambler and link status mask
            process_array2_reg("FM_PCS_10GBASER_CFG",[$channel],[$epl], undef, 1);
        } elsif ($sbusCfgSet == SBUS_CONFIG_10GBASEX) {
            $sbusCfgSet_ref = \%alta_serdesCfg_10GBaseX;
            # configure scrambler/desrambler and link status mask
            process_array2_reg("FM_PCS_10GBASEX_CFG",[$channel],[$epl], undef, 1);
        } elsif ($sbusCfgSet == SBUS_CONFIG_40BASER) {
            # check if the port is defined in the 40G port capable list
            die "ERROR: port $logPort is not 40G capable\n" if alta_check40Gport($logPort);
            $sbusCfgSet_ref = \%alta_serdesCfg_40GBaseR;
            # configure scrambler/desrambler and link status mask
            process_array1_reg("FM_PCS_40GBASER_CFG",[$epl], undef, 1);
        } else {
            die "ERROR: invalid SBUS configuration set\n";
        }

        # Configure REFCLK and bit rate via SBUS
        alta_eeprom_SBUS_write_access($sbusAddr, 0, ($sbusCfgSet_ref->{"refSel"} << 1) | 1);
        # disable Tx phase calibration
        alta_eeprom_SBUS_write_access($sbusAddr, 29, 0x00);
        # set tx-rate-sel-cntl and tx-rate-sel-cntl
        alta_eeprom_SBUS_write_access($sbusAddr, 54, $sbusCfgSet_ref->{"xxRateSel"});
        alta_eeprom_SBUS_write_access($sbusAddr, 59, $sbusCfgSet_ref->{"xxRateSel"});

        # FIXME: this config remains hardcoded, but it must be updated when
        # to the port configuration is completed.
        if ($sbusCfgSet == SBUS_CONFIG_10GBASER)
        {
            alta_eeprom_SBUS_write_access($sbusAddr, 90, 1);
            alta_eeprom_SBUS_write_access($sbusAddr, 99, 6);
        }

        # set rx_en and tx_en
        alta_eeprom_SBUS_write_access($sbusAddr, 34, 0x3);       
    }
    
    # complete processing of L2F_TABLE_256
    for (my $i= 0; $i <4; $i++) {
        next if $i == $altaL2fTablePortMask;
        for (my $j= 0; $j <=75; $j++) {
            process_array2_reg("FM_L2F_TABLE_256",[$j],[$i]);
        }
    }
}


##@method       alta_parseEepromConfigFile
#
# @brief        Parse the eeprom configuration file.
#
sub alta_parseEepromConfigFile
{
   my ($filename, $commandLine_ref) = @_;
   my $state = 0;
   my $logicalPort;
   my $physicalPort;
   my $portMode;
   my $portSpeed;
   my $laneNum;
   my $laneReversal;
   my $lanePolarity;
   my $activeMac;
   my $dfeMode;
   my $sbusCfgSet;
   my $defaultPortSpeed;
   my $loopback = 0;
   my $mask = "";

   die "Eeprom configuration file ($filename) does not exists\n" if (! -e $filename);

   open FILE, "<$filename" or die "Unable to open file $filename";

  while (<FILE>)
  {
      # skip comment and empty lines
      next if (/^#/);
      next if (/^s*$/);
      chomp;
      # remove leading and trailing spaces
      $_ =~ s/^\s+//;
      $_ =~ s/\s+$//;

      # check for the begining of a a new section
      if (/^\[/) {
          if ($state == 1 && defined $physicalPort)
          {
              # validate port specification
              alta_validatePortSpec($logicalPort,
                                    \$physicalPort,
                                    \$portMode,
                                    \$portSpeed,
                                    \$laneNum,
                                    \$laneReversal,
                                    \$lanePolarity,
                                    \$activeMac,
                                    \$dfeMode,
                                    \$defaultPortSpeed);
              # add port information to the table
              alta_setPortParameters($logicalPort,
                                     \$physicalPort,
                                     \$portMode,
                                     \$portSpeed,
                                     \$laneNum,
                                     \$laneReversal,
                                     \$lanePolarity,
                                     \$activeMac,
                                     \$dfeMode,
                                     \$sbusCfgSet,
                                     \$loopback,
                                     \$mask);
              $physicalPort = undef;
          }
          $state = 0;
      }

      if ($state == 0)
      {
          # state 0: only logical port sections are allowed
          if (/\[port\s+\d+\]/) {
              my @tokens = split;
              $logicalPort = $tokens[1] + 0;
              $state = 1;
              # initialize some variables
              $physicalPort = -1;
              $portMode = PORT_MODE_UNDEFINED;
              $activeMac = -1;
              $portSpeed = PORT_SPEED_NONE;
              $laneReversal = LANE_REVERSAL_NONE;
              $lanePolarity = LANE_POLARITY_NORMAL;
              $loopback = 0;
              $mask= "";
              next;
          }
          elsif (/\[40GportList\]/) {
              $state = 2;
              next;
          }
          elsif (/\[10GportList\]/) {
              $state = 3;
              next;
          }
          elsif (/\[commandLine\]/) {
              $state = 4;
              next;
          }
          elsif (/\[end\]/) {
              $state = 5;
              next;
          }
          else
          {
              die "Invalid port configuration file\n";
          }
      }
      elsif ($state == 1)
      {
          # port definition
          my ($parameter,$value) = split(/:/);
          $value =~ s/^\s+//;
          if ($parameter =~ /^physicalPort/) {
              die "Invalid physical port value\n" if ($value !~ /\d+/);
              $physicalPort = $value + 0;
              next;
          }
          elsif ($parameter =~ /^mode/) {
              my ($entry) = $alta_portMode_hash{$value};
              die "Invalid port $logicalPort mode: \"$value\"\n" if not defined $entry;
              ($portMode, $defaultPortSpeed, $laneNum, $dfeMode, $sbusCfgSet) = @$entry;
              next;
          }
          elsif ($parameter =~ /^speed/) {
              $portSpeed = $alta_portSpeed_hash{$value};
              die "Invalid port $logicalPort speed\n" if !defined $portSpeed;
              next;
          }
          elsif ($parameter =~ /^laneReversal/) {
              $value =~ s/^\s+//;
              $laneReversal = $alta_laneReversal_hash{$value};
              die "Invalid lane reversal spec for port $logicalPort\n" if !defined $laneReversal;
              next;
          }
          elsif ($parameter =~ /^lanePolarity/) {
              $value =~ s/^\s+//;
              $lanePolarity = $alta_lanePolarity_hash{$value};
              die "Invalid lane polarity spec for port $logicalPort\n" if !defined $lanePolarity;
              next;
          }
          elsif ($parameter =~ /^activeMac/) {
              die "Invalid active MAC value" if ($value !~ /\d+/);
              $activeMac = $value + 0;
              next;
          }
          elsif ($parameter =~ /^loopback/) {
              $value =~ s/^\s+//;
              $loopback = $on_off_hash{$value};
              die "Invalid loopback spec for port $logicalPort\n" if !defined $loopback;
              next;
          }
          elsif ($parameter =~ /^mask/) {
              $value =~ s/,/ /g;
              $value =~ s/^\s+//;
              $value =~ s/\s+/ /g;

              # valid values are: a) a list of logical ports or b) the string
              # "tp" (without quotes) to indicate that the API mask values must
              # be used.
              if ($value !~ /[d+\s]*/ && $value !~ /tp/) {
                  die "Invalid mask value for port $logicalPort\n";
              }
              $mask = $value;
              next;
          }
          else
          {
              die "Invalid port parameter\n";
          }
       }
       elsif ($state == 2 || $state == 3) {
           # lists of 40G capable and 10G capable ports
           my ($parameter,$value) = split(/:/);
           if ($parameter =~ /^ports/) {
               $value =~ s/^\s+//;
               my @portList = split(/,/, $value);

               foreach my $port (@portList) {
                   $port =~ s/^\s+//;
                   die "Invalid port specification\n" if ($port !~ /\d+\s*/);
                   if ($state == 2) {
                       push (@alta_list40GCapablePorts, ($port + 0));
                   }
                   else {
                       push (@alta_list10GCapablePorts, ($port + 0));
                   }
               }
               $state = 0;
               next;
           }
           else {
               die "Invalid port list\n" if ($parameter =~ /^ports/);
           }
       }
       elsif ($state == 4) {
           # command line
           my ($parameter,$value) = split(/:/);
           if ($parameter =~ /^options/) {
               # remove extra spaces
               $value =~ s/^\s+//;
               $value =~ s/\s+$//;
    
               ${$commandLine_ref} = $value;
               $state = 0;
               next;
           }
           else {
               die "Invalid command line options\n";
           }
       }
       elsif ($state == 5) {
           # ignore the rest of the file
           next;
       }
       else {
           # invalid state
           die "Invalid state reading Alta Port Configuration Port File\n";
       }
   }
   if ($debugOption) {
       printAltaPortTable();
   }
}

##@method       alta_check40Gport
#
# @brief        Check if a given port is in the list of 40G capable port
#               list.
#               
# @param[in]    $portToChk: logical port to check
# 
# @return       0 if the specified port is in the list of 40G capable ports
#              -1 otherwise
sub alta_check40Gport
{
    my ($portToChk) = @_;

    foreach my $port40G(@alta_list40GCapablePorts) {
        
        return 0 if ($port40G == $portToChk);
    }
    return -1;
}


##@method       alta_getSupportedPorts
#
# @brief        Fills the lists of 40G capable ports and 10G capable ports.
#               These lists are used to initialize the scheduler-ring.
#               Both lists are checked for range, alignment and duplication
#               errors.
#
sub alta_getSupportedPorts
{
    my ($listPorts10G_ref, $listPorts40G_ref) = @_;

    # get the list of 40G and 10G capable ports and perform the following tests:
    #  a: 40G port range
    #  b: 40G port alignment
    #  c: 10G port range
    #  d: port duplication
    #
    # Use an auxiliary array and the following values to code the port status:
    #  0: port available
    #  1: 10G port
    #  2: 40G port
    #  3: unavailable (taken by a 40G port)

    my @testPort = ((0) x 72);

    # process 40G ports
    die "Error: too many 40G ports" if @alta_list40GCapablePorts > 18;
    foreach my $port40G(@alta_list40GCapablePorts) {
        my $phyPort = $port40G;
        if ($listXXGCapablePorts_are_logicals) {
            $phyPort = $alta_portMappingTable[$port40G];
        }

        # check the range and alignment
        die "Error: out of range 40G port: $port40G" if (!defined $phyPort || 
                                                         $phyPort < 4 ||
                                                         $phyPort > 72);
        die "Error: bad alignmet of 40G port: $port40G" if ($phyPort & 0x03);
        # check if the port was already used
        die "Error: duplicated 40G port: $port40G" if ( $testPort[$phyPort] ||
                                                        $testPort[$phyPort + 1] ||
                                                        $testPort[$phyPort + 2] ||
                                                        $testPort[$phyPort + 3]);
        # mark the port as used and add it to the list
        $testPort[$phyPort] = 2;
        $testPort[$phyPort+1] = 3;
        $testPort[$phyPort+2] = 3;
        $testPort[$phyPort+3] = 3;
        push @{$listPorts40G_ref}, $phyPort;
    }

    # process 10G ports
    foreach my $port10G(@alta_list10GCapablePorts) {
        my $phyPort = $port10G;
        if ($listXXGCapablePorts_are_logicals) {
            $phyPort = $alta_portMappingTable[$port10G];
        }

        # check the range
        die "Error: out of range 10G port: $port10G" if (!defined $phyPort);

        # check the alignment and if the port was already used
        die "Error: duplicated 10G port: $port10G" if ( $testPort[$phyPort] != 0);

        # mark the port as used and add it to the list
        $testPort[$phyPort] = 1;
        push @{$listPorts10G_ref}, $phyPort;
    }
}


sub alta_selectFreePort
{
    my ($ports40GcolorList_ref, $ports10GcolorList_ref, $color) = @_;


    my $portToTest = $color << 2;
    if ($color == 0)
    {
        $portToTest += 0x10;
    }
    
    while ($portToTest < 76 )
    {
        if ((($portToTest >> 2) & 0x3) != $color)
        {
            $portToTest += 4;
            next;
        }

        my $FoundedPort = 0;

        foreach (@{$ports40GcolorList_ref})
        {
            if ($_ == $portToTest)
            {
                $FoundedPort = 1;
                last;
            }
        }
        if (! $FoundedPort)
        {
            foreach (@{$ports10GcolorList_ref})
            {
                if ($_ == $portToTest)
                {
                    $FoundedPort = 1;
                    last;
                }
            }
        }
        if (!$FoundedPort)
        {
            return $portToTest;
        }
        else
        {
            $portToTest++;
        }
    }
    # no port was found
    return -1
}

##@method       alta_initSchedulersRing
#
# @brief        Initializes both, Tx and Rx scheduler rings
#               This initialization MUST be performed after free list have been
#               initialized.
#               This initialization can only be done ONCE after reset,so this
#               function MUST be specifically excluded from the short eeprom
#               boot sequence.
#               See FM6000 Functional Specification, ch 22.
#
sub alta_initSchedulersRing
{
    my @ArrayColorsPorts10G = ();
    my @ArrayColorsPorts40G = ();
    my @listPorts10G = ();
    my @listPorts40G = ();


    # initialize arrays
    @{$ArrayColorsPorts10G[0]} = ();
    @{$ArrayColorsPorts10G[1]} = ();
    @{$ArrayColorsPorts10G[2]} = ();
    @{$ArrayColorsPorts10G[3]} = ();

    @{$ArrayColorsPorts40G[0]} = ();
    @{$ArrayColorsPorts40G[1]} = ();
    @{$ArrayColorsPorts40G[2]} = ();
    @{$ArrayColorsPorts40G[3]} = ();


    # Get the list of ports
    # No validation are performed here, the lists must be validated by the
    # called method.
    alta_getSupportedPorts(\@listPorts10G, \@listPorts40G);


    # fill the coloring tables
    foreach my $port40G (@listPorts40G)
    {
        my $color = ($port40G >> 2) & 0x3;
        push @{$ArrayColorsPorts40G[$color]}, $port40G;
    }

    foreach my $port10G (@listPorts10G)
    {
        my $color = ($port10G >> 2) & 0x3;
        push @{$ArrayColorsPorts10G[$color]}, $port10G;
    }


    for (my $color = 0; $color < 4; $color++)
    {
        my $tokenToAdd = 20;
        $tokenToAdd -= 4 if ($color == 0 || $color == 3);
        $tokenToAdd -= (4 * @{$ArrayColorsPorts40G[$color]} + @{$ArrayColorsPorts10G[$color]});
        
        while ($tokenToAdd-- > 0)
        {
            my $port = alta_selectFreePort($ArrayColorsPorts40G[$color],
                                           $ArrayColorsPorts10G[$color],
                                           $color);
            push @{$ArrayColorsPorts10G[$color]}, $port;
        }
    }
    
    # Init Schedulers
    
    # Locked token at slow ports [0..3] are shared.
    # Setup next port tokens for slow ports.

    # nextPort array has 20 elements, all of them are initialized with 0 exept
    # the first one.
    my @nextPort = ( 1 | (2<<8) | (3<<16),
                       0, 0, 0, 0,
                    0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0);

    # Two cases are considered: 64 and 72 ports. The difference is that the
    # first one has two less colors per rotation.
    my $loopMax = 16;
    if ( (4 * @listPorts40G + @listPorts10G) == 72)
    {
        # 72 ports case
        $loopMax = 18;
    }

    for (my $i = 0; $i < 4; $i++)
    {
        for (my $j = 0; $j < $loopMax; $j++)
        {
            my $token;
            my $color = ($j+1) & 0x3;
            if ($j/4 < @{$ArrayColorsPorts40G[$color]})
            {
                $token = $i + ${$ArrayColorsPorts40G[$color]}[$j/4];
            }
            else
            {
                $token = pop @{$ArrayColorsPorts10G[$color]};
            }
            my $offset = $token & 0x3;
            my $index = $token >> 2;
            $nextPort[$index] = (($token & 0xff) << ($offset * 8)) |
                                ((~(0xff << ($offset * 8)) & $nextPort[$index]));
            eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_TX_INIT_TOKEN"),
                                                      $token | (1 << 9));
            eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_RX_INIT_TOKEN"), 
                                                      $token | (1 << 9));
        }
    }

    # Program next port configuration to lock the tokens in the ring
    $nextPort[19] |= (78<<16);

    # unused ring spacing token
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_RX_INIT_TOKEN"), 0x24e);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_TX_INIT_TOKEN"), 0x24e);

    # slow ports
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_RX_INIT_TOKEN"), 0x200);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_RX_INIT_TOKEN"), 0x600);

    eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_RX_SLOW_PORT",0), 0x0f);

    for (my $i = 0; $i < 20 ; $i++)
    {
        eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_RX_NEXT_PORT", $i), $nextPort[$i]);
        eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_TX_NEXT_PORT", $i), $nextPort[$i]);
    }

    eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_RX_INIT_COMPLETE"), 1);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SSCHED_TX_INIT_COMPLETE"), 1);

    # set SchedPeriod = 8. Value derived from: 67.6ns / 125 MHz
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SWEEPER_CFG"),
                                              (0x00000000,       # lsw
                                               0x00000000,
                                               0x00000000,
                                               0x00000000,
                                               0x00000008));     # msw
}


##@method alta_mrlRegisterFix
#
# @brief  Use scan to initialize some inaccessible registers in MRL
#
#
sub alta_mrlRegisterFix()
{
    my $ctrlReg_addr      = lookup_addr("FM_SCAN_CONTROL");
    my $cfgDataReg_addr   = lookup_addr("FM_SCAN_CONFIG_DATA_IN");
    my $chainDataReg_addr = lookup_addr("FM_SCAN_CHAIN_DATA_IN");
    my $dataOutReg_addr   = lookup_addr("FM_SCAN_DATA_OUT");
    my $statusReg_addr    = lookup_addr("FM_SCAN_STATUS");
    my $delay = 100;

    # Permit Scan Operations
    eeprom_insert_alta_write_register_command($ctrlReg_addr, 0x10);
    eeprom_insert_delay_command($delay);

    my $mrlFixScanTableSize = $#alta_mrlFixScanTable + 1;

    for (my $index = 0; $index < $mrlFixScanTableSize; $index++) {
        my $ctrlIn = $alta_mrlFixScanTable[$index][0];
        my $dataIn = $alta_mrlFixScanTable[$index][1];

        eeprom_insert_alta_write_register_command($ctrlReg_addr, ($ctrlIn & 0x1f));

        if ($ctrlIn & 0x80) {
            # config cycle
            eeprom_insert_alta_write_register_command($cfgDataReg_addr, $dataIn);
        } else {
            # scan cycle
            eeprom_insert_alta_write_register_command($chainDataReg_addr, $dataIn);
        }
    }
    
    eeprom_insert_poll_command($statusReg_addr,
                               0x100,                   # must be ReqCount = RespCount
                               0x0300,                  # mask
                               50,                      # test 50 times
                               1000,                    # read every 1 ms
                               $eepromBootErrJmpAddr3); # on error jump to ...

    # Change back to M2S Special mode so the chip can enter
    # normal operating mode in the usual way.
    eeprom_insert_alta_write_register_command($cfgDataReg_addr, 0x80000040);

    # 40 ms delay
    eeprom_insert_delay_command(40_000);
}


##@method alta_initMemTables()
#
# @brief        Initializes the specified tables with their default values.
#
#
sub alta_initMemTables()
{
    # Specific table information such as the number of indexes,
    # the indexes range and the default values are gathered from
    # the register hash table.
    # If necessary, special initialization values may be specified as a second
    # parameter of eeprom_iniTab().
    # Many tables could already be initialized by BIST, so their initialization
    # is conditional.

    # Parser:
    altaCRMInitTab("FM_PARSER_RAM") if !$altaInitMemBist;
    altaCRMInitTab("FM_PARSER_CAM");
    altaCRMInitTab("FM_PARSER_INIT_STATE") if !$altaInitMemBist;
    altaCRMInitTab("FM_PARSER_INIT_FIELDS") if !$altaInitMemBist;
    # Mapper:
    altaCRMInitTab("FM_MAPPER_VID1_TABLE") if !$altaInitMemBist;
    altaCRMInitTab("FM_MAPPER_VID2_TABLE") if !$altaInitMemBist;
    altaCRMInitTab("FM_MAPPER_SRC_PORT_TABLE");
    altaCRMInitTab("FM_MAPPER_DMAC_CAM1");
    altaCRMInitTab("FM_MAPPER_DMAC_CAM2");
    altaCRMInitTab("FM_MAPPER_DMAC_CAM3");
    altaCRMInitTab("FM_MAPPER_DMAC_RAM1");
    altaCRMInitTab("FM_MAPPER_DMAC_RAM2");
    altaCRMInitTab("FM_MAPPER_DMAC_RAM3");
    altaCRMInitTab("FM_MAPPER_SMAC_CAM1");
    altaCRMInitTab("FM_MAPPER_SMAC_CAM3");
    altaCRMInitTab("FM_MAPPER_SMAC_RAM1");
    altaCRMInitTab("FM_MAPPER_SMAC_RAM3");
    altaCRMInitTab("FM_MAPPER_TYPE_CAM1");
    altaCRMInitTab("FM_MAPPER_TYPE_CAM2");
    altaCRMInitTab("FM_MAPPER_TYPE_RAM1");
    altaCRMInitTab("FM_MAPPER_TYPE_RAM2");
    altaCRMInitTab("FM_MAPPER_DIP_CAM1");
    altaCRMInitTab("FM_MAPPER_DIP_CAM2");
    altaCRMInitTab("FM_MAPPER_DIP_CAM3");
    altaCRMInitTab("FM_MAPPER_SIP_CAM1");
    altaCRMInitTab("FM_MAPPER_SIP_CAM2");
    altaCRMInitTab("FM_MAPPER_SIP_CAM3");
    altaCRMInitTab("FM_MAPPER_DIP_RAM1");
    altaCRMInitTab("FM_MAPPER_DIP_RAM2");
    altaCRMInitTab("FM_MAPPER_DIP_RAM3");
    altaCRMInitTab("FM_MAPPER_SIP_RAM1");
    altaCRMInitTab("FM_MAPPER_SIP_RAM2");
    altaCRMInitTab("FM_MAPPER_SIP_RAM3");

    altaCRMInitTab("FM_MAPPER_PROT_CAM1");
    altaCRMInitTab("FM_MAPPER_PROT_CAM2");
    altaCRMInitTab("FM_MAPPER_PROT_RAM1");
    altaCRMInitTab("FM_MAPPER_PROT_RAM2");
    altaCRMInitTab("FM_MAPPER_L4_DST_COMPARE");
    altaCRMInitTab("FM_MAPPER_QOS_CAM1");
    altaCRMInitTab("FM_MAPPER_QOS_CAM2");
    altaCRMInitTab("FM_MAPPER_QOS_RAM1");
    altaCRMInitTab("FM_MAPPER_QOS_RAM2");

    # FFU
    altaCRMInitTab("FM_FFU_BST_SCENARIO_CAM");
    altaCRMInitTab("FM_FFU_BST_ACTION") if !$altaInitMemBist;
    altaCRMInitTab("FM_FFU_BST_KEY") if !$altaInitMemBist;
    altaCRMInitTab("FM_FFU_BST_SCENARIO_CFG1");
    altaCRMInitTab("FM_FFU_BST_SCENARIO_CFG2");
    altaCRMInitTab("FM_FFU_BST_ROOT_KEYS");

    altaCRMInitTab("FM_FFU_SLICE_CAM");
    altaCRMInitTab("FM_FFU_SLICE_ACTION") if !$altaInitMemBist;
    altaCRMInitTab("FM_FFU_SLICE_SCENARIO_CAM");
    altaCRMInitTab("FM_FFU_SLICE_SCENARIO_CFG");
    altaCRMInitTab("FM_FFU_REMAP_SCENARIO_CAM");
    altaCRMInitTab("FM_HASH_LAYER3_PTABLE") if !$altaInitMemBist;
    # NextHop
    altaCRMInitTab("FM_NEXTHOP_WIDE") if !$altaInitMemBist;
    # Hash
    altaCRMInitTab("FM_HASH_LAYER2_ROTA_PTABLE") if !$altaInitMemBist;
    altaCRMInitTab("FM_HASH_LAYER2_ROTB_PTABLE") if !$altaInitMemBist;
    # L3AR
    altaCRMInitTab("FM_L3AR_CAM");
    # L2L_MAC
    altaCRMInitTab("FM_L2L_MAC_TABLE") if !$altaInitMemBist;
    # L2L
    altaCRMInitTab("FM_L2L_LOCK_TABLE") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2L_EVID1_TABLE") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2L_IVID1_TABLE") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2L_EVID2_TABLE") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2L_IVID2_TABLE") if !$altaInitMemBist;
    # L2L_SWEEPER
    altaCRMInitTab("FM_L2L_SWEEPER_FIFO") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2L_SWEEPER_CAM");
    # L2F
    altaCRMInitTab("FM_L2F_TABLE_4K") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2F_TABLE_256") if !$altaInitMemBist;
    # GLORT
    altaCRMInitTab("FM_GLORT_RAM") if !$altaInitMemBist;
    altaCRMInitTab("FM_GLORT_CAM");
    # POLICERS
    altaCRMInitTab("FM_POLICER_CFG_4K") if !$altaInitMemBist;
    altaCRMInitTab("FM_POLICER_CFG_1K") if !$altaInitMemBist;
    altaCRMInitTab("FM_POLICER_STATE_4K") if !$altaInitMemBist;
    altaCRMInitTab("FM_POLICER_STATE_1K") if !$altaInitMemBist;
    # EACL
    altaCRMInitTab("FM_EACL_CAM1");
    altaCRMInitTab("FM_EACL_CAM2");
    # LBS
    altaCRMInitTab("FM_LBS_CAM");
    # L2AR
    altaCRMInitTab("FM_L2AR_ACTION_CPU_CODE") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_TRAP_HEADER") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_MIRROR") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_QOS") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_MA_WRITEBACK") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_DGLORT") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_W16AB") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_W16CDEF") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_W8ABCDE") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_W4") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_VID") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_DMASK_IDX") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_STATS_IDX5AB") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_STATS_IDX5C") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_STATS_IDX12A") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_STATS_IDX12B") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_STATS_IDX16A") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_ACTION_STATS_IDX16B") if !$altaInitMemBist;
    altaCRMInitTab("FM_L2AR_CAM");
    altaCRMInitTab("FM_L2AR_CAM_DMASK");
    altaCRMInitTab("FM_L2AR_FLAGS_CAM");
    # CM
    altaCRMInitTab("FM_CM_QUEUE_STATE_INIT") if !$altaInitMemBist;
    altaCRMInitTab("FM_CM_PORT_RXMP_PRIVATE_WM", 0xffffffff);
    altaCRMInitTab("FM_CM_PORT_RXMP_HOG_WM", 0xffffffff);
    altaCRMInitTab("FM_CM_PORT_RXMP_PAUSE_ON_WM", 0xffffffff);
    altaCRMInitTab("FM_CM_PORT_RXMP_PAUSE_OFF_WM", 0xffffffff);
    altaCRMInitTab("FM_CM_TC_PC_MAP") if !$altaInitMemBist;
    altaCRMInitTab("FM_CM_PC_RXMP_MAP");
    altaCRMInitTab("FM_CM_PAUSE_CFG") if !$altaInitMemBist;
    altaCRMInitTab("FM_CM_PAUSE_PACING_CFG") if !$altaInitMemBist;
    altaCRMInitTab("FM_ERL_CFG", 0x20001000) if !$altaInitMemBist;
    altaCRMInitTab("FM_ERL_CFG_IFG");
    altaCRMInitTab("FM_CM_PORT_TXMP_HOG_WM", 0x00003fff) if !$altaInitMemBist;
    # CMM
    altaCRMInitTab("FM_CM_PORT_TXMP_IP_WM");
    altaCRMInitTab("FM_CM_PORT_TXMP_SAMPLING_PERIOD") if !$altaInitMemBist;
    # STATS_AR
    altaCRMInitTab("FM_STATS_AR_IDX_CAM");
    altaCRMInitTab("FM_STATS_AR_FLAGS_CAM1");
    altaCRMInitTab("FM_STATS_AR_FLAGS_CAM2");
    # MCAST_MID
    altaCRMInitTab("FM_MCAST_DEST_TABLE") if !$altaInitMemBist;
    # MCAST_POST
    altaCRMInitTab("FM_MCAST_VLAN_TABLE") if !$altaInitMemBist;
    # MONITOR
    altaCRMInitTab("FM_ESCHED_DRR_Q") if !$altaInitMemBist;
    altaCRMInitTab("FM_ESCHED_DRR_DC_INIT") if !$altaInitMemBist;
    # MOD
    altaCRMInitTab("FM_MOD_L2_VLAN1_TX_TAGGED") if !$altaInitMemBist;
    altaCRMInitTab("FM_MOD_L2_VLAN2_TX_TAGGED") if !$altaInitMemBist;
    altaCRMInitTab("FM_MOD_MAP_IDX12A") if !$altaInitMemBist;
    altaCRMInitTab("FM_MOD_MAP_DATA_W16A") if !$altaInitMemBist;
    altaCRMInitTab("FM_MOD_MAP_DATA_W16B") if !$altaInitMemBist;
    altaCRMInitTab("FM_MOD_MAP_DATA_W16C") if !$altaInitMemBist;
    altaCRMInitTab("FM_MOD_MAP_DATA_W16D") if !$altaInitMemBist;
    altaCRMInitTab("FM_MOD_CAM");
    # MGMT2
    altaCRMInitTab("FM_CRM_DATA");

   # The following tables cannot be initialized via CRM
   for (my $index = 0 ; $index < 4; $index++) {
       eeprom_insert_alta_write_register_command( lookup_addr("FM_FFU_BST_MASTER_VALID", $index), (0) );
   }
   for (my $index = 0 ; $index < 24; $index++) {
       eeprom_insert_alta_write_register_command( lookup_addr("FM_FFU_SLICE_MASTER_VALID", $index), (0) );
   }

   # set FFU_ATOMIC_APPLY
   eeprom_insert_alta_write_register_command( lookup_addr("FM_FFU_ATOMIC_APPLY"), 0x03);

   for (my $index = 0 ; $index < 64; $index++) {
       eeprom_insert_alta_write_register_command( lookup_addr("FM_CRM_COMMAND", $index), (0, 0) );
       eeprom_insert_alta_write_register_command( lookup_addr("FM_CRM_REGISTER", $index), (0, 0) );
       eeprom_insert_alta_write_register_command( lookup_addr("FM_CRM_PERIOD", $index), (0, 0) );
       eeprom_insert_alta_write_register_command( lookup_addr("FM_CRM_PARAM", $index), (0) );
   }
}

##@method       getShift2Value()
#
# @brief        Expresses the given number as a power of 2 plus a fraction.
#
# @param[in]    $value: number to be expressed as a power of 2.
# 
sub getShift2Value {
    my ($value) = @_;
    my $count = 0;
    my $auxVar = $value;

    if ($value > 0)
    {
        while($auxVar >= 2)
        {
            $auxVar >>= 1;
            $count++;
        }
        $auxVar = $value-2**$count;
    }
    return ($count, $auxVar);
}

##@method       altaCRMInitTab()
#
# @brief        Configures the CRM to initialize the table specified by $regName.
#               The whole table is filled with the optional parameter 
#               $initValue. If this parameter is not defined, then the default
#               value will be used.
#               The table parameters (indexes, default values, etc) are gathered
#               from the hash table.
#               In order to be processed by this method, a table must be tagged
#               as "s" in the hash table, otherwise an error message is generated
#               and the table is not initialized.
#
# @param[in]    $regName: name of the table to be initialized.
# 
# @param[in]    $initValue: the value to be used to initialize the table. This
#                parameter is optional and if it is not defined the default value
#                will be used instead.
#
sub altaCRMInitTab
{
    my ($regName, $initValue) = @_;
       
    my $entry = $regs_hash{$regName};
    die "Found no entry for register $regName\n" if not defined $entry;

    my ($wordcount, $indexcount, $portindex, $ordering,
        $addrsub, $defaultsub, @boundlist) = @$entry;

    # vefify that it is a table
    if ($indexcount > 0)
    {
        # get the table indexes and determine the number total of registers to set.
        my $regTotalToSet    = 1;
        my $regStride0;
        my $regSize1         = 2**0xf;
        my @regSizeShift1;
        my $regStride1;
        my @regStrideShift1;
        my $regSize2         = 2**0xf;
        my @regSizeShift2;
        my $regStride2;
        my @regStrideShift2;

        # compute the stride values.
        # the stride value will remain = 0 and it will be ignored for the
        # undefined indexes.
        $regStride0 = &$addrsub((1,0,0)) - &$addrsub((0,0,0));
        $regStride1 = &$addrsub((0,1,0)) - &$addrsub((0,0,0));
        $regStride2 = &$addrsub((0,0,1)) - &$addrsub((0,0,0));

        # get $regSizeX from $boundlist.
        my($tabIndexMin, $tabIndexMax) = @{$boundlist[0]};

        $regTotalToSet = ($tabIndexMax + 1);
        # if 2 or more indexes
        if ($indexcount > 1) {
            $regSize1 = $tabIndexMax + 1;
            ($tabIndexMin, $tabIndexMax) = @{$boundlist[1]};
            $regTotalToSet *= ($tabIndexMax + 1);
            # if 3 indexes
            if ($indexcount > 2)
            {
                $regSize2 = $tabIndexMax + 1;
                ($tabIndexMin, $tabIndexMax) = @{$boundlist[2]};
                $regTotalToSet *= ($tabIndexMax + 1);
            }
        }

        # this method only manages the case where $regstride0 == $wordcount
        # In this case, the register address is computed as:
        #   address = baseAddress + count0 * size0 + count1 * stride1 + count2 * stride2
        # where size0 is the wordcount or the register size in words.
        #
        if ($regStride0 != $wordcount) {
            if ($indexcount > 2) {
               print STDERR "ERROR: register stride0 is not equal to the size [words]\n";
               print STDERR " $regName will not initialized\n";
               return;
           }
           else
           {
               $regStride2 = $regStride1;
               $regStride1 = $regStride0;
               $regSize2   = $regSize1;
               $regSize1   = 1;
           }
        }

        # express the sizes/strides as a power of 2.
        @regSizeShift1    = getShift2Value($regSize1);
        @regStrideShift1 = getShift2Value($regStride1);
        @regSizeShift2    = getShift2Value($regSize2);
        @regStrideShift2 = getShift2Value($regStride2);

        # Special cases:
        #  $regSize1 cannot be expressed as a power of 2 for the following tables:
        #   FM_CM_PORT_RXMP_PRIVATE_WM
        #   FM_CM_PORT_RXMP_PAUSE_ON_WM
        #   FM_CM_PORT_RXMP_PAUSE_OFF_WM
        #   FM_ERL_CFG
        #   FM_CM_QUEUE_STATE_INIT
        #   FM_CM_PORT_TXMP_IP_WM
        #   FM_CM_PORT_TXMP_SAMPLING_PERIOD
        #   FM_CM_PORT_TXMP_SAMPLING_STATE
        #   FM_ESCHED_DRR_Q
        #   FM_L2AR_FLAGS_CAM
        # (this list is not extensive)  
        # 
        if ($regSizeShift1[1])
        {
            # in many of this cases it is OK to round up the size of the first
            # index to the next power of 2

            my $regSizeRoundUp1 = 2**($regSizeShift1[0]+1);
            
            # check that there is no gaps
            if ($regSizeRoundUp1 * $wordcount == $regStride1)
            {
                $regTotalToSet = $regTotalToSet * $regSizeRoundUp1 / $regSize1;
                $regSizeShift1[1] = 0;
                
                if ($indexcount == 2)
                {
                    # This is a very particular case.
                    # After the round up, the table may be considered as an one
                    # dimensional array
                    $regSize1   = 2**0xf;
                    $regSizeShift1[0] = 0xf;
                    $regStride1 = 0;
                    $regStrideShift1[0] = 0;
                }
                else
                {
                    # The table has 3 indices, so it cannot be considered as an one-dimensional array
                    $regTotalToSet = $regTotalToSet * ($regSizeShift1[0] + 1) / $regSizeShift1[0];
                    $regSizeShift1[0]++;
                }
            }
        }
        
        if ($regStrideShift1[1] || $regStrideShift2[1] )
        {
            # the workaround for this is to split the table.
            print STDERR "ERROR: block stride cannot be expressed as a power of 2\n";
            print STDERR " $regName will not initialized\n";
            return;
        }

        # configure CRM to initialize the table.

        # CRM_COMMAND
        #  Command   = Memory set (0)
        #  DataIndex = not used
        #  Count     = $regToSet (number total of register in the table)
        # Note: count is splitted in two parts: the lower 18 bits of count go
        #  in the low word and the rest (the 2 higher bits) go in the high word.
        my $crmCommandL = ($regTotalToSet & 0x3FFFF) << 14;
        my $crmCommandH = ($regTotalToSet & 0xc0000) >> 18;

        # CRM_REGISTER
        # Note1: size and stride values of the unused indexes are set to 2**0xf. In this way
        # was they will be ignored.
        # Note2: strideShift is defined as:
        #   value = 1 << (strideShift + 1)
        # instead of: 
        #   value = 1 << strideShift
        # as indicated in the current documentation (10/20/2011)
        if ($regStrideShift1[0]  && $regStrideShift1[0] != 0xf) {
            $regStrideShift1[0]--;
        }
        if ($regStrideShift2[0]  && $regStrideShift2[0] != 0xf) {
            $regStrideShift2[0]--;
        }
        my $crmRegisterL = &$addrsub(0,0,0) | $alta_crm_register_size[$wordcount-1];
        $crmRegisterL   |= $regSizeShift1[0]<<24 | $regStrideShift1[0] << 28;
        my $crmRegisterH = $regSizeShift2[0] | $regStrideShift2[0] << 4;
        
        # Add the command-register to the crm buffer (in the position 0)                                                
        eeprom_insert_alta_write_register_command(lookup_addr("FM_CRM_COMMAND",0),
                                                 ($crmCommandL, $crmCommandH) );
        eeprom_insert_alta_write_register_command(lookup_addr("FM_CRM_REGISTER",0),
                                                 ($crmRegisterL, $crmRegisterH) );
        eeprom_insert_alta_write_register_command(lookup_addr("FM_CRM_PERIOD",0),
                                                 (0, 0) );

        # Set the initialization value
        if (!defined $initValue)
        {
            $initValue = 0;
        }
        eeprom_insert_alta_write_register_command(lookup_addr("FM_CRM_PARAM",0), $initValue);


        # push a 'start-CRM' instruction. As the CRM command are pushed always in
        # the first position (index =0), both the first and the last command indexes
        # are equal to 0x0 and the CRM_CTRL value is 0x01.
        eeprom_insert_alta_write_register_command(lookup_addr("FM_CRM_CTRL"), 0x01);


        # push an eeprom Poll command to wait until the table initialization is
        # completed. This is indicated by a the Running bit of CRM_STATUS. 
        # poll max delay = 10 us * 10000 retries = 100ms
        eeprom_insert_poll_command(lookup_addr("FM_CRM_STATUS"),
                                   0x00,                    # wait until running flag is 0
                                   0x01,                    # running flag: bit 0
                                   10000,                   # test 10000 times
                                   10,                      # read every 10 us
                                   $eepromBootErrJmpAddr1); # on error jump to ...
    }
    else
    {
        print STDERR "ERROR: table $regName is not tagged as \"s\" or index number = 0 \n";
        print STDERR " $regName will not initialized\n";
        return;
    }
}

##@method alta_init_FC_BEM()
#
# @brief        This sequence configures the FC BEM system so that
#               it will handle mgmt and sweepers correctly. This 
#               is technically only needed if you're going
#               to access registers while traffic is flowing.
#
sub alta_init_FC_BEM
{
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",0), 0x0001450c);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",2), 0x000e680b);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",3), 0x00008202);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",4), 0x00005145);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",5), 0x00006186);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",6), 0x000134cb);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",8), 0x0000a282);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",9), 0x00009249);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",10), 0x00002082);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",11), 0x0000e386);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",12), 0x00008208);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",13), 0x0000e386);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",14), 0x0001860f);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_MGMT_CYCLES",15), 0x0001040f);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_UNROLL_ITER"), 0x00000fff);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_SWEEP_CYCLES",0), 0x00000001);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_SWEEP_CYCLES",1), 0x00000001);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_SWEEP_CYCLES",2), 0x00000001);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_SWEEP_PERIOD"), 0x000000e6);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_FC_MRL_ALIGN_TX_STATS"), 0x00000001);
}

##@method alta_generate_eeprom_header_helper()
#
# @brief        helper used by alta_generate_eeprom_header to configure the section
#               parameters.
#
sub alta_generate_eeprom_header_helper
{
     my ($index) = @_;

     my $spimode = (($alta_eeprom_image_mode[$index] << 5)  & 0xE0) | 
                   (($alta_eeprom_image_speed[$index] << 2) & 0x1C);
     push (@eeprom_header, $spimode);

     my $imageAddr = $alta_image_offsets[$index];
     push (@eeprom_header, ($imageAddr >> 16) & 0xff);
     push (@eeprom_header, ($imageAddr >> 8) & 0xff);
     push (@eeprom_header,  $imageAddr & 0xff);
}

##@method alta_generate_eeprom_header()
#
# @brief        Configures the eeprom header, that specifies the offsets, speed and
#               mode for the 4 images.
#
sub alta_generate_eeprom_header
{

    for (my $index = 0; $index <4; $index++) {
        alta_generate_eeprom_header_helper($index);
    }
}

# tahoe_get_config_portmask
#
# Return a mask indicating which ports are out of
# reset and, therefore, should be configured.
# On tahoe, port 0 is always out of reset and is
# always configured.

sub tahoe_get_config_portmask {
    my $FM_PORT_RESET = lookup_addr("FM_PORT_RESET");

    my $portmask = (~lookup_reg($FM_PORT_RESET))&0x1FFFFFE;
    $portmask |= 1;

    return $portmask;
}

# bali_get_config_portmask
#
# Return a mask indicating which ports are out of
# reset and, therefore, should be configured.
# On bali, port 0 may or may not be out of reset.

sub bali_get_config_portmask {
    my $portmask = 0;

    foreach my $port (0..24) {
        my $epc_addr = lookup_addr("FM_EPL_PORT_CTRL", $port);
        my $epc_val  = lookup_reg($epc_addr);
        if ($epc_val&1) {
            $portmask |= (1<<$port);
        }
    }

    return $portmask;
}

# alta_get_config_portmask
#
# Load Return a mask indicating which ports are out of
# reset and, therefore, should be configured.
#
sub alta_get_config_portmask {
    my $portmask = 0;
# TBD

    return $portmask;
}

# alta_eeprom_SBUS_write_access
#
# Perform an SBUS write request
#  
sub alta_eeprom_SBUS_write_access
{
    my ($addr, $register, $data) = @_;

    die "Invalid SBUS address" if $addr > 0xff;
    die "Invalid SBUS register specification" if $register > 0xff;
    
    # set data to be written
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SBUS_REQUEST"), $data );

    # clear FM_SBUS_COMMAND, this is required in order to execute a new command.
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SBUS_COMMAND"), 0);

    # write data into the specified address:register using SBUS_COMMAND
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SBUS_COMMAND"),
                                              1    << 24 |
                                              SBUS_OP_WRITE << 16 |
                                              ($addr & 0xff) << 8 |
                                              ($register & 0xff) );
    # SBUS access delay.
    # use a fixed delay instead of a polling method for SBus accesses to
    # save memory space.
    eeprom_insert_delay_command($sbus_wr_delay);

}

# alta_eeprom_SBUS_reset_devices
#
#  
sub alta_eeprom_SBUS_reset_devices
{
    
    # clear FM_SBUS_COMMAND, this is required in order to execute a new command.
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SBUS_COMMAND"), 0);

    # send a reset command to the SBUS
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SBUS_COMMAND"),
                                              1    << 24 |
                                              SBUS_OP_RESET << 16);
    # SBUS reset delay.
    # use a fixed delay instead of a polling method for SBus accesses to
    # save memory space.
    eeprom_insert_delay_command($sbus_reset_delay);
}

# alta_eeprom_SBUS_init
#
# Perform the SBUS initialization.
# This methode does not release the SBUS controller reset, which must be
# done in a previous step.
#  
sub alta_eeprom_SBUS_init
{
    # Set the SBUS clock divider to 16 to ensure that the ratio between
    # the SerDes REFCLK and the SBus clock is at least 4
    alta_eeprom_SBUS_write_access(0xfe, 0x0a, 0x04);

    # reset all devices attaches to the SBus ring
    alta_eeprom_SBUS_reset_devices();
}

# alta_load_spico_code
#
# Uploads machine code into the SPICO microcontroller.
# The upload is done using a fixed delays approch instead of
# a polling method.
# The SPICO's reset is supposed to be asserted before start loading
# the code
#  
sub alta_load_spico_code
{
    # take out the controller out of reset
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SBUS_SPICO"), 0);
    # spico acces delay
    eeprom_insert_delay_command($sbus_wr_delay);

    # spico wr-reg: SPICO[0x0C] = 0x03;
    alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x0C, 0x03);

    # spico wr-reg: SPICO[0x0C] = 0x01;
    alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x0C, 0x01);

    # spico wr-reg: SPICO[0x06] = 0x08;
    alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x06, 0x08);

    my $addr = 0;
    my $data = 0;
    for (my $addr = 0; $addr <= $#alta_spico_code; $addr++)
    {
        # SPICO[0x04] = addr[9:8]
        alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x04, ($addr >> 8));
        # SPICO[0x05] = addr[7:0]
        alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x05, $addr & 0xff);

        $data = $alta_spico_code[$addr];
        # SPICO[0x07] = data[7:0]
        alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x07, $data & 0xff);
        # SPICO[0x06] = data[9:8] | 0x0c
        alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x06, ($data >> 8) | 0x0c);
        # SPICO[0x06] = data[9:8] | 0x08
        alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x06, ($data >> 8) | 0x08);
    }

    # spico wr: SPICO[0x06] = 0x00;
    alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x06, 0x00);
    # spico wr: SPICO[0x0C] = 0x08;
    alta_eeprom_SBUS_write_access(SBUS_DEV_BROAD_ADDR_SPICO, 0x0C, 0x08);

    # enable the controller
    # FIXME: check this: after this point SPICO_RESET = 0; SPICO_ENABLE = 1;
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SBUS_SPICO"), 0x02);
    # delay
    eeprom_insert_delay_command($sbus_wr_delay);
}

# alta_init_bist_mem
#
# Generates the eeprom sequence to run the memory BIST in order to clear
# the 6T memories.
#  
sub alta_init_bist_mem
{
    # use 1 us delay
    my $delay = 1;
    my $i = 0;

    eeprom_insert_alta_write_register_command(lookup_addr("FM_SCAN_CONFIG_DATA_IN"), 0x63);
    eeprom_insert_delay_command($delay);

    # FIXME: is it necessary to read FM_SCAN_DATA_OUT  here??

    # scan mode configuration
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SCAN_CONFIG_DATA_IN"), 0x63 | (1<<31));
    eeprom_insert_delay_command($delay);

    # array replication configuration: 6 participants
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SCAN_CONFIG_DATA_IN"), 
                                              0xd55555 | 
                                              (0x08<<24) |
                                              (1<<31));
    eeprom_insert_delay_command($delay);

    # array replication configuration: 3 participants + 1 leader
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SCAN_CONFIG_DATA_IN"), 
                                              0x009555 | 
                                              (0x08<<24) |
                                              (1<<31));
    eeprom_insert_delay_command($delay);

    eeprom_insert_poll_command(lookup_addr("FM_BM_ENGINE_STATUS"),
                               0x0,                     # all engines must be 'idle'
                               0x03ff,                  # mask
                               10,                      # test 10 times
                               1,                       # read every 1 us
                               $eepromBootErrJmpAddr2); # on error jump to ...

    my $count = 0;
    my $accNdx = 0;
    my $sequence = 0;
    my $addr_BM_MARCH_SEQUENCE = lookup_addr("FM_BM_MARCH_SEQUENCE");
    my $addr_SRBM_MARCH_SEQUENCE = lookup_addr("FM_SRBM_MARCH_SEQUENCE");
    foreach my $bistCmmd (@marchTable)
    {
        $sequence |= ($bistCmmd << 4 * $count);
        if (++$count == 8)
        {
            # write the word into BM_MARCH_SEQUENCE
            eeprom_insert_alta_write_register_command($addr_BM_MARCH_SEQUENCE   + $accNdx, $sequence);
            $sequence = 0;
            $count = 0;
            $accNdx++;
        }
    }
    # write the last nibble
    if ($count)
    {
        eeprom_insert_alta_write_register_command($addr_BM_MARCH_SEQUENCE   + $accNdx, $sequence);
    }

    $count = 0;
    $accNdx = 0;
    $sequence = 0;
    foreach my $bistCmmd (@marchTable)
    {
        $sequence |= ($bistCmmd << 4 * $count);
        if (++$count == 8)
        {
            # write the word offset and into SRBM_MARCH_SEQUENCE
            eeprom_insert_alta_write_register_command($addr_SRBM_MARCH_SEQUENCE + $accNdx, $sequence);
            $sequence = 0;
            $count = 0;
            $accNdx++;
        }
    }
    # write the last nibble
    if ($count)
    {
        eeprom_insert_alta_write_register_command($addr_SRBM_MARCH_SEQUENCE + $accNdx, $sequence);
    }

    # CDP march config 1: 4 entries, word each
    # set Enable bit
    my $value = (1 << 21);
    for ($i = 0; $i < 4; $i++) {
        eeprom_insert_alta_write_register_command(lookup_addr("FM_CDP_BIST_MARCH_CONFIG",($i,0)), $value);
    }

    # SPDP march config: 5 entries, 1 word each
    # Enable bit remains the same than before.
    for ($i = 0; $i < 5; $i++) {
        eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_MARCH_CONFIG",($i,0)), $value);
    }

    # 6t CDP BIST engines configuration
    # set: PruneDefectReporting, SkipReadBadAddr, SkipWriteBadAddr and FlushDelayedWriteBeforeRead
    $value = (1<<2) | (1<<4) | (1<<5) | (1<<7);
    for ($i = 0; $i < 4; $i++) {
        eeprom_insert_alta_write_register_command(lookup_addr("FM_CDP_BIST_GENERAL_CONFIG",($i)), $value);
    }

    # 6T CDP chain controller configuration
    eeprom_insert_alta_write_register_command(lookup_addr("FM_CDP_BIST_CHAIN_LATENCY",(0,0)), 4);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_CDP_BIST_CHAIN_LATENCY",(0,1)), 4);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_CDP_BIST_CHAIN_LATENCY",(1,0)), 4);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_CDP_BIST_CHAIN_LATENCY",(2,0)), 4);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_CDP_BIST_CHAIN_LATENCY",(3,0)), 4);

    # 6T SPDP BIST engines configuration
    # set FlushDelayedWriteBeforeRead and AddressFormat
    $value = 1 << 2 | 1 <<3;
    for ($i = 0; $i < 4; $i++) {
        eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_GENERAL_CONFIG",($i)), $value);
    }
    # set FlushDelayedWriteBeforeRead
    $value = 1 << 2;
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_GENERAL_CONFIG",(4)), $value);

    # 6T SPDP chain controller configuration
    # [0] isPostRepair                = 1
    # [1] disableChainOps             = 0
    # [2] reportNonRepairableDetails  = 0
    $value = 1;
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_GENERAL_CONFIG",(0,0)), $value);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_GENERAL_CONFIG",(1,0)), $value);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_GENERAL_CONFIG",(1,1)), $value);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_GENERAL_CONFIG",(2,0)), $value);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_GENERAL_CONFIG",(3,0)), $value);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_GENERAL_CONFIG",(3,1)), $value);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_GENERAL_CONFIG",(4,0)), $value);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_GENERAL_CONFIG",(4,1)), $value);

    # highest memory address to be tested
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_MAX_ADDR",(0)),  4*1024-1);  # POL DDP
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_MAX_ADDR",(1)), 32*1024-1);  # 32K
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_MAX_ADDR",(2)), 16*1024-1);  # 16K
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_MAX_ADDR",(3)),  4*1024-1);  #  4K
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_MAX_ADDR",(4)),  1*1024-1);  # DDP/SDP

    # latency values
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_LATENCY",(0,0)),  4); # length     450
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_LATENCY",(1,0)),  4); # length     344
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_LATENCY",(1,1)),  4); # length     308
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_LATENCY",(2,0)),  4); # length     722
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_LATENCY",(3,0)),  6); # length   3,991
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_LATENCY",(3,1)),  6); # length   4,040
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_LATENCY",(4,0)), 10); # length  12,105
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_CHAIN_LATENCY",(4,1)), 10); # length  11,094

    # start CDP BIST engines
    for ($i = 0; $i < 4; $i++) {
        eeprom_insert_alta_write_register_command(lookup_addr("FM_CDP_BIST_START_SEQUENCE",($i)), 3);
    }

    # start SPDP BIST engines
    # core 1 exists only on certain chains with SDP/DDP memories
    # test only core 0 on the others
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_START_SEQUENCE",(0)),  SPDP_TEST_BOTH_CORES);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_START_SEQUENCE",(1)),  SPDP_TEST_CORE_0);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_START_SEQUENCE",(2)),  SPDP_TEST_CORE_0);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_START_SEQUENCE",(3)),  SPDP_TEST_CORE_0);
    eeprom_insert_alta_write_register_command(lookup_addr("FM_SPDP_BIST_START_SEQUENCE",(4)),  SPDP_TEST_BOTH_CORES);

    # wait until all engines are done (timeout = 5000ms)
    eeprom_insert_poll_command(lookup_addr("FM_BM_ENGINE_STATUS"),
                               0x0,                     # all engines must be 'idle'
                               0x03ff,                  # mask
                               5000,                    # test 5000 times
                               1000,                    # read every 1 ms
                               $eepromBootErrJmpAddr2); # on error jump to ...
}

# do_port_cfg_2_overrides
#
# Update PORT_CFG_2 values for all ports including port 0.
# Common to tahoe and bali.

sub do_port_cfg_2_overrides {
    my ($pc2_portmask) = @_;

    foreach my $port (0..24) {
        if ($configure_portmask & (1<<$port)) {
            my ($pc2_addr, $pc2_default) =
                lookup_addr_and_default("FM_PORT_CFG_2", $port);
            my $pc2_val = lookup_reg($pc2_addr);
            $pc2_val = $pc2_default if not defined $pc2_val;

            # Update PORT_CFG_2 to make sure the chip does not send frames
            # to ports that we are not configuring and/or are not up.
            my $new_pc2_val = $pc2_val & $pc2_portmask;

            foreach my $quad (["FM_PORT_CFG_2", $pc2_addr, $pc2_val, $new_pc2_val]) {
                my ($name, $addr, $old, $new) = @$quad;
                if ($old != $new) {
                    printf STDERR "Changed ${name}[$port] from 0x%x to 0x%x\n", $old, $new;
                    $reg_vals[$addr] = $new;
                }
            }
        }
    }
}


# tahoe_do_overrides
#
# Modify certain values loaded from the register dump according
# to the command-line overrides (-r and -w) for REFCLK selection
# and lane reversal/polarity.  This affects FM_PORT_CLK_SEL,
# FM_SERDES_CTRL_2, and FM_PCS_CFG_1.  Also restricts FM_PORT_CFG_2
# to only send to ports that are being configured.

sub tahoe_do_overrides {
    my ($prst_addr, $prst_default) =
        lookup_addr_and_default("FM_PORT_RESET");
    my $prst_val = lookup_reg($prst_addr);
    $prst_val = $prst_default if not defined $prst_val;

    my ($pclksel_addr, $pclksel_default) =
        lookup_addr_and_default("FM_PORT_CLK_SEL");
    my $pclksel_val = lookup_reg($pclksel_addr);
    $pclksel_val = $pclksel_default if not defined $pclksel_val;

    # Generate a portmask that includes only the ports that
    #   1) are being configured by this EEPROM image
    #   2) are out of reset (per PORT_RESET)
    # This mask is used to restrict PORT_CFG_2 values.
    my $pc2_portmask = $configure_portmask & ((~$prst_val)|1);

    # Update PORT_CFG_2 values for all ports including port 0.
    do_port_cfg_2_overrides($pc2_portmask);

    # Update clock selection plus two EPL registers on
    # physical ports only.
    my $new_pclksel_val = $pclksel_val;
    foreach my $port (1..24) {
        if ($configure_portmask & (1<<$port)) {
            my ($sc2_addr, $sc2_default) =
                lookup_addr_and_default("FM_SERDES_CTRL_2", $port);
            my $sc2_val = lookup_reg($sc2_addr);
            $sc2_val = $sc2_default if not defined $sc2_val;

            my ($pc1_addr, $pc1_default) =
                lookup_addr_and_default("FM_PCS_CFG_1", $port);
            my $pc1_val = lookup_reg($pc1_addr);
            $pc1_val = $pc1_default if not defined $pc1_val;

            my $wi = $wiring_info_per_port[$port];
            if (not defined $wi) {
                # use regdump values from PCS_CFG_1[20:19] because there was
                # no override specified.  Note: bits 0..7 of the wiring info
                # (representing polarity reversal) are not used on Tahoe.
                $wi = ((($pc1_val>>19)&3)<<8);
            }

            my $lane_mask = 0;

            # Figure out which lanes need to be brought out of reset.
            my $dps = ($pc1_val>>29)&3;
            if ($dps == 0) {
                # 4-lane mode
                $lane_mask = 0xF;
            } else {
                # 1-lane mode
                $lane_mask = ((($wi>>8)&1)?0x8:0x1) |
                    ((($wi>>9)&1)?0x8:0x1);
            }

            ##############################
            # Compute new register values.
            ##############################

            my $new_sc2_val = $sc2_val;
            my $new_pc1_val = $pc1_val;

            # Adjust PORT_CLK_SEL to use the correct reference clock.
            my $clk = $clocks_per_port[$port];
            if (defined $clk) {
                if ($clk == 2) {
                    if ($dps == 0) {
                        # 4-lane mode => REFCLKA
                        $clk = 0;
                    } else {
                        # 1-lane mode => REFCLKB
                        $clk = 1;
                    }
                }
                $new_pclksel_val &= ~(1<<$port);
                $new_pclksel_val |= (($clk&1)<<$port);
            }

            # Adjust SERDES_CTRL_2 so that LaneReset, LanePowerDown,
            # PLLResetAB, and PLLResetCD are correct for the set of
            # lanes in use.
            $new_sc2_val |= 0x3FF00;
            $new_sc2_val &= ~(($lane_mask&0xF)<<8);
            $new_sc2_val &= ~(($lane_mask&0xF)<<12);
            $new_sc2_val &= ~((($lane_mask&0x3)?1:0)<<16);
            $new_sc2_val &= ~((($lane_mask&0xC)?1:0)<<17);

            # Adjust PCS_CFG_1.Invert[RT]X_LaneOrdering
            $new_pc1_val &= ~(3<<19);
            $new_pc1_val |= ((($wi>>8)&3)<<19);

            foreach my $quad (["FM_SERDES_CTRL_2", $sc2_addr, $sc2_val, $new_sc2_val],
                    ["FM_PCS_CFG_1", $pc1_addr, $pc1_val, $new_pc1_val]) {
                my ($name, $addr, $old, $new) = @$quad;
                if ($old != $new) {
                    printf STDERR "Changed ${name}[$port] from 0x%x to 0x%x\n", $old, $new;
                    $reg_vals[$addr] = $new;
                }
            }
        }
    }

    foreach my $quad (["FM_PORT_CLK_SEL", $pclksel_addr, $pclksel_val, $new_pclksel_val]) {
        my ($name, $addr, $old, $new) = @$quad;
        if ($old != $new) {
            printf STDERR "Changed ${name} from 0x%x to 0x%x\n", $old, $new;
            $reg_vals[$addr] = $new;
        }
    }
}


# bali_do_overrides
#
# Modify certain values loaded from the register dump according
# to the command-line overrides (-r and -w) for REFCLK selection
# and lane reversal/polarity.  This affects FM_EPL_PORT_CTRL,
# FM_SERDES_CTRL_2, and FM_PCS_CFG_1.  Also clears
# MAC_CFG_2.DrainTX and restricts FM_PORT_CFG_2 to only send to
# ports that are being configured.

sub bali_do_overrides {

    # Generate a portmask that includes only the ports that
    #   1) are being configured by this EEPROM image
    #   2) are out of reset (per EPL_PORT_CTRL)
    # This mask is used to restrict PORT_CFG_2 values.
    my $pc2_portmask = 0;
    foreach my $port (0..24) {
        if ($configure_portmask & (1<<$port)) {
            my ($epc_addr, $epc_default) =
                lookup_addr_and_default("FM_EPL_PORT_CTRL", $port);
            my $epc_val = lookup_reg($epc_addr);
            $epc_val = $epc_default if not defined $epc_val;
            if ($epc_val&1) {
                $pc2_portmask |= (1<<$port);
            }
        }
    }

    # Update PORT_CFG_2 values for all ports including port 0.
    do_port_cfg_2_overrides($pc2_portmask);

    # Update EPL_PORT_CTRL clock selection plus three EPL registers on
    # physical ports only.
    foreach my $port (1..24) {
        if ($configure_portmask & (1<<$port)) {
            my ($epc_addr, $epc_default) =
                lookup_addr_and_default("FM_EPL_PORT_CTRL", $port);
            my $epc_val = lookup_reg($epc_addr);
            $epc_val = $epc_default if not defined $epc_val;

            my ($sc2_addr, $sc2_default) =
                lookup_addr_and_default("FM_SERDES_CTRL_2", $port);
            my $sc2_val = lookup_reg($sc2_addr);
            $sc2_val = $sc2_default if not defined $sc2_val;

            my ($pc1_addr, $pc1_default) =
                lookup_addr_and_default("FM_PCS_CFG_1", $port);
            my $pc1_val = lookup_reg($pc1_addr);
            $pc1_val = $pc1_default if not defined $pc1_val;

            my ($mc2_addr, $mc2_default) =
                lookup_addr_and_default("FM_MAC_CFG_2", $port);
            my $mc2_val = lookup_reg($mc2_addr);
            $mc2_val = $mc2_default if not defined $mc2_val;

            my $wi = $wiring_info_per_port[$port];
            if (not defined $wi) {
                # use regdump values (PCS_CFG_1[20:19], SERDES_CTRL_2[21:18],
                # SERDES_CTRL_2[25:22]) because there was no override specified
                $wi = ((($pc1_val>>19)&3)<<8) |
                    ((($sc2_val>>18)&0xF)<<4) |
                    ((($sc2_val>>22)&0xF));
            }

            my $lane_mask = 0;

            # Figure out which lanes need to be brought out of reset.
            my $dps = ($pc1_val>>29)&3;
            if ($dps == 0) {
                # 4-lane mode
                $lane_mask = 0xF;
            } else {
                # 1-lane mode
                $lane_mask = ((($wi>>8)&1)?0x8:0x1) |
                    ((($wi>>9)&1)?0x8:0x1);
            }

            ##############################
            # Compute new register values.
            ##############################

            my $new_epc_val = $epc_val;
            my $new_sc2_val = $sc2_val;
            my $new_pc1_val = $pc1_val;
            my $new_mc2_val = $mc2_val;

            # Adjust EPL_PORT_CTRL to use the correct reference clock.
            my $clk = $clocks_per_port[$port];
            if (defined $clk) {
                if ($clk == 2) {
                    if ($dps == 0) {
                        # 4-lane mode => REFCLKA
                        $clk = 0;
                    } else {
                        # 1-lane mode => REFCLKB
                        $clk = 1;
                    }
                }
                $new_epc_val &= ~(1<<2);
                $new_epc_val |= (($clk&1)<<2);
            }

            # Adjust SERDES_CTRL_2 so that LaneReset, LanePowerDown,
            # PLLResetAB, and PLLResetCD are correct for the set of
            # lanes in use.
            $new_sc2_val |= 0x3FF00;
            $new_sc2_val &= ~(($lane_mask&0xF)<<8);
            $new_sc2_val &= ~(($lane_mask&0xF)<<12);
            $new_sc2_val &= ~((($lane_mask&0x3)?1:0)<<16);
            $new_sc2_val &= ~((($lane_mask&0xC)?1:0)<<17);

            # Adjust SERDES_CTRL_2.RX_PolarityReversal (21:18) and
            #        SERDES_CTRL_2.TX_PolarityReversal (25:22) and
            $new_sc2_val &= ~0x3FC0000;
            $new_sc2_val |= ((($wi>>4)&0xF)<<18); # RX
            $new_sc2_val |= (($wi&0xF)<<22); # TX

            # Adjust PCS_CFG_1.Invert[RT]X_LaneOrdering
            $new_pc1_val &= ~(3<<19);
            $new_pc1_val |= ((($wi>>8)&3)<<19);

            # Make sure MAC_CFG_2.DrainTX is cleared
            $new_mc2_val &= ~(1<<9);

            foreach my $quad (["FM_EPL_PORT_CTRL", $epc_addr, $epc_val, $new_epc_val],
                    ["FM_SERDES_CTRL_2", $sc2_addr, $sc2_val, $new_sc2_val],
                    ["FM_PCS_CFG_1", $pc1_addr, $pc1_val, $new_pc1_val],
                    ["FM_MAC_CFG_2", $mc2_addr, $mc2_val, $new_mc2_val]) {
                my ($name, $addr, $old, $new) = @$quad;
                if ($old != $new) {
                    printf STDERR "Changed ${name}[$port] from 0x%x to 0x%x\n", $old, $new;
                    $reg_vals[$addr] = $new;
                }
            }
        }
    }
}

##@method configure_chip()
#
# @brief        Generic method to generate the chip configuration sequence.
#
sub configure_chip {
    if ($main::opt_2) {
        # Dummy byte when using a SPI EEPROM that requires 2-byte
        # addressing.  This is because Tahoe and Bali assume 3-byte
        # addressing.  Note that I2C always uses 2-byte addressing.
        push @eeprom_contents, 0;
        $eeprom_addr++;
    }

    # Determine which ports will be configured.  All ports that
    # are out of reset in the register dump will be brought out
    # of reset and configured.  There will be no configuration
    # writes to ports that are in reset.

    if ($main::opt_m) {
        die "Bad portmask argument: $main::opt_m"
            if $main::opt_m !~ /0x[0-9A-Fa-f]*/;
        $configure_portmask = hex($main::opt_m);
    } else {
        if ($chip_is == BALI) {
            $configure_portmask = bali_get_config_portmask();
        } 
        elsif ($chip_is == TAHOE)
        {
            $configure_portmask = tahoe_get_config_portmask();
        }
        elsif ($chip_is == ALTA)
        {
            $configure_portmask = alta_get_config_portmask();
        }
    }

    if ($chip_is == ALTA) {
        print STDERR sprintf("# Active portmask: 0x%8.8x 0x%8.8x 0x%8.8x\n\n", $alta_mask_hw, $alta_mask_mw, $alta_mask_lw);
    }
    else{
        printf STDERR "# Active portmask: 0x%x\n", $configure_portmask;
    }

    if ($chip_is == BALI) {
        bali_do_overrides();
        bali_configure_chip();
    } elsif ($chip_is == TAHOE) {
        tahoe_do_overrides();
        tahoe_configure_chip();
    } else {
        # ALTA configuration
        # TBD: overrides for ALTA
        alta_configure_chip();
    } 
}


if ($main::opt_T) {
    # Manatee function wrapper
print <<EOF;
sub fp_wr {
    my (\$addr, \$data) = \@_;
    my \$tp_str = sprintf "reg write 0x%x 0x%x", \$addr, \$data;
    tp(\$tp_str);
}

EOF
}

##@method config_app()
#
# @brief        Configures the application according the specified target chip.
#
sub config_app
{
    if ($chip_is == TAHOE) {
        # select the hash to use
        %regs_hash = %tahoe_regs_hash;

    } elsif ($chip_is == BALI) {
        # select the hash to use
        %regs_hash = %bali_regs_hash;

    } elsif ($chip_is == ALTA){
        # select the hash to use
        %regs_hash = %alta_regs_hash;

        # configure the Alta eeprom command set
        $eeprom_command{"write"} = 0x01;
        $eeprom_command{"poll"}  = 0x02;
        $eeprom_command{"wait"}  = 0x03;
        $eeprom_command{"boot"}  = 0x04;
        $eeprom_command{"lcnt"}  = 0x05;
        $eeprom_command{"loop"}  = 0x06;
        $eeprom_command{"end"}   = 0x0F;
        $eepromBootCmmdMask      = 0x07;

        # point the function references to alta's functions.
        $process_multiword_regs_ref = \&alta_process_multiword_regs;
        $process_multiword_regs_insrt_writes = \&alta_process_multiword_regs_insrt_writes;
    }
}


##@method getAppOpts()
#
# @brief        performs the command line processing.
#
sub getAppOpts
{
    my ($pass) = @_;

    if (not getopts('hk:b:c:tTxisp:20m:r:w:fzaS:HI:O:M:C:PX:BLF:DZ:')) {
        print STDERR "Error processing command line flags\n";
        show_usage_and_exit();
    }

    # option: -h (help)
    if ($main::opt_h) {
        show_usage_and_exit();
    }

    # option: -r (REFCLK selection override)
    if ($main::opt_r) {
        if ($main::opt_r eq "regvals") {
            # no change needed (default case)
            @wiring_info_per_port = (undef) x 25;
        } elsif ($main::opt_r eq "standard") {
            @wiring_info_per_port = (2) x 25;
        } else {
            if ($main::opt_r !~ /^[abrs]*$/) {
                print STDERR "Invalid REFCLK override (-r argument): $main::opt_r\n";
                show_usage_and_exit();
            }
            my $str = $main::opt_r . "r" x 24;
            foreach my $port (1..24) {
                my $sel = substr $str, ($port-1), 1;
                my $val = $opt_r_char_to_cpp{$sel};
                $clocks_per_port[$port] = $val;
                #print STDERR "Port $port: Selected clock $sel ($val)\n";
            }
        }
    }

    # option -w (Wiring configuration override)
    if ($main::opt_w) {
        if ($main::opt_w eq "regvals") {
            @wiring_info_per_port = (undef) x 25;
        } elsif ($main::opt_w eq "standard") {
            # no change needed (default case)
        } else {
            my @wconfiglist = split ":", $main::opt_w;
            foreach my $wconfig (@wconfiglist) {
                if ($wconfig !~ /^[[:xdigit:]]+=[[:xdigit:]]+$/) {
                    print STDERR "Invalid wiring configuration (-w argument): $wconfig\n";
                    show_usage_and_exit();
                }
                my ($portmask_str, $val_str) = split "=", $wconfig;
                my $portmask = hex $portmask_str;
                my $val = hex $val_str;
                foreach my $port (1..24) {
                    if ($portmask&(1<<$port)) {
                        $wiring_info_per_port[$port] = $val;
                    }
                }
            }
        }
    }

    # option -k (EEPROM size in Kbytes)
    if ($main::opt_k) {
        $eeprom_size_kb = $main::opt_k;
        die "Invalid EEPROM size $eeprom_size_kb" if ($eeprom_size_kb !~ /^[0-9]*$/);
    }

    # option -p (Bus clock period in ns)
    if ($main::opt_p) {
        $bus_clock_period_ns = $main::opt_p;
        die "Invalid clock period $bus_clock_period_ns ns" if ($bus_clock_period_ns !~ /^[0-9]*$/);
        die "Invalid clock period $bus_clock_period_ns ns" if
            $bus_clock_period_ns < 6 or $bus_clock_period_ns > 500;
    }

    # option -c (chip specification: tahoe, bali or alta)
    if ($main::opt_c && $pass == 1) {
        $chip_is = $chipDef_hash{$main::opt_c};
        if (!defined $chip_is) {
            print STDERR "Please use -c to specify tahoe, bali or alta\n";
            show_usage_and_exit();
        }
    }

    # option -S (Apply specified fixes)
    if ($main::opt_S) {
        my @fixes = split ',', $main::opt_S;
        foreach my $scanfix (@fixes) {
            if ($scanfix =~ /^sched$/i) {
                $adjust_scheduler_tokens = 1;
            }
            elsif ($scanfix =~ /^mrl$/i) {
                $altaMrlFix = 1;
            } else {
                print STDERR "Invalid scan fix: $scanfix\n";
                show_usage_and_exit();
            }
        }
    }

    # option -I (select target eeprom image)
    if ($main::opt_I) {
        $eeprom_target_image = $main::opt_I + 0;
        die "Invalid EEPROM target image" if ($eeprom_target_image !~ /^[0-3]$/);
    }
    # option -O (specify image offsets)
    if ($main::opt_O) {
        my @offsets = split ",",$main::opt_O;
        my $count = 0;
        foreach my $offsetval (@offsets) {
            $alta_image_offsets[$count] = hex $offsetval;
            # if a header must be included, offsets must be be greater or equal to 0x0100
            if ($main::opt_H) {
                die "Partition offset values must be greater or equal to 0x0100" if ($alta_image_offsets[$count] < 0x100);
            }
            $count++;
        }
        die "Invalid number of image offsets" if ($count != 4);
    }
    # option -Z (specify MGMT_SCRATCH register)
    if ($main::opt_Z) {
        my @regValuePairList = split ",",$main::opt_Z;
        
        foreach my $regValuePair (@regValuePairList)
        {
            my @regValuePair = split "=",$regValuePair;

            if ($regValuePair[0] =~ /^MGMT_SCRATCH/i )
            {
                $altaMgmtScratchReg = hex $regValuePair[1];
                die "(-Z) Invalid MGMT_SCRATCH register specification" if ($altaMgmtScratchReg == 0);
            }
            elsif ( $regValuePair[0] =~ /^PCI_CFG_ID/i )
            {
                $altaPcieIdReg = hex $regValuePair[1];
                die "(-Z) Invalid PCI_CFG_ID register specification" if ($altaPcieIdReg == 0);
            }
            elsif ( $regValuePair[0] =~ /^PCI_CFG_ID/i )
            {
                $altaPcieIdReg = hex $regValuePair[1];
                die "(-Z) Invalid PCI_CFG_ID register specification" if ($altaPcieIdReg == 0);
            }
            else
            {
                die "(-Z) Invalid register specification";
            }
        }
    }
    # option -M (image SPI mode)
    if ($main::opt_M) {
        my @SpiModes = split ",",$main::opt_M;
        my $count = 0;
        foreach my $mode (@SpiModes) {
            die "Invalid image SPI mode" if ($mode !~ /^[0-3]$/);
            $alta_eeprom_image_mode[$count] = $mode + 0;
            $count++;
        }
        die "Invalid number of image SPI modes" if ($count != 4);
    }
    # option -C (SPI clock rate -speed-)
    if ($main::opt_M) {
        my @SpiSpeeds = split ",",$main::opt_C;
        my $count = 0;
        foreach my $speed (@SpiSpeeds) {
            die "Invalid image SPI clock rate specification" if ($speed !~ /^[0-7]$/);
            $alta_eeprom_image_speed[$count] = $speed + 0;
            $count++;
        }
        die "Invalid number of image SPI speeds" if ($count != 4);
    }
    # option -P (generate a minimal, PCIe image)
    if ($main::opt_P) {
        # generate a minimal image
        $altaFIBMconfig = 0;
    }
    else
    {
        # default: generate a FIBM image 
        $altaFIBMconfig = 1;
    }
    # option -X (SPI lane reversal options)
    if ($main::opt_X) {
        die "Invalid PCIe lane reversal specification" if ($main::opt_X !~ /^[0-3]$/);
    }
    # option -F (Alta only: eeprom configuration file)
    if ($main::opt_F && $pass == 1) {
        # This option is only available for ALTA.
        if ($chip_is == ALTA) {
            # Parse the eeprom config file. The name of the file is in $main::opt_F
            # The eeprom config file may have an embedded command line, if so, getops()
            # is rerun using the command line defined in the file. Some options, like
            # the chip spceification ('-c') and the eeprom config file specification
            # ('-F') will be ignored in the second pass of getops().

            my $commandLine;
            alta_parseEepromConfigFile ($main::opt_F, \$commandLine);
            if (defined $commandLine) {
                # push the command line into @ARGV
                @ARGV = split /\s+/, $commandLine;
                # rerun getops with pass = 2.
                getAppOpts(2);
            }
        }
    }
    if ($main::opt_D) {
        # debug option
        $debugOption = 1;
    }
    if ($main::opt_B) {
        # Alta endianness
        $altaPcieEndianness = 1;
    }
}

##@method show_usage_and_exit()
#
# @brief        shows tool usage screen and quit the application
#
sub show_usage_and_exit
{
    print STDERR "Usage: regdump2eeprom.pl [options]\n";
    print STDERR "General command line options:\n";
    print STDERR "\t-h            Show this help text\n";
    print STDERR "\t-k <N>        Size of EEPROM in kbytes (default 8)\n";
    print STDERR "\t-b <N>        Size of EEPROM block in bytes (default 64)\n";
    print STDERR "\t-c <chip>     Specify \"tahoe\", \"bali\" or \"alta\"\n";
    print STDERR "\t-t            Output te commands (vs. raw bytes)\n";
    print STDERR "\t-T            Output TestPoint commands (vs. raw bytes)\n";
    print STDERR "\t-x            XML file for Aardvark (vs. raw bytes)\n";
    print STDERR "\t-i            Intel Hex 8-bit format (vs. raw bytes)\n";
    print STDERR "\t-s            Show EEPROM command sequence on STDERR\n";
    print STDERR "\t-p <N>        Specify bus clock period in ns (default: 30)\n";
    print STDERR "\t              (This affects EEPROM delay commands)\n";
    print STDERR "\t-2            Use a SPI EEPROM with 2-byte addressing\n";
    print STDERR "\t              (Do not use -2 for I2C EEPROMs)\n";
    print STDERR "\t-0            Allow ports to send traffic to the CPU port.\n";
    print STDERR "\t-m <maskval>  Override PORTMASK read from dump (format: 0xVALUE)\n";
    print STDERR "\t-r <refclks>  Override REFCLK selection from reg dump.  Options are:\n";
    print STDERR "\t                - \"regvals\" (use register values as-is) [default]\n";
    print STDERR "\t                - \"standard\" (use REFCLKA for 4-lane ports and\n";
    print STDERR "\t                  REFCLKB for 1-lane ports)\n";
    print STDERR "\t                - <string> A string in which each character represents\n";
    print STDERR "\t                  the clock selected for one port with the first character\n";
    print STDERR "\t                  representing port 1.  a=REFCLKA, b=REFCLKB, r=use registers\n";
    print STDERR "\t                  s=standard (as above).  Short strings are padded with \"r\"\n";
    print STDERR "\t-w <wconfig>  Specify wiring configuration (lane reversal and\n";
    print STDERR "\t              polarity reversal settings per port).  Options are:\n";
    print STDERR "\t                - \"regvals\" (use register values as-is)\n";
    print STDERR "\t                - \"standard\" (no lane or polarity reversal) [default]\n";
    print STDERR "\t                - <portmask1>=<val1>:<portmask2>=<val2>:...\n";
    print STDERR "\t                  where portmaskN is a 25-bit hex value representing\n";
    print STDERR "\t                  a set of ports and valN is a 10-bit hex value indicating\n";
    print STDERR "\t                  the settings (RX-lane-rev [9], TX-lane-rev [8],\n";
    print STDERR "\t                  RX-polarity-D:A [7:4], TX-lane-rev-D:A [3:0])\n";
    print STDERR "\t-f            Full FID table (write all entries, even if not used)\n";
    print STDERR "\t-z            Treat all unspecified registers as 0\n";
    print STDERR "\t-a            Write all registers for which a value has been specified\n";
    print STDERR "\t              even if the value is equal to the default value.\n";
    print STDERR "\t-S <fixList>  Apply specified list of fixes.\n";
    print STDERR "\t                <fixList>: fix1,fix2,...\n";
    print STDERR "\t                Available fixes: SCHED (scheduler scan fix), MRL (Alta A0 only)\n";
    print STDERR "\t-I <N>        Alta only: specify the target Image [0..3], (default is 0). Alta only\n";
    print STDERR "\t-H            Alta only: include the eeprom header in the output file\n";
    print STDERR "\t-O <partoff>  Alta only: specify the image offsets using the following format:\n";
    print STDERR "\t                - offset0,offset1,offset2,offset3\n";
    print STDERR "\t                  All the 4 offsets must be specified using hex format\n";
    print STDERR "\t                  Do not include <spaces>\n";
    print STDERR "\t                  if eeprom type = SPI, offsets must be >= 0x0100\n";
    print STDERR "\t-M <modes>    Alta only: specify the SPI mode to use for each image using the following format:\n";
    print STDERR "\t                - mode0,mode1,mode2,mode3\n";
    print STDERR "\t                  All the 4 modes must be specified\n";
    print STDERR "\t                  Do not include <spaces>\n";
    print STDERR "\t                  modeX range: [0..7]. See FM6000 Functional Specification, ch 25\n";
    print STDERR "\t-C <speed>    Alta only: specify the SPI clock rate (speed) to use for each image using the following format:\n";
    print STDERR "\t                - speed0,speed1,speed2,speed3\n";
    print STDERR "\t                  All the 4 speeds must be specified\n";
    print STDERR "\t                  Do not include <spaces>\n";
    print STDERR "\t                  speedX range: [0..7]. See FM6000 Functional Specification, ch 25\n";
    print STDERR "\t-P            Alta only: generate a minimal \"Min-PCIe config\" eeprom image\n";
    print STDERR "\t-X <revers>   Alta only: PCIe SERDES reverse lane. Options are:\n";
    print STDERR "\t                - \"0\": TxLaneReverse: no   RxLaneReverse: no\n";
    print STDERR "\t                - \"1\": TxLaneReverse: no   RxLaneReverse: yes\n";
    print STDERR "\t                - \"2\": TxLaneReverse: yes  RxLaneReverse: no\n";
    print STDERR "\t                - \"3\": TxLaneReverse: yes  RxLaneReverse: yes\n";
    print STDERR "\t-B            Alta only: configure PCIe as big endian (default is little endian)\n";
    print STDERR "\t-L            Alta only: initialize the SPICO microcontroller\n";
    print STDERR "\t-F <file>     Alta only: eeprom configuration file\n";

    print STDERR "\t-D            Alta only: print the port table for debug\n";
    print STDERR "\t-Z <reg>,<val> Alta only: set specified register with value\n";
                 "\t                - supported registers: MGMT_SCRATCH, PCI_CFG_ID\n";
    print STDERR "\t                - value must be spedified using hex format\n";


    print STDERR "\n";
    exit -1;
}

sub printAltaPortTable
{
    my $phyPort = 0;

    print STDERR "\n";

    print STDERR sprintf ("log phy mac   mod sp dfe lRev lPol  epl ch ln    ser nl     sbc lb  mask\n");
    foreach my $entry (@alta_port_def_table) {
        my ($phyPort2, $logPort, $actMac, $portMode, $portSpeed, $dfeMode, $laneRev, $lanePol, $epl, $channel, $lane, $serdes, $laneNum, $sbusCfgSet, $loopback, $mask) = @$entry;

        print STDERR sprintf ("%2.2d: % 2.2d, % d, 0x%2.2x, %d, %d, ",$phyPort2, $logPort, $actMac, $portMode, $portSpeed, $dfeMode);
        print STDERR sprintf ("| %d, %d, || %2.2d, %d, %d, ||| % 2.2d, %d |||| %d,  %d, %s\n", $laneRev, $lanePol, $epl, $channel, $lane, $serdes, $laneNum, $sbusCfgSet, $loopback, $mask);
    }
    print STDERR "\n";
}

