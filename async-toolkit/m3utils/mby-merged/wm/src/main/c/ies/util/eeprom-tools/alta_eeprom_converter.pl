#!/usr/local/bin/perl
# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             alta_eeprom_converter.pl
# Creation Date:    08/30/11
# Description:      
# Version:          1.0
#
# INTEL CONFIDENTIAL
# Copyright 2011 Intel Corporation. All Rights Reserved. 
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

use strict;

use Getopt::Std;

my $pcie_refclk_period = 8;     # PCIE_REFCLK clock period is 8 ns.
my @inputfiles;                 # list of input files
my $outputfile;                 # output file
my $eepromTypeIsI2C = 0;        # eeprom type (default = SPI)

# Eeprom header buffer
my @eepromHeaderBuffer;
my @imagelist;
my @eepromBufferPool;
my $eepromBufferIndex = 0;

# Eeprom Upper Linear Base Address, used to generate hexIntel-32
my $eepromULBA = 0;

my $reverseIndexOrder = 0;

my @alta_eeprom_partition_offsets = (
    0x100,      # default offset partition #0
    0x80000,    # default offset partition #1
    0x100000,   # default offset partition #2
    0x180000    # default offset partition #3
);

my @alta_eeprom_partition_mode =(
    0,          # default mode partition #0
    0,          # default mode partition #1
    0,          # default mode partition #2
    0           # default mode partition #3
);

my @alta_eeprom_partition_speed = (
    0,          # default speed partition #0
    0,          # default speed partition #1
    0,          # default speed partition #2
    0           # default speed partition #3
);

my $alta_eeprom_description = "Alta boot image";

my @alta_bootCommands = ("Not Defined",
                         "Initialize FFU Slice Numbers",
                         "Apply Bank Memory Repairs",
                         "Initialize All Scheduler Freelist",
                         "Initialize Only the Array Freelist",
                         "Initialize Only the Head Storage",
                         "Initialize Only the TXQ Freelist",
                         "Initialize Only the RXQ Freelist"
                         );

my %reg_name_by_addr = ();


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
# port-index:  which index is either a EPL or a port number (-1 for none)
#              the indicated index refers an EPL number if the index size
#              is 25 or a port number if the index size is 75.
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
        "FM_EPL_CFG_A"                          => [ 01, 01, 0, "u", sub { return (0x0e0000 + 0x000301 + 0x400 * $_[0]) }, sub { return (0x0c7d7899); } , [0, 24] ],
        "FM_EPL_CFG_B"                          => [ 01, 01, 0, "u", sub { return (0x0e0000 + 0x000302 + 0x400 * $_[0]) }, sub { return (0x00080000); } , [0, 24] ],
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
        "FM_PCS_10GBASEX_CFG"                   => [ 01, 01,-1, "u", sub { return (0x0e0000 + 0x000311 + 0x400 * $_[0]) }, sub { return (0x00); } , [0, 24] ],
        "FM_PCS_10GBASEX_RX_STATUS"             => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000312 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_PCS_10GBASEX_TX_STATUS"             => [ 01, 01, 0, "o", sub { return (0x0e0000 + 0x000313 + 0x400 * $_[0]) }, sub { } , [0, 24] ],
        "FM_PCS_40GBASER_CFG"                   => [ 01, 01,-1, "u", sub { return (0x0e0000 + 0x000314 + 0x400 * $_[0]) }, sub { return (0x000001ff); } , [0, 24] ],
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
        "FM_MAC_CFG"                            => [ 04, 02, 1, "u", sub { return (0x0e0000 + 0x000010 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x40000000, 0x00, 0x00, 0x00005381); } , [0, 3], [0, 24] ],
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
        "FM_PCS_1000BASEX_CFG"                  => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000022 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_PCS_1000BASEX_RX_STATUS"            => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000023 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_PCS_1000BASEX_TX_STATUS"            => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000024 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_PCS_10GBASER_CFG"                   => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000025 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
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
        "FM_SERDES_CFG"                         => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x000034 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x0aaaa005, 0x00); } , [0, 3], [0, 24] ],
        "FM_MULTI_PURPOSE_ERROR_COUNTER"        => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000036 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_PCS_40GBASER_RX_BIP_STATUS"         => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000036 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_PCS_10GBASER_RX_BER_STATUS"         => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000036 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_DISPARITY_ERROR_8B10B"              => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000036 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_LANE_CFG"                           => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000037 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00000001); } , [0, 3], [0, 24] ],
        "FM_LANE_STATUS"                        => [ 01, 02, 1, "o", sub { return (0x0e0000 + 0x000038 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { } , [0, 3], [0, 24] ],
        "FM_SERDES_RX_CFG"                      => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x000039 + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
        "FM_SERDES_TX_CFG"                      => [ 02, 02, 1, "u", sub { return (0x0e0000 + 0x00003a + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 3], [0, 24] ],
        "FM_SERDES_SIGNAL_DETECT"               => [ 01, 02, 1, "u", sub { return (0x0e0000 + 0x00003c + 0x80 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00); } , [0, 3], [0, 24] ],
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
        "FM_L2F_TABLE_256"                      => [ 03, 02,-1, "u", sub { return (0x180000 + 0x020000 + 0x4 * $_[0]+ 0x400 * $_[1]) }, sub { return (0x00, 0x00, 0x00); } , [0, 255], [0, 3] ],
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
        "FM_POLICER_STATE_4K"                   => [ 02, 02,-1, "u", sub { return (0x130000 + 0x008000 + 0x2 * $_[0]+ 0x2000 * $_[1]) }, sub { return (0x00, 0x00); } , [0, 4095], [0, 1] ],
        "FM_POLICER_STATE_1K"                   => [ 01, 01,-1, "u", sub { return (0x130000 + 0x00c000 + 0x1 * $_[0]) }, sub { return (0x00); } , [0, 1023] ],
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
        "FM_CM_PORT_TXMP_IP_WM"                 => [ 01, 02, 1, "u", sub { return (0x020000 + 0x000800 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0xffffffff); } , [0, 11], [0, 79] ],
        "FM_CM_PORT_TXMP_SAMPLING_PERIOD"       => [ 01, 02, 1, "u", sub { return (0x020000 + 0x001000 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { return (0x00); } , [0, 11], [0, 79] ],
        "FM_CM_PORT_TXMP_SAMPLING_STATE"        => [ 01, 02, 1, "o", sub { return (0x020000 + 0x001800 + 0x1 * $_[0]+ 0x10 * $_[1]) }, sub { } , [0, 11], [0, 79] ],
        "FM_CM_PORT_TXMP_ABOVE_IP"              => [ 03, 01,-1, "u", sub { return (0x020000 + 0x002000 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 11] ],
        "FM_CM_PORT_TXMP_BELOW_IP"              => [ 03, 01,-1, "u", sub { return (0x020000 + 0x002040 + 0x4 * $_[0]) }, sub { return (0x00, 0x00, 0x00); } , [0, 11] ],
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
        "FM_SW_IP"                              => [ 01, 00,-1, "u", sub { return (0x01c000 + 0x00001c ) }, sub { return (0x00); }  ],
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
        "FM_LCI_IP"                             => [ 01, 00,-1, "u", sub { return (0x000000 + 0x000003 ) }, sub { return (0x00); }  ],
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

my %regs_hash = %alta_regs_hash;

# process the command line
getAppOpts();

# validate inputfiles and outputfile
die "No input file was specified" if (@inputfiles == 0);

# Redirect the output if an output file has been defined.
if (defined $outputfile) {
    # check if outputfile already exists
    if (-e $outputfile)
    {
        if (!defined $main::opt_O) {
            my $answer;
        
            print "Output file $outputfile already exists.\n";
            print "Do you want to overwrite it (y/n)? :";
        
            chomp ($answer = <STDIN>);
            while ( $answer !~ /^[yYnN]$/)
            {
                print "\n Type 'y' or 'n': " ;
                chomp ($answer = <STDIN>);
            }
            if ($answer =~ /[nN]/)
            {
                print STDERR "Execution was cancelled by user\n\n;";
                exit 0 ;
            }
        }
        # delete old version of the outputfile
        unlink $outputfile;
    }
    
    # open output file
    open (OUTFILE, ">" , $outputfile) or die "Could not open file $outputfile. $!";
    select (OUTFILE);
}

# process inputfiles
if (defined $main::opt_x)
{
    hex2cmd();
}
elsif (defined $main::opt_c)
{
    cmd2hex();
}
else
{
    die "No convertion type was specified";
}

# close output file and exit
close OUTFILE;
exit 0;

##@method       hex2cmd()
#
# @brief        converts IntelHex files to command format.
#               The list of input files is in @inputfiles.
#               If there are no errors, output generated be each 
#               input file will be merged in the outputfile.
#               
sub hex2cmd
{
    my $errors = 0;
    my $filenum = 0;
    my @eepromHeaderBuff;
    my @imageTable;
    my @eepromSectionTableRef;
    my $skipImage;
    my $imageSelect;
    my $imageProcessed = 0;

    # create the register name table
    print STDERR "Creating register table...\n";
    fill_reg_name_by_addr_table();

    # process each file in the input file list. Only create the header for the
    # first one and verify it for the following ones
    print STDERR "Reading input file(s)\n";
    my $fileNum = 0;
    foreach my $infile(@inputfiles)
    {
        # read input files
        read_inputfile_hex($infile, \@eepromSectionTableRef, $fileNum, \@eepromHeaderBuff);
        $fileNum++;
    }

    # scan for alta images
    scan_data_for_images(\@eepromSectionTableRef, \@imageTable);

    # check if a specific image must be converted (default is all images)
    if ($main::opt_I)
    {
        $imageSelect = $main::opt_I + 0;
    }

    for (my $image = 0; $image < 4; $image++)
    {
        # process only the selected image
        next if defined $imageSelect && $imageSelect != $image;
        # process each image 
        if (defined $imageTable[$image])
        {
            generate_cmd_file($image, \@eepromSectionTableRef, $imageTable[$image]);
            $imageProcessed = 1;
        }
    }
    if ($imageProcessed) {
        print STDERR "\nFile conversion has been completed successfully\n" if (!$errors);
    } 
    elsif (defined $imageSelect)
    {
        print STDERR "\nERROR: image #$imageSelect not found\n";
        $errors = 1;
    }
    else
    {
        print STDERR "\nERROR: no image was found\n";
        $errors = 1;
    }

    exit $errors? -1 : 0;
}

##@method       read_inputfile_hex()
#
# @brief        read the specified IntelHex file into the data buffers
#
# @param[in]    $filename:  IntelHex file
#
# @param[in]    $eepromSectionTableRef: reference to the eeprom section table
# 
# @param[in]    $filnum:    input file sequence number.
#
# @param[in]    $eepromHeaderBuffRef: reference to the eeprom header buffer

sub read_inputfile_hex
{
    my ($filename, $eepromSectionTableRef, $filenum, $eepromHeaderBuffRef) = @_;
    my $errors = 0;

    # read the specified IntelHex file into the buffer
    read_intel_hex($filename, $eepromSectionTableRef, $filenum);

    # if eeprom type is I2C only 1 section is expected at addr 0x00
    if ($eepromTypeIsI2C)
    {
        die "Invalid I2C image" if @{$eepromSectionTableRef} > 1;
        my ($filenumber, $section, $baseAddr, @dataBuff) = shift @{$eepromSectionTableRef};
        die "I2C type image: invalid section" if $section > 0;
        die "I2C type image: invalid base address" if $baseAddr > 0;
        # print information about the eeprom header
        prnt_cmdfileHeader($eepromTypeIsI2C);
    }
    else
    {
        # eeprom type is SPI
        # check if in the 1st buffer there are some header information
        my $baseAddr      =  ${$eepromSectionTableRef}[2];
        my $arrayIndex    =  ${$eepromSectionTableRef}[3];

        # use the base address and the buffer size for the desition
        my $buffersize = $#{$eepromBufferPool[$arrayIndex]} +1;

        if ($baseAddr == 0x00 && $buffersize >= 0x10 && $buffersize < 0x100)
        {
            # this is a eeprom header section
            if ($filenum == 0)
            {
                # this is the first file, so try to read the eeprom header
                read_eeprom_header($arrayIndex, $eepromHeaderBuffRef);
            }
            else
            {
                # check that the header matches the first file's one
                check_eeprom_header($arrayIndex, $eepromHeaderBuffRef);
            }
        }

        $baseAddr   = ${$eepromSectionTableRef}[1*4+2];
        $arrayIndex = ${$eepromSectionTableRef}[1*4+3];
        $buffersize = $#{$eepromBufferPool[$arrayIndex]} +1;
        if ($baseAddr == 0x40)
        {
            # this is a eeprom description section
            if ($filenum == 0)
            {
                # try to read the eeprom description
                read_eeprom_description($arrayIndex);
            }
        }

    }
}

##@method       scan_data_for_images()
#
# @brief        scan the buffer pool for images
#
# 
# @param[in]    $eepromSectionTableRef: reference to the eeprom section table
#
# @param[in]    $imageTableRef: reference to the image table
#
sub scan_data_for_images
{
    my ($eepromSectionTableRef, $imageTableRef) = @_;

    # Get the total of sections found
    my $sectionNumber = $#{$eepromSectionTableRef} + 1;
    $sectionNumber /= 4;

    for (my $index =0; $index < $sectionNumber; $index++)
    {
        my $filenum = ${$eepromSectionTableRef}[$index*4];
        my $baseAddress = ${$eepromSectionTableRef}[$index*4+2];
        my $eepromBuffNdx = ${$eepromSectionTableRef}[$index*4+3];
        my $sectionSize = $#{$eepromBufferPool[$eepromBuffNdx]} +1;
        my $imageNdx = 0;

        foreach my $offsetAddr (@alta_eeprom_partition_offsets)
        {
            if ($baseAddress == $offsetAddr && $sectionSize > 0x20)
            {
                # image found
                die "Image #$imageNdx is defined in more than one file" if defined ${$imageTableRef}[$imageNdx];
                ${$imageTableRef}[$imageNdx] = $index;
                print STDERR "Found image #$imageNdx in file \'$inputfiles[$filenum]\'\n";
            }
            $imageNdx++;
            die "Too many images were found in file \'$inputfiles[$filenum]\'" if $imageNdx > 4;
        }
    }
}


##@method       read_intel_hex()
#
# @brief        read the specified IntelHex file into the buffer pointed by
#               $readBufferRef.
#               The input file may include one or more images; it also may
#               include an eeprom header. Accordint to this, the content of
#               the input file might be partitionned in several sections.
#
# @param[in]    $filename: IntelHex file
# 
# @param[in]    $readBufferRef: a reference to the input buffer.
# 
# @param[in]    $filenum: file sequence number
#
sub read_intel_hex
{
    my ($filename, $readBufferRef,$filenum) = @_;

    open (FILE, "<$filename") or die "Could not open file  '$filename' .$!";

    my $section = 0;
    my $extendedLinearAddressOffset = 0;
    my $currentBaseAddress;
    my $previousAddress;
    my $currentAddress;
    my $relativeAddress;
    my $incrementalAddr;
    my $maxRecordLen = 0;
    my @localBuffer;
    my @sectionDef;

    # hex format
    # :10 0000 00 01000C080000190201000C5400001A10 35
    # :10 0010 00 1004E40001000C040000000001000C00 CA

    while (<FILE>) {
        if (/^:[0-9A-Fa-f][0-9A-Fa-f]/ and not /:00/) {
            chomp;
            s/^://;
            my @fullRecord = ();
            while(length($_)>0) {
                push @fullRecord, hex(substr($_, 0, 2, ""));
            }
            # verify checksum
            my $checksum = 0;
            foreach my $databyte (@fullRecord) {
                $checksum += $databyte;
            }
            die "Checksum ERROR in file: \'$filename\'" if ($checksum & 0xff);
            # remove checksum byte.
            pop @fullRecord;

            my $recordLen  = shift @fullRecord;
            my $loadOffset = ((shift @fullRecord) << 8) + (shift @fullRecord);
            my $recType    = shift @fullRecord;
            if ($recType == 4) {
                # Extended Linear Address Record
                $extendedLinearAddressOffset = (((shift @fullRecord) << 8) + (shift @fullRecord)) << 16;
            } elsif ($recType == 0) {
                # Data Record
                my $currentAddress = $loadOffset + $extendedLinearAddressOffset;
                if (!defined $currentBaseAddress) {
                    $currentBaseAddress = $currentAddress;
                    $previousAddress = $currentAddress;
                }
                if ($recordLen > $maxRecordLen)
                {
                    $maxRecordLen = $recordLen;
                }

                # compute both the relative and the incremental addresses
                $relativeAddress = $currentAddress - $currentBaseAddress;
                $incrementalAddr = $currentAddress - $previousAddress;

                # data addresses must always increases, with no gaps
                die "Error: data inversion in input file" if $incrementalAddr < 0;
                # if a gap is found, then it must be a new image
                # (we consider that the record length is <= 0x20)
                if ($incrementalAddr > 2 * $maxRecordLen) {
                    #close the previous section and start a new one
                    # copy local buffer into the buffer pool
                    $eepromBufferPool[$eepromBufferIndex] = [ @localBuffer ];
                    push @{$readBufferRef},($filenum, $section, $currentBaseAddress, $eepromBufferIndex);
                    $eepromBufferIndex++;
                    $section++;
                    # reinit local variables
                    @localBuffer = ();
                    $currentBaseAddress = $currentAddress;
                    $previousAddress = $currentAddress;
                    $relativeAddress = 0;
                }
                # push the data into the the local buffer
                foreach my $byte (@fullRecord) {
                    $localBuffer[$relativeAddress++] = $byte;
                }
                $previousAddress = $currentAddress - 1;
            }elsif ($recType == 1) {
                # End of FileRecord
                last;
            }
            else {
                die "Not supported record type in IntelHex file";
            }
        }
    }
    # save local buffer if there are some data in it.
    if (@localBuffer > 0)
    {
        $eepromBufferPool[$eepromBufferIndex] = [ @localBuffer ];
        push @{$readBufferRef}, ($filenum, $section, $currentBaseAddress, $eepromBufferIndex);
        $eepromBufferIndex++;
    }
    close FILE;
}

##@method       generate_cmd_file()
#
# @brief        convert the the specified data buffer into the .cmd format.
#
# @param[in]    $image: image number to convert
# 
# @param[in]    $eepromSectionTableRef: reference to the eeprom section table
#
# @param[in]    $sectionTableIndex: index of the image to convert in the
#                  $eepromSectionTableRef.
#
sub generate_cmd_file {
    my ($image, $eepromSectionTableRef, $sectionTableIndex) = @_;

    my $baseAdd = ${$eepromSectionTableRef}[$sectionTableIndex*4+2];
    my $eepromBuffNdx = ${$eepromSectionTableRef}[$sectionTableIndex*4+3];
    my $sectionSize = $#{$eepromBufferPool[$eepromBuffNdx]} +1;
    my @binBuff = @{$eepromBufferPool[$eepromBuffNdx]} ;
    my $errors;
    my $i = 0;

    print "##################################################################\n";
    print "#> Eeprom Image $image\n";
    print "#> Offset: ".sprintf("0x%x", $alta_eeprom_partition_offsets[$image]) ."\n";

    my $version = get_mgmt_scratch_reg_value($eepromBuffNdx);
    my $sw_rev = sprintf "%02X", ($version >> 8) & 0xFF;
    $version = sprintf "%d", $version & 0xFF;
    print "#> Version: $sw_rev-$version    Format: $sw_rev-version (possible values 0-255)\n";
    print "##################################################################\n";


    while (($i+3)<@binBuff) {
        my $cmd = $binBuff[$i] & 0x0f;
        my $cmdOffset = 0;
        my $opt = ($binBuff[$i] >> 4) & 0x0f;
        my $val1 = get_word(@binBuff[($i+1)..($i+3)]);
        if ($cmd == 1) {
            # WRITE command
            # read extra arguments
            my $tAddr = $val1;
            my @datawords;
            my $j = 0;

            $opt++;
            for ($j; $j < $opt; $j++)
            {
                my $data = get_word(@binBuff[($i+$j*4+4)..($i+$j*4+7)]);
                push @datawords, $data;
            }
            # print the command
            foreach my $dataw (@datawords) {
                # get register name
                my $name = $reg_name_by_addr{$tAddr};
                if (not defined $name) {
                    printf STDERR "Register address 0x%06x not defined.\n", $val1;
                    $errors++;
                    $name = "UNKNOWN";
                }
                printf "WRITE %08x to $name (%06x)\n", $dataw, $tAddr;
                $tAddr += 1;
            }
            $cmdOffset = $opt * 4;
        } elsif ($cmd == 2) {
            # POLL command
            my $data = get_word(@binBuff[($i+4)..($i+7)]);
            my $mask = get_word(@binBuff[($i+8)..($i+11)]);
            my $maxRetry = get_word(@binBuff[($i+12)..($i+13)]);
            my $retryInterval = ((get_word(@binBuff[($i+14)..($i+15)])) * $pcie_refclk_period) / 1000;
            my $jumpAddress = get_word(@binBuff[($i+16)..($i+19)]);
            my $name = $reg_name_by_addr{$val1};
            printf "POLL %s (%06x) for %08x mask= %08x maxRetry= %d retryInterval[us]= %d jumpAddr= %08x\n",
                   $name, $val1, $data, $mask, $maxRetry, $retryInterval, $jumpAddress;
            $cmdOffset = 16;
        } elsif ($cmd == 3) {
            # WAIT command
            my $delay_us = ($pcie_refclk_period * $val1)/1000;
            printf "WAIT %06x (%.3f us)\n", $val1, $delay_us;
        } elsif ($cmd == 4) {
            # BOOT command
            my $bootCmmd = $alta_bootCommands[$val1 & 0x7];
            printf "BOOT %06x (%s)\n", $val1, $bootCmmd;
        } elsif ($cmd == 5) {
            # LCNT cmmand
            my $counter = $opt ? "Counter 1" : "Counter 2";
            printf "LCNT %06x into %s\n", $val1, $counter;
        } elsif ($cmd == 6) {
            my $counter = $opt ? "Counter 1" : "Counter 2";
            printf "LOOP %06x if %s\n", $val1, $counter;
        } elsif ($cmd == 15) {
            printf "FINISH\n";
        } else {
            printf "??UNKNOWN %02x %06x\n", $cmd, $val1;
            printf STDERR "Unknown command %02x\n", $cmd;
            $errors++;
        }
        $i += 4 + $cmdOffset;
    }
    if ($errors > 0) {
        print STDERR "Error count: $errors\n";
    }
}

##@method       get_mgmt_scratch_reg_value()
#
# @brief        Retreive the content of the FM_MGMT_SCRATCH register.
#
# @param[in]    $eepromBuffNdx: index of the image in the $eepromBufferPool
#
# @return       The register value.
#
sub get_mgmt_scratch_reg_value {
    my ($eepromBuffNdx) = @_;

    my @binBuff = @{$eepromBufferPool[$eepromBuffNdx]};
    my $value = 0;
    my $i = 0;

    while (($i+3)<@binBuff) {
        my $cmd = $binBuff[$i] & 0x0f;
        my $cmdOffset = 0;
        my $opt = ($binBuff[$i] >> 4) & 0x0f;
        my $val1 = get_word(@binBuff[($i+1)..($i+3)]);
        if ($cmd == 1)
        {
            # WRITE command
            my $name = $reg_name_by_addr{$val1};

            if (defined $name && $name =~ m/FM_MGMT_SCRATCH/)
            {
                $value = get_word(@binBuff[($i+4)..($i+7)]);
                last;
            }
            $opt++;
            $cmdOffset = $opt * 4;
        } 
        elsif ($cmd == 2)
        {
            # POLL command
            $cmdOffset = 16;
        }
        $i += 4 + $cmdOffset;
    }

    return $value;
}


##@method       read_eeprom_header()
#
# @brief        read eeprom configuration and the images offsets from the
#               eeprom header. See format of the eeprom header FocalPoint
#               6000, Funcional Specification, section 25.2 "Serial Boot
#               Rom Format"
#
# @param[in]    arrayIndex: index of the header buffer in $eepromBufferPool
#
# @param[in]    $headerBuffRef: reference to the eeprom header save buffer.
# 
#
sub read_eeprom_header
{
    my ($arrayIndex, $headerBuffRef) = @_;

    my $index = 0;
    my $error = 1;
    my $dataByte;

    # save the eeprom header into the buffer
    @{$headerBuffRef} = @{$eepromBufferPool[$arrayIndex]} ;

    while ($index < 4) {
        $dataByte = @{$headerBuffRef}[$index*4];
        $alta_eeprom_partition_mode[$index] = ($dataByte >> 5) & 0x3;
        $alta_eeprom_partition_speed[$index] = ($dataByte >> 2) & 0x7;
        $alta_eeprom_partition_offsets[$index] = get_word(@{$headerBuffRef}[($index*4+1)..($index*4+3)]) & 0xffffff;
        # validate the offset value
        if ($alta_eeprom_partition_offsets[$index] == 0 ||
            $alta_eeprom_partition_offsets[$index] == 0xffffff)
        {
            last;
        }
        # if all OK, set error to 0
        if (++$index == 4)
        {
            $error = 0;
        }
    }
    die "Invalid eeprom format\n" if $error;
    
    # print the file header and indicate the type of memory
    prnt_cmdfileHeader();

    $index = 0;
    while ($index < 4)
    {
        print "#> Image $index parameters:\n";
        print "#>   Mode:   ".sprintf"%x\n", $alta_eeprom_partition_mode[$index];
        print "#>   Speed:  ".sprintf"%x\n", $alta_eeprom_partition_speed[$index];
        print "#>   Offset: ".sprintf"0x%6.6x\n", $alta_eeprom_partition_offsets[$index];
        $index++;
    }
    print "#\n";
}

##@method       read_eeprom_description()
#
# @brief        read eeprom description section
#
# @param[in]    arrayIndex: index of the description buffer in $eepromBufferPool
#
# @param[in]    $headerBuffRef: reference to the eeprom description save buffer.
# 
#
sub read_eeprom_description
{
    my ($arrayIndex) = @_;

    my @descBuff;
    my $description;

    foreach my $num (@{$eepromBufferPool[$arrayIndex]})
    {
        my $char = chr($num);
        push (@descBuff, $char);
    }

    $description = join "", @descBuff;

    my $flag = 0;
    @descBuff = ();
    @descBuff = split "-", $description;

    print "##################################################################\n";
    print "#\n";

    foreach my $str (@descBuff)
    {
        if ($flag == 0)
        {
            $flag = 1;
            print "#> Description: $str\n";
        } 
        else 
        {
            print "#>              -$str\n";
        }
    }
    print "#> end_desc_marker\n";
    print "#\n";

}

##@method       prnt_cmdfileHeader()
#
# @brief        print the first lines of a command line header, and 
#               optionally indicates if the memory type is I2C
#
# @param[in]    $mtypeIsI2C: indicates that the memory type is I2C.
#
#
sub  prnt_cmdfileHeader
{
    my ($memIsI2C) = @_;

     print <<EOF;
##################################################################
## Generated by alta_eeprom_converter.pl 1.0
## Alta boot eeprom
##################################################################
##  NOTE: Do not modify the stucture of this header. Only Mode, 
##     Speed, Offset and target-image fields values are editable.
##  Multiwords registers: {0} indicates the LSW.
##################################################################
EOF

    if (defined $memIsI2C)
    {
        print "##  Eeprom type: I2C\n##\n";
    }
    else
    {
        print "##  Eeprom type: SPI\n##\n";
    }
}

##@method       check_eeprom_header()
#
# @brief        check the eeprom header against the previous saved values.
#               Generate warnings if any difference is fount in the SPI
#               parameteres and abort processing if the image offsets are 
#               different.
#
# @param[in]    arrayIndex: index of the header buffer in $eepromBufferPool
#
# @param[in]    $headerBuffRef: reference to the eeprom header save buffer.
# 
#
sub check_eeprom_header
{
    my ($arrayIndex, $headerBuffRef) = @_;

    my $index = 0;
    my $errors = 0;
    my $warnings = 0;


    for ($index = 0; $index < 0x10; $index++)
    {
        if ($eepromBufferPool[$arrayIndex][$index] != ${$headerBuffRef}[$index])
        {
            if ($index & 0x03)
            {
                # section offset is different
                $errors++;
            }
            else
            {
                # section SPI parameters are different (mode or speed)
                $warnings++;
            }
        }
    }
    die "ERROR: image offset are not the same for all files" if $errors;
    if ($warnings)
    {
        print STDERR "WARNING: some SPI parameters are not equal for all files\n";
    }
}

##@method       cmd2hex()
#
# @brief        converts 'command' files into IntelHex format.
#               The list of input files is in @inputfiles.
#               If there are no errors, output generated be each 
#               input file will be merged into the outputfile.
#               
sub cmd2hex
{
    my $errors = 0;
    my $filenum = 0;
    my @imageTable;
    my @imageSizeTable = (0,0,0,0);
    my @eepromSectionTable;
    my @eepromheader = ();
    my @eepromDescription = ();
    my $imageSelect;

    # process each file in the input file list. Only process the header information
    # for the first one and verify the consistency for the following ones
    print STDERR "Reading input file(s)\n";
    my $fileNum = 0;
    foreach my $infile(@inputfiles)
    {
        # read input files
        $errors = read_inputfile_cmd($infile, \@eepromSectionTable, $fileNum);
        $fileNum++;
    }
    # scan the read data for images
    scan_cmd_for_images(\@eepromSectionTable, \@imageTable);

        # check if a specific image must be converted (default is all images)
    if ($main::opt_I)
    {
        $imageSelect = $main::opt_I + 0;
    }

    if ($errors == 0)
    {
        # generate hex file
        my $headerFlag = 0;
        my $eofRecordFlag = 0;
        for (my $image = 0; $image < 4; $image++)
        {
            # process only the selected image
            next if defined $imageSelect && $imageSelect != $image;

            if (defined $imageTable[$image])
            {
                my $sectionNdx = $imageTable[$image];
                my $currentImage = $eepromSectionTable[$sectionNdx*4+2];
                my $bufferNdx = $eepromSectionTable[$sectionNdx*4+3];
                my @tmpDataBuff = @{$eepromBufferPool[$bufferNdx]};
                my $imageOffset = $alta_eeprom_partition_offsets[$currentImage];
                $imageSizeTable[$currentImage] = @tmpDataBuff + 0;
                # generate the eeprom header only if
                #  - option N (do not includ header) was not defined
                #  - memory type is SPI
                #  - headerFlag is not set (this is to include the header once, before
                #    the first image.
                if (!defined $main::opt_N &&
                    !$eepromTypeIsI2C &&
                     $headerFlag == 0)
                {
                    alta_generate_eeprom_header(\@eepromheader);
                    output_intel_hex_format(0, \@eepromheader);

                    alta_generate_eeprom_description(\@eepromDescription);
                    output_intel_hex_format(0x40, \@eepromDescription);
                    $headerFlag = 1;
                }
                output_intel_hex_format($imageOffset,\@tmpDataBuff);
                $eofRecordFlag = 1;
            }
        }
        if ($eofRecordFlag)
        {
            output_intel_hex_eof_record();
            
        }
        elsif (defined $imageSelect)
        {
            print STDERR "\nERROR: image #$imageSelect not found\n";
            $errors = 1;
        }
        else
        {
            print STDERR "\nERROR: no image was found\n";
            $errors = 1;
        }
        # verify images
        if ($errors == 0)
        {
            my $highAddr;

            if ($imageSizeTable[0]) {
                $highAddr = $alta_eeprom_partition_offsets[0] + $imageSizeTable[0];
                if ($imageSizeTable[1]) {
                    die "ERROR: Image #0 too large. Adjust image offsets\n" if ($highAddr > $alta_eeprom_partition_offsets[1]);
                }
                elsif ($imageSizeTable[2]) {
                    die "ERROR: Image #0 too large. Adjust image offsets\n" if ($highAddr > $alta_eeprom_partition_offsets[2]);
                }
                elsif ($imageSizeTable[3]) {
                    die "ERROR: Image #0 too large. Adjust image offsets\n" if ($highAddr > $alta_eeprom_partition_offsets[3]);
                }
                print STDERR sprintf("Image #0 = [0x%6.6x, 0x%6.6x]\n", $alta_eeprom_partition_offsets[0], $highAddr);
            }
            if ($imageSizeTable[1]) {
                $highAddr = $alta_eeprom_partition_offsets[1] + $imageSizeTable[1];
                if ($imageSizeTable[2]) {
                    die "ERROR: Image #0 too large. Adjust image offsets\n" if ($highAddr > $alta_eeprom_partition_offsets[2]);
                }
                elsif ($imageSizeTable[3]) {
                    die "ERROR: Image #0 too large. Adjust image offsets\n" if ($highAddr > $alta_eeprom_partition_offsets[2]);
                }
                print STDERR sprintf("Image #1 = [0x%6.6x, 0x%6.6x]\n", $alta_eeprom_partition_offsets[1], $highAddr);
            }
            if ($imageSizeTable[2]) {
                $highAddr = $alta_eeprom_partition_offsets[2] + $imageSizeTable[2];
                if ($imageSizeTable[3]) {
                    die "ERROR: Image #0 too large. Adjust image offsets\n" if ($highAddr > $alta_eeprom_partition_offsets[3]);
                }
                print STDERR sprintf("Image #2 = [0x%6.6x, 0x%6.6x]\n", $alta_eeprom_partition_offsets[2], $highAddr);
            }
            if ($imageSizeTable[3]) {
                $highAddr = $alta_eeprom_partition_offsets[3] + $imageSizeTable[3];
                print STDERR sprintf("Image #3 = [0x%6.6x, 0x%6.6x]\n", $alta_eeprom_partition_offsets[3], $highAddr);
            }
        }
    }

    if (!$errors) {
        print STDERR "\nFile conversion has been completed successfully\n";
        exit 0;
    }
    else
    {
        print "There were %d errors";
        exit -1;
    }
}

##@method       read_inputfile_cmd()
#
# @brief        Read specified .cmd.
#
# @param[in]    $filename: source file name (eeprom command file)
#
# @param[in]    $eepromSectionTableRef: reference to the eeprom section table
# 
# @param[in]    $filnum:    input file sequence number.
#
# @param[in]    $eepromHeaderBuffRef: reference to the eeprom header buffer
#
sub read_inputfile_cmd
{
    my ($filename, $eepromSectionTableRef, $filenum, $eepromHeaderBuffRef) = @_;

    my $headerDone = 0;
    my $currentImage;
    my $currentVersion;
    my $versionUsed = 0;
    my $section = 0;
    my $errors = 0;
    my @localBuffer = ();

    if ($main::opt_2) {
        push @localBuffer, 0;
    }

    open FILE, "<$filename" or die "Unable to open file $filename";

    # I2C eeprom types only hold one image at offset 0x00, so there they do not have
    # eeprom header.
    if ($eepromTypeIsI2C)
    {
        # set all the address offsets
        foreach my $addrOffset (@alta_eeprom_partition_offsets)
        {
            $addrOffset = 0;
        }
        $headerDone = 1;
    }

    while (<FILE>) {
        if (/^## Alta/ && !$headerDone && $filenum == 0)
        {
            if (parseCmdFileHeader(\*FILE)) {
                print STDERR "Using default eeprom partition\n";
            }
            $headerDone = 1;
            next;
        }
        elsif (/^## Generated/)
        {
            my @tokens = split;
            my @version = split /\./, $tokens[4];
            die "ERROR: invalid tool version" if ( ($version[0]+0) == 0 && ($version[1]+0) ==0 ); 
            if ($version[0]+0 >= 1) {
                $reverseIndexOrder = 1;
            }
            next;
        }
        elsif (/^#> Eeprom Image/)
        {
            # if there is an open image, close it and start a new one
            if (@localBuffer)
            {
                $eepromBufferPool[$eepromBufferIndex] = [ @localBuffer ];
                push @{$eepromSectionTableRef},($filenum, $section, $currentImage, $eepromBufferIndex);
                $eepromBufferIndex++;
                $section++;
                @localBuffer = ();
            }
            my @tokens = split;
            $currentImage = $tokens[3] + 0;
            die "Invalid image specification" if $currentImage > 3;
            next;
        }
        elsif (/^#> Version:/)
        {
            #> Version: A0-3    Format: A0-version (possible values 0-255)
            my @tokens = split;

            # example: A0-3
            my ($sw_rev, $version) = split ("-", $tokens[2]);
            $currentVersion = $sw_rev . sprintf "%02X", $version;
            $versionUsed = 0;
            next;
        }
        else
        {
            next if (/^#/); # skip comments
        }

        chomp;
        my ($cmd, @tokens) = split;
        if ($cmd eq "WRITE") {
            my @datawords;
            my ($regName, $addr, $wordindex) = regname2addr($tokens[2]);
            if (not defined $addr) {
                print STDERR "Unrecognized register: $regName\n";
                $errors++;
            }

            # Store the image version in the MGMT_SCRATCH register.
            if ($regName =~ m/FM_MGMT_SCRATCH/){
                $tokens[0] = $currentVersion;
                $versionUsed = 1;
            }

            push @datawords, hex($tokens[0]);
            my $wordcount = getWordCount($regName);
            for (my $n = 1 ; $n < $wordcount; $n++) {
                my $nextLine = <FILE>;
                my ($error, $data) = getNextWriteCommd($nextLine, $regName, $n);
                if (!$error) {
                    push @datawords, $data;
                }
                else
                {
                    print STDERR "Word count error in WRITE command\n";
                    # place the last line back onto the filehandle
                    seek(FILE,-length($nextLine), 1);
                    $errors++;
                }
            }
            # command WRITE

            my $cmmd = ((@datawords-1) << 4) | 0x1;
            push @localBuffer, ($cmmd, ($addr>>16)&0xFF, ($addr>>8)&0xFF, ($addr)&0xFF);
            foreach my $dataw (@datawords) {
                push @localBuffer, (($dataw>>24)&0xFF, ($dataw>>16)&0xFF,
                               ($dataw>>8)&0xFF,  ($dataw)&0xFF);
                }
        } elsif ($cmd eq "POLL") {
            $tokens[1] =~ s/(\)|\()//g;
            my $val = hex($tokens[1]);
            my $data = hex($tokens[3]);
            my $mask = hex($tokens[5]);
            my $maxRetries = $tokens[7] + 0;
            my $retryInterval = ($tokens[9] * 1000)/$pcie_refclk_period;
            my $jumpAddress = hex($tokens[11]);

            # command + address
            push @localBuffer, (0x02, ($val>>16)&0xFF, ($val>>8)&0xFF, ($val)&0xFF);
            # push data
            push @localBuffer, (($data>>24)&0xFF, ($data>>16)&0xFF, ($data>>8)&0xFF, ($data)&0xFF);
            # push mask
            push @localBuffer, (($mask>>24)&0xFF, ($mask>>16)&0xFF, ($mask>>8)&0xFF, ($mask)&0xFF);
            # push max retries & retry_interval
            push @localBuffer, (($maxRetries>>8)&0xFF,    ($maxRetries)&0xFF,
                           ($retryInterval>>8)&0xFF, ($retryInterval)&0xFF);
            # push jump address
            push @localBuffer, (($jumpAddress>>24)&0xFF, ($jumpAddress>>16)&0xFF,
                           ($jumpAddress>>8)&0xFF, ($jumpAddress)&0xFF);
        } elsif ($cmd eq "WAIT") {
            my $val = hex($tokens[0]);
            push @localBuffer, (0x03, ($val>>16)&0xFF, ($val>>8)&0xFF, ($val)&0xFF);
        } elsif ($cmd eq "BOOT") {
            my $val = hex($tokens[0]);
            push @localBuffer, (0x04, 0x00, 0x00, ($val)&0xFF);
        } elsif ($cmd eq "LCNT") {
            my $val = hex($tokens[0]);
            my $counter = $tokens[3] + 0;
            my $cmmd = ($counter << 4) | 0x05;
            push @localBuffer, ($cmmd, 0, ($val>>8)&0xFF, ($val)&0xFF);
        } elsif ($cmd eq "LOOP") {
            my $jmpAddr = hex($tokens[0]);
            my $counter = $tokens[3] + 0;
            my $cmmd = ($counter << 4) | 0x06;
            push @localBuffer, ($cmmd, ($jmpAddr>>16)&0xFF, ($jmpAddr>>8)&0xFF, ($jmpAddr)&0xFF);
        } elsif ($cmd eq "FINISH") {
            if ($versionUsed == 0) {
                $versionUsed = 1;
                # If the version has not been written to MGMT_SCRATCH above,
                # then do it here prior to insert the FINISH. 
                my @datawords;
                my ($regName, $addr, $wordindex) = regname2addr("FM_MGMT_SCRATCH[0]");

                if (defined $addr){
                    push @datawords, hex($currentVersion);

                    # command WRITE
                    my $cmmd = ((@datawords-1) << 4) | 0x1;
                    push @localBuffer, ($cmmd, ($addr>>16)&0xFF, ($addr>>8)&0xFF, ($addr)&0xFF);
                    foreach my $dataw (@datawords) {
                        push @localBuffer, (($dataw>>24)&0xFF, ($dataw>>16)&0xFF,
                                       ($dataw>>8)&0xFF,  ($dataw)&0xFF);
                    }
                }
            }
            push @localBuffer, (0xFF, 0xFF, 0xFF, 0xFF);
        } else {
            print STDERR "Unrecognized command: $cmd\n";
            $errors++;
        }
    }
    if (@localBuffer)
    {
        $eepromBufferPool[$eepromBufferIndex] = [ @localBuffer ];
        push @{$eepromSectionTableRef},($filenum, $section, $currentImage, $eepromBufferIndex);
        $eepromBufferIndex++;
        $section++;
        @localBuffer = ();
    }
    return $errors;
}

##@method       scan_cmd_for_images()
#
# @brief        scan the buffer pool for alta images
# 
# @param[in]    $eepromSectionTableRef: reference to the eeprom section table
#
# @param[in]    $imageTableRef: reference to the image table
#
sub scan_cmd_for_images
{
    my ($eepromSectionTableRef, $imageTableRef) = @_;

    # Get the total of sections sfound
    my $sectionNumber = $#{$eepromSectionTableRef} + 1;
    $sectionNumber >>= 2;

    for (my $index =0; $index < $sectionNumber; $index++)
    {
        my $filenum = ${$eepromSectionTableRef}[$index*4];
        my $imageNdx = ${$eepromSectionTableRef}[$index*4+2];

        die "Invalie image number (#$imageNdx)" if defined $imageNdx > 3;
        die "Image #$imageNdx already defined" if defined ${$imageTableRef}[$imageNdx];
        ${$imageTableRef}[$imageNdx] = $index;
        print STDERR "Found image #$imageNdx in file \'$inputfiles[$filenum]\'\n";
    }
}

##@method       output_intel_hex_format()
#
# @brief        generates an IntelHex file.
#
#
sub output_intel_hex_format
{
    my ($offset, $eeprom_contents_ref) = @_;

    my $blksize = 16;
    my $blkmax  = $blksize-1;

    my $blockaddr = 0;
    my $start_index = 0;
    while ($start_index < ($#{$eeprom_contents_ref} +1) ) 
    {
        my $end_index = $start_index + $blkmax;
        if ($end_index > $#{$eeprom_contents_ref}) {
            $end_index = $#{$eeprom_contents_ref};
        }

        my @block = @{$eeprom_contents_ref}[$start_index..$end_index];
        $start_index = $end_index + 1;

        my $count = scalar @block;

        # Byte count, two hex digits.
        # Address, four hex digits.
        # Record type, two hex digits.
        # Data, a sequence of n bytes.
        my $addr = $blockaddr+$offset;

        # This is for HexIntel-32
        # Check if the current ULBA is OK; otherwise an 'Extended Linear Address Record'
        # must be generated
        if ((($addr >> 16) & 0xffff) != $eepromULBA) {
            # update the Upper Linear Base Address
            $eepromULBA = ($addr >> 16) & 0xffff;
            # generate an Extended Linear Address Record (record type 4)
            # See Intel Hexadecimal Object File Format Specification, Revision A
            my $extRecordCksum = 6 + (($eepromULBA >> 8) & 0xff) + ($eepromULBA & 0xff);
            $extRecordCksum = (-$extRecordCksum) & 0xff;
            printf ":02000004%4.4X%02X\n",$eepromULBA,$extRecordCksum;
        }

        my @line = ($count, (($blockaddr+$offset)>>8)&0xFF, ($blockaddr+$offset)&0xFF, 0, @block);
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
}

sub output_intel_hex_eof_record
{
    print ":00000001FF\n";
}

##@method       get_word()
#
# @brief        convert the given byte sequence into a word
#
# @param[in]    @bytes: the byte sequence (MSB first)
#
# @return       the equivalent word.
#
sub get_word {
    my (@bytes) = @_;

    my $word = 0;
    foreach my $byte (@bytes) {
        $word <<= 8;
        $word |= $byte;
    }

    return $word;
}

##@method       regname2addr()
#
# @brief        returns the name, address and word order of the given register
#               fullname. The register fullname includes indices and the word order.
#
# @param[in]    $fullname: register full name (including indices and word order)
#
# @returns      $name: register name.
#               $addr: reister address
#               $wordindex: word order (0 is the LSW -Less Sign Word)
#
sub regname2addr {
    my ($fullname) = @_;

    my ($name, @indices);
    my $wordindex = 0;
    if ($fullname =~ /{/) {
        $wordindex = $fullname;
        $wordindex =~ s/.*{//;
        $wordindex =~ s/}.*//;
        $fullname =~ s/{.*//;
    }

    my ($name, @indices);
    if ($fullname =~ /\[|\]/) {
        $fullname =~ s/[][,]/ /g;
        ($name, my @local_indices) = split " ", $fullname;
        # Indices are reversed for versions 1.0 or higher of this tool
        if ($reverseIndexOrder) {
            @indices = reverse(@local_indices);
        } else {
            @indices = @local_indices;
        }
    } else {
        $name = $fullname;
    }
    my $addr = lookup_register_addr($name, @indices);
    return ($name, $addr, $wordindex);
}

##@method       getWordCount()
#
# @brief        returns the length of the given register in words.
#
# @param[in]    $fullname: register name
#
# @returns      the number of words.
#
sub getWordCount
{
    my ($fullname) = @_;

    $fullname =~ s/[][,]/ /g;
    my ($name, @indices) = split " ", $fullname;

    my $entry = $regs_hash{$name};
    die "Found no entry for reg $name" if not defined $entry;

    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;
    return $wordcount;
}


##@method       getNextWriteCommd()
#
# @brief        helper method used to read multiword WRITE commands.
#
# @param[in]    $line: current eeprom-command line.
#
# @param[in]    $regname: register name.
#
# @param[in]    $n: word order (0 is the LSW -Less Sign Word)
#
sub getNextWriteCommd
{
   my ($line, $regname, $n) = @_;

   my $error = 1;
   my $data;

   chomp $line;
   my ($cmd, @tokens) = split " ", $line;

   if ($cmd eq "WRITE") {
       $data = hex($tokens[0]);
       if ($tokens[2] =~ m/^$regname/) {
           $tokens[2] =~ s/}//;
           $tokens[2] =~ s/{/ /;
           my ($sname, $wordorder) = split " ", $tokens[2];
           if ($wordorder == $n) {
               $error = 0;
           }
       }
   }
   return ($error, $data);
}

##@method alta_generate_eeprom_header_helper()
#
# @brief        helper method used by alta_generate_eeprom_header to push
#               the offset, spi mode and spi speed information into the 
#               eeprom buffer.
#
# @param[in]    $index: image index, range: [0..3]
#
# @param[in]    $eepromHdrRef: reference to the eeprom header buffer
#
sub alta_generate_eeprom_header_helper
{
     my ($index, $eepromHdrRef) = @_;

     my $spimode = (($alta_eeprom_partition_mode[$index] << 5)  & 0xE0) |
                   (($alta_eeprom_partition_speed[$index] << 2) & 0x1C);
     push (@{$eepromHdrRef}, $spimode);

     my $partitionAddr = $alta_eeprom_partition_offsets[$index];
     push (@{$eepromHdrRef}, ($partitionAddr >> 16) & 0xff);
     push (@{$eepromHdrRef}, ($partitionAddr >> 8) & 0xff);
     push (@{$eepromHdrRef},  $partitionAddr & 0xff);
}

##@method alta_generate_eeprom_header()
#
# @brief        Configures the eeprom header, that specifies the offsets to the
#               every of the 4 images.
#               The offset values are currently harcoded.
#
# @param[in]    $eepromHdrRef: reference to the eeprom header buffer
#
sub alta_generate_eeprom_header
{
    my ($eepromHdrRef) = @_;

    for (my $index = 0; $index <4; $index++) {
        alta_generate_eeprom_header_helper($index, $eepromHdrRef);
        print STDERR " Image $index parameters:\n";
        print STDERR "    Mode:   ".sprintf"%x\n", $alta_eeprom_partition_mode[$index];
        print STDERR "    Speed:  ".sprintf"%x\n", $alta_eeprom_partition_speed[$index];
        print STDERR "    Offset: ".sprintf"%6.6x\n", $alta_eeprom_partition_offsets[$index];
    }
}

##@method alta_generate_eeprom_desciption()
#
# @brief        Push the EEPROM description section into the eeprom buffer.
#
# @param[in]    $eepromHdrRef: reference to the eeprom header buffer
#
sub alta_generate_eeprom_description
{
    my ($eepromDescRef) = @_;

    foreach my $char (split(//,$alta_eeprom_description))
    {
        push (@{$eepromDescRef}, ord($char));
    }
}

##@method       parseCmdFileHeader()
#
# @brief        parse the eeprom-command file header and gets the information
#               about partition offsets, SPI modes and clock rates.
#               The header is parsed using an finite automata and unless all
#               the steps are completed successfully an error message will be
#               generated and all the header information will be ignored.
#
# @param[in]    $filehandle: file handle reference.
#
sub parseCmdFileHeader
{
    my ($filehandle) = @_;
    my $image;
    my $state = 0;
    my $error = 1;
    my @offset;
    my @mode;
    my @speed;
    my $final_desc;

    while(<$filehandle>) {
        $error = 1;
        if (! /^#/) {
            seek(FILE,-length($_), 1);
            last;
        }
        next if (! /^#>/);
        $_ =~ s/^#>//;
        my $description = $_;
        chomp;
        my @tokens = split " ";
        if ($state == 0) {
            if ($tokens[0] eq "Image") {
                if ($tokens[1] =~ /^[0-3]$/) {
                    $image = $tokens[1] + 0;
                    $state++;
                    $error = 0;
                }
            }
        } elsif ($state == 1) {
            if ($tokens[0] eq "Mode:" &&
                $tokens[1] =~ /^[0-3]$/) {
               @mode[$image] = $tokens[1] +0;
               $state++;
               $error = 0;
            }
        } elsif ($state == 2) {
            if ($tokens[0] eq "Speed:" &&
                $tokens[1] =~ /^[0-7]$/) {
               @speed[$image] = $tokens[1] +0;
               $state++;
               $error = 0;
            }
        } elsif ($state == 3) {
            if ($tokens[0] eq "Offset:") {
                if (($offset[$image] = hex($tokens[1])) > 0xff) {
                    $error = 0;
                    # continue until complete 4 images
                    if ($image < 3) {
                       $state = 0;
                    }
                    else {
                        @alta_eeprom_partition_offsets = @offset;
                        @alta_eeprom_partition_mode = @mode;
                        @alta_eeprom_partition_speed = @speed;
                        # now look for the description field
                        $state = 4;
                    }
                }
            }
        } elsif ($state == 4) {
            if ($tokens[0] eq "Description:") {
                chomp($_ = $description);
                my @tokens = split ":";

                # Remove leading spaces
                $_ = $tokens[1];
                s/^\s+//; 
                $final_desc = $_;

            } elsif ($tokens[0] eq "end_desc_marker") {
                # indicates the end of the description
                # save the description string
                $alta_eeprom_description = $final_desc;
                $state = 5;
                $error = 0;
                last;

            } else {
                # Remove leading spaces
                $_ = $description;
                s/^\s+//; 
                chomp;
                $final_desc = $final_desc . $_;
            }
        } else {
            # invalid state;
            last;
        }
    }

    if ($state == 4)
    {
        print STDERR "Missing end_desc_marker\n";
        $state = 5;
    }

    if ($error != 0 || $state != 5 || $image != 3) {
        print STDERR "ERROR reading file header\n";
        print STDERR " File header parameters are ignored\n";
        $error = 1;
    }
    return $error;
}


##@method       getAppOpts()
#
# @brief        parse command line options.
#               convert the specified IntelHex file to the .cmd format
#
# @param[in]    $filename: IntelHex file
#
#
sub getAppOpts
{
    if (not getopts('hx:c:p:I:Ho:OT:')) {
        print STDERR "Error processing command line flags\n";
        show_usage_and_exit();
    }

    # option: -h (help)
    if ($main::opt_h) {
        show_usage_and_exit();
    }

    # option: -p (Specify bus clock period in ns)
    if ($main::opt_p) {
        $pcie_refclk_period = $main::opt_p;
        die "Invalid clock period $pcie_refclk_period ns" if ($pcie_refclk_period !~ /^[0-9]*$/);
        die "Invalid clock period $pcie_refclk_period ns" if
            $pcie_refclk_period < 6 or $pcie_refclk_period > 500;
    }

    # option: -I (Specify the eeprom image to process)
    if ($main::opt_I) {
        die "Invalid eeprom image" if ($main::opt_I !~ /^[0-3]$/);
    }

    # option: -x (Convert .cmd to .hex file)
    if (defined $main::opt_x )
    {
        die "Detected incompatible command option \'-c\'" if (defined $main::opt_c);
        @inputfiles = split(/,/,($main::opt_x));
    }

    # option: -c  (Convert .cmd file to .hex file\n");
    if (defined $main::opt_c) {
        die "Detected incompatible command option \'-x\'" if (defined $main::opt_x);
        @inputfiles = split(/,/,($main::opt_c));
    }

    # option: -o (output file)
    if (defined $main::opt_o) {
        $outputfile = ($main::opt_o);
    }

    # option: -T (eeprom type: SPI or I2C)
    if (defined $main::opt_T) {
        if ($main::opt_T =~ /^SPI$/) {
            $eepromTypeIsI2C = 0;
        } elsif ($main::opt_T =~ /^I2C$/) {
            # indicate that the eeprom type is I2C
            $eepromTypeIsI2C = 1;
        } else {
            die "Invalid type of eeprom";
        }
    }
}

##@method       show_usasge_and_exit()
#
# @brief        show the help screen and exit the application.
#
sub show_usage_and_exit
{
    print STDERR "Usage: alta_eeprom_converter.pl [options]\n";
    print STDERR "Purpose: Convert alta EEPROM images back-and-forth\n";
    print STDERR "  between intel-hex format and a human-editable\n";
    print STDERR "  format.\n";
    print STDERR "General command line options:\n";
    print STDERR "\t-h                  Show this help text\n";
    print STDERR "\t-x <input file>     Convert specified .hex file to .cmd file\n";
    print STDERR "\t-c <input file>     Convert specified .cmd file to .hex file\n";
    print STDERR "\t-o <output file>    Optional output file\n";
    print STDERR "\t-O                  Overwrite output file if it already exists\n"; 
    print STDERR "\t-p <N>              Specify PCIe ref clock period in ns (default: 8)\n";
    print STDERR "\t                    (This affects EEPROM delay commands)\n";
    print STDERR "\t-I <N>              Specify the target image [0..3] to be processed\n";
    print STDERR "\t-N                  Do not include the eeprom header in the output file\n";
    print STDERR "\t-T <type>           Specify eeprom type: 'SPI' (default) or 'I2C'";
    print STDERR "\t                     I2C eeprom type do not support multiple images";

    print STDERR "\n";
    exit;
}

##@method       lookup_register_addr()
#
# @brief        looks up the address of the given register name.
#
# @param[in]    $name: register name.
#
# @param[in]    @indices: list of indices (optional parameter)
#
sub lookup_register_addr {
    my ($name, @indices) = @_;

    my $entry = $regs_hash{$name};
    die "Found no entry for reg $name" if not defined $entry;

    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

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

    return $addr;
}

##@method       process_single_reg()
#
# @brief        process name of single registers.
#
sub process_single_reg {
    my ($name) = @_;

    my ($entry) = $regs_hash{$name};
    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

    die "Reg $name is not a single reg" if @boundlist != 0;

    my $addr = lookup_register_addr($name);

    if ($wordcount > 1) {
        for (my $index = 0; $index < $wordcount; $index++) {
            $reg_name_by_addr{$addr+$index} = $name . "{$index}";
        }
    } else {
        $reg_name_by_addr{$addr} = $name;
    }
}

##@method       process_array1_reg()
#
# @brief        process name of 1d array registers.
#
# @param[in]    $base: array name
#
sub process_array1_reg {
    my ($base) = @_;

    my ($entry) = $regs_hash{$base};
    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

    die "Reg $base is not a 1D array" if @boundlist != 1;
    my @bounds1 = @{$boundlist[0]};
    my @indices1 = ($bounds1[0]..$bounds1[1]);

    foreach my $i (@indices1) {
        my $addr = lookup_register_addr($base, $i);

        my $name  = "$base\[${i}\]";

        if ($wordcount > 1) {
            for (my $index = 0; $index < $wordcount; $index++) {
                $reg_name_by_addr{$addr+$index} = $name . "{$index}";
            }
        } else {
            $reg_name_by_addr{$addr} = $name;
        }
    }
}

##@method       process_array2_reg()
#
# @brief        process name of 2d array registers.
#
# @param[in]    $base: array name
#
sub process_array2_reg {
    my ($base) = @_;

    my ($entry) = $regs_hash{$base};
    my ($wordcount, $indexcount, $portindex, $ordering,
            $addrsub, $defaultsub, @boundlist) = @$entry;

    die "Reg $base is not a 2D array" if @boundlist != 2;
    my @bounds1 = @{$boundlist[0]};
    my @bounds2 = @{$boundlist[1]};
    my @indices1 = ($bounds1[0]..$bounds1[1]);
    my @indices2 = ($bounds2[0]..$bounds2[1]);

    foreach my $i (@indices1) {
        foreach my $j (@indices2) {

            my $addr = lookup_register_addr($base, $i, $j);

            my $name  = "$base\[${j}\]\[${i}\]";

            if ($wordcount > 1) {
                for (my $index = 0; $index < $wordcount; $index++) {
                    $reg_name_by_addr{$addr+$index} = $name . "{$index}";
                }
            } else {
                $reg_name_by_addr{$addr} = $name;
            }
        }
    }
}

##@method       process_array3_reg()
#
# @brief        process name of 3d array registers.
#
# @param[in]    $base: array name
#
sub process_array3_reg {
    my ($base) = @_;

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

    foreach my $i (@indices1) {
        foreach my $j (@indices2) {
            foreach my $k (@indices3) {

                my $addr = lookup_register_addr($base, $i, $j, $k);

                my $name  = "$base\[${k}\]\[${j}\]\[${i}\]";

                if ($wordcount > 1) {
                    for (my $index = 0; $index < $wordcount; $index++) {
                        $reg_name_by_addr{$addr+$index} = $name . "{$index}";
                    }
                } else {
                    $reg_name_by_addr{$addr} = $name;
                }
            }
        }
    }
}

##@method       fill_reg_name_by_addr_table()
#
# @brief        fill the reg_name_by_addr table with the name of every register.
#               The index and a multiword indicator are inclueded in the register
#               name.
#
sub fill_reg_name_by_addr_table
{
    foreach my $reg (sort keys %regs_hash) {
        if ($reg =~ m/^FM_NEXTHOP_WIDE/ ||
            $reg =~ m/^FM_NEXTHOP_NARROW/ ) {
            next;
        }
        my ($entry) = $regs_hash{$reg};
        my ($wordcount, $indexcount, $portindex, $ordering,
                $addrsub, $defaultsub, @boundlist) = @$entry;

        if ($indexcount == 0) {
            process_single_reg($reg);
        } elsif ($indexcount == 1) {
            process_array1_reg($reg);
        } elsif ($indexcount == 2) {
            process_array2_reg($reg);
        } elsif ($indexcount == 3) {
            process_array3_reg($reg);
        } else {
            die "Invalid indexcount: $indexcount";
        }
    }
}


