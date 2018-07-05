# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/RegisterUtils.pm
# Creation Date:    02/28/07
# Description:      Handlers for dealing with the registers
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
###############################################################################

package Applications::TestPoint::Common::RegisterUtilCore;
use strict;
use warnings;

require Exporter;
use Math::BigInt;
use SDKScalars;
use Applications::TestPoint::Common::Messages;
our @ISA = qw(
    Exporter
);
our @EXPORT = qw(
    handleRegRead
    handleRegWrite
    handleRegSetBit
    handleRegClearBit
    handleRegReadPort
    handleRegWritePort
    handleRegSetBitPort
    handleRegClearBitPort
    handleRegReadIndex
    handleRegWriteIndex
    handleRegSetBitIndex
    handleRegClearBitIndex
);
our @EXPORT_OK = qw(
    _performRead
    _performSetBit
    _performWrite
);

# spi eeprom access commands
# note: commands with "_AT" suffix are for AT45DB321D only
use constant {
    SPI_EEPROM_CMMD_READ_STATUS_REG     => 0x05,
    SPI_EEPROM_CMMD_WRITE_ENABLE        => 0x06,
    SPI_EEPROM_CMMD_WRITE_DISABLE       => 0x04,
    SPI_EEPROM_CMMD_READ_ID             => 0x90,
    SPI_EEPROM_CMMD_RDID                => 0x9f,
    SPI_EEPROM_CMMD_BULK_ERASE          => 0xc7,
    SPI_EEPROM_CMMD_SECTOR_ERASE        => 0xd8,
    SPI_EEPROM_CMMD_PAGE_PROGRAM        => 0x02,
    SPI_EEPROM_CMMD_READ_DATA_BYTES     => 0x03,
    SPI_EEPROM_CMMD_READ_STATUS_REG_AT  => 0xd7,
    SPI_EEPROM_CMMD_WRITE_BUFF1_AT      => 0x84,
    SPI_EEPROM_CMMD_WRITE_BUFF2_AT      => 0x87,
    SPI_EEPROM_CMMD_BLOCK_ERASE_AT      => 0x50,
    SPI_EEPROM_CMMD_ER_WR_BUFF1_AT      => 0x83,
    SPI_EEPROM_CMMD_ER_WR_BUFF2_AT      => 0x86,

};


# supported spi eeprom types
use constant {
    SPI_EEPROM_UNKNOWN                  => 0x00,
    SPI_EEPROM_SPANSION_S25FL129P       => 0x01,
    SPI_EEPROM_ATMEL_AT45DB321D_528     => 0x02,
    SPI_EEPROM_ATMEL_AT45DB321D_512     => 0x03
};

#---------------------------------------------
# For EEPROM image generator.
#
# Note: The keys in this hash must exactly
# match the tokens used in the command
# as defined in the XML file.
#---------------------------------------------
my %_eepromImageDefaultConfig = (
    'switch_number'     => 0,           # default switch: local (0)
    'switch_family'     => undef,       # switch family will be set at runtime
    'size'              => 8,           # kbytes
    'block'             => 64,          # bytes
    'type'              => "SPI_2",     # "SPI_2", "SPI_3" or "I2C"
    'part'              => SPI_EEPROM_UNKNOWN,
    'ebi_freq'          => 33.3,        # Bali ref clock [MHz]
    'pcie_freq'         => 125,         # Alta ref clock [MHz]
    'format'            => 'i',         # Image file format: intel-hex
    'include_cpu'       => 0,           # Don't Include traffic to CPU
    'ports'             => 0,           # All configured ports
    'unused_fids'       => 0,           # Don't include unused FIDs
    'lane_rev'          => [(0) x 25],  # 2-bit value per logical port
                                        # [RX (1), TX (0)]
    'polarity'          => [(0) x 25],  # 8-bit value per logical port
                                        # [RX-DCBA (7:4), TX-DCBA (3:0)]
    'refclk'            => [(0) x 25],  # REFCLK selection per logical port
    'minimal'           => 1,           # Alta: generate a full FIBM image
    'include_header'    => 1,           # Alta: include the eeprom header
    'image_number'      => 0,           # Alta: default image#: 0
                                        # Alta: image offsets
    'image_offset'      => [0x100, 0x80000, 0x100000, 0x180000],
    'eeprom_mode'       => [0,0,0,0],   # Alta: eeprom mode
    'eeprom_speed'      => [0,0,0,0],   # Alta: eeprom speed
    'pcie_lane_rev'     => 3,           # Alta: pcie lane reversal
                                        #  0: none; 1: Rx; 2: Tx; 3: Tx & Rx
    'pcie_endianness'   => 0,           # Alta: PCIe endianness.
                                        #  0: little endian (x86)
                                        #  1: big endian (ppc)
    'log40GportList'    => [],          # Alta: 40G capable port list (logical ports)
    'log10GportList'    => [],          # Alta: 10G capable port list (logical ports)
    'altaLoopbackList'  => [(0) x 75],  # Alta: used defined loopbacks (logical ports)
    'keep_tmp_files'    => 0,           # tells the handler to keep temporary files
                                        #  0: do not keep (default)
                                        #  1: keep temporary files (use only for debugging purposes)
    'initialize_spico'  => 0,           # Alta: include Spico code in the eeprom image
                                        #  0: do not include (default)
                                        #  1: include the Spico code
    'mgmt_scratch_reg'  => 0x0,         # Alta: use MGMT_SCRATCH register for version ID
    'pcie_cfg_id_reg'   => 0x155a8086   # Alta: Intel PCIe ID
);


use constant {
    EEPROM_PERFORM_ON_DEMAND_SECTOR_ERASE   => 0,
    EEPROM_PERFORM_BULK_ERASE               => 1,
    EEPROM_BULK_ERASED                      => 2
};

my %_eepromImageConfig = %_eepromImageDefaultConfig;
my $eepromEraseCtrl = EEPROM_PERFORM_ON_DEMAND_SECTOR_ERASE;
my @eepromErasedSectorList = ();

my %_eepromImageFormats = (
    'intel-hex' => 'i',     # Intel hex
    'tp'        => 'T',     # TestPoint commands
    'te'        => 't',     # TE commands
    'xml'       => 'x'      # XML for Aardvark
);

my %_yesOrNo = (
    'yes'       => 1,
    'no'        => 0
);

my %_onOrOff = (
    'on'        => 1,
    'off'       => 0
);

my %_clkAorB = (
    'b'         => 1,
    'a'         => 0
);

my %_portHalfMask = (
    "rx" => 2,
    "tx" => 1,
    "both" => 3,
);

my %_laneMask = (
    "d" => 8,
    "c" => 4,
    "b" => 2,
    "a" => 1,
    "all" => 0xF,
);

use constant {
    SPI_MODE_SINGLE      => 0,
    SPI_MODE_DUAL        => 1,
    SPI_MODE_QUAD        => 2,
    SPI_MODE_SINGLE_FAST => 3
};

# Alta only: eeprom mode
my @eeprom_mode = (
     "single",
     "dual",
     "quad",
     "sfast",
     "undef",
     "undef",
     "undef",
     "undef"
);

# Alta only: eeprom speed
my @eeprom_speed = (
     "0.5",
     "1.0",
     "2.0",
     "3.9",
     "7.8",
     "15.6",
     "31.2",
     "62.5"
);

# Alta only: PCIe lane reversal
my @pcie_lane_rev = (
      "none",
      "rx",
      "tx",
      "rx & tx"
);

# Alta only: PCIe endianness
my %_pcie_endianness = (
    "little_endian"     => 0,
    "big_endian"        => 1
);

my %_spiMode = (
    'single'        => 0,
    'dual'          => 1,
    'quad'          => 2,
    "single_fast"   => 3
);


# Alta only: port mode hash
my %alta_portMode_hash = (
    $FM_ETH_MODE_DISABLED       => "DISABLED",
    $FM_ETH_MODE_SGMII          => "SGMII",
    $FM_ETH_MODE_1000BASE_X     => "1000BASE_X",
    $FM_ETH_MODE_1000BASE_KX    => "1000BASE_KX",
    $FM_ETH_MODE_2500BASE_X     => "2500BASE_X",
    $FM_ETH_MODE_6GBASE_KR      => "6GBASE_KR",
    $FM_ETH_MODE_6GBASE_CR      => "6GBASE_CR",
    $FM_ETH_MODE_10GBASE_KR     => "10GBASE_KR",
    $FM_ETH_MODE_10GBASE_CR     => "10GBASE_CR",
    $FM_ETH_MODE_10GBASE_SR     => "10GBASE_SR",
    $FM_ETH_MODE_XAUI           => "XAUI",
    $FM_ETH_MODE_10GBASE_KX4    => "10GBASE_KX4",
    $FM_ETH_MODE_10GBASE_CX4    => "10GBASE_CX4",
    $FM_ETH_MODE_24GBASE_KR4    => "24GBASE_KR4",
    $FM_ETH_MODE_40GBASE_KR4    => "40GBASE_KR4",
    $FM_ETH_MODE_XLAUI          => "10GBASE_XALUI",
    $FM_ETH_MODE_24GBASE_CR4    => "24GBASE_CR4",
    $FM_ETH_MODE_40GBASE_CR4    => "40GBASE_CR4",
    $FM_ETH_MODE_40GBASE_SR4    => "40GBASE_SR4",
    $FM_ETH_MODE_AN_73          => "AN_73",
);

# Alta only: port speed hash
my %alta_portSpeed_hash = (
    10          => "10M",
    100         => "100M",
    1000        => "1G",
    2500        => "2.5G",
    10000       => "10G",
    20000       => "20G",
    40000       => "40G",
);

# Lane polarity hash
my %lanePolarity_hash = (
    0           => "normal",
    1           => "rx",
    2           => "tx",
    3           => "rxtx"
);

# Lane ordering hash
my %laneOrdering_hash = (
    0           => "none",
    1           => "rx",
    2           => "tx",
    3           => "rxtx"
);

use constant {
    SPI_EEPROM_REG_ADDR_SPI_TXDATA      => 0x01c034,
    SPI_EEPROM_REG_ADDR_SPI_RXDATA      => 0x01c035,
    SPI_EEPROM_REG_ADDR_SPI_HEADER      => 0x01c036,
    SPI_EEPROM_REG_ADDR_SPI_CTRL        => 0x01c037
};


##@cmethod private int _performRead(int     switchNum,
#                                   char    *register,
#                                   int     address,
#                                   int     i2c,
#                                   char    *string)
#
# @desc         Performs a register read
#
# @param[in]    switchNum The switch on which the register read is to be
#               performed
#
# @param[in]    register The name of the register to be read
#
# @param[in]    address The hardware address to be read
#
# @param[in]    i2c Indicate whether the register read is to be performed
#               through the I2C bus.
# 
# @param[in]    string The string to print before the read message is printed.
#               If @a string is not @c undef, @a string is printed, followed by
#               a colon and a blank
#
# @return       FM_OK if successful
sub _performRead
{
    my ($self, $switchNum, $register, $address, $i2c, $string) = @_;

    my $chip = $self->{CHIP};

    my $status;
    $chip->disableErrors();
    printf("%s: ", $string) if defined($string);
    if ($chip->registerIsMultiWord($register))
    {
        my $wordCount = $chip->registerGetWidth($register);
        my $words = [(0) x $wordCount];

        if (defined($i2c) && $i2c == 1)
        {
            $status = $chip->fmI2cReadUINT32Mult($switchNum, 
                                                 $address, 
                                                 $wordCount, 
                                                 $words);
        }
        else
        {
            $status = $chip->fmReadUINT32Mult($switchNum, 
                                              $address, 
                                              $wordCount, 
                                              $words);
        }

        goto ABORT if $status != $FM_OK;

        my $value = new Math::BigInt(0);
        map {
            $value->bior($words->[$_])->blsft($_ > 0 ? 32 : 0)
        } reverse(0 .. ($wordCount - 1));

        printf("%-30s 0x", $register);
        map {printf("%08x", $_)} reverse(@{$words});
        printf(" [%s]\n", $value->bstr());
    }
    else
    {
        my $value;

        if (defined($i2c) && $i2c == 1)
        {
            $status = $chip->fmI2cReadUINT32($switchNum, $address, \$value);
        }
        else
        {
            $status = $chip->fmReadUINT32($switchNum, $address, \$value);
        }

        goto ABORT if $status != $FM_OK;

        printf("%-30s 0x%08x [$value]\n", $register, $value);
    }
    $chip->enableErrors();
    return $status;

ABORT:
    printf("%-30s read failed!\n", $register);
    return $status;
}

##@cmethod private int _performSetBit(int   switchNum,
#                                     char  *register,
#                                     int   address,
#                                     int   bit,
#                                     bool  value)
#
# @desc         Sets a register bit
#
# @param[in]    switchNum The switch on which the register modification is to
#               be performed
#
# @param[in]    register The name of the register to be modified
#
# @param[in]    address The hardware address to be modified
#
# @param[in]    bit The bit to be set
#
# @param[in]    value The value the bit is to be set to
#
# @return       FM_OK if successful
sub _performSetBit
{
    my ($self, $switchNum, $register, $address, $bit, $value) = @_;

    my $chip = $self->{CHIP};

    my $status;
    $chip->disableErrors();
    if ($chip->registerIsMultiWord($register))
    {
        my $wordCount = $chip->registerGetWidth($register);
        my $words = [(0) x $wordCount];

        $status =
             $chip->fmReadUINT32Mult($switchNum, $address, $wordCount, $words);
        goto ABORT if $status != $FM_OK;

        $words->[int($bit / 32)] &= ~(1 << ($bit % 32));
        $words->[int($bit / 32)] |= (($value & 1) << ($bit % 32));

        $status =
            $chip->fmWriteUINT32Mult($switchNum, $address, $wordCount, $words);
    }
    else
    {
        my $ptr;

        $status = $chip->fmReadUINT32($switchNum, $address, \$ptr);
        goto ABORT if $status != $FM_OK;

        $ptr &= (~(1 << $bit) & 0xFFFFFFFF);
        $ptr |= ((($value & 1) << $bit) & 0xFFFFFFFF);

        $status = $chip->fmWriteUINT32($switchNum, $address, $ptr);
    }
    return $status;

ABORT:
    printf("%-30s read failed!\n", $register);
    return $status;
}

##@cmethod private int _performWrite(int   switchNum, 
#                                    int   address, 
#                                    char *value, 
#                                    int   i2c)
#
# @desc         Performs a register write
#
# @param[in]    switchNum The switch on which the register write is to be
#               performed
#
# @param[in]    register The name of the register to be written to
#
# @param[in]    address The hardware address to be written to
#
# @param[in]    value The string representation of the value to be written
#
# @param[in]    i2c Indicate whether the register write is to be performed
#               through the I2C bus.
# 
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub _performWrite
{
    my ($self, $switchNum, $register, $address, $value, $i2c) = @_;

    my $chip = $self->{CHIP};

    $value = new Math::BigInt($value);

    my $status;
    $chip->disableErrors();
    if ($chip->registerIsMultiWord($register))
    {
        my $wordCount = $chip->registerGetWidth($register);
        my $words = [(0) x $wordCount];

        map {
            $words->[$_] = $value->copy()->band(0xFFFFFFFF)->numify();
            $value->brsft(32);
        } (0 .. ($wordCount - 1));

        if (defined($i2c) && $i2c == 1)
        {
            $status = $chip->fmI2cWriteUINT32Mult($switchNum, 
                                                  $address, 
                                                  $wordCount, 
                                                  $words);
        }
        else
        {
            $status = $chip->fmWriteUINT32Mult($switchNum, 
                                               $address, 
                                               $wordCount, 
                                               $words);
        }
    }
    else
    {
        $value = $value->band(0xFFFFFFFF)->numify();

        if (defined($i2c) && $i2c == 1)
        {
            $status = $chip->fmI2cWriteUINT32($switchNum, 
                                              $address, 
                                              $value);
        }
        else
        {
            $status = $chip->fmWriteUINT32($switchNum, $address, $value);
        }
    }
    $chip->enableErrors();
    return $status == $FM_OK ? $FM_OK : $FM_FAIL;
}

##@cmethod public void handleRegRead(char *name, char *switchList, int i2c)
#
# @desc         Handles a single, possibly multi-word, register read
#
# @param[in]    register The name of the register to be read
#
# @param[in]    switchList The set of switches on which the register read is to
#               be performed
# 
# @param[in]    i2c Indicate whether the register read is to be performed
#               through the I2C bus.
sub handleRegRead
{
    my ($self, $register, $switchList, $i2c) = @_;

    my $chip = $self->{CHIP};

    if (!defined($i2c))
    {
        my $i2c = 0;
    }

    if (!defined($register))
    {
        print("Must specify <register>!\n");
        return;
    }

    if ($register =~ m/^0x/)
    {
        print("Global register access only supports named registers!\n");
        return;
    }
    if (!$chip->isRegister($register) || $chip->registerIsND($register))
    {
        print($TP_MSG_ERR_CSR_GLOBAL_INVALID);
        return;
    }

    my @switchList;
    if (!defined($switchList))
    {
        @switchList = @{[$self->tpGetSwitches]};
    }
    else
    {
        @switchList = $self->validateList($switchList,
                                         $self->tpPlatformGetSwitchRange);
    }

    my $address = $chip->$register(0);
    foreach my $switchNum (@switchList)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }

        printf("Switch %d:\n", $switchNum) if (scalar(@switchList) > 1);

        $self->_performRead($switchNum, $register, $address, $i2c);
    }
}

##@cmethod public void handleRegReadIndex(char  *register,
#                                         int   index,
#                                         char  *switchList
#                                         int   i2c)
#
# @desc         Handles a global indexed, possibly multi-word, register read
#
# @param[in]    register The name of the register to be read
#
# @param[in]    index The index of the register entry to be read
#
# @param[in]    switchList The set of switches on which the register read is to
#               be performed
# 
# @param[in]    i2c Indicate whether the register read is to be performed
#               through the I2C bus.
sub handleRegReadIndex
{
    my ($self, $register, @indices, $switchList, $i2c) = @_;

    my $chip = $self->{CHIP};

    my @bounds = $chip->registerGetBounds($register);

    my @dimension1 = (0);
    my @dimension2 = (0);
    my ($nD) = (0);
    
    if ($register =~ m/^0x/)
    {
        print("Index based register access only supports named registers!\n");
        return;
    }
    if (!$chip->isRegister($register)
        || (!$chip->registerIs1D($register) && !$chip->registerIs2D($register))
        || $chip->registerIsPortIndexed($register))
    {
        print($TP_MSG_ERR_CSR_INDEXED_INVALID);
        return;
    }

    if ($chip->registerIs1D($register))
    {
        my $index = shift(@indices);

        if (!defined($register) || !defined($index))
        {
            print("Must specify <register> <index>!\n");
            return;
        }

        $nD = 1;

        @dimension1 = $self->validateList($index, @{$bounds[0]});
        if (scalar(@dimension1) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }
    elsif ($chip->registerIs2D($register))
    {
        my $index1 = shift(@indices);
        my $index2 = shift(@indices);

        if (!defined($register) || !defined($index1) || !defined($index2))
        {
            print("Must specify <register> <index1> <index2>!\n");
            return;
        }

        $nD = 2;

        @dimension1 = $self->validateList($index1, @{$bounds[0]});
        if (scalar(@dimension1) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }

        @dimension2 = $self->validateList($index2, @{$bounds[1]});
        if (scalar(@dimension2) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }

    $switchList = shift(@indices);

    my @switchList;
    if (!defined($switchList))
    {
        @switchList = @{[$self->tpGetSwitches]};
    }
    else
    {
        @switchList = $self->validateList($switchList,
                                         $self->tpPlatformGetSwitchRange);
    }

    $i2c = shift(@indices);
    if (!defined($i2c))
    {
        my $i2c = 0;
    }

    foreach my $switchNum (@switchList)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }

        printf("Switch %d:\n", $switchNum) if (scalar(@switchList) > 1);

        foreach my $index1 (@dimension1)
        {
            foreach my $index2 (@dimension2)
            {
                my $address = $chip->$register($index1, $index2, 0);

                my $string = sprintf("%5d", $index1);
                if ($nD > 1)
                {
                    $string = sprintf("%s, %d", $string, $index2);
                }
                $self->_performRead($switchNum, $register, $address, $i2c, $string);
            }
        }
    }
}

##@cmethod public void handleRegReadPort(int port, char *register, int i2c)
#
# @desc         Handles a port indexed, possibly multi-word, register read
#
# @param[in]    port The set of ports to perform the register read for
#
# @param[in]    register The name of the register to be read
# @param[in]    i2c Indicate whether the register read is to be performed
#               through the I2C bus.
sub handleRegReadPort
{
    my ($self, $port, $register, @indices, $i2c) = @_;

    my @affectedPortList = ();

    my $chip = $self->{CHIP};

    my @bounds = $chip->registerGetBounds($register);

    my @dimension2 = (0);
    my ($nD) = (0);

    if ($register =~ m/^0x/)
    {
        print("Port based register access only supports named registers!\n");
        return;
    }
    if (!$chip->isRegister($register)
        || (!$chip->registerIs1D($register) && !$chip->registerIs2D($register))
        || !$chip->registerIsPortIndexed($register))
    {
        print($TP_MSG_ERR_CSR_PORT_INVALID);
        return;
    }

    if ($chip->registerIs1D($register))
    {
        if (!defined($port) || !defined($register))
        {
            print("Must specify <port> <register>!\n");
            return;
        }

        $nD = 1;
    }
    elsif ($chip->registerIs2D($register))
    {
        my $index = shift(@indices);

        if (!defined($port) || !defined($register) || !defined($index))
        {
            print("Must specify <port> <register> <index>!\n");
            return;
        }

        $nD = 2;

        @dimension2 = $self->validateList($index, @{$bounds[1]});
        if (scalar(@dimension2) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }

    $i2c = shift(@indices);
    if (!defined($i2c))
    {
        my $i2c = 0;
    }

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (scalar(@portList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    # Operate on each selected switch, one at a time
    foreach my $switchNum ($self->tpGetSwitches)
    {
        # Convert $port to an array of ports that appear only on this switch
        @portList = $self->validateExplicitList($TRUE, $port, 
                              $self->tpPlatformGetSwitchPortList($switchNum));

        # If none of the ports appear on this switch, go to the next switch
        if (scalar(@portList) == 0)
        {
           next;
        }

        # Add these ports to the affected list
        push(@affectedPortList, @portList);

        # Operate on each specified port that appears on this switch
        foreach my $globalPort (@portList)
        {
            my ($info, $logicalPort, $physPort, $sw);

            $sw = $switchNum;
            $logicalPort =
                ($self->tpPlatformMapGlobalToLogicalPort($globalPort))[1];
           
            $info = ($self->tpGetSwitchInfo())[$sw];

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                # Retrieve the switch number and logical port for the actual
                # chip.
                $chip->fmGetSwitchAndPortForSWAGPort($sw,
                                                     $logicalPort,
                                                     \$sw,
                                                     \$logicalPort);

                # Retrieve the switch information for the actual chip.
                $info = ($self->tpGetSwitchInfo())[$sw];
            }

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                ($sw, $physPort, @dimension2) =
                    $self->tpFM6000MapLogicalPortToPhysical($sw,
                                                            $logicalPort,
                                                            \@dimension2);
            }
            else
            {
                $chip->fmPlatformMapLogicalPortToPhysical($sw,
                                                          $logicalPort,
                                                          \$sw,
                                                          \$physPort);
            }

            foreach my $index (@dimension2)
            {
                my $address = $chip->$register($physPort, $index, 0);

                my $string = sprintf("Port %2d",  $globalPort);
                if ($nD > 1)
                {
                    $string = sprintf("%s [%d]", $string, $index);
                }
                $self->_performRead($sw, $register, $address, $i2c, $string);
            }
        }
    }

}   # end handleRegReadPort

##@cmethod public void handleRegWrite32(char *name, 
#                                       char *value, 
#                                       char *switchList)
#
# @desc         Handles a global, possibly multi-word, register write
#
# @param[in]    register The name of the register to be written to
#
# @param[in]    value The string representation of the value to be written
#
# @param[in]    switchList The set of switches on which the register write is
#               to be performed
# 
sub handleRegWrite32
{
    my ($self, $switchNum, $address, $value, $switchList) = @_;

    my $chip = $self->{CHIP};

    if (!defined($address) || !defined($value))
    {
        print("Must specify <address> <value>!\n");
        return;
    }

    if ($address =~ m/^0x/)
    {
        $address = hex($address);
    }

    if ($value =~ m/^0x/)
    {
        $value = hex($value);
    }

    return $chip->fmWriteUINT32($switchNum, $address, $value);
}


##@cmethod public void handleRegWrite64(char *name, 
#                                       char *value, 
#                                       char *switchList)
#
# @desc         Handles a global, possibly multi-word, register write
#
# @param[in]    register The name of the register to be written to
#
# @param[in]    value The string representation of the value to be written
#
# @param[in]    switchList The set of switches on which the register write is
#               to be performed
# 
sub handleRegWrite64
{
    my ($self, $switchNum, $address, $value, $switchList) = @_;

    my $chip = $self->{CHIP};

    if (!defined($address) || !defined($value))
    {
        print("Must specify <address> <value>!\n");
        return;
    }

    if ($address =~ m/^0x/)
    {
        $address = hex($address);
    }

    return $chip->fmWriteUINT64($switchNum, $address, Math::BigInt->new($value));
}


##@cmethod public void handleRegWrite(char *name, 
#                                     char *value, 
#                                     char *switchList
#                                     int  i2c)
#
# @desc         Handles a global, possibly multi-word, register write
#
# @param[in]    register The name of the register to be written to
#
# @param[in]    value The string representation of the value to be written
#
# @param[in]    switchList The set of switches on which the register write is
#               to be performed
# 
# @param[in]    i2c Indicate whether the register write is to be performed
#               through the I2C bus.
sub handleRegWrite
{
    my ($self, $register, $value, $switchList, $i2c) = @_;

    my $chip = $self->{CHIP};

    if (!defined($i2c))
    {
        my $i2c = 0;
    }

    if (!defined($register) || !defined($value))
    {
        print("Must specify <register> <value>!\n");
        return;
    }

    if ($register =~ m/^0x/)
    {
        print($TP_MSG_ERR_CSR_GLOBAL_INVALID_NAME);
        return;
    }
    if (!$chip->isRegister($register) || $chip->registerIsND($register))
    {
        print($TP_MSG_ERR_CSR_GLOBAL_INVALID);
        return;
    }

    my @switchList;
    if (!defined($switchList))
    {
        @switchList = @{[$self->tpGetSwitches]};
    }
    else
    {
        @switchList = $self->validateList($switchList,
                                         $self->tpPlatformGetSwitchRange);
    }

    my $address = $chip->$register(0);
    foreach my $switchNum (@switchList)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }

        $self->_performWrite($switchNum, $register, $address, $value, $i2c);
    }
}

##@cmethod public void handleRegWriteIndex(char *register,
#                                          char *value,
#                                          int  indices[],
#                                          char *switchList
#                                          int  i2c)
#
# @desc         Handles a global indexed, possibly multi-word, register write
#
# @param[in]    register The name of the register to be written to
#
# @param[in]    value The string representation of the value to be written
#
# @param[in]    indices The indices of the register entry to be written to
#
# @param[in]    switchList The set of switches on which the register write is
#               to be performed
#
# @param[in]    i2c Indicate whether the register write is to be performed
#               through the I2C bus.
# 
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub handleRegWriteIndex
{
    my ($self, $register, $value, @indices, $switchList, $i2c) = @_;

    my $chip = $self->{CHIP};

    my @bounds = $chip->registerGetBounds($register);

    my @dimension1 = (0);
    my @dimension2 = (0);
    my ($nD) = (0);

    if ($register =~ m/^0x/)
    {
        print($TP_MSG_ERR_CSR_INDEXED_INVALID_NAME);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    if (!$chip->isRegister($register)
        || (!$chip->registerIs1D($register) && !$chip->registerIs2D($register))
        || $chip->registerIsPortIndexed($register))
    {
        print($TP_MSG_ERR_CSR_INDEXED_INVALID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($chip->registerIs1D($register))
    {
        my $index = shift(@indices);

        if (!defined($register) || !defined($value) || !defined($index))
        {
            print("Must specify <register> <value> <index>!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        $nD = 1;

        @dimension1 = $self->validateList($index, @{$bounds[0]});
        if (scalar(@dimension1) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    elsif ($chip->registerIs2D($register))
    {
        my $index1 = shift(@indices);
        my $index2 = shift(@indices);

        if (!defined($register)
            || !defined($value)
            || !defined($index1)
            || !defined($index2))
        {
            print("Must specify <register> <value> <index1> <index2>!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        $nD = 2;

        @dimension1 = $self->validateList($index1, @{$bounds[0]});
        if (scalar(@dimension1) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        @dimension2 = $self->validateList($index2, @{$bounds[1]});
        if (scalar(@dimension2) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    $switchList = shift(@indices);
    
    my @switchList;
    if (!defined($switchList))
    {
        @switchList = @{[$self->tpGetSwitches]};
    }
    else
    {
        @switchList = $self->validateList($switchList,
                                         $self->tpPlatformGetSwitchRange);
    }

    $i2c = shift(@indices);
    if (!defined($i2c))
    {
        my $i2c = 0;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@switchList);
    foreach my $switchNum (@switchList)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }

        foreach my $index1 (@dimension1)
        {
            foreach my $index2 (@dimension2)
            {
                my $address = $chip->$register($index1, $index2, 0);

                my $status = $self->_performWrite($switchNum, 
                                                  $register,
                                                  $address,
                                                  $value,
                                                  $i2c);
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Cannot write to %s at (%d%s)!\n", $register,
                           $index1, $nD == 2 ? sprintf(", %d", $index2) : '');
                }
            }
        }
    }
    return $result;
}

##@cmethod public void handleRegWritePort(char  *port,
#                                         char  *register,
#                                         char  *value
#                                         int   i2c)
#
# @desc         Handles a port indexed, possible multi-word, register write
#
# @param[in]    port The set of ports to perform the register write for
#
# @param[in]    register The name of the register to be written to
#
# @param[in]    value The string representation of the value to be written
# @param[in]    i2c Indicate whether the register write is to be performed
#               through the I2C bus.
sub handleRegWritePort
{
    my ($self, $port, $register, $value, @indices, $i2c) = @_;
    
    my @affectedPortList = ();

    my $chip = $self->{CHIP};

    my @bounds = $chip->registerGetBounds($register);

    my @dimension2 = (0);
    my ($nD) = (0);

    if ($register =~ m/^0x/)
    {
        print($TP_MSG_ERR_CSR_PORT_INVALID_NAME);
        return;
    }
    if (!$chip->isRegister($register)
        || (!$chip->registerIs1D($register) && !$chip->registerIs2D($register))
        || !$chip->registerIsPortIndexed($register))
    {
        print($TP_MSG_ERR_CSR_PORT_INVALID);
        return;
    }

    if ($chip->registerIs1D($register))
    {
        if (!defined($port) || !defined($register) || !defined($value))
        {
            print("Must specify <port> <register> <value>!\n");
            return;
        }

        $nD = 1;
    }
    elsif ($chip->registerIs2D($register))
    {
        my $index = shift(@indices);

        if (!defined($port)
            || !defined($register)
            || !defined($value)
            || !defined($index))
        {
            print("Must specify <port> <register> <value> <index>!\n");
            return;
        }

        $nD = 2;

        @dimension2 = $self->validateList($index, @{$bounds[1]});
        if (scalar(@dimension2) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }

    $i2c = shift(@indices);
    if (!defined($i2c))
    {
        my $i2c = 0;
    }

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (scalar(@portList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    # Operate on each selected switch, one at a time
    foreach my $switchNum ($self->tpGetSwitches)
    {
        # Convert $port to an array of ports that appear only on this switch
        @portList = $self->validateExplicitList($TRUE, $port, 
                              $self->tpPlatformGetSwitchPortList($switchNum));

        # If none of the ports appear on this switch, go to the next switch
        if (scalar(@portList) == 0)
        {
           next;
        }

        # Add these ports to the affected list
        push(@affectedPortList, @portList);

        # Operate on each specified port that appears on this switch
        foreach my $globalPort (@portList)
        {
            my ($info, $logicalPort, $physPort, $sw);

            $sw = $switchNum;
            $logicalPort =
                ($self->tpPlatformMapGlobalToLogicalPort($globalPort))[1];

            $info = ($self->tpGetSwitchInfo())[$sw];

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                # Retrieve the switch number and logical port for the actual
                # chip.
                $chip->fmGetSwitchAndPortForSWAGPort($sw,
                                                     $logicalPort,
                                                     \$sw,
                                                     \$logicalPort);

                # Retrieve the switch information for the actual chip.
                $info = ($self->tpGetSwitchInfo())[$sw];
            }

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                ($sw, $physPort, @dimension2) =
                    $self->tpFM6000MapLogicalPortToPhysical($sw,
                                                            $logicalPort,
                                                            \@dimension2);
            }
            else
            {
                $chip->fmPlatformMapLogicalPortToPhysical($sw,
                                                          $logicalPort,
                                                          \$sw,
                                                          \$physPort);
            }

            foreach my $index (@dimension2)
            {
                my $address = $chip->$register($physPort, $index, 0);

                $self->_performWrite($sw, $register, $address, $value, $i2c);
            }
        }
    }

}   # end handleRegWritePort


##@cmethod public void handleRegSetBit(char *register,
#                                      int  bit,
#                                      char *switchList)
#
# @desc         Handles setting a bit in a global, possibly multi-word,
#               register
#
# @param[in]    register The name of the register to be modified
#
# @param[in]    bit The bit to be set
#
# @param[in]    switchList The set of switches on which the register
#               modification is to be performed
sub handleRegSetBit
{
    my ($self, $register, $bit, $switchList) = @_;

    my $chip = $self->{CHIP};

    if (!defined($register) || !defined($bit) )
    {
        print("Must specify <register> <bit>!\n");
        return;
    }

    if ($register =~ m/^0x/)
    {
        print($TP_MSG_ERR_CSR_GLOBAL_INVALID_NAME);
        return;
    }
    if (!$chip->isRegister($register) || $chip->registerIsND($register))
    {
        print($TP_MSG_ERR_CSR_GLOBAL_INVALID);
        return;
    }

    $bit = $self->str2decnum($bit);
    if (!defined($bit))
    {
        print($TP_MSG_ERR_CSR_INVALID_BIT);
        return;
    }

    my @switchList;
    if (!defined($switchList))
    {
        @switchList = @{[$self->tpGetSwitches]};
    }
    else
    {
        @switchList = $self->validateList($switchList,
                                         $self->tpPlatformGetSwitchRange);
    }

    my $address = $chip->$register(0);
    foreach my $switchNum (@switchList)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }

        $self->_performSetBit($switchNum, $register, $address, $bit, 1);
    }
}

##@cmethod public void handleRegSetBitIndex(char    *register,
#                                           int     index,
#                                           int     bit,
#                                           char    *switchList)
#
# @desc         Handles setting a bit in a global indexed, possibly multi-word,
#               register
#
# @param[in]    register The name of the register to be modified
#
# @param[in]    index The index of the register entry to be modified
#
# @param[in]    bit The bit to be set
#
# @param[in]    switchList The set of switches on which the register
#               modification is to be performed
sub handleRegSetBitIndex
{
    my ($self, $register, @indices, $bit, $switchList) = @_;

    my $chip = $self->{CHIP};

    my @bounds = $chip->registerGetBounds($register);

    my @dimension1 = (0);
    my @dimension2 = (0);
    my ($nD) = (0);

    if ($register =~ m/^0x/)
    {
        print($TP_MSG_ERR_CSR_INDEXED_INVALID_NAME);
        return;
    }
    if (!$chip->isRegister($register)
        || (!$chip->registerIs1D($register) && !$chip->registerIs2D($register))
        || $chip->registerIsPortIndexed($register))
    {
        print($TP_MSG_ERR_CSR_INDEXED_INVALID);
        return;
    }

    if ($chip->registerIs1D($register))
    {
        my $index = shift(@indices);
        $bit = shift(@indices);

        if (!defined($register) || !defined($index) || !defined($bit))
        {
            print("Must specify <register> <index> <bit>!\n");
            return;
        }

        $nD = 1;

        @dimension1 = $self->validateList($index, @{$bounds[0]});
        if (scalar(@dimension1) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }
    elsif ($chip->registerIs2D($register))
    {
        my $index1 = shift(@indices);
        my $index2 = shift(@indices);
        $bit = shift(@indices);

        if (!defined($register)
            || !defined($index1)
            || !defined($index2)
            || !defined($bit))
        {
            print("Must specify <register> <index1> <index2> <bit>!\n");
            return;
        }

        $nD = 2;

        @dimension1 = $self->validateList($index1, @{$bounds[0]});
        if (scalar(@dimension1) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }

        @dimension2 = $self->validateList($index2, @{$bounds[1]});
        if (scalar(@dimension2) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }

    $bit = $self->str2decnum($bit);
    if (!defined($bit))
    {
        print($TP_MSG_ERR_CSR_INVALID_BIT);
        return;
    }

    $switchList = shift(@indices);

    my @switchList;
    if (!defined($switchList))
    {
        @switchList = @{[$self->tpGetSwitches]};
    }
    else
    {
        @switchList = $self->validateList($switchList,
                                         $self->tpPlatformGetSwitchRange);
    }

    foreach my $switchNum (@switchList)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }

        foreach my $index1 (@dimension1)
        {
            foreach my $index2 (@dimension2)
            {
                my $address = $chip->$register($index1, $index2, 0);

                $self->_performSetBit($switchNum, $register, $address, $bit, 1);
            }
        }
    }
}

##@cmethod public void handleRegSetBitPort(char *port, char *register, int bit)
#
# @desc         Handles setting a bit in a port indexed, possibly multi-word,
#               register
#
# @param[in]    port The set of ports to perform the register modification for
#
# @param[in]    register The name of the register to be modified
#
# @param[in]    bit The bit to be set
sub handleRegSetBitPort
{
    my ($self, $port, $register, $bit, @indices) = @_;

    my @affectedPortList = ();

    my $chip = $self->{CHIP};
    
    my @bounds = $chip->registerGetBounds($register);

    my @dimension1 = (0);
    my @dimension2 = (0);
    my ($nD) = (0);

    if ($register =~ m/^0x/)
    {
        print($TP_MSG_ERR_CSR_PORT_INVALID_NAME);
        return;
    }
    if (!$chip->isRegister($register)
        || (!$chip->registerIs1D($register) && !$chip->registerIs2D($register))
        || !$chip->registerIsPortIndexed($register))
    {
        print($TP_MSG_ERR_CSR_PORT_INVALID);
        return;
    }

    if ($chip->registerIs1D($register))
    {
        if (!defined($port) || !defined($register) || !defined($bit))
        {
            print("Must specify <port> <register> <bit>!\n");
            return;
        }

        $nD = 1;
    }
    elsif ($chip->registerIs2D($register))
    {
        my $index = shift(@indices);

        if (!defined($port)
            || !defined($register)
            || !defined($bit)
            || !defined($index))
        {
            print("Must specify <port> <register> <bit> <index>!\n");
            return;
        }

        $nD = 2;

        @dimension2 = $self->validateList($index, @{$bounds[1]});
        if (scalar(@dimension2) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (scalar(@portList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    $bit = $self->str2decnum($bit);
    if (!defined($bit))
    {
        print($TP_MSG_ERR_CSR_INVALID_BIT);
        return;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        @portList = $self->validateExplicitList($TRUE, $port,
                            $self->tpPlatformGetSwitchPortList($switchNum));

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
            my ($info, $logicalPort, $physPort, $sw);

            $sw = $switchNum;
            $logicalPort =
                ($self->tpPlatformMapGlobalToLogicalPort($globalPort))[1];

            $info = ($self->tpGetSwitchInfo())[$sw];

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                # Retrieve the switch number and logical port for the actual
                # chip.
                $chip->fmGetSwitchAndPortForSWAGPort($sw,
                                                     $logicalPort,
                                                     \$sw,
                                                     \$logicalPort);

                # Retrieve the switch information for the actual chip.
                $info = ($self->tpGetSwitchInfo())[$sw];
            }

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                ($sw, $physPort, @dimension2) =
                    $self->tpFM6000MapLogicalPortToPhysical($sw,
                                                            $logicalPort,
                                                            \@dimension2);
            }
            else
            {
                $chip->fmPlatformMapLogicalPortToPhysical($sw,
                                                          $logicalPort,
                                                          \$sw,
                                                          \$physPort);
            }

            foreach my $index (@dimension2)
            {
                my $address = $chip->$register($physPort, $index, 0);

                $self->_performSetBit($sw, $register, $address, $bit, 1);
            }
        }
    }

}   # end handleRegSetBitPort


##@cmethod public void handleRegClearBit(char   *register,
#                                        int    bit,
#                                        char   *switchList)
#
# @desc         Handles clearing a bit in a global, possibly multi-word,
#               register
#
# @param[in]    bit The bit to be cleared
#
# @param[in]    switchList The set of switches on which the register
#               modification is to be performed
sub handleRegClearBit
{
    my ($self, $register, $bit, $switchList) = @_;

    my $chip = $self->{CHIP};

    if (!defined($register) || !defined($bit) )
    {
        print("Must specify <register> <bit>!\n");
        return;
    }

    if ($register =~ m/^0x/)
    {
        print("Global register access only supports named registers!\n");
        return;
    }
    if (!$chip->isRegister($register) || $chip->registerIsND($register))
    {
        print("Must specify valid global register!\n");
        return;
    }

    $bit = $self->str2intnum($bit);
    if (!defined($bit))
    {
        print("Must specify valid <bit> value!\n");
        return;
    }

    my @switchList;
    if (!defined($switchList))
    {
        @switchList = @{[$self->tpGetSwitches]};
    }
    else
    {
        @switchList = $self->validateList($switchList,
                                         $self->tpPlatformGetSwitchRange);
    }

    my $address = $chip->$register(0);
    foreach my $switchNum (@switchList)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }

        $self->_performSetBit($switchNum, $register, $address, $bit, 0);
    }
}

##@cmethod public void handleRegClearBit(char   *register,
#                                        int    index,
#                                        int    bit,
#                                        char   *switchList)
#
# @desc         Handles clearing a bit in a global indexed, possibly
#               multi-word, register
#
# @param[in]    index The index of the register entry to be modified
#
# @param[in]    bit The bit to be cleared
#
# @param[in]    switchList The set of switches on which the register
#               modification is to be performed
sub handleRegClearBitIndex
{
    my ($self, $register, @indices, $bit, $switchList) = @_;

    my $chip = $self->{CHIP};

    my @dimension1 = (0);
    my @dimension2 = (0);
    my ($nD) = (0);

    my @bounds = $chip->registerGetBounds($register);

    if ($register =~ m/^0x/)
    {
        print($TP_MSG_ERR_CSR_INDEXED_INVALID_NAME);
        return;
    }
    if (!$chip->isRegister($register)
        || (!$chip->registerIs1D($register) && !$chip->registerIs2D($register))
        || $chip->registerIsPortIndexed($register))
    {
        print($TP_MSG_ERR_CSR_INDEXED_INVALID);
        return;
    }

    if ($chip->registerIs1D($register))
    {
        my $index = shift(@indices);
        $bit = shift(@indices);

        if (!defined($register) || !defined($index) || !defined($bit))
        {
            print("Must specify <register> <index> <bit>!\n");
            return;
        }

        $nD = 1;

        @dimension1 = $self->validateList($index, @{$bounds[0]});
        if (scalar(@dimension1) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }
    elsif ($chip->registerIs2D($register))
    {
        my $index1 = shift(@indices);
        my $index2 = shift(@indices);
        $bit = shift(@indices);

        if (!defined($register)
            || !defined($index1)
            || !defined($index2)
            || !defined($bit))
        {
            print("Must specify <register> <index1> <index2> <bit>!\n");
            return;
        }

        $nD = 2;

        @dimension1 = $self->validateList($index1, @{$bounds[0]});
        if (scalar(@dimension1) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }

        @dimension2 = $self->validateList($index2, @{$bounds[1]});
        if (scalar(@dimension2) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }

    $bit = $self->str2decnum($bit);
    if (!defined($bit))
    {
        print($TP_MSG_ERR_CSR_INVALID_BIT);
        return;
    }

    $switchList = shift(@indices);
    
    my @switchList;
    if (!defined($switchList))
    {
        @switchList = @{[$self->tpGetSwitches]};
    }
    else
    {
        @switchList = $self->validateList($switchList,
                                         $self->tpPlatformGetSwitchRange);
    }

    foreach my $switchNum (@switchList)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }

        foreach my $index1 (@dimension1)
        {
            foreach my $index2 (@dimension2)
            {
                my $address = $chip->$register($index1, $index2, 0);

                $self->_performSetBit($switchNum, $register, $address, $bit, 0);
            }
        }
    }
}

##@cmethod public void handleRegClearBitPort(char   *port,
#                                            char   *register,
#                                            int    bit)
#
# @desc         Handles clearing a bit in a port indexed, possibly multi-word,
#               register
#
# @param[in]    port The set of ports to perform the register modification for
#
# @param[in]    register The name of the register to be modified
#
# @param[in]    bit The bit to be cleared
sub handleRegClearBitPort
{
    my ($self, $port, $register, $bit, @indices) = @_;
    
    my @affectedPortList = ();

    my $chip = $self->{CHIP};

    my @bounds = $chip->registerGetBounds($register);

    my @dimension1 = (0);
    my @dimension2 = (0);
    my ($nD) = (0);

    if ($register =~ m/^0x/)
    {
        print($TP_MSG_ERR_CSR_PORT_INVALID_NAME);
        return;
    }
    if (!$chip->isRegister($register)
        || (!$chip->registerIs1D($register) && !$chip->registerIs2D($register))
        || !$chip->registerIsPortIndexed($register))
    {
        print($TP_MSG_ERR_CSR_PORT_INVALID);
        return;
    }

    if ($chip->registerIs1D($register))
    {
        if (!defined($port) || !defined($register) || !defined($bit))
        {
            print("Must specify <port> <register> <bit>!\n");
            return;
        }

        $nD = 1;
    }
    elsif ($chip->registerIs2D($register))
    {
        my $index = shift(@indices);

        if (!defined($port)
            || !defined($register)
            || !defined($bit)
            || !defined($index))
        {
            print("Must specify <port> <register> <bit> <index>!\n");
            return;
        }

        $nD = 2;

        @dimension2 = $self->validateList($index, @{$bounds[1]});
        if (scalar(@dimension2) == 0)
        {
            print($TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY);
            return;
        }
    }

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (scalar(@portList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    $bit = $self->str2decnum($bit);
    if (!defined($bit))
    {
        print($TP_MSG_ERR_CSR_INVALID_BIT);
        return;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        @portList = $self->validateExplicitList($TRUE, $port,
                            $self->tpPlatformGetSwitchPortList($switchNum));

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
            my ($info, $logicalPort, $physPort, $sw);

            $sw = $switchNum;
            $logicalPort =
                ($self->tpPlatformMapGlobalToLogicalPort($globalPort))[1];

            $info = ($self->tpGetSwitchInfo())[$sw];

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                # Retrieve the switch number and logical port for the actual
                # chip.
                $chip->fmGetSwitchAndPortForSWAGPort($sw,
                                                     $logicalPort,
                                                     \$sw,
                                                     \$logicalPort);

                # Retrieve the switch information for the actual chip.
                $info = ($self->tpGetSwitchInfo())[$sw];
            }

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                ($sw, $physPort, @dimension2) =
                    $self->tpFM6000MapLogicalPortToPhysical($sw,
                                                            $logicalPort,
                                                            \@dimension2);
            }
            else
            {
                $chip->fmPlatformMapLogicalPortToPhysical($sw,
                                                          $logicalPort,
                                                          \$sw,
                                                          \$physPort);
            }

            foreach my $index (@dimension2)
            {
                my $address = $chip->$register($physPort, $index, 0);

                $self->_performSetBit($sw, $register, $address, $bit, 0);
            }
        }
    }

}   # end handleRegClearBitPort


##@cmethod public void handleRegDbgDump(int sw, char *register, int index1, int index2, int index3)
#
# @desc         Calls the API fmDbgDumpRegister function.
#
# @param[in]    sw is the switch on which to operate.
#
# @param[in]    register The name of the register to be read
#
# @param[in]    index1 is the primary index into the register (if any).
#
# @param[in]    index2 is the secondary index into the register (if any).
#
# @param[in]    index3 is the tertiary index into the register (if any).
sub handleRegDbgDump
{
    my ($self, $sw, $register, $index1, $index2, $index3) = @_;

    my $chip = $self->{CHIP};
    
    my $idxA = "0";
    my $idxB = "0";
    my $idxC = "0";

    if (!defined($register))
    {
        print("Must specify a register name!\n");
        return;
    }

    if (defined($index1))
    {
        $idxA = ($index1 eq "all") ? "-1" : $index1;
    }
    
    if (defined($index2))
    {
        $idxB = ($index2 eq "all") ? "-1" : $index2;
    }
    
    if (defined($index3))
    {
        $idxC = ($index3 eq "all") ? "-1" : $index3;
    }

    # Don't know the max range, so use a large value
    my @listA = $self->validateList($idxA, -1, 99999);
    my @listB = $self->validateList($idxB, -1, 99999);
    my @listC = $self->validateList($idxC, -1, 99999);

    foreach my $iC (@listC)
    {
        foreach my $iB (@listB)
        {
            foreach my $iA (@listA)
            {
                my $status = $chip->fmDbgDumpRegisterV3($sw, $iA, $iB, $iC, $register);
            }
        }
    }
    
}


##@cmethod public void handleRegDbgSet(int sw, char *register, fm_uint32 value, int word, int index1, int index2, int index3)
#
# @desc         Calls the API fmDbgWriteRegister function.
#
# @param[in]    sw is the switch on which to operate.
#
# @param[in]    register The name of the register to be read
#
# @param[in]    value is the 32-bit value to write
#
# @param[in]    word is the word offset into >32-bit wide registers
#
# @param[in]    index1 is the primary index into the register (if any).
#
# @param[in]    index2 is the secondary index into the register (if any).
#
# @param[in]    index3 is the tertiary index into the register (if any).
sub handleRegDbgSet
{
    my ($self, $sw, $register, $value, $word, $index1, $index2, $index3) = @_;

    my $chip = $self->{CHIP};
    
    my $wordOffset = 0;
    my $idxA = "0";
    my $idxB = "0";
    my $idxC = "0";    

    if (!defined($register))
    {
        print("Must specify a register name!\n");
        return;
    }
    
    if (!defined($value) )
    {
        print("Must specify a register value to write!\n");
        return;
    }
    
    if (defined($word))
    {
        $wordOffset = $word;
    }

    if (defined($index1))
    {
        $idxA = ($index1 eq "all") ? "-1" : $index1;
    }
    
    if (defined($index2))
    {
        $idxB = ($index2 eq "all") ? "-1" : $index2;
    }
    
    if (defined($index3))
    {
        $idxC = ($index3 eq "all") ? "-1" : $index3;
    }

    # Don't know the max range, so use a large value
    my @listA = $self->validateList($idxA, -1, 99999);
    my @listB = $self->validateList($idxB, -1, 99999);
    my @listC = $self->validateList($idxC, -1, 99999);

    foreach my $iC (@listC)
    {
        foreach my $iB (@listB)
        {
            foreach my $iA (@listA)
            {
                my $status = $chip->fmDbgWriteRegisterV3($sw, 
                                                         $wordOffset, 
                                                         $iA, 
                                                         $iB, 
                                                         $iC, 
                                                         $register, 
                                                         $value);
            }
        }
    }

}

##@cmethod public void handleRegDbgSetField(int sw, char *register, char *field, fm_uint32 value, int index1, int index2, int index3)
#
# @desc         Calls the API fmDbgWriteRegisterField function.
#
# @param[in]    sw is the switch on which to operate.
#
# @param[in]    register is he name of the register to be read
#
# @param[in]    field is the field name of specified register
#
# @param[in]    index1 is the primary index into the register (if any).
#
# @param[in]    index2 is the secondary index into the register (if any).
#
# @param[in]    index3 is the tertiary index into the register (if any).
sub handleRegDbgSetField
{
    my ($self, $sw, $register, $field, $value, $index1, $index2, $index3) = @_;

    my $chip = $self->{CHIP};
    
    my $idxA = "0";
    my $idxB = "0";
    my $idxC = "0";
    
    if (!defined($register))
    {
        print("Must specify a register name!\n");
        return;
    }
    
    if (!defined($field))
    {
        print("Must specify a field name!\n");
        return;
    }

    if (!defined($value) )
    {
        print("Must specify a field value to write!\n");
        return;
    }
    
    if (defined($index1))
    {
        $idxA = ($index1 eq "all") ? "-1" : $index1;
    }
    
    if (defined($index2))
    {
        $idxB = ($index2 eq "all") ? "-1" : $index2;
    }
    
    if (defined($index3))
    {
        $idxC = ($index3 eq "all") ? "-1" : $index3;
    }

    # Don't know the max range, so use a large value
    my @listA = $self->validateList($idxA, -1, 99999);
    my @listB = $self->validateList($idxB, -1, 99999);
    my @listC = $self->validateList($idxC, -1, 99999);

    foreach my $iC (@listC)
    {
        foreach my $iB (@listB)
        {
            foreach my $iA (@listA)
            {
                my $status = $chip->fmDbgWriteRegisterField($sw, 
                                                            $iA, 
                                                            $iB, 
                                                            $iC, 
                                                            $register, 
                                                            $field, 
                                                            Math::BigInt->new($value));
            }
        }
    }

    


}

##@cmethod public void handleRegDbgList(int sw, char globals, char ports)
#
# @desc         Calls the API fmDbgListRegisters function.
#
# @param[in]    sw is the switch on which to operate.
#
# @param[in]    globals indicates whether to list global registers.
#
# @param[in]    ports  indicates whether to list per-port registers.
sub handleRegDbgList
{
    my ($self, $sw, $globals, $ports, @indices) = @_;
    my $chip = $self->{CHIP};
    my $status = $chip->fmDbgListRegisters($sw, $globals, $ports);
}

##@cmethod public void handleRegDbgCapture(int sw, char *filename)
#
# @desc         Calls the API fmDbgDumpRegister function for the
#               FM_ALL_CONFIG_REGS composite register alias and
#               writes the output to a file. Used for generating
#               EEPROM images.
#
# @param[in]    sw is the switch on which to operate.
#
# @param[in]    filename is the name of the file in which to capture the
#               output.
sub handleRegDbgCapture
{
    my ($self, $sw, $filename) = @_;
    my $chip = $self->{CHIP};
    my $status;
    
    printf("Please wait...\n");
    
    
    # Register the callback and open the capture file.
    my %cookie1 = (type => "fm_int", value => 1); 
    my %cookie2 = (type => "fm_int", value => 0); 
    $chip->fmCaptureFileCallback($filename, \%cookie1, \%cookie2);
    
    if ($cookie2{value} != $FM_OK)
    {
        printf("Unable to open capture file: $filename.\n");
        return;
    }

    # Now dump the registers.                                         
    $status = $chip->fmDbgDumpRegisterV3($sw, 0, 0, 0, "FM_ALL_CONFIG_REGS");

    # Close the capture file, unregister the callback, restore console
    $cookie1{value} = 2;
    $chip->fmCaptureFileCallback($filename, \%cookie1, \%cookie2);
    
    if ($cookie2{value} != $FM_OK)
    {
        printf("Unable to close capture file: $filename.\n");
    }
    
}


# @desc         Convert a logical port mask to a physical port mask.
#
# @param[in]    $self object
#
# @param[in]    $logicalMask is the mask to convert.
sub mapLogicalMaskToPhysical
{
    my ($self, $logicalMask) = @_;
    my $chip = $self->{CHIP};
    
    my ($minPort, $maxPort) = $self->tpPlatformGetPortRange();
    my $physMask = 0;
    my $physPort;
    my $sw;
    
    for (my $logicalPort = $minPort ; $logicalPort <= $maxPort ; $logicalPort++)
    {
        if ( $logicalMask & (1 << $logicalPort) )
        {
            $chip->fmPlatformMapLogicalPortToPhysical(0,
                                                      $logicalPort, 
                                                      \$sw,
                                                      \$physPort);

            $physMask |= (1 << $physPort);
        }
    }
    
    return $physMask;
}



##@cmethod public void handleRegDbgEeprom(int sw, char *filename)
#
# @desc         Calls the API fmDbgDumpRegister function for the
#               FM_ALL_CONFIG_REGS composite register alias and
#               writes the output to a file, then calls the EEPROM
#               image file generation script.
#
# @param[in]    sw is the switch on which to operate.
#
# @param[in]    filename is the name of the file in which to capture the
#               output.
sub handleRegDbgEeprom
{
    my ($self, $sw, $filename) = @_;
    my $chip = $self->{CHIP};
    my $status;
    my $tempFile;

    # update the sw number
    $_eepromImageConfig{'switch_number'} = $sw;

    # show current eeprom config
    printf("EEPROM image file will be generated with these options:\n\n");
    handleShowEepromConfig($self);
    
    printf("\nThese may be changed with the 'set eeprom config' command\n");
    
    # config validation
    my $answer = "";
    while ($answer ne "y" && $answer ne "n")
    {
        printf("\nContinue with these options (y/n)? ");
        $answer = <STDIN>;
        chomp($answer);
    }
    print "$answer\n";
    return if ($answer eq "n");
    
    printf("\n\nRecording current chip configuration...\n");
    
    # Register the callback and open the capture file.
    my %cookie1 = (type => "fm_int", value => 1); 
    my %cookie2 = (type => "fm_int", value => 0); 
    $tempFile = $filename . ".tmp";
    
    if ( $_eepromImageConfig{'switch_family'} != $FM_SWITCH_FAMILY_FM6000 ||
         $_eepromImageConfig{'minimal'} == 0 )
    {
        $chip->fmCaptureFileCallback($tempFile, \%cookie1, \%cookie2);
        
        if ($cookie2{value} != $FM_OK)
        {
            printf("Unable to open capture file: $tempFile.\n");
            return;
        }
    
        # Set a flag in the API to indicate that FM_PORT_CFG_2 must be read
        # using the FM_PORT_MASK attribute to reflect the user's config,
        # because FM_PORT_CFG_2 will be 0x00000000 if the port is not up 
        # (no cable or loopback connected on the port). 
        $status = $chip->fmDbgSetMiscAttribute(0, $FM_DBG_ATTR_PORT_CFG2, 1);
    
        # Now dump the registers.                                         
        $status = $chip->fmDbgDumpRegisterV3($sw, 0, 0, 0, "FM_ALL_CONFIG_REGS");
    
        # Reset the flag
        $status = $chip->fmDbgSetMiscAttribute(0, $FM_DBG_ATTR_PORT_CFG2, 0);
    
        # Close the capture file, unregister the callback, restore console
        $cookie1{value} = 2;
        $chip->fmCaptureFileCallback($tempFile, \%cookie1, \%cookie2);
        
        if ($cookie2{value} != $FM_OK)
        {
            printf("Unable to close capture file: $tempFile.\n");
            return;
        }
    }
    
    my $result = -1;

    if ($_eepromImageConfig{'switch_family'} == $FM_SWITCH_FAMILY_FM6000)
    {
        # FM6000 case
        $result = $self->altaHandleRegDbgEeprom($sw, $filename, $tempFile);
    }
    else
    {
        # FM2000 & FM4000 case
        # 
        # Build option string
        my $cmdOptions = "";
        my $chipName = getChipName();
    
        if (!defined $chip)
        {
            # An error message was already printed by getChipName.
            if (-e $tempFile)
            {
                unlink ("$tempFile");
            }
            return;
        }
    
        
        $cmdOptions .= "-c $chipName";
            
        # Port mask
        if ( $_eepromImageConfig{'ports'} != 0 )
        {
            my $phyPortMask = mapLogicalMaskToPhysical($self, $_eepromImageConfig{'ports'});
            my $hexMask = sprintf "%lx", $phyPortMask;
            $cmdOptions = $cmdOptions . " -m 0x$hexMask";
        }
        
        if ( $_eepromImageConfig{'type'} eq "SPI_2" )
        {
            $cmdOptions = $cmdOptions . " -2";
        }
        
        # Booleans
        if ( $_eepromImageConfig{'include_cpu'} )
        {
            $cmdOptions = $cmdOptions . " -0";
        }
        
        if ( $_eepromImageConfig{'unused_fids'} )
        {
            $cmdOptions = $cmdOptions . " -f";
        }
    
        my $cpu_clk_freq = $_eepromImageConfig{'ebi_freq'};
        if ($cpu_clk_freq < 1 or $cpu_clk_freq > 133) {
            $cpu_clk_freq = 133;
        }
        # Round clock period down.  That will ensure the delay
        # calculation errs on the high side.
        my $cpu_clk_period = int(1000/$cpu_clk_freq);
        
        # Other options
        $cmdOptions = $cmdOptions . " -k $_eepromImageConfig{'size'}"
                                  . " -b $_eepromImageConfig{'block'}"
                                  . " -p $cpu_clk_period"
                                  . " -$_eepromImageConfig{'format'}";
    
        # Reference clock selection
        # Generates a 24-character string with the first character
        # representing physical port 1 and the last representing
        # physical port 24.
        # "a" = REFCLKA, "b" = REFCLKB, "r" = use register values,
        # "s" = standard (REFCLKA for 4-lane ports, REFCLKB for 1-lane ports)
        my $rstr = "r" x 24;
        for my $logPort (1..24) {
            my $refclk   = $_eepromImageConfig{'refclk'}->[$logPort];
            my $physPort;
            my $sw;
            $chip->fmPlatformMapLogicalPortToPhysical(0, $logPort, \$sw,
                                                      \$physPort);
            my $char = ($refclk&1)?"b":"a";
            if ($physPort >= 1 and $physPort <= 24) {
                substr $rstr, ($physPort-1), 1, $char;
            }
        }
        $cmdOptions .= " -r $rstr";
    
        # Lane reversal and polarity settings
        my %lrp_settings = ();
        for my $logPort (1..24) {
            my $lane_rev = $_eepromImageConfig{'lane_rev'}->[$logPort];
            my $polarity = $_eepromImageConfig{'polarity'}->[$logPort];
            my ($sw, $physPort);
            $chip->fmPlatformMapLogicalPortToPhysical(0, $logPort, \$sw,
                                                      \$physPort);
            my $cfg_val = (($lane_rev&3)<<8) | ($polarity&0xFF);
            if ($physPort >= 1 and $physPort <= 24) {
                if (not defined $lrp_settings{$cfg_val}) {
                    $lrp_settings{$cfg_val} = 0;
                }
                $lrp_settings{$cfg_val} |= (1<<$physPort);
            }
        }
        my @wlist = ();
        foreach my $setting (sort { $a <=> $b } keys %lrp_settings) {
            my $mask = $lrp_settings{$setting};
            push @wlist, sprintf("%x=%x", $mask, $setting);
        }
        my $wstr = join ":", @wlist;
        $cmdOptions .= " -w $wstr";
        
        $cmdOptions .= " -S SCHED";
    
        # Call external script to generate image file.
        my $eepromScriptName = "regdump2eeprom.pl";
    
        printf("Generating EEPROM image - calling $eepromScriptName with these options: \n");
        printf("    $cmdOptions\n");
    
        $result = system("Applications/TestPoint/Common/$eepromScriptName $cmdOptions < $tempFile > $filename");
    }

    if (-e $tempFile && !$_eepromImageConfig{'keep_tmp_files'})
    {
        unlink ("$tempFile");
    }
        
    if ($result)
    {
        printf("\nEEPROM image file generation failed.\n");
    }
    else
    {
        printf("\nEEPROM image file $filename generated successfully.\n");
    }
}


##@cmethod public void altaHandleRegDbgEeprom(int sw, char *filename)
#
# @desc         Calls the API fmDbgDumpRegister function for the
#               FM_ALL_CONFIG_REGS composite register alias and
#               writes the output to a file, then calls the EEPROM
#               image file generation script.
#
# @param[in]    sw is the switch on which to operate.
#
# @param[in]    filename is the name of the file in which to capture the
#               output.
sub altaHandleRegDbgEeprom
{
    my ($self, $sw, $filename, $tempFile) = @_;
    my $chip = $self->{CHIP};
    my $result = -1;

    # ignore port lists for minimal images
    if ( !$_eepromImageConfig{'minimal'} ) {
        # check that log40GportList and/or log10GportList are defined.
        # This is required in order to initialize the SchedulerRing.
        # Quit immediatly if both lists are not defined.
        if (!(@{$_eepromImageConfig{'log40GportList'}} + 0) &&
            !(@{$_eepromImageConfig{'log10GportList'}} + 0) )
        {
            print "ERROR: 40G and 10G capable port lists are not defined\n";
            print "       At least one of these lists must be defined in order to initialize the Scheduler Ring\n";
            return $result;
        }
    }

    # Build option string
    my $eepromOptions = "";
        
    # Booleans
    if ( $_eepromImageConfig{'include_cpu'} )
    {
        $eepromOptions = $eepromOptions . " -0";
    }
    
    my $cpu_clk_freq = $_eepromImageConfig{'ebi_freq'};
    if ($cpu_clk_freq < 1 or $cpu_clk_freq > 133) {
        $cpu_clk_freq = 133;
    }
    # Round clock period down.  That will ensure the delay
    # calculation errs on the high side.
    my $cpu_clk_period = int(1000/$cpu_clk_freq);
    
    # Other options
    my $altaEepromConfigFile;

    $eepromOptions = $eepromOptions . " -k $_eepromImageConfig{'size'}"
                              . " -b $_eepromImageConfig{'block'}"
                              . " -p $cpu_clk_period"
                              . " -$_eepromImageConfig{'format'}";

    if ($_eepromImageConfig{'type'} eq 'SPI'){
        # Image offsets
        $eepromOptions .= sprintf(" -O %x,%x,%x,%x",
                                $_eepromImageConfig{'image_offset'}[0],
                                $_eepromImageConfig{'image_offset'}[1],
                                $_eepromImageConfig{'image_offset'}[2],
                                $_eepromImageConfig{'image_offset'}[3]);
        # SPI modes (for each image)
        $eepromOptions .= sprintf(" -M %d,%d,%d,%d",
                                $_eepromImageConfig{'eeprom_mode'}[0],
                                $_eepromImageConfig{'eeprom_mode'}[1],
                                $_eepromImageConfig{'eeprom_mode'}[2],
                                $_eepromImageConfig{'eeprom_mode'}[3]);
    
        # SPI clock rates -speeds- (for each image)
        $eepromOptions .= sprintf(" -C %d,%d,%d,%d",
                                $_eepromImageConfig{'eeprom_speed'}[0],
                                $_eepromImageConfig{'eeprom_speed'}[1],
                                $_eepromImageConfig{'eeprom_speed'}[2],
                                $_eepromImageConfig{'eeprom_speed'}[3]);
        # image number
        $eepromOptions .= " -I $_eepromImageConfig{'image_number'}";
        #   include_header 
        if ( $_eepromImageConfig{'include_header'} )
        {
            $eepromOptions = $eepromOptions . " -H";
        }
    }
    else
    {
        # I2C eeprom (only 1 image at 0x00)
        $eepromOptions .= " -O 0,0,0,0";
    }
    #   minimal (PCIe config) image
    if ( $_eepromImageConfig{'minimal'} )
    {
        $eepromOptions .= " -P";
    }
    #   PCIe lane reversal option
    if ($_eepromImageConfig{"pcie_lane_rev"})
    {
        $eepromOptions .= " -X $_eepromImageConfig{'pcie_lane_rev'}";
    }
    # PCIe endianness
    if ($_eepromImageConfig{"pcie_endianness"})
    {
        # add big endian switch 
        $eepromOptions .= " -B";
    }
    # Include Spico code
    if ($_eepromImageConfig{"initialize_spico"})
    {
        # add "Init Spico" switch 
        $eepromOptions .= " -L";
    }


    # Alta A0 only: include the MRL fix:
    if ( ($_eepromImageConfig{"mgmt_scratch_reg"} & 0xff00 ) == 0xa000)
    {
        $eepromOptions .= " -S MRL";
    }

    # add MGMT_SCRATCH register init
    $eepromOptions .= " -Z" . sprintf(" MGMT_SCRATCH=0x%x",$_eepromImageConfig{"mgmt_scratch_reg"});

    if ( $_eepromImageConfig{"pcie_cfg_id_reg"} != 0 )
    {
        $eepromOptions .= sprintf(",PCI_CFG_ID=0x%x", $_eepromImageConfig{"pcie_cfg_id_reg"} );
    }

    # create the eeprom spec file
    $altaEepromConfigFile = $self->altaCreateEepromSpecFile($sw, \$eepromOptions);
    if (defined $altaEepromConfigFile)
    {
        # Call external script to generate image file.
        my $eepromScriptName = "regdump2eeprom.pl";
        my $cmdLine = "-c alta -F $altaEepromConfigFile";

        print "Generating EEPROM image - calling $eepromScriptName with these options: \n    $cmdLine\n";

        if ($_eepromImageConfig{'minimal'} == 0)
        {
            $result = system("Applications/TestPoint/Common/$eepromScriptName $cmdLine < $tempFile > $filename");
        }
        else
        {
            $result = system("Applications/TestPoint/Common/$eepromScriptName $cmdLine > $filename");
        }

        unlink ("$altaEepromConfigFile") if (!$_eepromImageConfig{'keep_tmp_files'});
    }
    return $result;
}

##@cmethod      altaPrintPhysicalPortList
#
# @desc         create a comma separated list of ports.
#               The list  @logicalPortList is used as input. 
#               The output string  the list the physical ports.
#
sub altaPrintPhysicalPortList
{
    my ($self, $sw, @logicalPortList ) = @_;
    my $chip = $self->{CHIP};

    my @phyPortList;
    foreach my $logicalPort (@logicalPortList)
    {
        my ($logSw, $phyPort);
        $chip->fmPlatformMapLogicalPortToPhysical($sw, $logicalPort, \$logSw, \$phyPort);
        push @phyPortList, $phyPort;
    }
    print join (', ',@phyPortList) . "\n";
}

##@cmethod      altaCreateEepromSpecFile
#
# @desc         create an eeprom configuration (Alta only)
#
sub altaCreateEepromSpecFile
{
    my ($self, $sw, $commandLine_ref) = @_;

    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    # create a temporary file
    my $fileNum = -1;
    my $fileName;
    do {
        $fileNum++;
        $fileName = "altaEepromCfg" . $fileNum . ".cfg";
    } while (-e $fileName);

    # open output file
    open (OUTFILE, ">" , $fileName) or die "Could not open file $fileName. $!";
    select (OUTFILE);

    # print the eeprom header
    print <<EOF;
# Alta eeprom configuration file
#
# Ports not included in this file are considered as DISABLED
# If the port mode is DISABLED, all the other parameters are ignored.
# In the case of many modes, some parameters are redundant because they
# are implicit in the mode definition, however if they are included, they
# must be consistent with the specified mode, otherwise an error will be
# generated.
#
# Embedded eeprom image generator command line:
# Tag [commandLine]
# options: command line (optional) 
#
# Lists of ports 40G and 10G capables
# Tag: [40Gportlist], [10GportList]
# ports: list of physical ports 10G or 40G capable, comma separated
#
# Port parameter description:
# Tag: [portXX]
#
#  physiscalPort: physical port attached to the logical port (required) .
#  mode:    DISABLED
#           SGMII
#           1000BASE_X
#           1000BASE_KX
#           10GBASE_KR
#           10GBASE_CR
#           10GBASE_SR
#           XAUI
#           10GBASE_KX4
#           10GBASE_CX4
#           40GBASE_KR4
#           XLAUI
#           40GBASE_CR4
#           40GBASE_SR4
#       (required).
#           
#  speed: 1G, 2.5G, 10G, 40G (required for SGMII, optional for the other modes)
#           
#  lanes: 1, 2, 4 (optional) 
#  laneReversal: none, rx, tx, rxtx. (optional, default is 'none')
#  lanePolarity: normal, rx, tx, rxtx (optional, default is 'normal')
#  activeMac:    (optional, default is '0')
#  loopback:     on, off (optional, default is off)
#  mask:         "tp", logical port list (comma sep), "" 
#                 (optional, default is "")
#####################################################################
#####################################################################

EOF
    # embedded command line info
    if (defined $commandLine_ref and ${$commandLine_ref} ne "")
    {
        print "[commandLine]\noptions: " . ${$commandLine_ref} . "\n";
    }

    if ( !$_eepromImageConfig{'minimal'} ) 
    {
       # 40G and 10G capable logical ports. This is required for the SchedulerRing
        # initialization.
    
        if (@{$_eepromImageConfig{'log40GportList'}} + 0)
        {
            print "\n[40GportList]\nports: ";
            $self->altaPrintPhysicalPortList($sw, @{$_eepromImageConfig{'log40GportList'}});
        }
        if (@{$_eepromImageConfig{'log10GportList'}} + 0)
        {
            print "\n[10GportList]\nports: ";
            $self->altaPrintPhysicalPortList($sw, @{$_eepromImageConfig{'log10GportList'}});
        }
    
        # add port definition
        # get the list of only the local switch
        my @logPortList = $self->tpPlatformGetLogicalPortList($sw, "local,lag");
    
        foreach my $logPort (@logPortList)
        {
            my ($logSw, $phyPort);
            $chip->fmPlatformMapLogicalPortToPhysical($sw, $logPort, \$logSw, \$phyPort);
            next if $self->tpPlatformIsCPUPort($logPort);
    
    
            my @info = (0) x 4;
            my ($mode, $state);
            my %void;
    
            $chip->fmGetPortState($sw, $logPort, \$mode, \$state, \@info);
    
            # ignore ports in down state
            next if ($state != $FM_PORT_STATE_UP);
            # get the port mode
            %void = (type => "fm_uint32", value => 0);
            $chip->fmGetPortAttribute($sw, $logPort, $FM_PORT_ETHERNET_INTERFACE_MODE, \%void);
            my $eth_mode = $void{value};
    
            # skip disabled ports
            next if (!$eth_mode);
    
            # print logical port, physical port, and eth-mode
            print "\n[port $logPort]\n";
            print "physicalPort: $phyPort\n";
            print "mode: $alta_portMode_hash{$eth_mode}\n";
    
            # port speed
            %void = (type => "fm_uint32", value => 0);
            $chip->fmGetPortAttribute($sw, $logPort, $FM_PORT_SPEED, \%void);
    
            my $speed = $alta_portSpeed_hash{$void{value}};
            # set a default value
            $speed = "UNKNOWN" if (!defined $speed);
            print "speed: $speed\n";
    
            # port active mac
            if ((3 < $phyPort && $phyPort < 20) || (43 < $phyPort && $phyPort < 52))
            {
                %void = (type => "fm_int", value => 0);
                $status = $chip->fmGetPortAttribute($sw,  $logPort, $FM_PORT_SELECT_ACTIVE_MAC, \%void);
                if ($status == $FM_OK)
                {
                    print "activeMac: $void{value}\n";
                }
            }
    
            # lane polarity
            %void = (type => "fm_int", value => $FM_PORT_LANE_POLARITY_NORMAL);
    
               # FIXME: $FM_PORT_RX_LANE_POLARITY not implemented in fmGetPortAttribute() yet
       #       $status = $chip->fmGetPortAttribute($sw,  $logPort, $FM_PORT_RX_LANE_POLARITY, \%void);
              
               if ($status == $FM_OK)
            {
                # set rx lane polarity
                my $lanePol = ($void{value} == $FM_PORT_LANE_POLARITY_NORMAL ? 0 : 1);
    
                $void{value} = $FM_PORT_LANE_POLARITY_NORMAL;
    
                # FIXME: $FM_PORT_RX_LANE_POLARITY not implemented in fmGetPortAttribute() yet
    #            $status = $chip->fmGetPortAttribute($sw,  $logPort, $FM_PORT_TX_LANE_POLARITY, \%void);
    
                if ($status == $FM_OK)
                {
                    $lanePol |=  ($void{value} == $FM_PORT_LANE_POLARITY_NORMAL ? 0 : 2);
    
                    print "lanePolarity: $lanePolarity_hash{$lanePol}\n";
                }
            }
    
            # lane ordering
            %void = (type => "fm_int", value => 0);
            $status = $chip->fmGetPortAttribute($sw,  $logPort, $FM_PORT_RX_LANE_ORDERING, \%void);
            if ($status == $FM_OK)
            {
                # set rx lane polarity
                my $laneOrdering = ($void{value} == $FM_PORT_LANE_ORDERING_NORMAL ? 0 : 1);
    
                $void{value} = 0;
                $status = $chip->fmGetPortAttribute($sw,  $logPort, $FM_PORT_TX_LANE_ORDERING, \%void);
                if ($status == $FM_OK)
                {
                    $laneOrdering |=  ($void{value} == $FM_PORT_LANE_ORDERING_NORMAL ? 0 : 2);
    
                    print "laneReversal: $laneOrdering_hash{$laneOrdering}\n";
                }
            }
            
            # user defined loopbacks
            if (defined $_eepromImageConfig{'altaLoopbackList'}[$logPort] &&
                $_eepromImageConfig{'altaLoopbackList'}[$logPort] > 0)
            {
                print "loopback: on\n";
            }
    
            # user defined masks
            # TBD
        }
    }

    print "\n[end]\n";

    close OUTFILE;
    select(STDOUT);
    return $fileName;
}


##@cmethod void altaEepromConfigCheck()
#
# @desc         check Alta eeprom configuration
#
sub altaEepromConfigCheck
{
    my ($self) = @_;

    # Only 2 types of eeprom are supported: SPI (default) or I2C
    if ($_eepromImageConfig{'type'} ne 'I2C')
    {
        $_eepromImageConfig{'type'} = 'SPI';
    }
}

##@cmethod      getChipName()
#
# @desc         returns the chip name for the current switch family.
#
sub getChipName
{
    my ($self) = @_;
    my $switchFamily = $_eepromImageConfig{'switch_family'};
    my $chip;

    if ( $switchFamily == $FM_SWITCH_FAMILY_FM2000 )
    {
        $chip = "tahoe";
    }
    elsif ( $switchFamily == $FM_SWITCH_FAMILY_FM4000 )
    {
        $chip = "bali";
    }
    elsif ( $switchFamily == $FM_SWITCH_FAMILY_FM6000 )
    {
        $chip = "alta";
    }
    else
    {
        print("ERROR: Unrecognized device type $switchFamily\n");
    }
    return $chip;
}


##@cmethod public void handleShowEepromConfig()
#
# @desc         Display the current EEPROM image generation configuration.
#
sub handleShowEepromConfig
{
    my ($self) = @_;
    my $switchNum = 0;
    my $listFormatName;
    my $portList;
    my $formatName = "Unrecognized!";
    
    # Set switch_family 
    my $sw = $_eepromImageConfig{'switch_number'};
    my $switchFamily = (($self->tpGetSwitchInfo())[$sw])->{'switchFamily'};
    $_eepromImageConfig{'switch_family'} = $switchFamily;

    # Alta only: perform a precheck to verify the eeprom configuration.
    if ( $switchFamily == $FM_SWITCH_FAMILY_FM6000)
    {
        altaEepromConfigCheck($self);
    }

    # skip this for Alta. it is not supported yet.
    if ( $switchFamily != $FM_SWITCH_FAMILY_FM6000)
    {
        printf("Board Wiring Settings for destination board\n");
        printf("                   Lane      Polarity Reversal\n");
        printf("                 Reversal    RX Lane   TX Lane\n");
        printf("Port   REFCLK    RX    TX    D C B A   D C B A\n");
        printf("----   ------    --    --    -------   -------\n");
        foreach my $logPort (1..24) {
            my $refclk   = $_eepromImageConfig{'refclk'}->[$logPort];
            my $lane_rev = $_eepromImageConfig{'lane_rev'}->[$logPort];
            my $polarity = $_eepromImageConfig{'polarity'}->[$logPort];
            my @ynstr = ();
            for (my $p=1; $p>=0; $p--) {
                push @ynstr, (($lane_rev>>$p)&1)?"Y":"n";
            }
            for (my $p=7; $p>=0; $p--) {
                push @ynstr, (($polarity>>$p)&1)?"Y":"n";
            }
            printf(" %2d      %s        %s     %s" . 
                    "    %s %s %s %s   %s %s %s %s\n",
                    $logPort, ($refclk&1)?"B":"A", @ynstr);
        }
        printf("\n");
    }
    
    my $chip = getChipName();
    if (defined $chip)
    {
        printf("Chip                         $chip\n");
    }
    else
    {
        print "ERROR: undefined switch family\n";
        return;
    }
    printf("Switch number                $_eepromImageConfig{'switch_number'}\n");
    printf("EEPROM size (kbytes)         $_eepromImageConfig{'size'}\n");
    printf("EEPROM block (bytes)         $_eepromImageConfig{'block'}" .
            " (used for xml format)\n");
    printf("EEPROM type                  $_eepromImageConfig{'type'}\n");
    if ( $switchFamily != $FM_SWITCH_FAMILY_FM6000)
    {
        printf("EBI clock frequency          %.1f MHz\n",
                $_eepromImageConfig{'ebi_freq'});
    }
    else
    {
        printf("PCIe clock frequency         %.1f MHz\n",
                $_eepromImageConfig{'pcie_freq'});
    }

    # Lookup the format code    
    while ( ($listFormatName, my $formatCode) = each(%_eepromImageFormats) ) 
    {
        if ($formatCode eq $_eepromImageConfig{'format'})
        {
            $formatName = $listFormatName;
        }
    }
    
    printf("Image file format            $formatName\n");
    printf("Include traffic to CPU port  %s\n",
           $_eepromImageConfig{'include_cpu'} ? "yes" : "no");
    
    # Display port mask.
    foreach my $swNum ($self->tpGetSwitches)
    {
        $portList = $self->stringifyPortMask($swNum, $_eepromImageConfig{'ports'});
        last;
    }
    
    printf("Ports to write               %s\n", length($portList) > 0 ? $portList : "all");
    
    printf("Include unused FIDs          %s\n",
            $_eepromImageConfig{'unused_fids'} ? "yes" : "no");

    # Show Alta specific configuration
    if ( $switchFamily == $FM_SWITCH_FAMILY_FM6000)
    {
        # show SPI specific parameters
        if ($_eepromImageConfig{'type'} eq 'SPI'){
            printf("Eeprom image offsets         0x%6.6x,  0x%6.6x,  0x%6.6x,  0x%6.6x\n",
                    $_eepromImageConfig{'image_offset'}[0],
                    $_eepromImageConfig{'image_offset'}[1],
                    $_eepromImageConfig{'image_offset'}[2],
                    $_eepromImageConfig{'image_offset'}[3]);
            printf("Eeprom modes                 %8.8s,  %8.8s,  %8.8s,  %8.8s\n",
                    @eeprom_mode[$_eepromImageConfig{'eeprom_mode'}[0]],
                    @eeprom_mode[$_eepromImageConfig{'eeprom_mode'}[1]],
                    @eeprom_mode[$_eepromImageConfig{'eeprom_mode'}[2]],
                    @eeprom_mode[$_eepromImageConfig{'eeprom_mode'}[3]]);
            printf("Eeprom speeds                %5.5sMHz,  %5.5sMHz,  %5.5sMHz,  %5.5sMHz\n",
                    @eeprom_speed[$_eepromImageConfig{'eeprom_speed'}[0]],
                    @eeprom_speed[$_eepromImageConfig{'eeprom_speed'}[1]],
                    @eeprom_speed[$_eepromImageConfig{'eeprom_speed'}[2]],
                    @eeprom_speed[$_eepromImageConfig{'eeprom_speed'}[3]]);

            printf("Include eeprom header        %s\n",
                    $_eepromImageConfig{'include_header'} ? "yes" : "no");
            printf("Generate image number        $_eepromImageConfig{'image_number'}\n");
        }
        printf("Generate minimal image       %s\n",
                $_eepromImageConfig{'minimal'} ? "yes" : "no");
        printf("PCIe lane reversal           %s\n", 
                @pcie_lane_rev[$_eepromImageConfig{"pcie_lane_rev"}]);
        printf("PCIe endianness              %s endian\n",
                $_eepromImageConfig{'pcie_endianness'} ? "big" : "little");
        printf("PCIe ID                      0x%8.8x\n", 
                $_eepromImageConfig{"pcie_cfg_id_reg"} );
        printf("Initialize Spico             %s\n",
                $_eepromImageConfig{'initialize_spico'} ? "yes" : "no");
       
        $self->printPortList("40G capable port list (log)  ",
                             $_eepromImageConfig{'log40GportList'});
        $self->printPortList("10G capable port list (log)  ",
                             $_eepromImageConfig{'log10GportList'});
        my @loopbackList;

        my $loopbackPort = 0;
        foreach my $loopbackOn (@{$_eepromImageConfig{'altaLoopbackList'}})
        {
            if ($loopbackOn > 0)
            {
                push @loopbackList,$loopbackPort;
            }
            $loopbackPort++;
        }
        $self->printPortList("Loopback port list (log)     ", \@loopbackList);
    }

    printf("Image version                0x%8.8x\n",
            $_eepromImageConfig{"mgmt_scratch_reg"} );
}

##@cmethod void printPortList
#
# @desc         Display a list of ports
#
sub printPortList
{
    my ($self, $string, $portList_ref) = @_;

    my $strLen = length($string);
    my @localPortList = @{$portList_ref};
    if ((@localPortList + 0) == 0) {
        # empty list case
        print "$string--\n";
    }
    else {
        my @subList;
        while (@localPortList + 0)
        {
            @subList = splice (@localPortList, 0, 12);
            print "$string" . join (', ',@subList) . "\n";
            $string = " " x $strLen;
        }
    }
}

##@cmethod public void handleSetEepromConfig()
#
# @desc         Set an EEPROM image generation configuration parameter.
#
# @param[in]    $attribute is the configuration parameter name
#
# @param[in]    @args is the list of configuration parameter values
#
sub handleSetEepromConfig
{
    my ($self, $attribute, @args) = @_;

    if ( $attribute eq "lane_rev" ||
         $attribute eq "polarity" ||
         $attribute eq "refclk" )
    {
        $self->handleSetEepromBoardConfig($attribute, @args);
        return;
    }

    if ($attribute eq "pcie_lane_rev"   ||
        $attribute eq "image_offset"    ||
        $attribute eq "image_spi_mode"  ||
        $attribute eq "image_spi_speed" ||
        $attribute eq "eeprom_version"  ||
        $attribute eq "pcie_endianness")
    {
        $self->handleSetAltaEepromBoardConfig($attribute, @args);
        return;
    }

    my $value = shift @args;

    my $chip = $self->{CHIP};
    
    if ( $attribute eq "size" ||
         $attribute eq "block")
    {
        # Numeric values
        if ($value =~ /\D/) 
        {
            printf("Value must be numeric!\n");
            return;
        }
    }
    elsif ( $attribute eq "ebi_freq" )
    {
        # Floating-point values
        if ($value !~ /^\d\d*$/ and $value !~ /^\d\d*\.\d*/) 
        {
            printf("Value must be numeric!\n");
            return;
        }
    }
    elsif ( $attribute eq "type" )
    {
        # Notes for Bali: SPI_3 and SPI are equivalent
        # Notes for Alta: SPI, SPI_2 and SPI_3 are equivalent
        if ($value ne "SPI_2" and $value ne "SPI_3" and
            $value ne "SPI"   and $value ne "I2C")
        {
            printf("Invalid eeprom type!\n");
            return;
        }
    }
    elsif ( $attribute eq "include_cpu" ||
            $attribute eq "unused_fids" )
    {
        # Booleans
        if (!exists( $_yesOrNo{$value} ))
        {
            printf("Invalid choice!\n");
            return;
        }

        $value = $_yesOrNo{$value};
    }
    elsif ( $attribute eq "format" )
    {
        # Format identifiers
        if (!exists( $_eepromImageFormats{$value} ))
        {
            printf("Invalid format!\n");
        }

        $_eepromImageConfig{$attribute} = $_eepromImageFormats{$value};
        return;
    }
    elsif ( $attribute eq "ports" )
    {
        # Port list
        my @portList = $self->validateList($value, $self->tpPlatformGetPortRange());
        
        if (!defined($value) || (scalar(@portList) == 0))
        {
            print("Must specify a valid port or port range!\n");
            return;
        }
        
        # Convert portList to mask
        my $portMask = 0x0;
        
        foreach my $port (@portList)
        {
            my ($sw, $logPort) =
                          $self->tpPlatformMapGlobalToLogicalPort($port);
            $portMask = $portMask | (1 << $logPort);
        }

        # If only port 0 was specified, then change mask to zero to
        # indicate reset to default mask (set of ports actually
        # configured.
        if ($portMask == 0x01)
        {
            $portMask = 0;
        }

        $value = $portMask;
    }
    elsif ( $attribute eq "image_number")
    {
        if ($value !~ /^[0-3]$/) 
        {
            printf("Invalid target image specification\n");
            return;
        }
    }
    elsif ( $attribute eq "minimal"             ||
            $attribute eq "include_header"      ||
            $attribute eq "initialize_spico"    ||
            $attribute eq "keep_tmp_files")
    {
        if (defined $value && $value =~ /^(yes|no)/) 
        {
            $value = $_yesOrNo{$value};
        }
        else
        {
            printf("You must type \"yes\" or \"no\"\n");
            return;
        }
    }
    elsif ( $attribute eq "40G_ports"  ||
            $attribute eq "10G_ports"  ||
            $attribute eq "loopback_ports"  )
    {

        # get the port list
        my @portList = ();
        if (defined($value))
        {
            @portList = $self->validateList($value, 1, 72);
            if (scalar(@portList) == 0)
            {
                print("Must specify a valid port or port range!\n");
                return;
            }
        }
        
        if ($attribute eq "40G_ports")
        {
            # list of logical 40G capable ports
            @{$_eepromImageConfig{"log40GportList"}} = @portList;
        }
        elsif ($attribute eq "10G_ports")
        {
            # list of logical 10G capable ports
            @{$_eepromImageConfig{"log10GportList"}} = @portList;
        }
        else
        {
            # list of logical ports w/loopback
            @{$_eepromImageConfig{"altaLoopbackList"}} = (0) x 75;
            if (@portList + 0)
            {
                foreach my $logPort (@portList)
                {
                    $_eepromImageConfig{"altaLoopbackList"}[$logPort] = 1;
                }
            }
        }
    }
    else
    {
        printf("Unrecognized EEPROM configuration parameter!\n");
        return;
    }

    # Set the configuration parameter.
    $_eepromImageConfig{$attribute} = $value;
}


##@cmethod public void handleSetEepromBoardConfig()
#
# @desc         Set an EEPROM image generation configuration parameter
#               that pertains to board wiring (lane reversal, polarity
#               reversal, and reference clock selection).
#
# @param[in]    $attribute is the configuration parameter name
#
# @param[in]    $portlist_arg is a list of ports to reconfigure
#
# @param[in]    @args is a variable list of arguments depending on attribute
#
sub handleSetEepromBoardConfig
{
    my ($self, $attribute, $portlist_arg, @args) = @_;

    my $chip = $self->{CHIP};

    if ( $attribute eq "lane_rev" ||
            $attribute eq "polarity" ||
            $attribute eq "refclk" )
    {
        # Port list
        my @portList = $self->validateList($portlist_arg,
                $self->tpPlatformGetPortRange());
        
        if (!defined($portlist_arg) || (scalar(@portList) == 0))
        {
            print("Must specify a valid port or port range!\n");
            return;
        }

        my $change_mask = 0;
        my $new_val = 0;

        if ($attribute eq "lane_rev" or $attribute eq "polarity")
        {
            my $which_half = 0; # bit 1: RX, bit 0: TX
            my $which_lanes = 0; # bit 3..0: lanes D..A

            my $half = shift @args;
            if (not defined $half or $half =~ /^,*$/)
            {
                print("Must specify rx, tx, or both\n");
                return;
            }
            my @tmp = split ",", $half;
            foreach my $t (@tmp) {
                my $m = $_portHalfMask{$t};
                if (not defined $m)
                {
                    print("Got $t instead of rx, tx, or both\n");
                    return;
                }
                $which_half |= $m;
            }

            if ($attribute eq "polarity")
            {
                my $lanes = shift @args;
                if (not defined $lanes or $lanes =~ /^,*$/)
                {
                    print("Must specify lane a, b, c, d, or all\n");
                    return;
                }
                my @tmp = split ",", $lanes;
                foreach my $t (@tmp) {
                    my $m = $_laneMask{$t};
                    if (not defined $m)
                    {
                        print("Got $t instead of a, b, c, d, or all\n");
                        return;
                    }
                    $which_lanes |= $m;
                }
                # Convert from 4-bit lane mask to 8-bit depending on
                # RX, TX, or BOTH
                $which_lanes &= 0xF;
                $which_lanes =
                    (($which_lanes*(($which_half>>1)&1))<<4) |
                    (($which_lanes*(($which_half)&1)));
            }

            my $on_off_arg = shift @args;
            if (not defined $on_off_arg)
            {
                print("Must specify on or off\n");
                return;
            }
            my $on_off = $_onOrOff{$on_off_arg};
            if (not defined $on_off)
            {
                print("Must specify on or off\n");
                return;
            }

            if ($attribute eq "lane_rev")
            {
                $change_mask = $which_half;
                $new_val = $which_half*$on_off;
            }
            elsif ($attribute eq "polarity")
            {
                $change_mask = $which_lanes;
                $new_val = $which_lanes*$on_off;
            }
        }
        elsif ($attribute eq "refclk")
        {
            my $a_b_arg = shift @args;
            if (not defined $a_b_arg)
            {
                print("Must specify a or b as the refclk to use\n");
                return;
            }
            my $a_b = $_clkAorB{$a_b_arg}; 
            if (not defined $a_b)
            {
                print("Must specify a or b as the refclk to use\n");
                return;
            }

            $change_mask = 1;
            $new_val = $a_b;
        }
        
        # Set the configuration parameter.
        my @vals = @{$_eepromImageConfig{$attribute}};
        foreach my $port (@portList)
        {
            my ($sw, $logPort) =
                          $self->tpPlatformMapGlobalToLogicalPort($port);

            if ($logPort < 25) {
                $vals[$logPort] &= ~$change_mask;
                $vals[$logPort] |= $new_val;
            } else {
                printf("Confused about logical port $logPort.  Skipping.\n");
            }
        }
        $_eepromImageConfig{$attribute} = [@vals];
    }
    else
    {
        printf("Unrecognized EEPROM configuration parameter!\n");
        return;
    }
}

##@cmethod public void handleSetAltaEepromBoardConfig()
#
# @desc         Set an Alta specific EEPROM image generation configuration
#               parameter that pertains to board wiring (pcie lane reversal)
#
# @param[in]    $attribute is the configuration parameter name
#
# @param[in]    @args is a variable list of arguments depending on attribute
#
sub handleSetAltaEepromBoardConfig
{
    my ($self, $attribute, @args) = @_;

    my $chip = $self->{CHIP};

    if ( $attribute eq "pcie_lane_rev")
    {
        my $change_mask = 0;
        my $new_val = 0;
        my $which_half = 0;                 # bit 1: TX, bit 0: RX

        my $half = shift @args;
        if ( $half eq "tx")
        {
            $change_mask = 0x2;
        }
        elsif ( $half eq "rx")
        {
            $change_mask = 0x1;
        }
        elsif ( $half eq "both")
        {
            $change_mask = 0x3;
        }
        else
        {
            print("Must specify rx, tx, or both\n");
            return;
        }
        my $on_off_arg = shift @args;
        if ($on_off_arg eq "on")
        {
            $new_val = 0x3;
        }
        elsif ($on_off_arg eq "off")
        {
            $new_val = 0x0;
        }
        else
        {
            print("Must specify on or off\n");
            return;
        }
        # Set the configuration parameter.
        my $curVal = $_eepromImageConfig{"pcie_lane_rev"};
        $curVal &= ~$change_mask;
        $curVal |= ($change_mask & $new_val);
        $_eepromImageConfig{"pcie_lane_rev"} = $curVal;
    }
    elsif ($attribute eq 'image_offset'   ||
           $attribute eq 'image_spi_mode' ||
           $attribute eq 'image_spi_speed')
    {
        my $imageTarget = (shift @args) + 0;

        if (defined $imageTarget && 
            $imageTarget < 4)
        {
            if ($attribute eq 'image_offset')
            {
                my $offset = hex(shift @args);
                if (defined $offset)
                {
                    $_eepromImageConfig{'image_offset'}[$imageTarget] = $offset;
                }
                else
                {
                    print "Must specify the image offset\n";
                }
            }
            elsif ($attribute eq 'image_spi_mode')
            {
                my $mode = shift @args;
                if (defined $mode && defined $_spiMode{$mode})
                {
                    $_eepromImageConfig{'eeprom_mode'}[$imageTarget] = $_spiMode{$mode};
                }
                else
                {
                    print "Must specify a valid spi mode\n";
                }
            }
            elsif ($attribute eq 'image_spi_speed')
            {
                my $speed = (shift @args) + 0 ;
                if (defined $speed && $speed < 8)
                {
                    $_eepromImageConfig{'eeprom_speed'}[$imageTarget] = $speed;
                }
                else
                {
                    print "Must specify a valid spi speed\n";
                }
            }
            else
            {
                print "Unrecognized EEPROM configuration parameter!\n";
            }
        }
        else
        {
            print "Must specify a valid image number [0..3]\n"
        }
    }
    elsif ($attribute eq 'pcie_endianness')
    {
        my $value = shift @args;
        if (defined $value && defined $_pcie_endianness{$value})
        {
            $_eepromImageConfig{'pcie_endianness'} = $_pcie_endianness{$value};
        }
        else
        {
            print "Must specify little_endian or big_endian\n";
        }
    }
    elsif ( $attribute eq "eeprom_version" )
    {
        my $version = hex(shift @args);

        if (defined $version && $version != 0)
        {
            $_eepromImageConfig{'mgmt_scratch_reg'} = $version;
        }
        else
        {
            print "Must specify the image version\n";
        }
    }
    else
    {
        printf("Unrecognized EEPROM configuration parameter!\n");
        return;
    }
}


##@cmethod public void handleResetEepromConfig()
#
# @desc         Reset the EEPROM image generation configuration to defaults.
#
sub handleResetEepromConfig
{
    %_eepromImageConfig = %_eepromImageDefaultConfig;
}

##@cmethod public void handleRegDbgFfu(bool full)
#
# @desc         Calls the API fmDbgDumpFFU function.
#
# @param[in]    full indicates that invalid slices should be included.
#
sub handleRegDbgFfu
{
    my ($self, $full) = @_;

    my $chip = $self->{CHIP};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    
    my $validOnly = $TRUE;
    if ( defined($full) )
    {
        $validOnly = $FALSE;
    }

    # Operate on each selected switch one at a time
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $info = ($self->tpGetSwitchInfo())[$switchNum];

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            printf("Ignoring register operation on aggregate switch $switchNum\n");
            next;
        }
        
        print("\n");
        printf("Switch %d: \n", $switchNum) if ($switchCount > 1);
        
        my $status = $chip->fmDbgDumpFFU($switchNum, $validOnly, $validOnly);
    }

}

##@cmethod public void handleShowEepromPart()
#
# @desc         Display the boot eeprom part
#
sub handleShowEepromPart
{
    my ($self) = @_;
    my $swNum  = 0;
    my $err    = $FM_OK;

    $self->_spiEnable($swNum);
    $self->_spiEeprom_identify($swNum);

    if ($_eepromImageConfig{'part'} == SPI_EEPROM_SPANSION_S25FL129P)
    {
        print "Boot memory = Spansion S25FL129P\n\n";
    }
    elsif ($_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_528 )
    {
        print "Boot memory = Atmel AT45DB321D-528\n\n";
    }
    elsif ($_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_512 )
    {
        print "Boot memory = Atmel AT45DB321D-512\n\n";
    }
    else
    {
        print"ERROR: cannot recognize the type of memory\n";
        $err = $FM_FAIL;
    }
    $self->_spiDisable($swNum);

    return $err;
}


##@cmethod void _spiEepromCmmd_rdStatReg(int     switchNum,
#                                        int     mask,
#                                        int     condition)
#
# @desc         Reads the eeprom status registers. If the pair mask-value
#               is specified, this function remains in a loop until condition
#               is accomplished.
#               This is only for Spansion S25FL129P
#               
# @param[in]    switchNum The target switch
# 
# @param[in]    mask The read value mask. This is an optional parameter.
# 
# @param[in]    value The expected reference value. This is optional, but it
#               must be defined if a mask was specified.
#
sub _spiEepromCmmd_rdStatReg
{
    my ($self, $switchNum, $mask, $condition) = @_;

    my $statReg = 0;
    my $error   = 0;

    my $chip = $self->{CHIP};
    if (!defined $mask)
    {
        $mask  = 0;
        $condition = 0;
    }
    if (!defined $condition)
    {
        print "ERROR in eeprom access: bad mask-condition spec\n";
        return (1,0);
    }
    
    # get the current SPI_CTRL value
    my $spi_ctrl;
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
    $spi_ctrl &= 0x7ff;
    # send 1 byte header with no arguments
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_HEADER, SPI_EEPROM_CMMD_READ_STATUS_REG);
    # first loop only: set send header flag and header size.
    $spi_ctrl |= ((0b0001 << 11) | (0b01 << 15));
    do{
        # read 1 data byte
        $spi_ctrl |= (0b0100 << 11);
        $spi_ctrl |= (0b01 << 17);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);
        $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_RXDATA, \$statReg);
        $spi_ctrl &= 0x7ff;
        $error = ($statReg & 0b01100000);
    } while (($statReg & $mask) != $condition || $error );
    # release CS
    $spi_ctrl |= (0b1000 << 11);
    $self->_spiSetControlReg($switchNum, $spi_ctrl);
    return ($error, $statReg);
}

##@cmethod void _spiEepromCmmd_rdStatRegAT(int     switchNum,
#                                          int     mask,
#                                          int     condition)
#
# @desc         Reads the eeprom status registers. If the pair mask-value
#               is specified, this function remains in a loop until condition
#               is accomplished.
#               This is only for Atmel AT45DB321D
#               
# @param[in]    switchNum The target switch
# 
# @param[in]    mask The read value mask. This is an optional parameter.
# 
# @param[in]    value The expected reference value. This is optional, but it
#               must be defined if a mask was specified.
#
sub _spiEepromCmmd_rdStatRegAT
{
    my ($self, $switchNum, $mask, $condition) = @_;

    my $statReg = 0;
    my $error   = 0;

    my $chip = $self->{CHIP};
    if (!defined $mask)
    {
        $mask  = 0;
        $condition = 0;
    }
    if (!defined $condition)
    {
        print "ERROR in eeprom access: bad mask-condition spec\n";
        return (1,0);
    }
    
    # get the current SPI_CTRL value
    my $spi_ctrl;
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
    $spi_ctrl &= 0x7ff;
    # send 1 byte header with no arguments
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_HEADER, SPI_EEPROM_CMMD_READ_STATUS_REG_AT);
    # first loop only: set send header flag and header size.
    $spi_ctrl |= ((0b0001 << 11) | (0b01 << 15));
    do{
        # read 1 data byte
        $spi_ctrl |= (0b0100 << 11);
        $spi_ctrl |= (0b01 << 17);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);
        $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_RXDATA, \$statReg);
        $spi_ctrl &= 0x7ff;
        $error = ($statReg & 0b00111100) != 0b00110100;
    } while (($statReg & $mask) != $condition || $error );
    # release CS
    $spi_ctrl &= 0x7ff;
    $spi_ctrl |= (0b1000 << 11);
    $self->_spiSetControlReg($switchNum, $spi_ctrl);
    return ($error, $statReg);
}

##@cmethod void _spiEepromCmmd_writeEnable(int     switchNum,
#                                          int     enable)
#
# @desc         Sends a writeEnable or writeDisable command to the eeprom.
#               
# @param[in]    switchNum The target switch.
# @param[in]    enable The enable (=1) or disable (=0) SPI control.
#
sub _spiEepromCmmd_writeEnable
{
    my ($self, $switchNum, $enable) = @_;

    my $chip = $self->{CHIP};
    my $command = SPI_EEPROM_CMMD_WRITE_DISABLE;
    if (defined $enable && $enable == 1)
    {
        $command = SPI_EEPROM_CMMD_WRITE_ENABLE;
    }

    # get the current SPI_CTRL value
    my $spi_ctrl;
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
    $spi_ctrl &= 0x7ff;

    # header: 1 byte, no arguments
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_HEADER, $command);
    # send header (1 byte), no data, deselect when done
    $spi_ctrl = $spi_ctrl | (0b1001 << 11) | (0b01 << 15);
    $self->_spiSetControlReg($switchNum, $spi_ctrl);
}

##@cmethod void _spiEepromCmmd_readID(int     switchNum)
#
# @desc         Sends a readID command to the eeprom.
#               
# @param[in]    switchNum The target switch.
#
sub _spiEepromCmmd_readID
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{CHIP};
    my $deviceId = 0;

    # get the current SPI_CTRL value
    my $spi_ctrl;
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
    $spi_ctrl &= 0x7ff;

    # header; command (1 byte) + address (3 bytes) = 0x000000 (read order = manufacturer_ID; device_ID)
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_HEADER, SPI_EEPROM_CMMD_READ_ID << 24);
    # send header (4 bytes), read 2 data bytes, deselect when done
    $spi_ctrl = $spi_ctrl | (0b1101 << 11) | (0b10 << 17);
    $self->_spiSetControlReg($switchNum, $spi_ctrl);
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_RXDATA, \$deviceId);
    return $deviceId;
}

##@cmethod void _spiEeprom_identify(int     switchNum)
#
# @desc         Identify the type of eeprom. Only the following ones are 
#               supported: Spansion S25FL129P, Atmel AT45DB321D-528 and
#               Atmel AT45DB321D-512.
#               
# @param[in]    switchNum The target switch.
#
sub _spiEeprom_identify
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{CHIP};

    if ($_eepromImageConfig{'part'} == SPI_EEPROM_UNKNOWN)
    {
        # Try to read the eeprom ID
        my $spi_ctrl;
        my $deviceId;

        $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
        $spi_ctrl &= 0x7ff;

        $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_HEADER, SPI_EEPROM_CMMD_RDID);# << 24);
        $spi_ctrl = $spi_ctrl | (0b0001 << 11) | (0b01 << 15);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);

        $spi_ctrl &= 0x7ff;
        $spi_ctrl |= (0b1100 << 11);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);
        $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_RXDATA, \$deviceId);
        if ($deviceId == 0x1f270100)
        {
            # eeprom is an Atmel on seacliff or IZ1, check the page size
            $spi_ctrl &= 0x7ff;
            $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_HEADER, SPI_EEPROM_CMMD_READ_STATUS_REG_AT << 24);
            $spi_ctrl = $spi_ctrl | (0b1101 << 11) | (0b00 << 15) | (0b01 << 17);
            $self->_spiSetControlReg($switchNum, $spi_ctrl);

            my $statReg;

            $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_RXDATA, \$statReg);
            # verify that bits 5..2 are equal to 0b1101
            if (($statReg & 0x3c) == 0x34)
            {
                # bit zero indicates the page size
                $_eepromImageConfig{'part'} = ($statReg & 0x01) ? SPI_EEPROM_ATMEL_AT45DB321D_512 : SPI_EEPROM_ATMEL_AT45DB321D_528;
            }
        }
        else
        {
            # try to read the Spansion ID on Barcelona
            # 
            # assert eeprom hold
            $chip->fmPlatformWriteFPGA(0, 0x14, 1);

            $spi_ctrl &= 0x7ff;

            $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_HEADER, SPI_EEPROM_CMMD_RDID);
            $spi_ctrl = $spi_ctrl | (0b0001 << 11) | (0b01 << 15);
            $self->_spiSetControlReg($switchNum, $spi_ctrl);

            $spi_ctrl &= 0x7ff;
            $spi_ctrl |= (0b1100 << 11);
            $self->_spiSetControlReg($switchNum, $spi_ctrl);
            $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_RXDATA, \$deviceId);

            if ($deviceId == 0x0120184d)
            {
                $_eepromImageConfig{'part'} = SPI_EEPROM_SPANSION_S25FL129P;
            }
            else
            {
                # deassert eeprom hold
                $chip->fmPlatformWriteFPGA(0, 0x14, 0);
            }
        }
    }

    return $_eepromImageConfig{'part'};
}

##@cmethod void _spiEepromCmmd_bulkErase(int     switchNum)
#
# @desc         Sends a bulkErase command to the eeprom.
#               Only for Spansion S25FL129P
#               
# @param[in]    switchNum The target switch.
#
sub _spiEepromCmmd_bulkErase
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{CHIP};

    # get the current SPI_CTRL value
    my $spi_ctrl;
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
    $spi_ctrl &= 0x7ff;

    # header = command (1 byte), no arguments
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_HEADER, SPI_EEPROM_CMMD_BULK_ERASE);
    # send header (1 byte), no data, deselect when done
    $spi_ctrl = $spi_ctrl | (0b1001 << 11) | (0b01 << 15);
    $self->_spiSetControlReg($switchNum, $spi_ctrl);
    # wait until bulk erase is complete (check device ready status)
    my $error;
    my $status;
    ($error, $status) = $self->_spiEepromCmmd_rdStatReg($switchNum, 0x1, 0x0);
    if ($error) {
        print "ERROR during eeprom bulk erase. EEPROM status =" . sprintf("0x%2.2x",$status) . "\n";
    }
    return ($error);
}


##@cmethod void _spiEepromCmmd_bulkEraseAT(int     switchNum)
#
# @desc         Performs a Chip Erase Operation
#               Atmel AT45DB321D only
#               
# @param[in]    switchNum The target switch.
#
sub _spiEepromCmmd_bulkEraseAT
{
    my ($self, $switchNum) = @_;
    my $error = $FM_OK;

    my $chip = $self->{CHIP};

    # get the current SPI_CTRL value
    my $spi_ctrl;
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);

    # According to the datasheet Errata, Chip-Erase command is not reliable, so
    # bulk erase is performed using the Block-Erase command (1 Block = 8 Pages)
    my $blockAddress = 0;
    my $absoluteAddr = 0;
    my $progressCnt  = 0;

    print "\n  >";
    for ($blockAddress = 0; $blockAddress < 1024; $blockAddress++ )
    {
        $spi_ctrl &= 0x7ff;
        # header; command (1 byte) + address (3 bytes)

        $absoluteAddr = ($_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_528) ?
                        ($blockAddress << 13) : 
                        ($blockAddress << 12) ;
                        
        $chip->fmWriteUINT32($switchNum, 
                             SPI_EEPROM_REG_ADDR_SPI_HEADER, 
                             SPI_EEPROM_CMMD_BLOCK_ERASE_AT << 24 | $absoluteAddr);

        # send header (4 bytes), no data, de-assert CS when done
        $spi_ctrl = $spi_ctrl | (0b1001 << 11);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);
        # wait until bulk erase is complete (check device ready status)
        my $status;
        ($error, $status) = $self->_spiEepromCmmd_rdStatRegAT($switchNum, 0x80, 0x80);
        if ($error) {
            print "\nERROR during eeprom sector erase. Sector = $blockAddress; EEPROM status =" . sprintf("0x%2.2x",$status) . "\n";
        }
        # show the progress indic.
        if ( !($blockAddress & 0x07) ) {
            print">";
            if (++$progressCnt >= 16)
            {
                $progressCnt = 0;
                print "\e[1G\e[2K  >";
            }
        }
    }
    print "\n";
    return ($error);
}



##@cmethod void _spiEepromCmmd_sectorErase(int     switchNum,
#                                          int     address)
#
# @desc         Sends a sectorErase command to the eeprom.
#               
# @param[in]    switchNum The target switch.
# 
# @param[in]    address of the sector to be erased.
#
sub _spiEepromCmmd_sectorErase
{
    my ($self, $switchNum, $addr) = @_;

    my $chip = $self->{CHIP};

    # get the current SPI_CTRL value
    my $spi_ctrl;
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);

    $spi_ctrl &= 0x7ff;

    # header: command (1 byte) + address (3 bytes)
    $chip->fmWriteUINT32($switchNum,
                         SPI_EEPROM_REG_ADDR_SPI_HEADER,
                         (SPI_EEPROM_CMMD_SECTOR_ERASE << 24) | ($addr & 0xffffff));
    # send header (4 bytes), no data, deselect when done
    $spi_ctrl = $spi_ctrl | (0b1001 << 11);
    $self->_spiSetControlReg($switchNum, $spi_ctrl);
    # wait until sector erase is complete (check device ready status)
    my $error;
    my $status;
    ($error, $status) = $self->_spiEepromCmmd_rdStatReg($switchNum, 0x1, 0x0);
    if ($error) {
        print "ERROR during eeprom sector erase\n" if $error;
    } 
    return ($error);
}

##@cmethod void _spiEepromCmmd_pageProgram(int     switchNum,
#                                          int     addr,
#                                          int     byteCount,
#                                          int*    dataBuffer_ref)
#
# @desc         Writes byteCount bytes into the eeprom at sddr. 
#               Spansion S25FL129P only. 
#               
# @param[in]    switchNum The target switch.
# 
# @param[in]    addr The destination eeprom address.
# 
# @param[in]    byteCount The number of bytes to be programmed.
# 
# @param[in]    dataBuffer_ref A reference to the data buffer.
#
sub _spiEepromCmmd_pageProgram
{
    my ($self, $switchNum, $addr, $byteCount , $dataBuffer_ref) = @_;

    my $chip = $self->{CHIP};
    my $error = 0;

    # check the number of bytes to be programmed
    if ($byteCount > 0 && $byteCount <= 256)
    {
        # get the current SPI_CTRL value
        my $spi_ctrl;
        $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
        $spi_ctrl &= 0x7ff;
    
        # header: command (1 byte) + address (3 bytes)
        $chip->fmWriteUINT32($switchNum,
                             SPI_EEPROM_REG_ADDR_SPI_HEADER,
                             (SPI_EEPROM_CMMD_PAGE_PROGRAM << 24) | ($addr & 0xffffff));
        # first loop only: set send header flag. Following loops: shift only data.
        $spi_ctrl |= (0b0001 << 11);
        
        my $index = 0;
        while ( $index < $byteCount)
        {
            # determine the number of data bytes to send [1..4]
            my $compoundNum = ($byteCount - $index) > 3 ? 4 : ($byteCount - $index);
            # set 'shift data' flag and number of data bytes
            $spi_ctrl |= ((0b0100 << 11) | (($compoundNum & 0x03) << 17));

            my $txData = 0;
            while ($compoundNum--)
            {
                $txData = ($txData << 8) | ${$dataBuffer_ref}[$index++];
            }
            # set data to be written
            $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_TXDATA, $txData);
            # send command to the eeprom
            $self->_spiSetControlReg($switchNum, $spi_ctrl);
            $spi_ctrl &= 0x7ff;
        }

        # release CS
        $spi_ctrl |= (0b1000 << 11);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);

        # wait until erase is complete (check device ready status)
        my $status;
        ($error, $status) = $self->_spiEepromCmmd_rdStatReg($switchNum, 0x1, 0x0);
        print "ERROR during eeprom sector erase\n" if $error;
    }
    else
    {
        print "ERROR byte count is out of range in Page_Program\n";
    }
    return ($error);
}

##@cmethod void _spiEepromCmmd_readDataBytes(int     switchNum,
#                                            int     addr,
#                                            int     byteCount,
#                                            int*    dataBuffer_ref)
#
# @desc         Read byteCount bytes from the eeprom. The data is placed into
#               into the buffer pointed by dataBuffer_ref.
#               Atmel AT45DB321D and Spansion S25FL129P.
#               
# @param[in]    switchNum The target switch.
# 
# @param[in]    addr The eeprom address.
# 
# @param[in]    byteCount The number of bytes to be read.
# 
# @param[in]    dataBuffer_ref A reference to the destination data buffer.
#
sub _spiEepromCmmd_readDataBytes
{
    my ($self, $switchNum, $addr, $byteCount , $dataBuffer_ref) = @_;

    my $chip = $self->{CHIP};

    # check the number of bytes to be programmed
    if ($byteCount > 0 ) {
        # get the current SPI_CTRL value
        my $spi_ctrl;
        $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
        $spi_ctrl &= 0x7ff;
    
        if ($_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_528)
        {
            # convert the address into 528bytes/page format
            $addr = (($addr/528)<<10)|($addr%528);
        }

        # header: command (1 byte) + address (3 bytes)
        $chip->fmWriteUINT32($switchNum,
                             SPI_EEPROM_REG_ADDR_SPI_HEADER,
                             (SPI_EEPROM_CMMD_READ_DATA_BYTES << 24) | ($addr & 0xffffff));
        # first loop only: set send header flag. Following loops: shift only data.
        $spi_ctrl |= (0b0001 << 11);
        
        my $index = 0;
        while ( $index < $byteCount)
        {
            # determine the number of data bytes to read [1..4]
            my $compoundNum = ($byteCount - $index) > 3 ? 4 : ($byteCount - $index);
            # set 'shift data' flag and number of data bytes
            $spi_ctrl |= ((0b0100 << 11) | (($compoundNum & 0x03) << 17));

            # send the command to the eeprom
            $self->_spiSetControlReg($switchNum, $spi_ctrl);

            my $rxData = 0;
            $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_RXDATA, \$rxData);

            # push the read data into the array
            while ($compoundNum)
            {
                $compoundNum--;
                ${$dataBuffer_ref}[$index++] = ($rxData >> ($compoundNum * 8)) & 0xff;
            }
            $spi_ctrl &= 0x7ff;
        }

        # release CS
        $spi_ctrl |= (0b1000 << 11);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);
    } else {
        print "ERROR byte count is out of range in Read Data Bytes\n";
    }
}

##@cmethod void _spiSetControlReg(int     switchNum,
#                                 int     $value)
#
# @desc         Writes SPI_CTRL with value and wait until the operation is
#               completed, then writes again SPI_CTRL setting command = 0.
#               This is required because SPI_CTRL is an idempotent register.
#
# @param[in]    switchNum The target switch.
#               
# @param[in]    value The value to be written into SPI_CTRL.
#
sub _spiSetControlReg
{
    my ($self, $switchNum, $value) = @_;

    my $chip = $self->{CHIP};
    my $spi_ctrl = 0;

    # write SPI_CTRL
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, $value);

    do {
        $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
    } while ($spi_ctrl & (1 << 21));

    # write back SPI_CTRL with command = 0
    $spi_ctrl &= 0xffff87ff;
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, $spi_ctrl);
}

##@cmethod void _spiEnable(int     switchNum)
#
# @desc         Enable spi access.
#
# @param[in]    switchNum The target switch.
#
sub _spiEnable
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{CHIP};

    if ($_eepromImageConfig{'part'} == SPI_EEPROM_SPANSION_S25FL129P)
    {
        # assert eeprom hold
        $chip->fmPlatformWriteFPGA(0, 0x14, 1);
    }

    # enable SPI controller
    my $value;
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$value);

    # keep current freq setting and set SPI Enable.
    $value &= 0x3ff;
    $value |= 1<<10;
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, $value);
}

##@cmethod void _spiDisable(int     switchNum)
#
# @desc         Disable spi access. Eeprom's Hold pin remains asserted.
#
# @param[in]    switchNum The target switch.
#
sub _spiDisable
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{CHIP};


    # disable SPI controller
    my $value;

    # read SPI_CTRL
    $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$value);
    $value &= 0x3ff;
    $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, $value);

    if ($_eepromImageConfig{'part'} == SPI_EEPROM_SPANSION_S25FL129P)
    {
        # deassert eeprom hold
        $chip->fmPlatformWriteFPGA(0, 0x14, 0);
    }
}

##@cmethod void _spiEeprom_writePageSP(int switchNum, int address, &buffer)
#
# @desc         Write an eeprom page on Spansion S25FL129P.
#               This function writes until 256 consecutive data bytes.
#               The buffer must have a 256 bytes data alignment
#               Eeprom is erased on demand. The erase mechanism can be 
#               specified as bulk-erase or sector-erase.
#
# @param[in]    switchNum The target switch.
# 
# @param[in]    address The data base address.
#
# @param[in]    buffer_ref A reference to the data buffer.
#
sub _spiEeprom_writePageSP
{
    my ($self, $switchNum, $address, $buffer_ref) = @_;
    my $error = -1;
    my $buffLen = @{$buffer_ref} + 0;

    # data validation
    if ($buffLen > 0x100) {
        print STDERR "ERROR: data page contains too much data\n";
    } elsif ($address > 0xffffff ||
             $address + $buffLen > 0xffffff) {
        print STDERR "ERROR: address out of bounds\n";
    } else {
        # check if an eeprom erase is required
        if ($eepromEraseCtrl == EEPROM_PERFORM_BULK_ERASE) {
            # bulk erase
            print STDERR "\e[1G\e[2K SPI eeprom: performing bulk erase ... ";
            $self->_spiEepromCmmd_writeEnable($switchNum,1);
            $self->_spiEepromCmmd_bulkErase($switchNum);
            print STDERR "done\n";
            $eepromEraseCtrl = EEPROM_BULK_ERASED;
        } elsif ($eepromEraseCtrl == EEPROM_PERFORM_ON_DEMAND_SECTOR_ERASE) {
            # check if the current sector has already been erased
            my $sectorFound = 0;
            my $sector = $address & 0xff0000;
            foreach my $erasedSector (@eepromErasedSectorList)
            {
                if ($sector == $erasedSector)
                {
                    $sectorFound = 1;
                    last;
                }
            }
            if (!$sectorFound)
            {
                my $sectorStr = sprintf("(0x%6.6x)",$sector);

                print STDERR "\e[1G\e[2K SPI eeprom: sector erase $sectorStr ... ";
                $self->_spiEepromCmmd_writeEnable($switchNum,1);
                $self->_spiEepromCmmd_sectorErase($switchNum, $sector);
                unshift (@eepromErasedSectorList, $sector);
                print STDERR "done\n";
            }
        }
        # write page 
        $self->_spiEepromCmmd_writeEnable($switchNum,1);
        $error = $self->_spiEepromCmmd_pageProgram($switchNum, $address, $buffLen, $buffer_ref);
    }
    return $error;
}


##@cmethod void _spiEeprom_writePageAT(int switchNum, int address, &buffer)
#
# @desc         Write an page on the AT45DB321D flash memory.
#               This function writes 528 or 512 consecutive data bytes into
#               the specified memory page. The size of the memory page must
#               be previously determined calling _spiEeprom_identify().
#               Eeprom is erased on demand.
#
# @param[in]    switchNum The target switch.
# 
# @param[in]    address The data base address.
#
# @param[in]    buffer_ref A reference to the data buffer.
#
sub _spiEeprom_writePageAT
{
    my ($self, $switchNum, $address, $dataBuffer_ref) = @_;
    my $error = $FM_FAIL;
    my $chip = $self->{CHIP};
    my $buffLen = @{$dataBuffer_ref} + 0;

    # data validation
    if (($_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_528  && $buffLen != 528)  ||
        ($_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_512  && $buffLen != 512)   )
    {
        print STDERR "ERROR: invalid page size: $buffLen\n";
    }
    elsif (($_eepromImageConfig{'part'} != SPI_EEPROM_ATMEL_AT45DB321D_528)  && 
           ($_eepromImageConfig{'part'} != SPI_EEPROM_ATMEL_AT45DB321D_512)  )
    {
        print STDERR "ERROR: this function only supports AT45DB321D\n";
    }
    elsif ($address > 0xffffff || $address + $buffLen > 0xffffff)
    {
        print STDERR "ERROR: address out of bounds\n";
    }
    else
    {
        # write data into the buffer #1.
        my $spi_ctrl;
        $chip->fmReadUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_CTRL, \$spi_ctrl);
        $spi_ctrl &= 0x7ff;

        # header: command (1 byte) + address (3 bytes)
        # address is always 0x00000 because we are writing the full buffer 
        $chip->fmWriteUINT32($switchNum,
                             SPI_EEPROM_REG_ADDR_SPI_HEADER,
                             (SPI_EEPROM_CMMD_WRITE_BUFF1_AT << 24));
        # first loop only: shift header and data. Following loops: shift only data.
        $spi_ctrl |= (0b0001 << 11);

        my $index = 0;
        while ( $index < $buffLen)
        {
            my $txData = ${$dataBuffer_ref}[$index+0] << 24 |
                         ${$dataBuffer_ref}[$index+1] << 16 |
                         ${$dataBuffer_ref}[$index+2] <<  8 |
                         ${$dataBuffer_ref}[$index+3];
            $index += 4;

            # set data to be written
            $chip->fmWriteUINT32($switchNum, SPI_EEPROM_REG_ADDR_SPI_TXDATA, $txData);

            # send command to the eeprom (always send packets of 4 bytes of data)
            $spi_ctrl |= (0b0100 << 11);
            $self->_spiSetControlReg($switchNum, $spi_ctrl);
            $spi_ctrl &= 0x7ff;
        }

        # release CS
        $spi_ctrl |= (0b1000 << 11);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);

        # program the main memory using buffer #1 (w/built-in erase) 
        # adjust the address if the page size is 528 bytes/page.
        if ($_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_528 )
        {
            $address = (int($address/528)) << 10;
        }

        $chip->fmWriteUINT32($switchNum,
                             SPI_EEPROM_REG_ADDR_SPI_HEADER,
                             (SPI_EEPROM_CMMD_ER_WR_BUFF1_AT << 24) | $address);

        # send the header and release CS
        $spi_ctrl &= 0x7ff;
        $spi_ctrl |= (0b1001 << 11);
        $self->_spiSetControlReg($switchNum, $spi_ctrl);

        # wait until erase is complete (check device ready status)
        my $status;
        ($error, $status) = $self->_spiEepromCmmd_rdStatRegAT($switchNum, 0x80, 0x80);
        print "ERROR during eeprom page program\n" if $error;
    }
    return $error;
}


##@cmethod void _spiEeprom_verify(int swNum, int address, &buffer,int &diffCount)
#
# @desc         Verify an eeprom page.
#               This function reads the eeprom and compare the read data
#               against the data buffer. The comparision stops when 
#               diffCount becomes zero.
#               Both types of memories, Spansion S25FL129P and Atmel AT45DB321D,
#               are supported.
#
# @param[in]    switchNum The target switch.
# 
# @param[in]    address The data base address.
#
# @param[in]    buffer_ref A reference to the data buffer.
#
# @param[in]    diffCount_ref A reference to "differences to repport" down
#                 counter.
#
sub _spiEeprom_verifyPage
{
    my ($self, $swNum, $address, $buffer_ref, $diffCount_ref) = @_;
    my $error = -1;
    my $byteCount = @{$buffer_ref} + 0;

    # data validation
    if ($address > 0xffffff ||
        $address + $byteCount > 0xffffff) {
        print STDERR "ERROR: address out of bounds\n";
    } elsif (${$diffCount_ref} > 0){
        my @buffer;

        # read a data block from the eeprom
        $self->_spiEepromCmmd_readDataBytes($swNum, $address, $byteCount, \@buffer);

        for (my $index = 0; $index < $byteCount; $index++)
        {
            if (${$buffer_ref}[$index] != $buffer[$index])
            {
                printf STDERR ("\e[1G\e[2K Difference found at [0x%6.6x] >> eeprom: 0x%2.2x  >> file: 0x%2.2x\n  >",
                       $address + $index,
                       $buffer[$index],
                       ${$buffer_ref}[$index]);
                last if --${$diffCount_ref} == 0;
            }
        }
        $error = 0;
    }
    return $error;
}


##@cmethod void _spiEepromWrite(int swNum, hexFilename)
#
# @desc         Write the SPI eeprom with the content of hexFilename.
#               Supported hexFilename formats are I16HEX (16 bit addressing)
#               and I32HEX (32 bit addressing).
#               Eeprom sectors are erased on demand at less that full erase
#               option was specified.
#               Both types of memories, Spansion S25FL129P and Atmel AT45DB321D,
#               are supported.
#
# @param[in]    switchNum The target switch.
#
# @param[in]    hexFilename The source HexIntel file.
#
sub _spiEepromWrite
{
    my ($self, $swNum, $hexFilename) = @_;

    my @dataBuff;
    my $error = $FM_OK;
    my $pageSize;
    my $progressCnt1 = 0;
    my $progressCnt2 = 0;


    # Set page size
    $self->_spiEnable($swNum);
    $self->_spiEeprom_identify($swNum);

    if ( $_eepromImageConfig{'part'} == SPI_EEPROM_SPANSION_S25FL129P)
    {
        $pageSize = 256;

    # restore $eepromEraseCtrl
        if ($eepromEraseCtrl == EEPROM_PERFORM_ON_DEMAND_SECTOR_ERASE)
        {
        @eepromErasedSectorList = ();
        } 
        else
        {
        $eepromEraseCtrl = EEPROM_PERFORM_BULK_ERASE;
    }
    }
    elsif ( $_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_528 )
    {
        $pageSize = 528;

    }
    elsif ( $_eepromImageConfig{'part'} == SPI_EEPROM_ATMEL_AT45DB321D_512 )
        {
        $pageSize = 512;
        }
    else
        {
        print"ERROR: cannot recognize the type of memory\n";
        $error = $FM_FAIL;

            $self->_spiDisable($swNum);
        return $error;
        }

    if (!open (FILE, "<$hexFilename"))
        {
        print STDERR "ERROR: cannot open file $hexFilename for reading\n";
        $error = $FM_FAIL;
    } 
    else
    {
        my @recBuff;
            my $loadOffset = 0;
            my $recDataByteCount = 0;
        my $extendedLinearAddress = 0;
        my $absoluteAddr = 0;
            my $eof_flag = 0;
        my $currentPageAddr = -1;
        my $pageAddr = -1;
        my $pageByteOffset = 0;
        my $bufferByteCount = 0;

        print "\n  >";
        while (<FILE>)
        {
            # process next record
            chomp;
            $error = processHexIntelRecord(\$_,\$loadOffset,\$recDataByteCount,\@recBuff,\$extendedLinearAddress,\$eof_flag);

            if ($error)
            {
                # specific error messages are displayed by procHexIntelRecord
                # nothing more to do here.
                last;
            } 
            elsif ($eof_flag                &&
                   $currentPageAddr >= 0    &&
                   $bufferByteCount >  0 )
            {
                # End Of File: write remaining data in the eeprom and quit
                if ( $pageSize == 256)
                {
                    # write a page into the S25FL129P
                    $self->_spiEeprom_writePageSP($swNum, $currentPageAddr * $pageSize, \@dataBuff);
                }
                else
                {
                    # write a page into the AT45DB321D
                    $self->_spiEeprom_writePageAT($swNum, $currentPageAddr * $pageSize, \@dataBuff);
                }
                last;
            }
            else
            {
                # determine the absolute address, the page address and the page offset
                $absoluteAddr = $loadOffset + $extendedLinearAddress;

                # fill the buffer
                while ($recDataByteCount--)
                {
                    $pageAddr = int($absoluteAddr / $pageSize);
                    $pageByteOffset  = $absoluteAddr % $pageSize;

                    if ( $pageAddr != $currentPageAddr )
                    {
                        if ($currentPageAddr >= 0 && ($bufferByteCount > 0))
                        {
                            # Write the current page
                            if ( $pageSize == 256)
                            {
                                # write a page into the S25FL129P
                                $self->_spiEeprom_writePageSP($swNum, $currentPageAddr * $pageSize, \@dataBuff);
                            }
                            else
                            {
                                # write a page into the AT45DB321D
                                $self->_spiEeprom_writePageAT($swNum, $currentPageAddr * $pageSize, \@dataBuff);
                            }
                        }
                        # initialize the new page
                        @dataBuff = (0xff) x $pageSize;
                        $currentPageAddr = $pageAddr;
                        $bufferByteCount = 0;
                    }
                    $dataBuff[$pageByteOffset] = shift @recBuff;
                    $bufferByteCount++;
                    $absoluteAddr++;
                }
            }
            # show the progress indicator
            if (++$progressCnt1 >= 64) {
                $progressCnt1 = 0;
                print">";
                if (++$progressCnt2 >= 16)
                {
                    $progressCnt2 = 0;
                    print "\e[1G\e[2K  >";
                }
            }
        }
        close FILE;
        $self->_spiDisable($swNum);
        print "\n";
    }
    return $error;
}

##@cmethod void _spiEepromVerify(int swNum, hexFilename)
#
# @desc         Verify the SPI eeprom against hexFilename.
#               Supported hexFilename formats are I16HEX (16 bit addressing)
#               and I32HEX (32 bit addressing).
#
# @param[in]    swNum The target switch.
#
# @param[in]    hexFilename The HexIntel file used as reference.
#
# @param[in]    maxDiffs The maximum number of diferences to report before
#                 is stopped.
sub _spiEepromVerify
{
    my ($self, $swNum, $hexFilename, $maxDiffs) = @_;

    my @dataBuff;
    my $error = $FM_FAIL;
    my $extendedLinearAddress = 0;
    my $newExtendedLinearAddress = 0;
    my $pBufferBaseAddr = 0;
    my $pBufferNextAddr = 0;
    my $progressCnt1 = 0;
    my $progressCnt2 = 0;

    # By default, the max number of differences to display is 10.
    $maxDiffs = 10 if !defined $maxDiffs || $maxDiffs <= 0;
    my $diffCount = $maxDiffs;

    if (!open (FILE, "<$hexFilename"))
    {
        print STDERR "ERROR: cannot open file $hexFilename\n";
        return $error;
    }

    print "\n  >";
    $self->_spiEnable($swNum);
    if ($self->_spiEeprom_identify($swNum) == SPI_EEPROM_UNKNOWN)
    {
        $self->_spiDisable($swNum);
        close FILE;
        print"ERROR: cannot recognize the type of memory\n";
        return $FM_FAIL;
    }

    while (<FILE>)
    {
        my $loadOffset = 0;
        my $recDataByteCount = 0;
        my $eof_flag = 0;
        my @recBuff;

        if ($diffCount == 0)
        {
            print STDERR "\n Too many differences verifying file $hexFilename\n";
            last;
        }

        # process record
        chomp;
        $error = processHexIntelRecord(\$_,\$loadOffset,\$recDataByteCount,\@recBuff,\$newExtendedLinearAddress,\$eof_flag);

        if ($error)
        {
            # specific error messages are displayed by procHexIntelRecord
            # nothing more to do here, quit immediatly.
            last;
        } elsif ($eof_flag) {
            # End Of File: verify remaining data.
            if ($pBufferBaseAddr != $pBufferNextAddr)
            {
                $self->_spiEeprom_verifyPage($swNum, $pBufferBaseAddr + $extendedLinearAddress, \@dataBuff, \$diffCount);
            }
            last;
        } elsif ($newExtendedLinearAddress != $extendedLinearAddress){
            # there is a new Extended Linear Address: sync current page.
            if ($pBufferBaseAddr != $pBufferNextAddr)
            {
                $self->_spiEeprom_verifyPage($swNum, $pBufferBaseAddr + $extendedLinearAddress, \@dataBuff, \$diffCount);
                # clear the buffer
                @dataBuff = ();
            }
            # update the extended Linear Address
            $extendedLinearAddress = $newExtendedLinearAddress;
            $pBufferBaseAddr = 0;
            $pBufferNextAddr = 0;
        } elsif ($recDataByteCount) {
            # force verify if:
            # 1- there is a gap in the stream.
            # 2- dataBuff lenght is > 0xff
            if ($pBufferBaseAddr != $pBufferNextAddr &&
                ($pBufferNextAddr != $loadOffset ||
                 ($pBufferNextAddr + $recDataByteCount - 1 - $pBufferBaseAddr) > 0xff)) {
                    $self->_spiEeprom_verifyPage($swNum, $pBufferBaseAddr + $extendedLinearAddress, \@dataBuff, \$diffCount);
                    # clear the buffer
                    @dataBuff = ();
                    $pBufferBaseAddr = 0;
                    $pBufferNextAddr = 0;
            }
                
            # if the buffer is empty, update address pointers and  append
            # new data to the page buffer 
            if ($pBufferBaseAddr == $pBufferNextAddr)
            {
                $pBufferBaseAddr = $loadOffset;
                $pBufferNextAddr = $loadOffset;
            }
            push(@dataBuff, @recBuff);
            $pBufferNextAddr += $recDataByteCount;
        }
        if (++$progressCnt1 >= 64) {
            $progressCnt1 = 0;
            print">";
            if (++$progressCnt2 >= 16)
            {
                $progressCnt2 = 0;
                print "\e[1G\e[2K  >";
            }
        }
    }
    $self->_spiDisable($swNum);
    close FILE;
    print "\n";

    my $diffs = $maxDiffs - $diffCount;
    if ($error)
    {
        print STDERR " ERROR verifying EEPROM\n";
    } elsif ($diffCount == $maxDiffs) {
        print STDERR " Verify completed: no differences were found\n";
        $error = $FM_OK;
    } elsif ($diffCount > 0) {
        $diffs = $maxDiffs - $diffCount;
        print STDERR " Verify completed: $diffs differences found\n";
    } else {
        print STDERR " Verify completed: $diffs or more differences were found\n";
    }
    return $error;
}

##@cmethod void _spiEepromDump(int swNum, int startAddr, int endAddr)
#
# @desc         Dump the SPI eeprom and write hexFilename using the
#               specified start and end address.
#
# @param[in]    swNum The target switch.
#
# @param[in]    startAddr The start address.
#
# @param[in]    endAddr The end address.
#

sub _spiEepromDump
{
    my ($self, $swNum, $startAddr, $endAddr) = @_;

    # define default values
    $startAddr = 0 if !defined $startAddr;
    $endAddr = 0xffffff if !defined $endAddr;

    # variables used to read the eeprom
    my @buffer;
    my $blockStartIndex = 0;
    my $blockEndIndex = 0;
    my $curAddr = $startAddr;
    my @block;
    my $pageEndAddr;
    my $bytesToRead;

    $self->_spiEnable($swNum);
    if ($self->_spiEeprom_identify($swNum) == SPI_EEPROM_UNKNOWN)
    {
        $self->_spiDisable($swNum);
        print"ERROR: cannot recognize the type of memory\n";
        return $FM_FAIL;
    }

    while ($curAddr < $endAddr)
    {
        # read blocks of 256 bytes (max) from the eeprom
        # try to keep a 256 byte boundary aligment.
        $bytesToRead = 256 - ($curAddr & 0x0ff);
        if ($bytesToRead > ($endAddr - $curAddr + 1))
        {
            $bytesToRead = ($endAddr - $curAddr + 1);
        }

        # read a data block from the eeprom
        $self->_spiEepromCmmd_readDataBytes($swNum, $curAddr, $bytesToRead, \@buffer);
        $pageEndAddr = $curAddr + $bytesToRead;
        $blockStartIndex = 0;

        while ($curAddr < $pageEndAddr)
        {
            # split up the buffer into (maximum) 16 bytes blocks
            $blockEndIndex = $blockStartIndex + 15 - ($curAddr & 0x0f);
            if (($pageEndAddr - $curAddr) < ($blockEndIndex - $blockStartIndex)) {
                $blockEndIndex = $pageEndAddr - $curAddr + $blockStartIndex -1;
            }

            @block = @buffer[$blockStartIndex..$blockEndIndex];
            dumpDataBlock($curAddr,@block);                                             
            # update $curAddr & $blockStartIndex
            $curAddr += ($blockEndIndex-$blockStartIndex) + 1;
            $blockStartIndex = $blockEndIndex + 1;
        }
    }
    $self->_spiDisable($swNum);
    return $FM_OK;
}

##@cmethod void _spiEepromErase( )
#
# @desc         Performs a full eeprom erase.
#
# @param[in]    swNum The target switch.
#

sub _spiEepromErase
{
    my ($self, $swNum) = @_;

    # validation
    my $answer = "";
    while ($answer ne "y" && $answer ne "n")
    {
        printf("\nConfirm eeprom bulk erase (y/n)? ");
        $answer = <STDIN>;
        chomp($answer);
    }
    print "$answer\n";
    return if ($answer eq "n");
    
    $self->_spiEnable($swNum);
    if ($self->_spiEeprom_identify($swNum) == SPI_EEPROM_UNKNOWN)
    {
        $self->_spiDisable($swNum);
        print"ERROR: cannot recognize the type of memory\n";
        return $FM_FAIL;
    }
    elsif ($_eepromImageConfig{'part'} == SPI_EEPROM_SPANSION_S25FL129P)
    {
        $self->_spiEepromCmmd_writeEnable($swNum,1);
        $self->_spiEepromCmmd_bulkErase($swNum);
        $self->_spiEepromCmmd_writeEnable($swNum,0);
    }
    else
    {
        $self->_spiEepromCmmd_bulkEraseAT($swNum);
    }
    $self->_spiDisable($swNum);
    return $FM_OK;
}


##@cmethod void _spiEepromRead(int swNum, char* hexFilename, 
#                              int startAddr, int endAddr, int recordSize)
#
# @desc         Read the SPI eeprom and write hexFilename.
#               hexFilename format is I32HEX (32 bit addressing).
#
# @param[in]    swNum The target switch.
#
# @param[in]    hexFilename The HexIntel file to be written.
#
# @param[in]    startAddr The start address.
#
# @param[in]    endAddr The end address.
#
# @param[in]    recordSize The HexIntel record size.
#

sub _spiEepromRead
{
    my ($self, $swNum, $hexFilename, $startAddr, $endAddr, $recordSize) = @_;

    my $oldhandle;
    my $error = $FM_FAIL;

    # if $hexFilename is not defined, send the output to the terminal
    # otherwise, try to open hexFilename for writing 
    if (defined $hexFilename) {
        # check if outputfile already exists
        if (-e $hexFilename)
        {
            my $answer;
        
            print STDERR "Output file $hexFilename already exists.\n";
            print STDERR "Do you want to overwrite it (y/n)? :";
        
            chomp ($answer = <STDIN>);
            while ( $answer !~ /^[yYnN]$/)
            {
                print STDERR "\n Type 'y' or 'n': " ;
                chomp ($answer = <STDIN>);
            }
            if ($answer =~ /[nN]/)
            {
                print STDERR "Execution was cancelled by user\n;";
                return $error;
            }
            # delete old version of the outputfile
            unlink $hexFilename;
        }
        # open output file
        unless (open (OUTFILE, ">" , $hexFilename)) {
            print STDERR " ERROR: cannot opent file $hexFilename\n";
            return $error;
        }
        $oldhandle = select (OUTFILE);
    }

    # define default values
    $startAddr = 0 if !defined $startAddr;
    $endAddr = 0xffffff if !defined $endAddr;
    $recordSize = 16 if !defined $recordSize;

    # variables used to read the eeprom
    my @buffer;
    my $blockStartIndex = 0;
    my $blockEndIndex = 0;
    my $curAddr = $startAddr;
    my $extendedLinearAddress = 0;
    my @block;
    my $pageEndAddr;
    my $allFFline = 0;
    my $bytesToRead;

    # generate the hexFile
    $self->_spiEnable($swNum);
    if ($self->_spiEeprom_identify($swNum) == SPI_EEPROM_UNKNOWN)
    {
        $self->_spiDisable($swNum);
        print"ERROR: cannot recognize the type of memory\n";
        return $FM_FAIL;
    }

    while ($curAddr < $endAddr)
    {
        # read blocks of 256 bytes (max) from the eeprom
        # try to keep a 256 byte boundary aligment.
        $bytesToRead = 256 - ($curAddr & 0x0ff);
        if ($bytesToRead > ($endAddr - $curAddr + 1))
        {
            $bytesToRead = ($endAddr - $curAddr + 1);
        }

        # read a data block from the eeprom
        $self->_spiEepromCmmd_readDataBytes($swNum, $curAddr, $bytesToRead, \@buffer);
        $pageEndAddr = $curAddr + $bytesToRead;
        $blockStartIndex = 0;

        while ($curAddr < $pageEndAddr)
        {
            # split up the buffer into 16 bytes (maximum) blocks            
            $blockEndIndex = $blockStartIndex + $recordSize - 1 - ($curAddr & 0x0f);
            if (($pageEndAddr - $curAddr) < ($blockEndIndex - $blockStartIndex)) {
                $blockEndIndex = $pageEndAddr - $curAddr + $blockStartIndex -1;
            }

            @block = @buffer[$blockStartIndex..$blockEndIndex];
    
            # skip all ff lines but the first one
            my $ffcount = @block + 0;
            for my $val (@block) {
                if ($val == 0xff) {
                    $ffcount--;
                } else {
                    $allFFline = 0;
                    last;
                }
            }
            if (!$ffcount)
            {
                ++$allFFline;
            }
            if ($allFFline < 2)
            {
                    
                # check if the extended linear address is valid
                if (($curAddr & 0xffff0000) != $extendedLinearAddress)
                {
                    # generate an Extended Linear Address Record (record type 4)
                    # See Intel Hexadecimal Object File Format Specification, Revision A
                    my $extRecordCksum = 6 + (($curAddr >> 24) & 0xff) + (($curAddr >> 16) & 0xff);
                    $extRecordCksum = (-$extRecordCksum) & 0xff;
        
                    printf ":02000004%4.4X%02X\n",($curAddr >> 16) & 0xffff, $extRecordCksum;
                    $extendedLinearAddress = ($curAddr & 0xffff0000);
                }
        
                # prepend the block with: record length, the address and the record type (0)
                my @blockHeader = ($blockEndIndex-$blockStartIndex+1, ($curAddr>>8)&0xff, $curAddr & 0xff, 0);
                unshift (@block, @blockHeader);
                    
                # checksum calculation
                my $csum = 0;
                for my $val (@block) { $csum += $val; }
                $csum = ((($csum&0xff)^0xff)+1)&0xff;
                push @block, $csum;
        
                # print a line
                print ":";
                for my $dataByte (@block) {printf("%2.2x",$dataByte);}
                print "\n";
            }
            # update $curAddr & $blockStartIndex
            $curAddr += ($blockEndIndex-$blockStartIndex) + 1;
            $blockStartIndex = $blockEndIndex + 1;
        }
    }
    # add the end record
    print ":00000001FF\n";
    $self->_spiDisable($swNum);
    $error = $FM_OK;

    # restore default output and close the handle
    if (defined $oldhandle)
    {
        select ($oldhandle);
        close OUTFILE;
    }
    print STDERR "File $hexFilename successfully created\n";
    return $error;
}



##@cmethod void processHexIntelRecord(int     switchNum, hexFilename)
#
# @desc         Write the SPI eeprom with the content of hexFilename.
#               The format of the file hexFilename must be I32HEX            
#               to support 32 bit addressing schema.
#               
#
# @param[in]    switchNum The target switch.
#
# @param[in]    hexFilename The source HexIntel file.
sub processHexIntelRecord
{
    my ($record_ref, $loadOffset_ref, $recLen_ref, $recBuff_ref, $xLinearAddr_ref, $eof_ref) = @_;
    my $error = 0;

    # validate the Intel Hex record format
    if (${$record_ref} =~ /^:[0-9A-Fa-f][0-9A-Fa-f]/) {
        s/^://;
        # convert to binary format and verify checksum
        my $checksum = 0;
        while(length($_)>0) {
            my $dataByte = hex(substr(${$record_ref}, 0, 2, ""));
            $checksum += $dataByte;
            push @{$recBuff_ref}, $dataByte;
        }
        if ($checksum & 0xff)
        {
            print STDERR "Checksum ERROR in Intel-Hex file\n";
            $error = -1;
            return;
        }
        # remove checksum byte.
        pop @{$recBuff_ref};

        ${$recLen_ref}  = shift @{$recBuff_ref};
        ${$loadOffset_ref} = ((shift @{$recBuff_ref}) << 8) + (shift @{$recBuff_ref});
        my $recType  = shift @{$recBuff_ref};
        if ($recType == 4) {
            # Extended Linear Address Record
            ${$xLinearAddr_ref} = (((shift @{$recBuff_ref}) << 8) + (shift @{$recBuff_ref})) << 16;
        } elsif ($recType == 1) {
            # End of FileRecord
            ${$eof_ref} = 1;
        } elsif ($recType == 0) {
            # Data Record
            # Nothing to do here.
        }
        else {
            print STDERR "ERROR: Not supported record type in IntelHex file\n";
            $error = -1;
        }
    } else {
        print STDERR "ERROR: invalid IntelHex file format\n";
        $error = -1;
    }
    return $error;
}

##@cmethod void dumpDataBlock(byte dataBlock[])
#
# @desc         Dumps a block of data of 16 bytes maximum.
#
# @param[in]    baseAddress is the base address of the data block.
#
# @param[in]    dataBlock   the data array of data.
sub dumpDataBlock
{
    my ($addr, @data) = @_;

    my $dataLen = @data + 0;
    my $fillBefore = $addr - ($addr & 0xfffff0);
    my $fillAfter = 16 - $dataLen - $fillBefore;

    printf("0x%6.6x: ",$addr & 0xfffff0);
    dumpFill($fillBefore,"-- ");
    foreach my $byte (@data)
    {
        printf("%2.2x ",$byte);
    }
    dumpFill($fillAfter,"-- ");
    print "   ";

    dumpFill($fillBefore,".");
    foreach my $byte (@data)
    {
        if ($byte > 0x80 || $byte <= 0x20)
        {
            $byte = 0x2e;
        }
        printf("%c",$byte);
    }
    print "\n";
}

sub dumpFill
{
    my ($rep,$str) = @_;

    for (my $i = 0; $i < $rep; $i++)
    {
        print "$str";
    }
}

1;
