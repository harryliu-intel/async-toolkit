# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/L2.pm
# Creation Date:    03/01/07
# Description:      Layer 2 frame generation support for Test Engine 2 tests
#
# INTEL CONFIDENTIAL
# Copyright 2006 - 2011 Intel Corporation. All Rights Reserved. 
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

package Scripts::GenFrame::L2;
use strict;
use warnings;

require Exporter;
use Base::Const;
use Base::Enum qw(:ISL);
use Scripts::CRC;
use Scripts::Utilities;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    addAlternateVLANheader
    addRLTheader
    addVLANheader
    genCBPAUSEFrame
    genCBPAUSEheader
    genL2header
    genPAUSEFrame
    genPAUSEheader
    get4bVLANPri
    getDstMac
    getFrameID
    getL3Index
    getMAC
    getSrcMac
    getVLAN
    getVLANCFI
    randMac
    setAlternateVLANheader
    setL2da
    setL2sa
    setL2type
    setVLANheader
    updateL2Chksum
    generateMacSequence
    generateL2FrameSequence
    padFrame
);
our %EXPORT_TAGS = (
    CONSTANTS   => [qw(
        DA_BPDU
        DA_LACP
        DA_8021x
        DA_GMRP
        DA_GVRP
    )]
);
Exporter::export_tags('CONSTANTS');

use constant DA_BPDU        => "01:80:C2:00:00:00";
use constant DA_LACP        => "01:80:C2:00:00:02";
use constant DA_8021x       => "01:80:C2:00:00:03";
use constant DA_GMRP        => "01:80:C2:00:00:20";
use constant DA_GVRP        => "01:80:C2:00:00:21";

##@method public char** addAlternateVLANheader(int etherType, int VID, int priority, int cfi, char **pDataArray)
# Adds a VLAN header with the specified ethertype (which may be different from
# 0x8100) to byte 12..15
# @param[in] etherType The Ethernet type of the VLAN tag. Examples are 0x8100,
# 0x88a8, 0x9100, etc.
# @param[in] VID The VLAN ID
# @param[in] priority The 3-bit VLAN priority
# @param[in] cfi The CFI bit
# @param[in,out] pDataArray A Perl reference to an array containing the packet
# data
# @return A Perl reference to an array containing the modified packet data
sub addAlternateVLANheader($$$$$)
{
    validatePrototype(@_, 5);
    my ($etherType, $VID, $priority, $cfi, $pDataArray) = @_; 

    my $length = scalar(@{$pDataArray});
    my $tmpData = [];
    for (my $i=12 ; $i<$length ; $i++)
    {
        $tmpData->[$i - 12] = $pDataArray->[$i];
    }
    setAlternateVLANheader($etherType, $VID, $priority, $cfi, $pDataArray);
    for (my $i=12 ; $i<$length ; $i++)
    {
        $pDataArray->[$i + 4] = $tmpData->[$i - 12];
    }
    return $pDataArray;
}

##@method public char** addRLTheader(char **pRLTArray, char **pDataArray)
# Adds a 12-byte RLT header at byte 12..24
# @param[in] pRLTArray A Perl reference to an array containing a RLT tag
# @param[in,out] pDataArray A Perl reference to an array containing the packet
# data
# @return A Perl reference to an array containing the modified packet data
sub addRLTheader($$)
{
    validatePrototype(@_, 2);
    my ($pRLTArray, $pDataArray) = @_; 

    my $length = scalar(@{$pDataArray});
    my $tmpData = [];
    for (my $i=12 ; $i<$length ; $i++)
    {
        $tmpData->[$i - 12] = $pDataArray->[$i];
    }
    for (my $i=0 ; $i<12 ; $i++)
    {
        $pDataArray->[$i + 12] = $pRLTArray->[$i];
    }
    for (my $i=12 ; $i<$length ; $i++)
    {
        $pDataArray->[$i + 12] = $tmpData->[$i - 12];
    }
    return $pDataArray;
}

##@method public char** addVLANheader(int VID, int priority, char **pDataArray)
# Adds a VLAN header to an existing packet at byte 12 .. 15. The VLAN header is
# identified by an Ethernet type of 0x8100 (802.1Q Tag Protocol Type)
# @param[in] VID The VLAN ID
# @param[in] priority The 3-bit VLAN priority
# @param[in,out] pDataArray A Perl reference to an array containing the packet
# data
# @return A Perl reference to an array containing the modified packet data
# @note The CFI bit is unset
sub addVLANheader($$$)
{
    validatePrototype(@_, 3);
    my ($VID, $priority, $pDataArray) = @_; 

    return addAlternateVLANheader(0x8100, $VID, $priority, 0x0, $pDataArray);
}

##@method public char** genCBPAUSEFrame(char *sa, char *da, int type, int opcode, int enableVector, int pad, int length, int badCrc, int[] pauseTime)
# @param[in] sa The Layer 2 source address
# @param[in] da @optional The Layer 2 destination address. The default is
# 01:80:C2:00:00:01
# @param[in] type @optional The Ethernet type. The default is 0x8808
# @param[in] opcode @optional The MAC control opcode. The default is 0x0101
# @param[in] enableVector @optional The class enable vector. By default a class
# enable vector will be generated based on the non-zero entries of the @a
# pauseTime argument
# @param[in] pad @optional The value with which to pad the Cisco Class Based
# PAUSE frame. The default is 0x0
# @param[in] length @optional The length of the Cisco Class Based PAUSE frame.
# The default is 64 byte
# @param[in] badCrc @optional The value with which to corrupt the frame check
# sequence (FCS). The default is 0, i.e. no FCS corruption
# @param[in] pauseTime @optional An 8-entry Perl array containing the pause
# timer values for each class. The default for all pause timer values is 0xffff
# @return A Perl reference to an array containing a Cisco Class Based PAUSE
# frame
sub genCBPAUSEFrame($;$$$$$$$@)
{
    my ($sa, $da, $type, $opcode, $enableVector, $pad, $length, $badCrc, @pauseTime) = @_;

    $da = "01:80:C2:00:00:01" unless defined $da;
    $type = 0x8808 unless defined $type;
    $opcode = 0x0101 unless defined $opcode;
    $length = 64 unless defined $length;
    $pad = 0x0 unless defined $pad;
    $badCrc = 0x0 unless defined $badCrc;
    @pauseTime = (defined($pauseTime[0]) ? @pauseTime : (0xffff) x 8);

    if (scalar(@pauseTime) != 8)
    {
        my @format = (
            "ERROR",
            "\@pauseTime",
            "Incorrect number of entries (expected %d got %d)"
        );
        traceback(sprintf(join(": ", @format), 8, scalar(@pauseTime)));
        die "stopped";
    }

    if (!defined($enableVector))
    {
        # Generate the class enable vector based on the non-zero pause timer
        # values.
        for (my $i=0; $i<scalar(@pauseTime); $i++)
        {
            if ($pauseTime[$i] >= 0)
            {
                $enableVector |= (0x1 << $i);
            }
        }
    }

    my $pDataArray = genL2header($da, $sa);
    setL2type($type, $pDataArray);
    $pDataArray->[14] = (($opcode >> 8) & 0xff);
    $pDataArray->[15] = ($opcode & 0xff);
    $pDataArray->[16] = 0x0;
    $pDataArray->[17] = $enableVector;
    for (my $i=0 ; $i<scalar(@pauseTime) ; $i++ )
    {
        if ($pauseTime[$i] >= 0)
        {
            $pDataArray->[18 + 2 * $i] = (($pauseTime[$i] >> 8) & 0xff);
            $pDataArray->[19 + 2 * $i] = ($pauseTime[$i] & 0xff);
        }
        else
        {
            $pDataArray->[18 + 2 * $i] = 0x0;
            $pDataArray->[19 + 2 * $i] = 0x0;
        }
    }
    for (my $i=34 ; $i<($length - 4) ; $i++)
    {
        $pDataArray->[$i] = $pad;
    }
    my $crc = CRC::CRC32($pDataArray, $length - 4) + $badCrc;
    for (my $i=0 ; $i<4 ; $i++)
    {
        $pDataArray->[$length - 4 + $i] = (($crc >> (8 * $i)) & 0xff);
    }
    return $pDataArray;
}

##@method public char** genCBPAUSEheader(char *sa, int[] pauseTime)
# Genere a Cisco Class Based PAUSE header
# @param[in] sa The Layer 2 source address
# @param[in] pauseTime An 8-entry Perl array containing the pause timer values
# for each class. The default for all pause timer values is 0xffff
# @return A Perl reference to an array containing a Cisco Class Based PAUSE
# frame
sub genCBPAUSEheader($@)
{
    my ($sa, @pauseTime) = @_;

    return genCBPAUSEFrame($sa, undef, undef, undef, undef, undef, undef,
                           undef, @pauseTime);
}

##@method public char** genL2header(char *da, char *sa)
# Generate a basic layer 2 frame given a destination MAC address and a source
# MAC address.
# @param[in] da Destination MAC address
# @param[in] sa Source MAC address
# @return A basic Layer 2 frame
sub genL2header($$)
{
    validatePrototype(@_, 2);
    my ($da, $sa) = @_;

    my $pDataArray = [];
    setL2da($da, $pDataArray);
    setL2sa($sa, $pDataArray);
    setL2type(0x8000, $pDataArray);
    return $pDataArray;
}

##@method public char** genPAUSEFrame(char* sa, char *da, int type, int opcode, int pad, int length, int badCrc)
# @param[in] sa The Layer 2 source address
# @param[in] da @optional The Layer 2 destination address. The default is
# 01:80:C2:00:00:01
# @param[in] type @optional The Ethernet type. The default is 0x8808
# @param[in] opcode @optional The MAC control opcode. The default is 0x0001
# @param[in] pad @optional The value with which to pad the PAUSE frame. The
# default is 0x0
# @param[in] length @optional The length of the PAUSE frame. The default is 64
# byte
# @param[in] badCrc @optional The value with which to corrupt the frame check
# sequence (FCS). The default is 0, i.e. no FCS corruption
# @return A Perl reference to an array containing a PAUSE frame
sub genPAUSEFrame($;$$$$$$)
{
    my ($sa, $da,  $type, $opcode, $pad, $length, $badCrc) = @_;

    $da = "01:80:C2:00:00:01" unless defined $da;
    $type = 0x8808 unless defined $type;
    $opcode = 0x0001 unless defined $opcode;
    $pad = 0 unless defined $pad;
    $length = 64 unless defined $length;
    $badCrc = 0 unless defined $badCrc;

    my $pauseTime = 0xffff;

    my $pDataArray = genL2header($da, $sa);
    setL2type($type, $pDataArray);
    $pDataArray->[14] = (($opcode >> 8) & 0xff);
    $pDataArray->[15] = ($opcode & 0xff);
    $pDataArray->[16] = (($pauseTime >> 8) & 0xff);
    $pDataArray->[17] = ($pauseTime & 0xff);

    for (my $i=18 ; $i<($length - 4) ; $i++)
    {
        $pDataArray->[$i] = $pad;
    }

    my $crc = CRC::CRC32($pDataArray, $length - 4) + $badCrc;
    for (my $i=0 ; $i<4 ; $i++)
    {
        $pDataArray->[$length - 4 + $i] = (($crc >> (8 * $i)) & 0xff);
    }
    return $pDataArray;
}

##@method public char** genPAUSEheader(int pauseTime, char *sa, char *da)
# Generate an IEEE 802.3 PAUSE header
# @param[in] pauseTime The pause timer value
# @param[in] sa The Layer 2 source address
# @param[in] da @optional The Layer 2 destination address. The default is
# 01:80:C2:00:00:01
# @return A Perl reference to an array containing a basic IEEE 802.3 PAUSE
# packet
sub genPAUSEheader($$;$)
{
    my ($pauseTime, $sa, $da) = @_;

    $da = (defined($da) ? $da : "01:80:C2:00:00:01");

    my $opcode = 0x0001;
    my $length = 64;

    my $pDataArray = genL2header($da, $sa);
    setL2type(0x8808, $pDataArray);

    $pDataArray->[14] = (($opcode >> 8) & 0xff);
    $pDataArray->[15] = ($opcode & 0xff);
    $pDataArray->[16] = (($pauseTime >> 8) & 0xff);
    $pDataArray->[17] = ($pauseTime & 0xff);
    # Pad the IEEE 802.3 PAUSE frame with zeros.
    for (my $i=0 ; $i<42 ; $i++)
    {
        $pDataArray->[18 + $i] = 0x0;
    }
    my $crc = CRC::CRC32($pDataArray, $length - 4);
    for (my $i=0 ; $i<4 ; $i++)
    {
        $pDataArray->[$length - 4 + $i] = (($crc >> (8 * $i)) & 0xff);
    }
    return $pDataArray;
}

##@method public int get4bVLANPri(char **pDataArray)
# Extract the 4-bit VLAN priority from the first VLAN tag
# @param[in] pDataArray A perl reference to an array containing the packet data
# to be parsed
# @return The 4-bit VLAN priority
sub get4bVLANPri($)
{
    my ($pDataArray) = @_;

    return (($pDataArray->[14] >> 4) & 0xf);
}

##@method public char* getDstMac(char **pDataArray)
# Extract the destination MAC address from the specified packet
# @param[in] pDataArray A perl reference to an array containing the packet data
# to be parsed
# @return The destination MAC address in the form @c "AA:BB:CC:DD:EE:FF"
sub getDstMac($)
{
    my ($pDataArray) = @_;

    my $DA = sprintf("%02x:%02x:%02x:%02x:%02x:%02x",
                     $pDataArray->[0], $pDataArray->[1],
                     $pDataArray->[2], $pDataArray->[3], 
                     $pDataArray->[4], $pDataArray->[5]);
    return $DA;
}

##@method public int getFrameID(char **pDataArray)
# Retrieve the frame's Java testbench ID
# @param[in] pDataArray A perl reference to an array containing the packet data
# to be parsed
# @return The frame's Java testbench ID
sub getFrameID($)
{
    my ($pDataArray) = @_;

    my $length = scalar(@{$pDataArray});
    my $id = (($pDataArray->[$length - 8] << 24)
              | ($pDataArray->[$length - 7] << 16)
              | ($pDataArray->[$length - 6] << 8)
              | ($pDataArray->[$length - 5]));
    return $id;
}

##@method public int getL3Index(char* pDataArray, int ISLTag)
# Retrieve the starting index of the Layer 3 data
# @param[in,out] pDataArray A Perl reference to the data from which to retrieve
# information
# @param[in] ISLTag @optional The ISL tag to be expected. Valid choices are:
# <ul>
#   <li> Base::Enum::ISL_NONE()
#   <li> Base::Enum::ISL_F64()
#   <li> Base::Enum::ISL_F96()
#   <li> Base::Enum::ISL_OTHER_32B()
#   <li> Base::Enum::ISL_OTHER_64B()
#   <li> Base::Enum::ISL_OTHER_96B()
# </ul>
# The default is @c Base::Enum::ISL_NONE()
# @return The starting index of the Layer 3 data
sub getL3Index($;$)
{
    my ($pDataArray, $ISLTag) = @_;

    $ISLTag = (defined($ISLTag) ? $ISLTag : ISL_NONE());

    my $tagLength = {
        ISL_NONE()      . ""    => 0,
        ISL_F64()       . ""    => 8,
        ISL_F96()       . ""    => 12,
        ISL_OTHER_32B() . ""    => 4,
        ISL_OTHER_64B() . ""    => 8,
        ISL_OTHER_96B() . ""    => 12,
    };
    my $ISLTagLength = (exists($tagLength->{$ISLTag})
                        ? $tagLength->{$ISLTag}
                        : 0);
    my $VLAN_TAG_0_MSB = 12 + $ISLTagLength;
    my $VLAN_TAG_0_LSB = 13 + $ISLTagLength;

    my ($msb, $lsb) = ($VLAN_TAG_0_MSB, $VLAN_TAG_0_LSB);
    for ( ; ; $msb += 4, $lsb += 4)
    {
        if (defined($pDataArray->[$msb]) && defined($pDataArray->[$lsb]))
        {
            my $type = (($pDataArray->[$msb] << 8) | $pDataArray->[$lsb]);
            if (!(($type == 0x8100) || ($type == 0x88a8) || ($type == 0x9100)))
            {
                # Exit if the Ethernet type field does not contain a VLAN type
                last;
            }
        }
        else
        {
            last;
        }
    }
    return ($msb + 2);
}

##@method public int[] getMAC(char *macAddress)
# Convert the string representation of a MAC address, e.g. "00:1D:00:30:0A:FF"
# into its integer representation
# @param[in] macAddress The MAC address to be converted
# @return The parsed MAC address (<i>Since Perl does not support 64-bit
# integers, the parsed MAC address will returned as two 32-bit integers in the
# order (hi, lo)</i>)
sub getMAC($)
{
    validatePrototype(@_, 1);
    my ($macAddress) = @_;

    my @data = (0) x 2;
    my @bytes = split(/:/, $macAddress);
    my $length = scalar(@bytes);
    for (my $i=0 ; $i<6 ; $i++)
    {
        $data[1 - int($i / 4)] |=
                   ($i < $length ? (hex(pop(@bytes)) << (8 * ($i % 4))) : 0x0);
    }
    return @data;
}

##@method public char* getSrcMac(char **pDataArray)
# Extract the source MAC address from the specified packet
# @param[in] pDataArray A perl reference to an array containing the packet data
# to be parsed
# @return The source MAC address in the form @c "AA:BB:CC:DD:EE:FF"
sub getSrcMac($)
{
    my ($pDataArray) = @_;

    my $SA = sprintf("%02x:%02x:%02x:%02x:%02x:%02x", 
                     $pDataArray->[6], $pDataArray->[7],
                     $pDataArray->[8], $pDataArray->[9], 
                     $pDataArray->[10], $pDataArray->[11]);
    return $SA;
}

##@method public int[] getVLAN(char **pDataArray)
# Extract the VLAN ID and VLAN priority from the first VLAN tag that is part of
# the specified packet
# @param[in] pDataArray A perl reference to an array containing the packet data
# to be parsed
# @return The VLAN ID and VLAN priority in the order <tt>(vid, vpri)</tt>
sub getVLAN($)
{
    my ($pDataArray) = @_;

    my ($vid, $vpri) = (undef, undef);
    if ($pDataArray->[12] == 0x81 && $pDataArray->[13] == 0x00)
    {
        $vid = ((($pDataArray->[14] & 0xf) << 8) | $pDataArray->[15]);
        $vpri = (($pDataArray->[14] & 0xe0) >> 5);
    }
    return ($vid, $vpri);
}

##@method public int getVLANCFI(char **pDataArray)
# Extract the CFI bit from the first VLAN tag. It is assumed the frame is VLAN
# tagged
# @param[in] pDataArray A perl reference to an array containing the packet data
# to be parsed
# @return CFI bit
sub getVLANCFI($)
{
    my ($pDataArray) = @_;

    return (($pDataArray->[14] >> 4) & 0x1);
    
}

##@method public char* randMac(int mc)
# Generate a random Layer 2 address
# @param[in] mc set to 1 to generate a random multicast address, set to 0 or
# undef to generate a random unicast address
# @return A random Layer 2 address in the form @c "AA:BB:CC:DD:EE:FF"
sub randMac(;$)
{
    validatePrototype(@_, 0, 1);
    my ($mc) = @_;

    # Clear bit so MC frames are not generated
    my $address;
    if((not defined $mc) || $mc == 0)
    {
         $address = sprintf("%02x", int(rand(256)) & 0xfe);
    }
    else
    {
         $address = sprintf("%02x", int(rand(256)) | 0x1 );
    }
    for (my $i=0 ; $i<5 ; $i++)
    {
        $address = sprintf("%s:%02x", $address, int(rand(256)) & 0xff);
    }
    return $address;
}

##@method public char** setAlternateVLANheader(int etherType, int VID, int priority, int cfi, char **pDataArray)
# Sets the VLAN header at byte 12..15 to the specified ethertype (which may be
# different from 0x8100)
# @param[in] etherType The Ethernet type of the VLAN tag. Examples are 0x8100,
# 0x88a8, 0x9100, etc.
# @param[in] VID The VLAN ID
# @param[in] priority The 3-bit VLAN priority
# @param[in] cfi The CFI bit
# @param[in,out] pDataArray A Perl reference to an array containing the packet
# data
# @return A Perl reference to an array containing the modified packet data
sub setAlternateVLANheader($$$$$)
{
    validatePrototype(@_, 5);
    my ($etherType, $VID, $priority, $cfi, $pDataArray) = @_; 

    $pDataArray->[12] = (($etherType >> 8) & 0xff);
    $pDataArray->[13] = ($etherType & 0xff);
    my $vlanTag = ((($priority & 0x7) << 13)
                   | (($cfi & 0x1) <<12)
                   | ($VID & 0xfff));
    $pDataArray->[14] = (($vlanTag >> 8) & 0xff);
    $pDataArray->[15] = ($vlanTag & 0xff);
    return $pDataArray;
}

##@method public char** setL2da(char *da, char **pDataArray)
# Set the Layer 2 destination address to the specified value
# @param[in] da The Layer 2 destination address
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 2
# header
# @return A Perl reference to an array containing the modified Layer 2 header
sub setL2da($$)
{
    my ($da, $pDataArray) = @_; 

    my ($mac_hi, $mac_lo) = getMAC($da);
    for (my $i=0 ; $i<6 ; $i++)
    {
        $pDataArray->[5 - $i] = 
                    ((($i >= 4 ? $mac_hi : $mac_lo) >> (8 * ($i % 4))) & 0xff);
    }
    return $pDataArray;
}

##@method public char** setL2sa(char *sa, char **pDataArray)
# Set the Layer 2 source address to the specified value
# @param[in] sa The Layer 2 source address
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 2
# header
# @return A Perl reference to an array containing the modified Layer 2 header
sub setL2sa($$)
{
    my ($sa, $pDataArray) = @_; 

    my ($mac_hi, $mac_lo) = getMAC($sa);
    for (my $i=0 ; $i<6 ; $i++)
    {
        $pDataArray->[11 - $i] = 
                    ((($i >= 4 ? $mac_hi : $mac_lo) >> (8 * ($i % 4))) & 0xff);
    }
    return $pDataArray;
}

##@method public char** setL2type(int type, char **pDataArray)
# Set the first non-VLAN Layer 2 type / length field to the specified value
# @param[in] type The Layer 2 type / length value
# @param[in,out] pDataArray A Perl reference to an array containing the packet
# data
# @return A Perl reference to an array containing the modified packet data
sub setL2type($$)
{
    validatePrototype(@_, 2);
    my ($type, $pDataArray) = @_;

    my $index = getL3Index($pDataArray) - 2;

    $pDataArray->[$index] = (($type >> 8) & 0xff);
    $pDataArray->[$index + 1] = ($type & 0xff);

    return $pDataArray;
}

##@method public char** setVLANheader(int VID, int priority, char **pDataArray)
# Sets the VLAN header of an existing packet at byte 12 .. 15. The VLAN header
# is identified by an Ethernet type of 0x8100 (802.1Q Tag Protocol Type)
# @param[in] VID The VLAN ID
# @param[in] priority The 3-bit VLAN priority
# @param[in,out] pDataArray A Perl reference to an array containing the packet
# data
# @return A Perl reference to an array containing the modified packet data
# @note The CFI bit is unset
sub setVLANheader($$$)
{
    validatePrototype(@_, 3);
    my ($VID, $priority, $pDataArray) = @_; 

    return setAlternateVLANheader(0x8100, $VID, $priority, 0x0, $pDataArray);
}

##@method public char** updateL2Chksum(char **pDataArray, bool corrupt)
# Update the Layer 2 frame check sequence (FCS)
# @param[in,out] pDataArray A Perl reference to an array containing packet data
# @param[in] corrupt @optional A boolean indicating whether or not to corrupt
# the the frame check sequence (FCS)
# @return A Perl reference to an array containing the modified packet data
sub updateL2Chksum($;$)
{
    validatePrototype(@_, 1, 2);
    my ($pDataArray, $corrupt) = @_;

    $corrupt = (defined($corrupt) ? $corrupt : FM_FALSE);

    my $length = scalar(@{$pDataArray});
    my $crc = CRC::CRC32($pDataArray, $length - 4);
    if ($corrupt)
    {
        $crc = ~$crc;
    }
    for (my $i=0 ; $i<4 ; $i++)
    {
        $pDataArray->[$length - 4 + $i] = (($crc >> (8 * $i)) & 0xff);
    }
    return $pDataArray;
}

# Generates an incrementing sequence of MAC addresses.
#   baseMac => First MAC address in the sequence. Formatted as
#       "XX:XX:XX:XX:XX:"
#   count => Length of sequence. Currently limited to < 256.
# Returns an array of MAC addresses.
sub generateMacSequence($$)
{
    my $baseMac = shift;
    my $count = shift;

    my @macs = ();
    for(my $i = 0; $i < $count; $i++)
    {
        my $post = sprintf("%02X", $i);
        push @macs, ($baseMac . $post);
    }
    return @macs;
}

## @method generateL2FrameSequence(char* base source MAC,
#                                  char* base dest MAC,
#                                  int sequence length)
#
sub generateL2FrameSequence($$$)
{
    my $baseSourceMac = shift;
    my $baseDestMac = shift;
    my $length = shift;

    my @sourceMacs = generateMacSequence($baseSourceMac, $length);
    my @destMacs = generateMacSequence($baseDestMac, $length);

    my @frames = ();
    for(my $i = 0; $i < $length; $i++)
    {
        my $frameHeader = GenFrame::genL2header($sourceMacs[$i], $destMacs[$i]);
        push @frames, ($frameHeader);
    }
    return @frames;
}

## @method padFrame(int length, char *data)
#   Returns an array with data padded to length. 
#   @param[in] length The length to pad to
#   @param[data] data The bytestream to pad
sub padFrame
{
    my ($length, @packet) = @_;
    
    my $padLength = $length - scalar(@packet);
    for(my $i = 0; $i < $padLength; $i++) 
    {
        push @packet, ($i & 0xff)
    }
    
    return @packet;
}

1;
