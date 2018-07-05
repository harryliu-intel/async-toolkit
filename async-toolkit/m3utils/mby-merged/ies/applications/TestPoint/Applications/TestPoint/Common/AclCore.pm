# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/AclCore.pm
# Creation Date:    December 14, 2007
# Description:      TestPoint interface to ACL compiler
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

package Applications::TestPoint::Common::AclCore;
use strict;
use warnings;
use Applications::TestPoint::Common::Messages;

use SDKScalars;
use Math::BigInt;

# Using the same hack for switch number that AclSimpleCore.pm uses.
# Not sure what the right thing to do is.
my $SWITCH_NUM = 0;

# Specifies that ParseAclList or ParseRuleList should accept the
# "all" specification.
my $ALLOW_ALL = 1;

# Specifies that validateList should generate a detailed error message
# if it encounters an invalid input.
my $VERBOSE_DETAIL = 2;

my %miscFlags =
    (ipOptionsFlagged => $FM_ACL_FLAG_IP_OPTIONS_FLAGGED,
     dontFragment => $FM_ACL_FLAG_DONTFRAG,
     headFragment => $FM_ACL_FLAG_HEADFRAG);

my %tcpFlags =
    (fin => $FM_TCP_FLAG_FIN,
     syn => $FM_TCP_FLAG_SYN,
     rst => $FM_TCP_FLAG_RST,
     psh => $FM_TCP_FLAG_PSH,
     ack => $FM_TCP_FLAG_ACK,
     urg => $FM_TCP_FLAG_URG);

my %etherTypes =
    (0x0800 => 'IPv4',
     0x0806 => 'ARP',
     0x86dd => 'IPv6');

my %islFrameTypes = 
    ($FM_ACL_ISL_FTYPE_NORMAL               => 'normal',
     $FM_ACL_ISL_FTYPE_ROUTED               => 'routed',
     $FM_ACL_ISL_FTYPE_SPECIAL              => 'special');

my %vlanTagTypes =
    ($FM_ACL_VLAN_TAG_TYPE_NONE        => 'none',
     $FM_ACL_VLAN_TAG_TYPE_STANDARD    => 'standard',
     $FM_ACL_VLAN_TAG_TYPE_USER_A      => 'typeA',
     $FM_ACL_VLAN_TAG_TYPE_USER_B      => 'typeB',
     $FM_ACL_VLAN_TAG_TYPE_VLAN1       => 'vlan1',
     $FM_ACL_VLAN_TAG_TYPE_VLAN2       => 'vlan2',
     $FM_ACL_VLAN_TAG_TYPE_ONLY_VLAN1  => 'vlan1Only',
     $FM_ACL_VLAN_TAG_TYPE_ONLY_VLAN2  => 'vlan2Only',
     $FM_ACL_VLAN_TAG_TYPE_VLAN1_VLAN2 => 'vlan1_vlan2');

my %fragTypes = 
    ($FM_ACL_FRAG_COMPLETE         => 'complete',
     $FM_ACL_FRAG_HEAD             => 'headFrag',
     $FM_ACL_FRAG_SUB              => 'subFrag',
     $FM_ACL_FRAG_COMPLETE_OR_HEAD => 'completeOrFrag');

my %frameTypes = 
    ($FM_ACL_FRAME_TYPE_NON_IP => 'nonIp',
     $FM_ACL_FRAME_TYPE_IPV4   => 'ipv4',
     $FM_ACL_FRAME_TYPE_IPV6   => 'ipv6');

my %trillTypes = 
    ($FM_ACL_TRILL_TYPE_ANY       => 'any',
     $FM_ACL_TRILL_TYPE_UNICAST   => 'ucast',
     $FM_ACL_TRILL_TYPE_MULTICAST => 'mcast');

my %protocols =
    (0 => 'HOPOPT',
     1 => 'ICMP',
     2 => 'IGMP',
     3 => 'GGP',
     4 => 'IP',
     5 => 'ST',
     6 => 'TCP',
     7 => 'CBT',
     8 => 'EGP',
     9 => 'IGP',
     10 => 'BBN-RCC-MON',
     11 => 'NVP-II',
     12 => 'PUP',
     13 => 'ARGUS',
     14 => 'EMCON',
     15 => 'XNET',
     16 => 'CHAOS',
     17 => 'UDP',
     18 => 'MUX',
     19 => 'DCN-MEAS',
     20 => 'HMP',
     21 => 'PRM',
     22 => 'XNS-IDP',
     23 => 'TRUNK-1',
     24 => 'TRUNK-2',
     25 => 'LEAF-1',
     26 => 'LEAF-2',
     27 => 'RDP',
     28 => 'IRTP',
     29 => 'ISO-TP4',
     30 => 'NETBLT',
     31 => 'MFE-NSP',
     32 => 'MERIT-INP',
     33 => 'DCCP',
     34 => '3PC',
     35 => 'IDPR',
     36 => 'XTP',
     37 => 'DDP',
     38 => 'IDPR-CMTP',
     39 => 'TP++',
     40 => 'IL',
     41 => 'IPv6',
     42 => 'SDRP',
     43 => 'IPv6-Route',
     44 => 'IPv6-Frag',
     45 => 'IDRP',
     46 => 'RSVP',
     47 => 'GRE',
     48 => 'DSR',
     49 => 'BNA',
     50 => 'ESP',
     51 => 'AH',
     52 => 'I-NLSP',
     53 => 'SWIPE',
     54 => 'NARP',
     55 => 'MOBILE',
     56 => 'TLSP',
     57 => 'SKIP',
     58 => 'IPv6-ICMP',
     59 => 'IPv6-NoNxt',
     60 => 'IPv6-Opts',
     62 => 'CFTP',
     64 => 'SAT-EXPAK',
     65 => 'KRYPTOLAN',
     66 => 'RVD',
     67 => 'IPPC',
     69 => 'SAT-MON',
     70 => 'VISA',
     71 => 'IPCV',
     72 => 'CPNX',
     73 => 'CPHB',
     74 => 'WSN',
     75 => 'PVP',
     76 => 'BR-SAT-MON',
     77 => 'SUN-ND',
     78 => 'WB-MON',
     79 => 'WB-EXPAK',
     80 => 'ISO-IP',
     81 => 'VMTP',
     82 => 'SECURE-VMTP',
     83 => 'VINES',
     84 => 'TTP',
     85 => 'NSFNET-IGP',
     86 => 'DGP',
     87 => 'TCF',
     88 => 'EIGRP',
     89 => 'OSPFIGP',
     90 => 'Sprite-RPC',
     91 => 'LARP',
     92 => 'MTP',
     93 => 'AX.25',
     94 => 'IPIP',
     95 => 'MICP',
     96 => 'SCC-SP',
     97 => 'ETHERIP',
     98 => 'ENCAP',
     100 => 'GMTP',
     101 => 'IFMP',
     102 => 'PNNI',
     103 => 'PIM',
     104 => 'ARIS',
     105 => 'SCPS',
     106 => 'QNX',
     107 => 'A/N',
     108 => 'IPComp',
     109 => 'SNP',
     110 => 'Compaq-Peer',
     111 => 'IPX-in-IP',
     112 => 'VRRP',
     113 => 'PGM',
     115 => 'L2TP',
     116 => 'DDX',
     117 => 'IATP',
     118 => 'STP',
     119 => 'SRP',
     120 => 'UTI',
     121 => 'SMP',
     122 => 'SM',
     123 => 'PTP',
     124 => 'ISIS',
     125 => 'FIRE',
     126 => 'CRTP',
     127 => 'CRUDP',
     128 => 'SSCOPMCE',
     129 => 'IPLT',
     130 => 'SPS',
     131 => 'PIPE',
     132 => 'SCTP',
     133 => 'FC',
     134 => 'RSVP-E2E-IGNORE',
     135 => 'Mobility',
     136 => 'UDPLite',
     137 => 'MPLS-in-IP',
     138 => 'manet',
     139 => 'HIP');

my %nonIPPayloadChunks =
(
    'nbChunks'         =>  '4',
    'chunkSize'        =>  '8',     #In Bytes
    'cArrayTypeSize'   =>  '1',     #In Bytes
);

my %L4DeepInspectionExtChunks =
(
    'nbChunks'         =>  '4',
    'chunkSize'        =>  '8',     #In Bytes
    'cArrayTypeSize'   =>  '1',     #In Bytes
);

my %conditions =
(
    'smac'              => [$FM_ACL_MATCH_SRC_MAC, "src", '%s', 'ff:ff:ff:ff:ff:ff'],
    'dmac'              => [$FM_ACL_MATCH_DST_MAC, "dst", '%s', 'ff:ff:ff:ff:ff:ff'],
    'ethtype'           => [$FM_ACL_MATCH_ETHERTYPE, "ethType", '0x%04x', 0xffff, \%etherTypes],
    'vlan'              => [$FM_ACL_MATCH_VLAN, "vlanId", '%d', 4095],
    'vlan2'             => [$FM_ACL_MATCH_VLAN2, "vlanId2", '%d', 4095],
    'priority'          => [$FM_ACL_MATCH_PRIORITY, "vlanPri", '%d', 15],
    'priority2'         => [$FM_ACL_MATCH_PRIORITY2, "vlanPri2", '%d', 15],
    'sip'               => [$FM_ACL_MATCH_SRC_IP, "srcIp"],
    'dip'               => [$FM_ACL_MATCH_DST_IP, "dstIp"],
    'protocol'          => [$FM_ACL_MATCH_PROTOCOL, "protocol", '%d', 0xff, \%protocols],
    'flags'             => [$FM_ACL_MATCH_FLAGS, "flags", '%d', 7, \%miscFlags],
    'traffic'           => [$FM_ACL_MATCH_DSCP, "dscp", '%d', 0x3f],
    'flow'              => [$FM_ACL_MATCH_FLOW, "flow", '0x%05x', 0xfffff],
    'ttl'               => [$FM_ACL_MATCH_TTL, "ttl", '%d', 0xff],
    'l4-src-port'       => [$FM_ACL_MATCH_L4_SRC_PORT_WITH_MASK, "L4SrcStart", '%d', 0xFFFF],
    'l4-dst-port'       => [$FM_ACL_MATCH_L4_DST_PORT_WITH_MASK, "L4DstStart", '%d', 0xFFFF],
    'tcp-flags'         => [$FM_ACL_MATCH_TCP_FLAGS, "tcpFlags", '%d', 0x3f, \%tcpFlags],
    'l4-entry'          => [$FM_ACL_MATCH_L4_ENTRY, "L4Entry", '%d', 0xFFFF],
    'l4-deep-inspection'  => [$FM_ACL_MATCH_L4_DEEP_INSPECTION,
                              "L4DeepInspection", undef, "18446744073709551615"],
    'l4-deep-inspection2' => [$FM_ACL_MATCH_L4_DEEP_INSPECTION_EXT, "L4DeepInspectionExt", undef, 
                         "18446744073709551615", undef, \%L4DeepInspectionExtChunks],
    'non-ip-payload'    => [$FM_ACL_MATCH_NON_IP_PAYLOAD, "nonIPPayload", undef, 
                         "18446744073709551615", undef, \%nonIPPayloadChunks],
    'switch-priority'   => [$FM_ACL_MATCH_SWITCH_PRIORITY, "switchPri", '%d', 15],
    'ingress-port'      => [$FM_ACL_MATCH_INGRESS_PORT_MASK, "ingressPortMask", '0x%06x'],
    'vlan-tag'          => [$FM_ACL_MATCH_VLAN_TAG_TYPE, "vlanTag", '%s', undef, \%vlanTagTypes],
    'tos'               => [$FM_ACL_MATCH_TOS, "tos", '%d', 0xff],
    'port-set'          => [$FM_ACL_MATCH_INGRESS_PORT_SET, "portSet", '%d', undef],
    'ip-length'         => [$FM_ACL_MATCH_IP_LENGTH, "ipLength", '%d', 0xffff],
    'ftype'             => [$FM_ACL_MATCH_ISL_FTYPE, "fType", '%s', undef, \%islFrameTypes],
    'isl-user'          => [$FM_ACL_MATCH_ISL_USER, "islUser", '%d', 0xff],
    'src-glort'         => [$FM_ACL_MATCH_SRC_GLORT, "srcGlort", '%d', 0xffff],
    'dst-glort'         => [$FM_ACL_MATCH_DST_GLORT, "dstGlort", '%d', 0xffff],
    'frag-type'         => [$FM_ACL_MATCH_FRAG, "fragType", '%s', undef, \%fragTypes],
    'frame-type'        => [$FM_ACL_MATCH_FRAME_TYPE, "frameType", '%s', undef, \%frameTypes],
    'map-src'           => [$FM_ACL_MATCH_SOURCE_MAP, "mappedSourcePort", '%d', 0xff],
    'map-protocol'      => [$FM_ACL_MATCH_PROTOCOL_MAP, "mappedProtocol", '%d', 0xf],
    'map-l4-src'        => [$FM_ACL_MATCH_L4_SRC_PORT_MAP, "mappedL4SrcPort", '%d', 0xffff],
    'map-l4-dst'        => [$FM_ACL_MATCH_L4_DST_PORT_MAP, "mappedL4DstPort", '%d', 0xffff],
    'map-smac'          => [$FM_ACL_MATCH_SRC_MAC_MAP, "mappedSrcMac", '%d', 0xf],
    'map-dmac'          => [$FM_ACL_MATCH_DST_MAC_MAP, "mappedDstMac", '%d', 0xf],
    'map-ethtype'       => [$FM_ACL_MATCH_ETH_TYPE_MAP, "mappedEthType", '%d', 0xf],
    'map-ip-length'     => [$FM_ACL_MATCH_IP_LENGTH_MAP, "mappedIpLength", '%d', 0xf],
    'map-sip'           => [$FM_ACL_MATCH_SRC_IP_MAP, "mappedSrcIp", '%d', 0xf],
    'map-dip'           => [$FM_ACL_MATCH_DST_IP_MAP, "mappedDstIp", '%d', 0xf],
    'map-vlan'          => [$FM_ACL_MATCH_VLAN_MAP, "mappedVlanId", '%d', 0xfff],
    'trill-smac'        => [$FM_ACL_MATCH_TRILL_SRC_MAC, "trillSrc", '%s', 'ff:ff:ff:ff:ff:ff'],
    'trill-type'        => [$FM_ACL_MATCH_TRILL_TYPE, "trillType", '%s', undef, \%trillTypes],
    'trill-srb'         => [$FM_ACL_MATCH_TRILL_SRB, "trillSRB", '%d', 0xffff],
    'trill-rrb'         => [$FM_ACL_MATCH_TRILL_RRB, "trillRRB", '%d', 0xffff],
);

my %actions =
(
    'permit'            => [$FM_ACL_ACTIONEXT_PERMIT, undef],
    'deny'              => [$FM_ACL_ACTIONEXT_DENY, undef],
    'trap'              => [$FM_ACL_ACTIONEXT_TRAP, undef],
    'mirror'            => [$FM_ACL_ACTIONEXT_MIRROR, "mirrorPort", '%d', undef],
    'count'             => [$FM_ACL_ACTIONEXT_COUNT, undef],
    'log'               => [$FM_ACL_ACTIONEXT_LOG, undef],
    'police'            => [$FM_ACL_ACTIONEXT_POLICE, "policer", '%d', undef],
    'vlan'              => [$FM_ACL_ACTIONEXT_SET_VLAN, "vlan", '%d', undef],
    'dscp'              => [$FM_ACL_ACTIONEXT_SET_DSCP, "dscp", '%d', undef],
    'user'              => [$FM_ACL_ACTIONEXT_SET_USER, "user", '0x%02x', 0xff],
    'redirect'          => [$FM_ACL_ACTIONEXT_REDIRECT, "logicalPort", '%d', undef],
    'flood-dest'        => [$FM_ACL_ACTIONEXT_SET_FLOOD_DEST, "logicalPort", '%d', undef],
    'lbg'               => [$FM_ACL_ACTIONEXT_LOAD_BALANCE, "lbgNumber", '%d', undef],
    'route'             => [$FM_ACL_ACTIONEXT_ROUTE, "groupId", '%d', undef],
    'vlan-priority'     => [$FM_ACL_ACTIONEXT_SET_VLAN_PRIORITY,
                            "vlanPriority", '%d', undef],
    'switch-priority'   => [$FM_ACL_ACTIONEXT_SET_SWITCH_PRIORITY,
                            "switchPriority", '%d', undef],
    'count-notify'      => [$FM_ACL_ACTIONEXT_COUNT_NOTIFY, undef],
    'noroute'           => [$FM_ACL_ACTIONEXT_NOROUTE, undef],
    'trap-always'       => [$FM_ACL_ACTIONEXT_TRAP_ALWAYS, undef],
    'l3-hash-prof'      => [$FM_ACL_ACTIONEXT_SET_L3_HASH_PROFILE, "l3HashProfile", '%d', undef],
    'vlan2'             => [$FM_ACL_ACTIONEXT_SET_VLAN2, "vlan2", '%d', undef],
    'vlan2-priority'    => [$FM_ACL_ACTIONEXT_SET_VLAN2_PRIORITY,
                            "vlanPriority2", '%d', undef],
    'mirror-grp'        => [$FM_ACL_ACTIONEXT_MIRROR_GRP, "mirrorGrp", '0x%01x', undef],
    'srcGlort'          => [$FM_ACL_ACTIONEXT_SET_SRC_GLORT, "srcGlort", '0x%03x', undef],
    'tunnelId'          => [$FM_ACL_ACTIONEXT_SET_INGRESS_TUNNEL_ID, "tunnelId", '%d', undef],
    'distTree'          => [$FM_ACL_ACTIONEXT_SET_TRILL_DIST_TREE, "distTree", '%d', undef],
    'precedence'        => [$FM_ACL_ACTIONEXT_SET_PRECEDENCE, "precedence", '%d', undef],
    'tunnelIdSrc'       => [$FM_ACL_ACTIONEXT_SET_TUNNEL_ID_SGLORT, "tunnelId", '%d', undef, "srcGlort", '0x%03x'],
    'vn-tunnel'         => [$FM_ACL_ACTIONEXT_VN_TUNNEL, "vnTunnel", "%d", undef],
);

##
# <frameType> keywords for the create acl command.
# 
my @frameTypes =
(
    ["any",         $FM_ACL_SCENARIO_ANY_FRAME_TYPE],
    ["ipv4",        $FM_ACL_SCENARIO_IPv4],
    ["ipv6",        $FM_ACL_SCENARIO_IPv6],
    ["non-ip",      $FM_ACL_SCENARIO_NONIP],
);

##
# <routingType> keywords for the create acl command.
# 
my @routingTypes =
(
    ["any",         $FM_ACL_SCENARIO_ANY_ROUTING_TYPE],
    ["switched",    $FM_ACL_SCENARIO_SWITCHED],
    ["multicast",   $FM_ACL_SCENARIO_MULTICAST_ROUTED],
    ["unicast",     $FM_ACL_SCENARIO_UNICAST_ROUTED],
    ["any-glort",   $FM_ACL_SCENARIO_ANY_ROUTING_GLORT_TYPE],
    ["switched-glort", $FM_ACL_SCENARIO_SWITCHED_GLORT],
    ["mcast-glort", $FM_ACL_SCENARIO_MCAST_ROUTED_GLORT],
    ["ucast-glort", $FM_ACL_SCENARIO_UCAST_ROUTED_GLORT],
);

##
# <tunnelType> keywords for the create acl command.
#
my @tunnelTypes =
(
    ["any",             $FM_ACL_SCENARIO_ANY_TUNNEL_TYPE],
    ["trill-unicast",   $FM_ACL_SCENARIO_TRILL_UCAST],
    ["trill-multicast", $FM_ACL_SCENARIO_TRILL_MCAST],
    ["vn-tunnel",       $FM_ACL_SCENARIO_VN_TUNNEL],
);

##
# ip mapper modes for the apply acl command.
# 
my %ipMapperModes =
(
    'disabled'      =>  $FM_ACL_COMPILE_FLAG_IP_MAPPER_DISABLED,
    'limited'       =>  $FM_ACL_COMPILE_FLAG_IP_MAPPER_LIMITED,
    'normal'        =>  $FM_ACL_COMPILE_FLAG_IP_MAPPER_NORMAL,
    'aggressive'    =>  $FM_ACL_COMPILE_FLAG_IP_MAPPER_AGGRESSIVE,
);

##
# L4 port mapper modes for the apply acl command.
# 
my %l4MapperModes =
(
    'disabled'      =>  $FM_ACL_COMPILE_FLAG_L4_MAPPER_DISABLED,
    'limited'       =>  $FM_ACL_COMPILE_FLAG_L4_MAPPER_LIMITED,
    'normal'        =>  $FM_ACL_COMPILE_FLAG_L4_MAPPER_NORMAL,
    'aggressive'    =>  $FM_ACL_COMPILE_FLAG_L4_MAPPER_AGGRESSIVE,
);


##
# ScenarioToString
# 
# @desc         Converts a bitmask to a comma-separated string of 
#               scenario keywords.
# 
# @param[in]    $table is a scenario keyword table.
# 
# @param[in]    $value is the bitmask to be converted.
# 
# @return       Comma-separated string.
# 
sub ScenarioToString
{
    my ($table, $value) = @_;

    my $str = "";

    foreach my $entry (@{$table})
    {
        my ($name, $mask) = @{$entry};

        if (($value & $mask) == $mask)
        {
            $str .= "," if length($str) ne 0;
            $str .= $name;
            $value &= ~$mask;
        }
    }

    return $str;

}   # end ScenarioToString


##
# StringToScenario
# 
# @desc         Converts a scenario keyword to a bitmask value.
# 
# @param[in]    $table is a scenario keyword table.
# 
# @param[in]    $str is a string specifying a scenario keyword.
# 
# @return       Bitmask value, or undef if no match was found.
# 
sub StringToScenario
{
    my ($table, $str) = @_;

    $str =~ tr/A-Z/a-z/;

    my $len = length($str);

    foreach my $entry (@{$table})
    {
        my ($name, $mask) = @{$entry};

        if ($str eq $name)
        {
            return $mask;
        }
    }

    return undef;

}   # end StringToScenario

##
# ChunkToArray
# 
# @desc         Converts chunk to an array.
# 
# @param[in]    %chunkDefRef is a reference to the chunk and array format hash.
# 
# @param[in]    %chunkID is the chunk ID.
# 
# @param[in]    $chunk is the chunk to convert.
# 
# @return       Array.
# 
sub ChunkToArray
{
    my ($chunkDefRef, $chunkID, $chunk) = @_;
    my %chunkDef = %$chunkDefRef;
    
    my $bigChunk = Math::BigInt->new($chunk);
    my $arrayWidthPerChunk = $chunkDef{'chunkSize'} / $chunkDef{'cArrayTypeSize'};
    my $arraySize = $chunkDef{'nbChunks'} * $arrayWidthPerChunk; 
    my @newArray = ((0,) x $arraySize);

    my $arrayPosition = $chunkID * $arrayWidthPerChunk;

    #Fill the array
    for (my $i = 0; $i < $arrayWidthPerChunk; $i++)
    {
        my $bigChunkTemp = $bigChunk->copy();
        my $shiftAmmount = ($arrayWidthPerChunk - 1 - $i) * 8 * $chunkDef{'cArrayTypeSize'};
        my $nbMaskedBits = $chunkDef{'cArrayTypeSize'} * 8;
        my $maskValue = (2 ** $nbMaskedBits) - 1;

        $bigChunkTemp->brsft($shiftAmmount);
        $bigChunkTemp->band($maskValue);

        my $chunkShiftedMasked = $bigChunkTemp->bstr() ;
        $newArray[$i+$arrayPosition] = $chunkShiftedMasked; 
    }


    return @newArray;
    
}   #end ChunkToArray

##
# MergeArray
# 
# @desc         Merges an orignal array with an array generated by ChunkToArray.
# 
# @param[in]    %chunkDefRef is a reference to the chunk and array format hash.
# 
# @param[in]    %chunkID is the chunk ID.
# 
# @param[in]    $originalArray is the original array.
# 
# @param[in]    $newArray is the array generated by ChunkToArray.
# 
# @return       Array.
#
sub MergeArrays
{
    my ($chunkDefRef, $chunkID, $originalArray, $newArray) = @_;
    my %chunkDef = %$chunkDefRef;

    my $arrayWidthPerChunk = $chunkDef{'chunkSize'} / $chunkDef{'cArrayTypeSize'};
    my $arraySize = $chunkDef{'nbChunks'} * $arrayWidthPerChunk; 
    my @mergedArray = @$originalArray;
    my $arrayPosition = $chunkID * $arrayWidthPerChunk;

    #Merge items
    for (my $i = $arrayPosition; $i < $arrayPosition+$arrayWidthPerChunk; $i++)
    {
        $mergedArray[$i] = $$newArray[$i];
    }
    
    return @mergedArray;

}   #end MergeArrays

##
# ArrayToChunk
# 
# @desc         Converts an array of data to a hash of chunks.
# 
# @param[in]    %chunkDefRef is a reference to the chunk and array format hash.
# 
# @param[in]    $arrayRef is the reference array to convert.
# 
# @return       A hash containing the keys 1,2,..n and the values of associated
#               chunk in hexadecimal format.
#
sub ArrayToChunk
{
    my ($chunkDefRef, $arrayRef) = @_;
    my %chunkDef = %$chunkDefRef;

    my $arrayWidthPerChunk = $chunkDef{'chunkSize'} / $chunkDef{'cArrayTypeSize'};
    my $arraySize = $chunkDef{'nbChunks'} * $arrayWidthPerChunk;
    my %chunkHash;
    my $txt = "";
    my $hashIndex = 0;

    foreach my $entry (@$arrayRef)
    {
        my $tmpTxt = sprintf ("%X", $entry);

        #fill the hex string with 0's if needed
        while(length($tmpTxt) < ($chunkDef{'cArrayTypeSize'} * 2))
        {
            $tmpTxt = "0" . $tmpTxt;
        }
        $txt .= "$tmpTxt";

        if (length($txt) >= ($chunkDef{'chunkSize'} * 2))
        {
            $chunkHash{$hashIndex++} = "0x" . $txt;
            $txt = "";
        }

    }

    return %chunkHash;
}   #end ArrayToChunk



##
# InvalidScenarioValue
# 
# @desc         Reports an invalid scenario keyword.
#               Writes an error message followed by a list of acceptable
#               values.
# 
# @param[in]    $table is a scenario keyword table.
# 
# @param[in]    $label is a string specifying the type of scenario keyword.
# 
# @param[in]    $str is a string specifying the scenario keyword.
# 
sub InvalidScenarioValue
{
    my ($table, $label, $str) = @_;

    printf("'%s' is not a valid %s\n", $str, $label);

    my @valid;
    foreach my $entry (@{$table})
    {
        push(@valid, $entry->[0]);
    }

    printf("It should be one of: %s\n", join(', ', @valid));

}   # end InvalidScenarioValue


##
# ParseScenarioValues
# 
# @desc         Parses the scenario parameters from a create acl command,
#               and returns a numeric value.
# 
# @param[in]    $frameType is a string specifying a comma-separated list
#               of <frameType> keywords.
# 
# @param[in]    $routingType is a string specifying a comma-separated list
#               of <routingType> keywords.
#
# @param[in]    $tunnelType is a string specifying a comma-separated list
#               of <tunnelType> keywords.
# 
# @return       Integer representation of the scenario bitmask, or
#               undef if a parsing error occurred.
#               
sub ParseScenarioValues
{
    my ($frameType, $routingType, $tunnelType) = @_;

    my $scenario = 0;

    $frameType =~ s/\s//g;

    foreach my $ftype (split(/,/, $frameType))
    {
        if ($ftype ne "")
        {
            my $fvalue = StringToScenario(\@frameTypes, $ftype);
    
            if (!defined($fvalue))
            {
                InvalidScenarioValue(\@frameTypes, "frametype", $ftype);
                return undef;
            }
    
            $scenario |= $fvalue;
        }
    }

    $routingType =~ s/\s//g;

    foreach my $rtype (split(/,/, $routingType))
    {
        if ($rtype ne "")
        {
            my $rvalue = StringToScenario(\@routingTypes, $rtype);

            if (!defined($rvalue))
            {
                InvalidScenarioValue(\@routingTypes, "routingtype", $rtype);
                return undef;
            }

            $scenario |= $rvalue;
        }
    }

    if ($tunnelType ne "")
    {
        $tunnelType =~ s/\s//g;

        foreach my $ttype (split(/,/, $tunnelType))
        {
            if ($ttype ne "")
            {
                my $tvalue = StringToScenario(\@tunnelTypes, $ttype);

                if (!defined($tvalue))
                {
                    InvalidScenarioValue(\@tunnelTypes, "tunneltype", $ttype);
                    return undef;
                }

                $scenario |= $tvalue;
            }
        }
    }

    return $scenario;

}   # end ParseScenarioValues


##
# ParseCompilerFlags
# 
# @descr        Parses the compiler options from an apply acl command,
#               and returns a numeric value.
# 
# @param[in]    spec is a string specifying the list of compiler options.
# 
# @return       A tuple of the form (status, value, quiet).
# @return       status is FM_OK if the request succeeds, or an error
#               code if a parsing error occurs.
# @return       value is an integer representation of the compiler flags,
#               or undef if a parsing error occurs.
# @return       quiet is non-zero if a "quiet" compile/apply was requested,
#               or zero if a normal compile/apply should be performed.
# 
sub ParseCompilerFlags
{
    my ($spec) = @_;

    my $flags = $FM_ACL_COMPILE_FLAG_PARALLEL;
    my $quiet = 0;

    $spec =~ s/\s//g;
    $spec =~ tr/A-Z/a-z/;

    foreach my $item (split(',', $spec))
    {
        if ($item =~ /^ipmapper[:=]([a-z]+)$/)
        {
            my $val = $ipMapperModes{$1};
            if (defined($val))
            {
                $flags &= ~$FM_ACL_COMPILE_FLAG_IP_MAPPER_AGGRESSIVE;
                $flags |= $val;
            }
            else
            {
                print "'$1' is not a valid ip mapper mode.\n";
                return ($FM_ERR_INVALID_ARGUMENT, undef);
            }
        }
        elsif ($item eq "ipmapper")
        {
            $flags &= ~$FM_ACL_COMPILE_FLAG_IP_MAPPER_AGGRESSIVE;
            $flags |= $FM_ACL_COMPILE_FLAG_IP_MAPPER_NORMAL;
        }
        elsif ($item eq "noipmapper")
        {
            $flags &= ~$FM_ACL_COMPILE_FLAG_IP_MAPPER_AGGRESSIVE;
        }
        elsif ($item =~ /^l4mapper[:=]([a-z]+)$/)
        {
            my $val = $l4MapperModes{$1};
            if (defined($val))
            {
                $flags &= ~$FM_ACL_COMPILE_FLAG_L4_MAPPER_AGGRESSIVE;
                $flags |= $val;
            }
            else
            {
                print "'$1' is not a valid L4 mapper mode.\n";
                return ($FM_ERR_INVALID_ARGUMENT, undef);
            }
        }
        elsif ($item eq "l4mapper")
        {
            $flags &= ~$FM_ACL_COMPILE_FLAG_L4_MAPPER_AGGRESSIVE;
            $flags |= $FM_ACL_COMPILE_FLAG_L4_MAPPER_NORMAL;
        }
        elsif ($item eq "nol4mapper")
        {
            $flags &= ~$FM_ACL_COMPILE_FLAG_L4_MAPPER_AGGRESSIVE;
        }
        elsif ($item eq "parallel")
        {
            $flags |= $FM_ACL_COMPILE_FLAG_PARALLEL;
        }
        elsif ($item eq "noparallel")
        {
            $flags &= ~$FM_ACL_COMPILE_FLAG_PARALLEL;
        }
        elsif ($item eq "nondisruptive")
        {
            $flags |= $FM_ACL_COMPILE_FLAG_NON_DISRUPTIVE;
        }
        elsif ($item eq "tryalloc")
        {
            $flags |= $FM_ACL_COMPILE_FLAG_TRY_ALLOC;
        }
        elsif ($item eq "quiet")
        {
            $quiet = 1;
        }
        else
        {
            print "'$item' is not a valid compiler option.\n";
            return ($FM_ERR_INVALID_ARGUMENT, undef);
        }
    }

    return ($FM_OK, $flags, $quiet);

}   # end ParseCompilerFlags


##
# ParseAclList
# 
# @descr        Parses the ACL specification from a command and returns
#               a list of ACL numbers.
# 
# @param[in]    aclSpec is a string specifying a list of ACL numbers.
# 
# @param[in]    allowAll is TRUE if the specification may be "all",
#               FALSE otherwise.
# 
# @return       A list of ACL numbers. Will be empty if an error occurs.
# 
sub ParseAclList
{
    my ($self, $aclSpec, $allowAll) = @_;

    if ((defined $aclSpec) && ($aclSpec =~ m/all/i))
    {
        if (defined($allowAll) && $allowAll)
        {
            return ("all");
        }
        else
        {
            print "May not specify \"all\" ACLs for this command\n";
            return ();
        }
    }

    return $self->validateList($aclSpec, 0, $FM_MAX_ACLS, $VERBOSE_DETAIL);

}   # end ParseAclList


##
# ParseRuleList
# 
# @descr        Parses the rule specification from a command and returns
#               a list of rule numbers.
# 
# @param[in]    ruleSpec is a string specifying a list of rule numbers.
# 
# @param[in]    allowAll is TRUE if the specification may be "all",
#               FALSE otherwise.
# 
# @return       A list of rule numbers. Will be empty if an error occurs.
# 
sub ParseRuleList
{
    my ($self, $ruleSpec, $allowAll) = @_;

    if ((defined $ruleSpec) && ($ruleSpec =~ m/all/i))
    {
        if (defined($allowAll) && $allowAll)
        {
            return ("all");
        }
        else
        {
            print "May not specify \"all\" rules for this command\n";
            return ();
        }
    }

    return $self->validateList($ruleSpec, 0, $FM_MAX_ACL_RULES, $VERBOSE_DETAIL);

}   # end ParseRuleList


##
# ParsePortSetList
# 
# @descr        Parses the port-set specification from a command and returns
#               a list of port-set numbers.
# 
# @param[in]    $portSetSpec is a string specifying a list of port-set numbers.
# 
# @param[in]    allowAll is TRUE if the specification may be "all",
#               FALSE otherwise.
# 
# @return       A list of port set numbers. Will be empty if an error occurs.
# 
sub ParsePortSetList
{
    my ($self, $portSetSpec, $allowAll) = @_;
    my $maxPortSet;
    my $api = $self->{'CHIP'};

    if ($portSetSpec =~ m/all/i)
    {
        if (defined($allowAll) && $allowAll)
        {
            return ("all");
        }
        else
        {
            print "May not specify \"all\" port-sets for this command\n";
            return ();
        }
    }

    $api->disableErrors();

    $maxPortSet = $api->fmGetIntApiAttribute(
                                $FM_AAK_API_FM4000_MAX_ACL_PORT_SETS,
                                $FM_AAD_API_FM4000_MAX_ACL_PORT_SETS);

    $api->enableErrors();

    return $self->validateList($portSetSpec, 0, $maxPortSet, $VERBOSE_DETAIL);

}   # end ParsePortSetList


##
# GetAclList
# 
# @descr        Returns an expanded list of ACL numbers.
# 
# @param[in]    sw is the switch on which to operate.
# 
# @param[in]    acls is the list of ACL numbers specified on the command.
# 
# @return       A list of ACL numbers.
# 
sub GetAclList
{
    my ($self, $sw, @acls) = @_;

    if (scalar(@acls) == 1 && $acls[0] eq "all")
    {
        my $api = $self->{'CHIP'};
        my $acl = 0;
        my $status;

        @acls = ();

        $api->disableErrors();
        $status = $api->fmGetACLFirst($sw, \$acl);
        while ($status == $FM_OK)
        {
            push @acls, $acl;
            $status = $api->fmGetACLNext($sw, $acl, \$acl);
        }
        $api->enableErrors();
    }

    return @acls;

}   # GetAclList


##
# GetRuleList
# 
# @descr        Returns an expanded list of rule numbers.
# 
# @param[in]    sw is the switch on which to operate.
# 
# @param[in]    acl is the acl number on which to operate.
# 
# @param[in]    rules is the list of rule numbers specified for
#               the command.
# 
# @return       A list of rule numbers.
# 
sub GetRuleList
{
    my ($self, $sw, $acl, @rules) = @_;

    if (scalar(@rules) == 1 && $rules[0] eq "all")
    {
        my $api = $self->{'CHIP'};
        my $cond = Math::BigInt->new(0);
        my ($rule, $action) = 0;
        my $value = SDK::fm_aclValue->new();
        my $param = SDK::fm_aclParamExt->new();
        my $status;

        @rules = ();

        $api->disableErrors();

        $status = $api->fmGetACLRuleFirstExt($sw,
                                             $acl,
                                             \$rule,
                                             \$cond,
                                             $value,
                                             \$action,
                                             $param);

        while ($status == $FM_OK)
        {
            push @rules, $rule;

            $status = $api->fmGetACLRuleNextExt($sw,
                                                $acl,
                                                $rule,
                                                \$rule,
                                                \$cond,
                                                $value,
                                                \$action,
                                                $param);
        }

        $api->enableErrors();
    }

    return @rules;

}   # GetRuleList


##
# GetCounterRules
# 
# @descr        Returns a list of rule numbers with counters.
# 
# @param[in]    sw is the switch on which to operate.
# 
# @param[in]    acl is the acl number on which to operate.
# 
# @param[in]    rules is the list of rule numbers specified for
#               the command.
# 
# @return       A list of rule numbers.
# 
sub GetCounterRules
{
    my ($self, $sw, $acl, @rules) = @_;

    if (scalar(@rules) == 1 && $rules[0] eq "all")
    {
        my $api = $self->{'CHIP'};
        my $cond = Math::BigInt->new(0);
        my ($rule, $action) = 0;
        my $value = SDK::fm_aclValue->new();
        my $param = SDK::fm_aclParamExt->new();
        my $status;

        @rules = ();

        $api->disableErrors();

        $status = $api->fmGetACLRuleFirstExt($sw,
                                             $acl,
                                             \$rule,
                                             \$cond,
                                             $value,
                                             \$action,
                                             $param);

        while ($status == $FM_OK)
        {
            push @rules, $rule if ($action & $FM_ACL_ACTIONEXT_COUNT);

            $status = $api->fmGetACLRuleNextExt($sw,
                                                $acl,
                                                $rule,
                                                \$rule,
                                                \$cond,
                                                $value,
                                                \$action,
                                                $param);
        }

        $api->enableErrors();
    }

    return @rules;

}   # GetCounterRules


##
# GetAclPortSetList
# 
# @descr        Returns an expanded list of port-set numbers.
# 
# @param[in]    sw is the switch on which to operate.
# 
# @param[in]    portSets is the list of port-set numbers specified on the command.
# 
# @return       A list of port set-numbers.
# 
sub GetAclPortSetList
{
    my ($self, $sw, @portSets) = @_;

    if (scalar(@portSets) == 1 && $portSets[0] eq "all")
    {
        my $api = $self->{'CHIP'};
        my $portSet = 0;
        my $status;

        @portSets = ();

        $api->disableErrors();
        $status = $api->fmGetACLPortSetFirst($sw, \$portSet);
        while ($status == $FM_OK)
        {
            push @portSets, $portSet;
            $status = $api->fmGetACLPortSetNext($sw, $portSet, \$portSet);
        }
        $api->enableErrors();
    }

    return @portSets;

}   # GetAclPortSetList


##@cmethod private int tpHandleShowAcl(const char *acls, const char *rule)
#
# @desc         Displays one or more ACLs.
#
# @param[in]    acls is a list of ACL numbers.
#
# @param[in]    rules is an optional list of rule numbers.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleShowAcl
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs < 1)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    my $cond = Math::BigInt->new(0);
    my $action = 0;
    my $value = SDK::fm_aclValue->new();
    my $param = SDK::fm_aclParamExt->new();

    # Get <rules> parameter.
    my @rules = ("all");
    if ($nargs >= 2)
    {
        @rules = $self->ParseRuleList(shift @_, $ALLOW_ALL);
        if (!@rules)
        {
            print "Must specify a valid rule number or range!\n";
            return $FM_FAIL;
        }
    }
    
    my $countLeading = sub {
        my $x = $_[0];
        my @bits = split(//, sprintf('%032b', Applications::TestPoint::Common::RoutingCore::ntohl($x)));
        my $result = 0;
        while ($#bits >= 0 and shift(@bits) eq '1')
        {
            $result++;
        }
        return $result;

    };  # end $countLeading

    my $countLeadingIPv6 = sub {
        my $result = 0;
        for (my $i = 3 ; $i >= 0 ; $i--)
        {
            return $result + $countLeading->($_[$i]) if ($_[$i] != 0xffffffff);
            $result += 32;
        }
        return $result;

    };  # end $countLeadingIPv6

    my $printHalfRule = sub {
        my ($start, $continuation, $bitmask, $struct, $table, $empty) = @_;

        my $name;

        foreach $name (sort keys %$table)
        {
            my ($bit, $field, $fmt, $max, $translation, $chunkDef) = @{$table->{$name}};
            if (($bit & $bitmask) != 0)
            {
                my $fieldMask;
                my $field2;
                
                if ($bit == $FM_ACL_ACTIONEXT_SET_TUNNEL_ID_SGLORT)
                {
                    $field2 = $translation;
                    $translation = undef;
                    $chunkDef = undef;
                }
                elsif ($bit == $FM_ACL_MATCH_L4_SRC_PORT_WITH_MASK ||
                       $bit == $FM_ACL_MATCH_L4_DST_PORT_WITH_MASK)
                {
                    $fieldMask = ($bit == $FM_ACL_MATCH_L4_SRC_PORT_WITH_MASK ? 
                                  "L4SrcMask" : "L4DstMask");
                }
                else
                {
                    $fieldMask = defined $field ? $field . "Mask" : undef;
                }

                my $hasMask = (defined $max and
                               $struct->{$fieldMask} ne $max);

                my $val;
                my $txt = "";

                my $doFormat = sub {
                    if (defined $fmt)
                    {
                        $txt .= sprintf($fmt, $val);
                    }
                    else
                    {
                        # special case for 64-bit integers
                        # $val contains integer as decimal string;
                        # we want to reformat as hex string
                        my $big = Math::BigInt->new($val);
                        my $hex = $big->as_hex();
                        # pad to always be 16 hex digits
                        # (string length 18 because of "0x")
                        while (length($hex) < 18)
                        {
                            $hex =~ s/0x/0x0/;
                        }
                        $txt .= $hex;
                    }
                    
                    if (defined $translation)
                    {
                        if ($name =~ /flags$/)
                        {
                            my $s = "";
                            my $flag;
                            
                            foreach $flag (sort keys %$translation)
                            {
                                my $x = $translation->{$flag};
                                if (($val & $x) != 0)
                                {
                                    $s .= "|" if ($s ne "");
                                    $s .= $flag;
                                }
                            }
                            
                            $txt .= " " unless ($hasMask);
                            $txt .= "($s)" unless ($s eq "");
                        }
                        elsif (not $hasMask)
                        {
                            my $s = $translation->{$val};
                            $txt .= " ($s)" if (defined $s);
                        }
                    }
                };

                $val = defined $field ? $struct->{$field} : undef;

                if (defined $field and ref $val eq "SDK::fm_ipAddr")
                {
                    my $tpip = Types::tp_ipAddr->new();
                    my $status = $self->GetIpAddr($val, $tpip);
                    return $status unless ($status == $FM_OK);
                    my $ipStr;
                    $status = $self->InetNToP($tpip, \$ipStr);
                    return $status unless ($status == $FM_OK);
                    $txt = $ipStr;

                    my $mask = $struct->{$fieldMask};
                    my $prefix;
                    my $maxPrefix;
                    if ($mask->{'isIPv6'})
                    {
                        $maxPrefix = 128;
                        $prefix = $countLeadingIPv6->(@{$mask->{'addr'}});
                    }
                    else
                    {
                        $maxPrefix = 32;
                        $prefix = $countLeading->($mask->{'addr'}->[0]);
                    }
                    $txt .= "/$prefix" unless ($prefix == $maxPrefix);
                }
                elsif (defined $field and defined $chunkDef) # $val is an array reference
                {
                    #Display chunks
                    my $mask = $struct->{$fieldMask};
                    my %valueChunks = ArrayToChunk($chunkDef, $val);
                    my %maskChunks = ArrayToChunk($chunkDef, $mask);
                    
                    $txt .= "\n";

                    foreach (sort keys %valueChunks)
                    {
                        my $valueWithMask = $valueChunks{$_} . "/" . $maskChunks{$_};
                        $txt .= sprintf("%s %-18s %s\n", "          ", 
                                        "Chunk #" . $_, $valueWithMask);
                    }

                    #remove last "newline"
                    $txt =~ s/\n$//;
                }
                elsif (defined $field) # $val is a scalar of some sort
                {
                    $doFormat->();
                    if ($hasMask)
                    {
                        $txt .= "/";
                        $val = $struct->{$fieldMask};
                        $doFormat->();
                    }
                }

                if (defined $field2)
                {
                    $txt .= "/";
                    $val = $struct->{$field2};
                    $doFormat->();
                }

                printf "%s %-18s %s\n", $start, $name, $txt;
                $start = $continuation;
            }

        }   # end foreach $name (sort keys %$table)

        if ($start ne $continuation)
        {
            # This means nothing was printed, so we print a placeholder
            # rather than nothing
            printf "%s %s\n", $start, $empty;
        }

        return $FM_OK;

    };  # end $printHalfRule

    my $printRule = sub {
        my $rule = shift;
        my $status;
        $status = $printHalfRule->(sprintf("%10d", $rule), "        &&",
                                   $cond, $value, \%conditions,
                                   "(matches any)");
        return $status unless ($status == $FM_OK);
        $status = $printHalfRule->("        ->", "         +",
                                   $action, $param, \%actions, "(no action)");
        return $status;

    };  # end $printRule

    my $printArgs = sub {
        my $switch = shift;
        my $acl = shift;

        # Get the ACL arguments.
        my $args = SDK::fm_aclArguments->new();

        $api->disableErrors();
        my $status = $api->fmGetACL($switch, $acl, $args);
        $api->enableErrors();

        return $status unless ($status == $FM_OK);

        # Display scenario and precedence parameters.
        my $scenarios = $args->{'scenarios'};
        my $precedence = $args->{'precedence'};

        my $layout = "%10s %-18s %s\n";

        printf($layout, 
               "#",
               "frametypes",
               ScenarioToString(\@frameTypes, $scenarios));

        printf($layout,
               "#",
               "routingtypes",
               ScenarioToString(\@routingTypes, $scenarios));

        printf($layout,
               "#",
               "precedence",
               $precedence);

        return $FM_OK;

    };  # end $printArgs

    my $printPorts = sub {
        my $switch = shift;
        my $acl = shift;

        # Get the port associations.
        my $portAndType = SDK::fm_aclPortAndType->new();
        my @ingressPorts = ();
        my @egressPorts = ();

        $api->disableErrors();
        my $status = $api->fmGetACLPortFirst($switch, $acl, $portAndType);
        while ($status == $FM_OK)
        {
            my $logicalPort = $portAndType->{'port'};
            my $globalPort = $self->tpPlatformMapLogicalToFaceplatePort($switch,
                                                            $logicalPort);
            
            if ($portAndType->{'type'} == $FM_ACL_TYPE_INGRESS)
            {
                push @ingressPorts, $globalPort;
            }
            else
            {
                push @egressPorts, $globalPort;
            }
            
            $status = $api->fmGetACLPortNext($switch, $acl, $portAndType);
        }
        $api->enableErrors();

        return $status unless ($status == $FM_ERR_NO_MORE);

        # Display ingress and egress port associations.
        my $layout = "%10s %-18s %s\n";

        my $portString = $self->StringifyList(@ingressPorts);

        if ($portString ne "")
        {
            printf($layout,
                   "#",
                   "ingressPorts",
                   $portString);
        }

        $portString = $self->StringifyList(@egressPorts);

        if ($portString ne "")
        {
            printf($layout,
                   "#",
                   "egressPorts",
                   $portString);
        }

        return $FM_OK;

    };  # end $printPorts

    my $printRuleIngressPorts = sub {
        my $switch = shift;
        my $acl = shift;
        my $rule = shift;

        my @ingressPorts = ();
        my $logicalPort;

        $api->disableErrors();
        my $status = $api->fmGetACLRuleIngressPortFirst($switch, $acl, $rule,
                                                        \$logicalPort);
        while ($status == $FM_OK)
        {
            my $globalPort = $self->tpPlatformMapLogicalToFaceplatePort($switch,
                                                            $logicalPort);

            push @ingressPorts, $globalPort;

            $status = $api->fmGetACLRuleIngressPortNext($switch, $acl, $rule,
                                                  $logicalPort, \$logicalPort);
        }
        $api->enableErrors();

        return $status unless ($status == $FM_ERR_NO_ACL_RULE_INGRESS_PORT);

        # Display ingress port associations.
        my $layout = "%10s %-18s %s\n";

        my $portString = $self->StringifyList(@ingressPorts);

        if ($portString ne "")
        {
            printf($layout,
                   "",
                   "rule ingress ports",
                   $portString);
        }

        return $FM_OK;

    };  # end $printPorts

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $rule;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        print("\n");
        printf("Switch %d:\n", $switchNum) if ($switchCount > 1);

        my @aclList = $self->GetAclList($switchNum, @acls);

        foreach my $acl (@aclList)
        {
            print "    ACL #$acl:\n";

            if ($rules[0] eq "all")
            {
                # Show all rules.
                my $getFirst = sub {
                    $api->disableErrors();
                    my $status = $api->fmGetACLRuleFirstExt($switchNum, $acl, 
                                                            \$rule,
                                                            \$cond, $value,
                                                            \$action, $param);
                    $api->enableErrors();
                    return $status;
                };

                my $getNext = sub {
                    $api->disableErrors();
                    my $status = $api->fmGetACLRuleNextExt($switchNum, $acl,
                                                           $rule, \$rule,
                                                           \$cond, $value,
                                                           \$action, $param);
                    $api->enableErrors();
                    return $status;
                };
    
                for (my $get = $getFirst ; $get->() == $FM_OK ; $get = $getNext)
                {
                    my $status = $printRule->($rule);
                    if ($status != $FM_OK)
                    {
                        print("Unable to display Rule $rule in ACL $acl.\n");
                    }

                    $status = $printRuleIngressPorts->($switchNum, $acl, $rule);
                    if ($status != $FM_OK)
                    {
                        print("Unable to display Rule $rule in ACL $acl ingress ports.\n");
                    }
                }
    
                my $status = $printArgs->($switchNum, $acl);
                if ($status != $FM_OK)
                {
                    print("Unable to display ACL $acl arguments.\n");
                }
                
                $status = $printPorts->($switchNum, $acl);
                if ($status != $FM_OK)
                {
                    print("Unable to display ACL $acl ports.\n");
                }
            }
            else
            {
                # Show specified rules.
                foreach $rule (@rules)
                {
                    my $status = $api->fmGetACLRule($switchNum, $acl, $rule,
                                                    \$cond, $value, \$action, $param);
                    if ($status != $FM_OK)
                    {
                        print("Unable to display Rule $rule in ACL $acl.\n");
                    }
        
                    $status = $printRule->($rule);
                    if ($status != $FM_OK)
                    {
                        print("Unable to display Rule $rule in ACL $acl.\n");
                    }

                    $status = $printRuleIngressPorts->($switchNum, $acl, $rule);
                    if ($status != $FM_OK)
                    {
                        print("Unable to display Rule $rule in ACL $acl ingress ports.\n");
                    }
                }
            }

        }   # end foreach my $acl (@aclList)
        
    }   # end foreach my $switchNum ($self->tpGetSwitches)

    return $FM_OK;

}   # end tpHandleShowAcl


##@cmethod private int tpHandleCreateAcl(const char *acls)
#
# @desc         Creates one or more ACLs.
#
# @param[in]    acls is the list of ACL numbers to create.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleCreateAcl
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs == 2)
    {
        print "Incorrect number of arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs > 6)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    # Get <frameType>, <routingType>, and <tunnelType> parameters.
    my $scenarios = 
        $FM_ACL_SCENARIO_ANY_FRAME_TYPE | $FM_ACL_SCENARIO_ANY_ROUTING_TYPE | $FM_ACL_SCENARIO_ANY_TUNNEL_TYPE;
    my $precedence = $FM_ACL_DEFAULT_PRECEDENCE;

    if ($nargs >= 3)
    {
        my $frameType   = shift @_;
        my $routingType = shift @_;
        my $tunnelType  = "";

        if ($nargs >= 4)
        {
            my $nextArg = shift @_;

            if ($nextArg eq 'tunnel')
            {
                if ($nargs < 5)
                {
                    print "Must specify a tunnel type!\n";
                    return $FM_FAIL;
                }

                $tunnelType  = shift @_;

                if ($nargs >= 6)
                {
                    $precedence = shift @_;
                }
            }
            else
            {
                $precedence = $nextArg;
            }
        }

        $scenarios = ParseScenarioValues($frameType, $routingType, $tunnelType);
        return $FM_FAIL if !defined($scenarios);
    }

    # Parse <precedence> parameter.
    if ($precedence !~ /^\d+$/ && $precedence !~ /^0x[\da-f]+$/i)
    {
        print "'$precedence' is not a legal number.\n";
        return $FM_FAIL;
    }
    if ($precedence < 0 || $precedence > $FM_MAX_ACL_PRECEDENCE)
    {
        print "'$precedence' is not a valid precedence level.\n";
        return $FM_FAIL;
    }

    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status;

        foreach my $acl (@acls)
        {
            $api->disableErrors();
            $status = $api->fmCreateACLExt($switchNum, $acl, 
                                           $scenarios, $precedence);
            $api->enableErrors();
            
            if ($status == $FM_OK)
            {
                print "Created ACL $acl on switch $switchNum\n";
            }
            else
            {
                print "Can't create ACL $acl on switch $switchNum: ", 
                                            $api->fmErrorMsg($status), "\n";
                next SWITCH;
            }

        }   # end foreach my $acl (@acls)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

}   # end tpHandleCreateAcl


##@cmethod private int tpHandleDelAcl(const char *acls)
#
# @desc         Deletes one or more ACLs.
#
# @param[in]    acls is the list of ACL numbers to delete.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleDelAcl
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs < 1)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs > 1)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    my $rtnStatus = $FM_OK;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @aclList = $self->GetAclList($switchNum, @acls);

        foreach my $acl (@aclList)
        {
            my $status = $api->fmDeleteACL($switchNum, $acl);
    
            if ($status != $FM_OK)
            {
                print "Failed to delete ACL $acl on switch $switchNum\n";
                $rtnStatus = $status;
            }
        }
    }

    return $rtnStatus;

}   # end tpHandleDelAcl


##@cmethod private int tpHandleCreateAclRule(const char *acls, const char *rules)
#
# @desc         Creates one or more ACL rules.
#
# @param[in]    acls is a list of ACL numbers.
#
# @param[in]    rules is a list of rule numbers.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleCreateAclRule
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs < 1)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    # Get <rules> parameter.
    my @rules = $self->ParseRuleList(shift @_);
    if (!@rules)
    {
        print "Must specify a valid rule number or range!\n";
        return $FM_FAIL;
    }

    my $cond = Math::BigInt->new(0);
    my $action = 0;
    my $value = SDK::fm_aclValue->new();
    my $param = SDK::fm_aclParamExt->new();
    my $rtnStatus = $FM_OK;
    my $status;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @aclList = $self->GetAclList($switchNum, @acls);

        foreach my $acl (@aclList)
        {
            foreach my $rule (@rules)
            {
                $api->disableErrors();
                $status = $api->fmAddACLRuleExt($switchNum,
                                                $acl,
                                                $rule,
                                                $cond,
                                                $value,
                                                $action,
                                                $param);
                $api->enableErrors();
                
                if ($status == $FM_OK)
                {
                    print "Created rule $rule in ACL $acl " . 
                          "on switch $switchNum\n";
                }
                else
                {
                    print "Can't create rule $rule in ACL $acl " . 
                          "on switch $switchNum: ", 
                          $api->fmErrorMsg($status), "\n";
                    $rtnStatus = $status;
                }

            }   # end foreach my $rule (@rules)

        }   # end foreach my $acl (@aclList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    return $rtnStatus;

}   # end tpHandleCreateAclRule


##@cmethod private int tpHandleDelAclRule(const char *acls, const char *rules)
#
# @desc         Deletes one or more ACL rules.
#
# @param[in]    acls is a list of ACL numbers.
#
# @param[in]    rules is a list of rule numbers.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleDelAclRule
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    # Get <rules> parameter.
    my @rules = $self->ParseRuleList(shift @_, $ALLOW_ALL);
    if (!@rules)
    {
        print "Must specify a valid rule number or range!\n";
        return $FM_FAIL;
    }

    my $rtnStatus = $FM_OK;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @aclList = $self->GetAclList($switchNum, @acls);

        foreach my $acl (@aclList)
        {
            my @ruleList = $self->GetRuleList($switchNum, $acl, @rules);

            foreach my $rule (@ruleList)
            {
                my $status = $api->fmDeleteACLRule($switchNum, $acl, $rule);
        
                if ($status != $FM_OK)
                {
                    print "Failed to remove rule $rule from ACL $acl " . 
                          "on switch switchNum\n";
                    $rtnStatus = $status;
                }

            }   # end foreach my $rule (@ruleList)

        }   # end foreach my $acl (@aclList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    return $rtnStatus;

}   # end tpHandleDelAclRule


##@cmethod private int tpHandleApplyAcl()
#
# @desc         Compile and apply ACLs
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleApplyAcl
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

#   if ($nargs > 1)
#   {
#       print "Too many arguments\n";
#       return $FM_FAIL;
#   }

    my $flagsSpec = ($nargs >= 1) ? shift @_ : "";

    my ($status, $compilerFlags, $quiet) = ParseCompilerFlags($flagsSpec);
    return $FM_FAIL if $status != $FM_OK;
    
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $statusTextLength = 4096;
        my $statusText = ' ' x $statusTextLength;

        my $oldAutoFlush = $|;
        $| = 1;

        if (!$quiet)
        {
            print("\nCompiling");
            print(" for switch $switchNum") if ($switchCount > 1);
            print("...\n");
        }

        if (($compilerFlags & $FM_ACL_COMPILE_FLAG_TRY_ALLOC) == 0)
        {
            $status = $api->fmCompileACL($switchNum, \$statusText,
                                         $statusTextLength,
                                         $compilerFlags);
        }
        else
        {
            my $tryAllocRange = ($nargs > 1) ? shift @_ : "0:23";

            my ($startRange, $stopRange) = split(':', $tryAllocRange);

            my $tryAlloc = new SDK::fm_aclCompilerTryAlloc();
            $tryAlloc->{'aclFirstSlice'} = $startRange;
            $tryAlloc->{'aclLastSlice'} = $stopRange;

            my %void = (type => "fm_aclCompilerTryAlloc", value => $tryAlloc);

            $status = $api->fmCompileACLExt($switchNum, \$statusText,
                                            $statusTextLength,
                                            $compilerFlags,
                                            \%void);
        }

        # Remove the terminating NUL and any garbage after it
        if ( ($status != $FM_OK) || !$quiet)
        {
            $statusText =~ s/\0.*$//s;
            print $statusText if ($statusText =~ /\S/);
        }

        if (($status == $FM_OK) &&
            ($compilerFlags & $FM_ACL_COMPILE_FLAG_TRY_ALLOC) == 0)
        {
            if (!$quiet)
            {
                print("Applying to switch");
                print(" $switchNum") if ($switchCount > 1);
                print("...\n");
            }

            if ($compilerFlags & $FM_ACL_COMPILE_FLAG_NON_DISRUPTIVE)
            {
                $status = $api->fmApplyACL($switchNum,
                                           $FM_ACL_APPLY_FLAG_NON_DISRUPTIVE);
            }
            else
            {
                $status = $api->fmApplyACL($switchNum, 0);
            }
        }

        $| = $oldAutoFlush;
    }

    if (!$quiet)
    {
        print "Done!\n";
    }

    return $FM_OK;

}   # end tpHandleApplyAcl


##@cmethod private int tpHandleAddAclRule(const char *which, const char *acls, const char *rules, const char *what, const char *parameter)
#
# @desc         Adds a condition or action to an ACL rule.
#
# @param[in]    which is one of: "condition", "action"
#
# @param[in]    acls is a list of ACL numbers.
#
# @param[in]    rules is a list of rule numbers.
#
# @param[in]    what is one of: "dip", "dmac", "ethtype", "flags", "flow",
#               "l4-deep-inspection", "l4-deep-inspection2", "l4-dst-port", "l4-src-port", "port-set",
#               "priority", "protocol", "sip", "smac", "tcp-flags", "traffic",
#               "ttl", "vlan", "count", "count-notify", "deny", "dscp", "log",
#               "mirror", "non-ip-payload", "permit", "police", "switch-priority", "trap",
#               "user", "vlan", "vlan-priority", "redirect", "noroute", "vn-tunnel"
#
# @param[in]    Chunk ID, applies to: non-ip-payload
#
# @param[in]    parameter is one of:
#               IP address and optional mask (e. g. 2001:db8::/32),
#               MAC address and optional mask (e. g. 00:15:ed:00:00:00/ff:ff:ff:00:00:00),
#               EtherType and optional mask (e. g. 0x0800/0xff00),
#               flags value and optional mask (e. g. 1/3),
#               Flow Label and optional mask (e. g. 0x12340/0xffff0),
#               deep inspection value and optional mask (e. g. 0xabcdef1234567890/0xfffffffffffffff0),
#               deep inspection chunk ID, value and optional mask (e. g. 2 0xabcdef1234567890/0xffffffffffffffff),
#               TCP/UDP port number (e. g. 8080),
#               VLAN priority and optional mask (e. g. 2/3),
#               protocol value and optional mask (e. g. 59/128),
#               TCP flags and optional mask (e. g. 5/15),
#               DSCP value and optional mask (e. g. 0x3/0xf),
#               Time to Live value and optional mask (e. g. 0/0xf0),
#               VLAN ID and optional mask (e. g. 0x120/0xff0),
#               New DSCP (e. g. 0x0),
#               Logical port to mirror to (e. g. 12),
#               Non IP Payload chunk ID, value and optional mask (e. g. 2 0xabcdef1234567890/0xffffffffffffffff),
#               policer number (e. g. 123),
#               new switch priority (e. g. 13),
#               New user bits and optional mask (e. g. 0b10110000/0b11110000),
#               new VLAN ID (e. g. 4094),
#               new VLAN priority (e. g. 7)
#               Logical port to redirect to (e. g. 12)
#               port set (e.g. 0)
#               virtual-network tunnel ID (eg. 1 to 4094)
# 
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleAddAclRule
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs > 6)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs < 4)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <which> parameter.
    my $which = shift @_;

    if ($which ne "condition" and $which ne "action")
    {
        print "'$which' is not 'condition' or 'action'\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    # Get <rules> parameter.
    my @rules = $self->ParseRuleList(shift @_, $ALLOW_ALL);
    if (!@rules)
    {
        print "Must specify a valid rule number or range!\n";
        return $FM_FAIL;
    }

    # Get <what> parameter.
    my $what = shift @_;
    
    my $table;

    if ($which eq "condition")
    {
        $table = \%conditions;
    }
    else
    {
        $table = \%actions;
    }

    if (not exists $table->{$what})
    {
        print "'$what' is not a legal $which.  It should be one of: ",
              join(", ", sort keys %$table), ".\n";
        return $FM_FAIL;
    }

    my ($bit, $field, $fmt, $max, $translation, $chunkDef) = @{$table->{$what}};
    my $shouldHaveParameter = (defined $field);
    my $shouldhaveChunks = (defined $chunkDef);
    
    my $chunkID;
    my $field2;

    # Get <chunks> parameter.
    if ($shouldhaveChunks)
    {
        $chunkID = shift @_;
    }

    # Get <parameter> parameter.
    my $parameter = shift @_;
    
    my $hasParameter = (defined $parameter);
    my $hasChunks = (defined $chunkID);

    if ($shouldhaveChunks and (not $hasChunks))
    {
        print "'$what' requires a chunk ID and a parameter\n";
        return $FM_FAIL;
    }
    elsif ((not $shouldhaveChunks) and ($nargs > 5))
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }


    if ($bit == $FM_ACL_ACTIONEXT_SET_TUNNEL_ID_SGLORT)
    {
        # Uses the chunkID variable to store the Source Glort
        my $temp;
        $temp = $parameter;
        $parameter = $chunkID;
        $chunkID = $temp;
        $hasChunks = undef;
        $field2 = $translation;
        $translation = undef;
    }
    elsif ($hasChunks and ($chunkID >= $chunkDef->{'nbChunks'}))
    {
        print "chunk ID \"$chunkID\" is not in range [0-" . 
               $chunkDef->{'nbChunks'} - 1 . "]\n";
        return $FM_FAIL;
    }

    
    if ($shouldHaveParameter and not $hasParameter)
    {
        print "'$what' requires a parameter\n";
        return $FM_FAIL;
    }

    if ($hasParameter and not $shouldHaveParameter)
    {
        print "'$what' does not take a parameter\n";
        return $FM_FAIL;
    }

    my ($value, $mask) = (undef, undef);
    if ($hasParameter)
    {
        my $isIP = (not defined $fmt and not defined $max);
        my $shouldHaveMask = (defined $max or $isIP);

        if ($parameter =~ m%^([^/]+)/([^/]+)$%)
        {
            ($value, $mask) = ($1, $2);
            if (not $shouldHaveMask)
            {
                print "'$what' does not take a mask\n";
                return $FM_FAIL;
            }
        }
        else
        {
            $value = $parameter;
            $mask = $max;
        }

        if (defined $max and $max =~ /:/) # MAC address
        {
            my $addr;
            foreach $addr ($value, $mask)
            {
                if ($self->validateL2Address($addr) != $FM_OK)
                {
                    print "'$addr' is not a legal MAC address\n";
                    return $FM_FAIL;
                }
            }
        }
        elsif ($isIP) # IP address
        {
            my $ipAddr = Types::tp_ipAddr->new();
            my $status = $self->InetPToN($value, $ipAddr);
            if ($status != $FM_OK)
            {
                print "'$value' is not a legal IP address\n";
                return $status;
            }
            $value = SDK::fm_ipAddr->new();
            $status = $self->SetIpAddr($ipAddr, $value);
            if ($status != $FM_OK)
            {
                print "Trouble converting IP address\n";
                return $status;
            }
            if (defined $mask and $mask !~ /^\d+$/)
            {
                print "'$mask' is not a legal number\n";
                return $FM_OK;
            }
            my $maskBits;
            if (not defined $mask)
            {
                $maskBits = 128;
            }
            elsif ($value->{'isIPv6'})
            {
                $maskBits = $mask;
            }
            else
            {
                $maskBits = $mask + 96;
            }

            if ($maskBits > 128)
            {
                print "'$mask' is too big\n";
                return $FM_OK;
            }

            $mask = SDK::fm_ipAddr->new();
            $mask->{'isIPv6'} = $value->{'isIPv6'};
            
            my $bigMask = Math::BigInt->bone();
            $bigMask->blsft(Math::BigInt->new(128 - $maskBits));
            $bigMask->bsub(Math::BigInt->bone());
            $bigMask->bxor(Math::BigInt->new("0xffffffffffffffffffffffffffffffff"));

            my @addrArray = (0) x 4;

            for (my $i = 0 ; $i < 4 ; $i++)
            {
                my $x = Math::BigInt->new("0xffffffff");
                $x->band($bigMask);
                $addrArray[$i] = Applications::TestPoint::Common::RoutingCore::htonl($x->numify());
                $bigMask->brsft(Math::BigInt->new("32"));
            }

            $mask->{'addr'} = \@addrArray;
        }
        elsif (($which eq "condition") &&
               ($bit == $FM_ACL_MATCH_INGRESS_PORT_MASK))
        {
            my @portList = $self->validateList($value, $self->tpPlatformGetPortRange());
            my $mask = 0;

            if (!( @portList))
            {
                printf("Ingress port condition requires a valid port list\n"); 
            }

            foreach (@portList)
            {
                $mask |= (1 << $_);
            }

            $value = $mask;
        }
        else # neither a MAC address nor an IP address
        {
            my $bigMax = (defined($max) ? Math::BigInt->new($max) : undef);
            my $isFlags = (defined $translation and $what =~ /flags$/);
            my $error = undef;
            my $parseNum = sub {
                my $arg = $_[0];
                my $big = Math::BigInt->new($arg);

                if ($big->is_nan() and defined $translation)
                {
                    my %lcTrans;

                    if ($isFlags)
                    {
                        %lcTrans = map(lc, %$translation);
                    }
                    else
                    {
                        %lcTrans = reverse map(lc, %$translation);
                    }

                    my $bad = $FALSE;
                    my $n = 0;
                    my @flags = split(/\|/, $arg);
                    my $flag;
                    foreach $flag (@flags)
                    {
                        if (exists $lcTrans{lc($flag)})
                        {
                            $n |= $lcTrans{lc($flag)};
                        }
                        else
                        {
                            $bad = $TRUE;
                        }
                    }

                    $big = Math::BigInt->new($n) if (not $bad);
                }

                if ($big->is_nan())
                {
                    $error = "'$arg' is not a legal number.  Must be a " .
                        "decimal integer, a hex integer prefixed with '0x', ";
                    $error .= "or " if (not defined $translation);
                    $error .= "a binary integer prefixed with '0b'";
                    if (defined $translation)
                    {
                        $error .= ", or ";
                        $error .= ($isFlags ?
                                   "an '|'-separated list of: " :
                                   "one of the following: ");
                        my @possibilities = ($isFlags ?
                                             keys(%$translation) :
                                             values(%$translation));
                        $error .= join(", ", sort { lc($a) cmp lc($b) }
                                       @possibilities);
                    }
                    $error .= ".";
                }
                elsif (defined $bigMax and $big->bcmp($bigMax) > 0)
                {
                    $error = "'$arg' is not a legal value.  Must be less " .
                        "than or equal to ";
                    if (defined $fmt)
                    {
                        $error .= sprintf($fmt, $max);
                    }
                    else
                    {
                        $error .= $bigMax->as_hex();
                    }
                    $error .= ".";
                }

                return (defined($fmt) ? $big->bstr() : $big);
            };

            $value = $parseNum->($value);
            $mask = $parseNum->($mask) if (defined $mask);
            if (defined $error)
            {
                print $error, "\n";
                return $FM_FAIL;
            }
        }
    }   # end if ($hasParameter)

    my $cond = Math::BigInt->new(0);
    my $action = 0;
    my $val = SDK::fm_aclValue->new();
    my $param = SDK::fm_aclParamExt->new();
    my $rtnStatus = $FM_OK;
    my $status;
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @aclList = $self->GetAclList($switchNum, @acls);

        foreach my $acl (@aclList)
        {
            my @ruleList = $self->GetRuleList($switchNum, $acl, @rules);

            foreach my $rule (@ruleList)
            {
                $status = $api->fmGetACLRule($switchNum, $acl, $rule,
                                             \$cond, $val, \$action, $param);
                if ($status != $FM_OK)
                {
                    $rtnStatus = $status;
                    next;
                }
                
                my ($bitmask, $structure);
                if ($which eq "condition")
                {
                    $bitmask = \$cond;
                    $structure = $val;
                }
                else
                {
                    $bitmask = \$action;
                    $structure = $param;
                }
                
                $$bitmask |= $bit;
                if ($hasParameter)
                {
                    if (!$hasChunks)
                    {
                        if ($bit == $FM_ACL_ACTIONEXT_SET_TUNNEL_ID_SGLORT)
                        {
                            $structure->{$field} = $value;
                            $chunkID = hex($chunkID) if ($chunkID =~ /\D/);
                            $structure->{$field2} = $chunkID;
                        }
                        elsif ($bit == $FM_ACL_MATCH_L4_SRC_PORT_WITH_MASK ||
                               $bit == $FM_ACL_MATCH_L4_DST_PORT_WITH_MASK)
                        {
                            my $fieldMask = ($bit == $FM_ACL_MATCH_L4_SRC_PORT_WITH_MASK ? 
                                             "L4SrcMask" : "L4DstMask");
                            $structure->{$field} = $value;
                            $structure->{$fieldMask} = $mask if (defined $mask);
                        }
                        else
                        {
                            $structure->{$field} = $value;
                            $structure->{$field . "Mask"} = $mask if (defined $mask);
                        }
                        
                    }
                    else
                    {
                        my @valueArray = ChunkToArray($chunkDef, $chunkID, $value);
                        my @maskArray = ChunkToArray($chunkDef, $chunkID, $mask);
                        my @mergedValueArray = MergeArrays($chunkDef, 
                                                      $chunkID, 
                                                      $structure->{$field},
                                                      \@valueArray);
                        my @mergedMaskArray = MergeArrays($chunkDef, 
                                                      $chunkID, 
                                                      $structure->{$field . "Mask"},
                                                      \@maskArray);


                        $structure->{$field} = \@mergedValueArray;
                        $structure->{$field . "Mask"} = \@mergedMaskArray;
                    }
                }
                
                $status = $api->fmDeleteACLRule($switchNum, $acl, $rule);
                if ($status != $FM_OK)
                {
                    $rtnStatus = $status;
                    next;
                }
                
                $status = $api->fmAddACLRuleExt($switchNum, $acl, $rule,
                                                $cond, $val, $action, $param);
                if ($status != $FM_OK)
                {
                    $rtnStatus = $status;
                    next;
                }

            }   # end foreach my $rule (@ruleList)

        }   # end foreach my $acl (@aclList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    return $rtnStatus;

}   # end tpHandleAddAclRule


##
# @cmethod private int tpHandleRenumberAclRule(const char* acl, const char* oldRule, const char* newRule)
#
# @desc         Changes the rule number of an ACL rule.
# 
# @param[in]    acl is ACL number.
# 
# @param[in]    oldRule is the old rule number.
# 
# @param[in]    newRule is the new rule number.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleRenumberAclRule
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs > 3)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs < 3)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    my ($acl, $oldRule, $newRule) = @_;

    my $number;
    foreach $number ($acl, $oldRule, $newRule)
    {
        if ($number !~ /^\d+$/ and $number !~ /^0x[\da-f]+$/i)
        {
            print "'$number' is not a legal number.\n";
            return $FM_FAIL;
        }
    }

    my $rtnStatus = $FM_OK;
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $api->fmRenumberACLRule($switchNum, $acl, $oldRule, 
                                          $newRule);

        if ($status != $FM_OK)
        {
            print "Failed to renumber ACL $acl Rule $oldRule to $newRule ".
                "on switch $switchNum\n";
            $rtnStatus = $status;
        }
    }

    return $rtnStatus;
    
}   # end tpHandleRenumberAclRule


##@cmethod private int tpHandleResetAclRule(const char *which, const char *acls, const char *rules)
#
# @desc         Removes all conditions or actions from one or more ACL rules.
#
# @param[in]    which is one of: "condition", "action"
#
# @param[in]    acls is a list of ACL numbers or "all".
#
# @param[in]    rules is a list of rule numbers or "all".
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleResetAclRule
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs > 3)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs < 3)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <which> parameter.
    my $which = shift @_;

    if ($which ne "condition" and $which ne "action")
    {
        print "'$which' is not 'condition' or 'action'\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    # Get <rules> parameter.
    my @rules = $self->ParseRuleList(shift @_, $ALLOW_ALL);
    if (!@rules)
    {
        print "Must specify a valid rule number or range!\n";
        return $FM_FAIL;
    }

    my $rtnStatus = $FM_OK;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $cond = Math::BigInt->new(0);
        my $action = 0;
        my $val = SDK::fm_aclValue->new();
        my $param = SDK::fm_aclParamExt->new();

        my @aclList = $self->GetAclList($switchNum, @acls);

        foreach my $acl (@aclList)
        {
            my @ruleList = $self->GetRuleList($switchNum, $acl, @rules);

            foreach my $rule (@ruleList)
            {
                my $status = $api->fmGetACLRule($switchNum, $acl, $rule,
                                             \$cond, $val, \$action, $param);
                if ($status != $FM_OK)
                {
                    print("Can't reset rule $rule in ACL $acl on switch $switchNum.\n");
                    $rtnStatus = $status;
                    next;
                }
                
                if ($which eq "condition")
                {
                    $cond = 0;
                }
                else
                {
                    $action = 0;
                }
        
                $status = $api->fmDeleteACLRule($switchNum, $acl, $rule);
        
                if ($status != $FM_OK)
                {
                    print("Can't reset rule $rule in ACL $acl on switch $switchNum.\n");
                    $rtnStatus = $status;
                    next;
                }
                
                $status = $api->fmAddACLRuleExt($switchNum, $acl, $rule,
                                                $cond, $val, $action, $param);

            }   # end foreach my $rule (@ruleList)

        }   # end foreach my $acl (@aclList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    return $rtnStatus;

}   # end tpHandleResetAclRule


##@cmethod private int tpHandleAddPortAcl(const char *ports, const char *acl, const char *type)
#
# @desc         Associate ACLs with ports.
#
# @param[in]    ports is a list of logical port numbers.
#
# @param[in]    acls is a list of ACL numbers.
#
# @param[in]    type is one of: "ingress", "egress"
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleAddPortAcl
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs > 3)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs < 3)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <ports> parameter.
    my $ports = shift @_;

    my @globalPortList = $self->validateList(
            $ports,
            $self->tpPlatformGetFaceplatePortRange());

    if (scalar(@globalPortList) == 0)
    {
        print("Invalid port list: $ports\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    # Get <type> parameter.
    my $type = shift @_;

    if ($type ne "ingress" and $type ne "egress")
    {
        print "'$type' is not 'ingress' or 'egress'\n";
        return $FM_FAIL;
    }

    my $aclType = 
        ($type eq "ingress") ? $FM_ACL_TYPE_INGRESS : $FM_ACL_TYPE_EGRESS;

    my @affectedPortList = ();
    my $rtnStatus = $FM_OK;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList(
                $TRUE,
                $ports,
                $self->tpPlatformGetSwitchPortList($switchNum));
        
        next if (scalar(@portList) == 0);

        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may 
            # actually be wrong for some platforms.
            my ($sw, $logicalPort) =
                $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            my @aclList = $self->GetAclList($switchNum, @acls);

            foreach my $acl (@aclList)
            {
                my $status = $api->fmAddACLPort($switchNum,
                                                $acl,
                                                $logicalPort,
                                                $aclType);
    
                if ($status != $FM_OK)
                {
                    print "Failed to add ACL $acl to port $globalPort " .
                          "on switch $switchNum\n";
                    $rtnStatus = $status;
                }

            }   # end foreach my $acl (@aclList)

        }   # end foreach my $globalPort (@portList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $rtnStatus;

}   # end tpHandleAddPortAcl


##@cmethod private int tpHandleDelPortAcl(const char *port, const char *acl)
#
# @desc         Dissociate ACLs from ports.
#
# @param[in]    ports is a list of port numbers.
#
# @param[in]    acls is a list of ACL numbers.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleDelPortAcl
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <ports> parameter.
    my $ports = shift @_;

    my @globalPortList = $self->validateList(
            $ports,
            $self->tpPlatformGetFaceplatePortRange());

    if (scalar(@globalPortList) == 0)
    {
        print("Invalid port list: $ports\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    my $portAndType = SDK::fm_aclPortAndType->new();
    my @affectedPortList = ();

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList(
                $TRUE,
                $ports,
                $self->tpPlatformGetSwitchPortList($switchNum));

        next if (scalar(@portList) == 0);

        push (@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

            my @aclList = $self->GetAclList($switchNum, @acls);

            foreach my $acl (@aclList)
            {
                foreach my $type ($FM_ACL_TYPE_INGRESS, $FM_ACL_TYPE_EGRESS)
                {
                    $portAndType->{'port'} = $logPort; # = $port;
                    $portAndType->{'type'} = $type;

                    $api->fmDeleteACLPortExt($switchNum, $acl, $portAndType);
                }

            }   # end foreach my $acl (@aclList)

        }   # end foreach my $port (@portList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}   # end tpHandleDelPortAcl


##@cmethod private int tpHandleShowPortAcl(const char *ports, const char *type)
#
# @desc         Shows ACLs associated with ports.
#
# @param[in]    ports is a list of port numbers.
#
# @param[in]    type is one of: "ingress", "egress"
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleShowPortAcl
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }
    
    my ($ports, $type) = @_;

    if ($type ne "ingress" and $type ne "egress")
    {
        print "'$type' is not 'ingress' or 'egress'\n";
        return $FM_FAIL;
    }

    my @globalPortList = $self->validateList(
            $ports,
            $self->tpPlatformGetFaceplatePortRange());

    if (!scalar(@globalPortList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $aclAndType = SDK::fm_aclAclAndType->new();
    
    my $getFirst = sub {
        my $switch = shift;
        my $port = shift;

        $api->disableErrors();
        my $status = $api->fmGetPortACLFirstExt($switch, $port,
                                                $aclAndType);
        $api->enableErrors();
        return $status;
    };

    my $getNext = sub {
        my $switch = shift;
        my $port = shift;

        $api->disableErrors();
        my $status = $api->fmGetPortACLNextExt($switch, $port,
                                               $aclAndType);
        $api->enableErrors();
        return $status;
    };
   
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my @affectedPortList = ();

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList(
                $TRUE,
                $ports, 
                $self->tpPlatformGetSwitchPortList($switchNum, $TRUE));
        
        next if (scalar(@portList) == 0);
        
        push(@affectedPortList, @portList);

        print("\n");
        printf("Switch %d:\n", $switchNum) if ($switchCount > 1);
        
        foreach my $port (@portList)
        {
            my @acls = ();
            
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            for (my $get = $getFirst ; 
                 $get->($switchNum, $logPort) == $FM_OK ; 
                 $get = $getNext)
            {
                if ($type eq "ingress" &&
                    $aclAndType->{'type'} == $FM_ACL_TYPE_INGRESS or
                    $type eq "egress" &&
                    $aclAndType->{'type'} == $FM_ACL_TYPE_EGRESS)
                {
                    push @acls, $aclAndType->{'acl'} ;
                }
            }
    
            my $aclString = $self->StringifyList(@acls);
    
            print("  Port $port: ");
            
            if ($aclString eq "")
            {
                print "No associated ACLs\n";
            }
            else
            {
                print "$aclString \n";
            }
            
        }   # next port
        
    }   # next switch

    print("\n");
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}   # end tpHandleShowPortAcl


##
# tpHandleShowAclCounter
# 
# @desc         Displays one or more ingress ACL counters.
# 
# @param[in]    acls is a list of ACL numbers.
# 
# @param[in]    rules is a list of rule numbers.
# 
sub tpHandleShowAclCounter
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs > 3)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    # Get <rules> parameter.
    my @rules = $self->ParseRuleList(shift @_, $ALLOW_ALL);
    if (!@rules)
    {
        print "Must specify a valid rule number or range!\n";
        return $FM_FAIL;
    }

    my $aclCounters = SDK::fm_aclCounters->new();
    my $aclRuleInfo = SDK::fm_aclRulePolicerInfo->new();
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $info = shift @_;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        print("\n");
        printf("Switch %d:\n", $switchNum) if ($switchCount > 1);

        my @aclList = $self->GetAclList($switchNum, @acls);

        foreach my $acl (@aclList)
        {
            my @ruleList = $self->GetCounterRules($switchNum, $acl, @rules);

            foreach my $rule (@ruleList)
            {
                $api->disableErrors();
                my $status = $api->fmGetACLCountExt($switchNum,
                                                    $acl,
                                                    $rule,
                                                    $aclCounters);

                my $status2 = $FM_OK;
                if (defined($info))
                {
                   my $attr = $FM_ACL_RULE_POLICER_INFO;
                   my %void = (type => "fm_aclRulePolicerInfo", value => $aclRuleInfo);
                   $status2 = $api->fmGetACLRuleAttribute($switchNum,
                                                             $acl,
                                                             $rule,
                                                             $attr,
                                                             \%void);
                }
                $api->enableErrors();
    
                if ($status != $FM_OK || $status2 != $FM_OK)
                {
                    print "Error retrieving counter for rule $rule " . 
                          "of ACL $acl on switch $switchNum.\n";
                    next;
                }

                print "ACL $acl Rule $rule:\n";
    
                printf("    Frames:  %s [%s]\n",
                       $aclCounters->{cntPkts}->as_hex(),
                       $aclCounters->{cntPkts}->bstr());

                printf("    Octets:  %s [%s]\n",
                       $aclCounters->{cntOctets}->as_hex(),
                       $aclCounters->{cntOctets}->bstr());

                if (defined($info))
                {
                   my $value_ref = $aclRuleInfo->{'type'};
                   my @value = @$value_ref;

                   my $index_ref = $aclRuleInfo->{'index'};
                   my @index = @$index_ref;

                   for (my $bank = 0; $bank < scalar(@value); $bank++)
                   {
                       if ($value[$bank] == $FM_ACL_BANK_TYPE_POLICE)
                       {
                           printf("    Policer Type Bank %d with Index %d\n",
                                  $bank, $index[$bank]);
                       }
                       elsif ($value[$bank] == $FM_ACL_BANK_TYPE_COUNT)
                       {
                           printf("    Counter Type Bank %d with Index %d\n",
                                  $bank, $index[$bank]);
                       }
                   }
                }

            }   # end foreach my $rule (@ruleList)

        }   # end foreach my $acl (@aclList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    return $FM_OK;

}   # end tpHandleShowAclCounter


##
# tpHandleShowAclEgressCounter
# 
# @descr        Displays one or more egress ACL counters.
# 
# @param[in]    ports is a list of egress port numbers.
# 
sub tpHandleShowAclEgressCounter
{
    my ($self, $ports) = @_;
    my $api = $self->{'CHIP'};

    my @globalPortList = $self->validateList(
            $ports,
            $self->tpPlatformGetPortRange());

    if (scalar(@globalPortList) == 0)
    {
        print $TP_MSG_ERR_PORT_INVALID_ARRAY;
        return;
    }
    
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $aclCounters = SDK::fm_aclCounters->new();
    my @affectedPortList = ();
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList(
                $TRUE,
                $ports,
                $self->tpPlatformGetSwitchPortList($switchNum));
        
        # If none of the ports appear on this switch, go to the next switch
        next if (scalar(@portList) == 0);
        
        push(@affectedPortList, @portList);
        
        print("\n");
        printf("Switch %d:\n", $switchNum) if ($switchCount > 1);
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            printf("Port $port:\n");

            my $status = $api->fmGetACLEgressCount($switchNum,
                                                   $logPort,
                                                   $aclCounters);

            if ($status != $FM_OK)
            {
                print "  Error $status in fmGetACLEgressCount, port $port.\n";
                next;
            }

            printf("    Frames:  %s [%s]\n",
                   $aclCounters->{cntPkts}->as_hex(),
                   $aclCounters->{cntPkts}->bstr());

            printf("    Octets:  %s [%s]\n",
                   $aclCounters->{cntOctets}->as_hex(),
                   $aclCounters->{cntOctets}->bstr());

        }   # end foreach my $port (@portList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}   # end tpHandleShowAclEgressCounter


##
# Handles the <code>reset acl egress-counter</code> command.
# 
sub tpHandleResetAclEgressCounter
{
    my ($self, $ports) = @_;
    my $api = $self->{'CHIP'};

    my $physSw;
    my $physPort;

    my @globalPortList = $self->validateList(
            $ports,
            $self->tpPlatformGetPortRange());

    if (scalar(@globalPortList) == 0)
    {
        print $TP_MSG_ERR_PORT_INVALID_ARRAY;
        return;
    }

    my @affectedPortList = ();

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList(
                $TRUE,
                $ports,
                $self->tpPlatformGetSwitchPortList($switchNum));
        
        next if (scalar(@portList) == 0);
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may 
            # actually be wrong for some platforms.
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            $api->fmPlatformMapLogicalPortToPhysical($switchNum, $logPort, 
                                                     \$physSw, \$physPort);
    
            my $frames = Math::BigInt->bzero();
            my $octets = Math::BigInt->bzero();
    
            my $status = $api->fm4000FFUSetEgressACLCounter(
                    $physSw,
                    $physPort,
                    $frames,
                    $octets);
    
            if ($status != $FM_OK)
            {
                print "Failed to reset egress counter for port $port " .
                      "on switch $switchNum\n";
            }

        }   # end foreach my $port (@portList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

    return $FM_OK;

}   # end tpHandleResetAclEgressCounter


##
# tpHandleResetAclCounter
# 
# @desc         Resets one or more ingress ACL counters.
# 
# @param[in]    acls is a list of ACL numbers.
# 
# @param[in]    rules is a list of rule numbers.
# 
sub tpHandleResetAclCounter
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }

    # Get <acls> parameter.
    my @acls = $self->ParseAclList(shift @_, $ALLOW_ALL);
    if (!@acls)
    {
        print "Must specify a valid ACL number or range!\n";
        return $FM_FAIL;
    }

    # Get <rules> parameter.
    my @rules = $self->ParseRuleList(shift @_, $ALLOW_ALL);
    if (!@rules)
    {
        print "Must specify a valid rule number or range!\n";
        return $FM_FAIL;
    }

    my $rtnStatus = $FM_OK;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @aclList = $self->GetAclList($switchNum, @acls);

        foreach my $acl (@aclList)
        {
            my @ruleList = $self->GetCounterRules($switchNum, $acl, @rules);

            foreach my $rule (@ruleList)
            {
                $api->disableErrors();
                my $status = $api->fmResetACLCount($switchNum, $acl, $rule);
                $api->enableErrors();
                
                if ($status != $FM_OK)
                {
                    print "Failed to reset counter for rule $rule " . 
                          "in ACL $acl on switch $switchNum\n";
                    $rtnStatus = $status;
                }
            }
        }
    }
   
    return $rtnStatus;

}   # end tpHandleResetAclCounter


##
# tpHandleCreateAclPortSet
#
# @brief        Handles creation of a port-set.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub tpHandleCreateAclPortSet
{
    my ($self) = @_;

    my $chip = $self->{'CHIP'};

    my $portSet = [];
    my $previousPortSet = undef;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $sw ($self->tpGetSwitches)
    {
        my ($status);

        $status = $chip->fmCreateACLPortSet($sw, \$portSet->[$sw]);
        if ($status != $FM_OK
            || (defined($previousPortSet) && $portSet->[$sw] != $previousPortSet))
        {
            goto ABORT;
        }
        $previousPortSet = $portSet->[$sw];
    }

    printf("Created port-set %d\n", $previousPortSet);
    return $FM_OK;

ABORT:
    print("Cannot create port-set!\n");
    foreach my $sw ($self->tpGetSwitches)
    {
        if (defined($portSet->[$sw]))
        {
            $chip->fmDeleteACLPortSet($sw, $portSet->[$sw]);
        }
    }

    return $FM_FAIL;

}   # end tpHandleCreateAclRule


##
# tpHandleDelAclPortSet
#
# @brief        Handles deleting a port-set.
#
# @param[in]    $portSet The port-set to be deleted.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelAclPortSet
{
    my ($self, $portSet) = @_;

    my $chip = $self->{'CHIP'};

    my (@portSetList);

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $switchNum ($self->tpGetSwitches)
    {
            my ($status);

            $status = $chip->fmDeleteACLPortSet($switchNum, $portSet);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("Cannot delete ACL port-set %d!\n", $portSet);
            }
    }

    return $result;
}   # end tpHandleDelAclPortSet

##
# tpHandleAddPortSetPort
#
# @desc         Associate ports to a port-set.
#
# @param[in]    ports is a list of logical port numbers.
#
# @param[in]    port-set is a port-set numbers.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleAddPortSetPort
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <ports> parameter.
    my $ports = shift @_;

    my @globalPortList = $self->validateList(
            $ports,
            $self->tpPlatformGetFaceplatePortRange());

    if (scalar(@globalPortList) == 0)
    {
        print("Invalid port list: $ports\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $portSet = shift @_;

    my @affectedPortList = ();
    my $rtnStatus = $FM_OK;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList(
                $TRUE,
                $ports,
                $self->tpPlatformGetSwitchPortList($switchNum));
        
        next if (scalar(@portList) == 0);

        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may 
            # actually be wrong for some platforms.
            my ($sw, $logicalPort) =
                $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            my $status = $api->fmAddACLPortSetPort($switchNum,
                                                   $portSet,
                                                   $logicalPort);
    
            if ($status != $FM_OK)
            {
                print "Failed to add port-set $portSet to port $globalPort " .
                    "on switch $switchNum\n";
                $rtnStatus = $status;
            }

        }   # end foreach my $globalPort (@portList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $rtnStatus;

}   # end tpHandleAddPortSetPort

##
# tpHandleDelPortSetPort
#
# @desc         Remove ports from port-set.
#
# @param[in]    ports is a list of logical port numbers.
#
# @param[in]    portSet is a port-set numbers.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleDelPortSetPort
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <ports> parameter.
    my $ports = shift @_;

    my @globalPortList = $self->validateList(
            $ports,
            $self->tpPlatformGetFaceplatePortRange());

    if (scalar(@globalPortList) == 0)
    {
        print("Invalid port list: $ports\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $portSet = shift @_;

    my @affectedPortList = ();
    my $rtnStatus = $FM_OK;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList(
                $TRUE,
                $ports,
                $self->tpPlatformGetSwitchPortList($switchNum));
        
        next if (scalar(@portList) == 0);

        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may 
            # actually be wrong for some platforms.
            my ($sw, $logicalPort) =
                $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            my $status = $api->fmDeleteACLPortSetPort($switchNum,
                                                      $portSet,
                                                      $logicalPort);
    
            if ($status != $FM_OK)
            {
                print "Failed to delete port-set $portSet to port $globalPort " .
                    "on switch $switchNum\n";
                $rtnStatus = $status;
            }

        }   # end foreach my $globalPort (@portList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $rtnStatus;

}   # end tpHandleDelPortSetPort


##
# tpHandleShowAclPortSet
# 
# @descr        Displays one or more port-set and associated ports.
# 
# @param[in]    portSets is a list of port-set numbers.
# 
sub tpHandleShowAclPortSet
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    if ($nargs < 1)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }
    elsif ($nargs > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }

     # Get <portSets> parameter.
    my @portSets = $self->ParsePortSetList(shift @_, $ALLOW_ALL);
    if (!@portSets)
    {
        print "Must specify a valid port-set number or range!\n";
        return $FM_FAIL;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portSetList = $self->GetAclPortSetList($switchNum, @portSets);

        foreach my $portSet (@portSetList)
        {
            my $port = 0;
            my $status;

            my @ports = ();

            $api->disableErrors();
            $status = $api->fmGetACLPortSetPortFirst($switchNum,
                                                     $portSet,
                                                     \$port);
            if ($status == $FM_ERR_INVALID_ACL_PORT_SET)
            {
                print "Invalid port-set number $portSet.\n";
            }
            else
            {
                while ($status == $FM_OK)
                {       
                    push @ports, $port;
                    $status = $api->fmGetACLPortSetPortNext($switchNum,
                                                            $portSet,
                                                            $port,
                                                            \$port);
                }
                $api->enableErrors();
                print "Port Set $portSet: @ports\n";
            }

        }   # end foreach my $acl (@aclList)

    }   # end foreach my $switchNum ($self->tpGetSwitches)

    return $FM_OK;

}   # end tpHandleShowAclPortSet

##
# tpHandleSetAclProp
# 
# @descr        Set ACL Property
# 
# @param[in]    acl is the acl Id to work with.
# 
# @param[in]    prop is the acl property to configure.
# 
# @param[in]    arg is the acl property argument.
# 
sub tpHandleSetAclProp
{
    my ($self, $acl, $prop, $arg) = @_;

    my $chip = $self->{'CHIP'};

    my $result = $FM_OK;
    my $attribute;
    my %void;

    if ($prop eq "table")
    {
        $attribute = $FM_ACL_TABLE_SELECTION;
        if ($arg eq "best-fit")
        {
            %void = (type => "fm_int", value => $FM_ACL_TABLE_BEST_FIT);
        }
        elsif ($arg eq "table0")
        {
            %void = (type => "fm_int", value => $FM_ACL_TABLE_0);
        }
        elsif ($arg eq "table1")
        {
            %void = (type => "fm_int", value => $FM_ACL_TABLE_1);
        }
        elsif ($arg eq "bst")
        {
            %void = (type => "fm_int", value => $FM_ACL_TABLE_BST);
        }
        else
        {
            print "Unknown ACL table $arg\n";
            return $FM_FAIL;
        }
    }
    else
    {
        print "Unknown ACL property $prop\n";
        return $FM_FAIL;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $switchNum ($self->tpGetSwitches)
    {
            my ($status);

            $status = $chip->fmSetACLAttribute($switchNum, $acl, $attribute, \%void);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("Cannot Set ACL Attribute %d!\n", $attribute);
            }
    }

    return $result;

}   # end tpHandleSetAclProp

1;
