# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/LoggingCore.pm
# Creation Date:    July 1, 2008.
# Description:      TestPoint commands for logging.
#
# INTEL CONFIDENTIAL
# Copyright 2008 - 2011 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::LoggingCore;

use strict;
use warnings;

use SDKScalars;
use Scripts::Utilities;

##
# Basic logging levels.
# 
# With the exception of api_entry and api_exit, these are all represented
# by a single flag bit.
# 
my %simpleLevel =
(
    'api_entry'         =>  [$FM_LOG_LEVEL_FUNC_ENTRY_API,
                             $FM_LOG_LEVEL_FUNC_ENTRY],
    'api_entry_verbose' =>  $FM_LOG_LEVEL_FUNC_ENTRY_API_VERBOSE,
    'api_exit'          =>  [$FM_LOG_LEVEL_FUNC_EXIT_API,
                             $FM_LOG_LEVEL_FUNC_EXIT],
    'api_exit_verbose'  =>  $FM_LOG_LEVEL_FUNC_EXIT_API_VERBOSE,
    'debug'             =>  $FM_LOG_LEVEL_DEBUG,
    'debug_verbose'     =>  $FM_LOG_LEVEL_DEBUG_VERBOSE,
    'debug2'            =>  $FM_LOG_LEVEL_DEBUG2,
    'debug3'            =>  $FM_LOG_LEVEL_DEBUG3,
    'error'             =>  $FM_LOG_LEVEL_ERROR,
    'fatal'             =>  $FM_LOG_LEVEL_FATAL,
    'func_entry'        =>  $FM_LOG_LEVEL_FUNC_ENTRY,
    'func_entry_verbose' => $FM_LOG_LEVEL_FUNC_ENTRY_VERBOSE,
    'func_exit'         =>  $FM_LOG_LEVEL_FUNC_EXIT,
    'func_exit_verbose' =>  $FM_LOG_LEVEL_FUNC_EXIT_VERBOSE,
    'info'              =>  $FM_LOG_LEVEL_INFO,
    'print'             =>  $FM_LOG_LEVEL_PRINT,
    'warning'           =>  $FM_LOG_LEVEL_WARNING,

);  # %simpleLevel

##
# Combination logging levels.
# 
my %comboLevel =
(
    # Basic entry/exit logging (API only).
    'api'               =>  $FM_LOG_LEVEL_API,

    # Basic entry/exit logging (API and internal functions).
    'func'              =>  $FM_LOG_LEVEL_FUNC_ENTRY        |
                            $FM_LOG_LEVEL_FUNC_EXIT,

    # The verbose levels.
    'verbose'           =>  $FM_LOG_LEVEL_FUNC_ENTRY_VERBOSE        |
                            $FM_LOG_LEVEL_FUNC_EXIT_VERBOSE         |
                            $FM_LOG_LEVEL_FUNC_ENTRY_API_VERBOSE    |
                            $FM_LOG_LEVEL_FUNC_EXIT_API_VERBOSE     |
                            $FM_LOG_LEVEL_DEBUG_VERBOSE,

    # Extended entry/exit logging (API only).
    'verbose_api'       =>  $FM_LOG_LEVEL_API                       |
                            $FM_LOG_LEVEL_FUNC_ENTRY_API_VERBOSE    |
                            $FM_LOG_LEVEL_FUNC_EXIT_API_VERBOSE,

    # Extended entry/exit logging (API and internal functions).
    'verbose_func'      =>  $FM_LOG_LEVEL_FUNC_ENTRY                |
                            $FM_LOG_LEVEL_FUNC_EXIT                 |
                            $FM_LOG_LEVEL_FUNC_ENTRY_API_VERBOSE    |
                            $FM_LOG_LEVEL_FUNC_EXIT_API_VERBOSE     |
                            $FM_LOG_LEVEL_FUNC_ENTRY_VERBOSE        |
                            $FM_LOG_LEVEL_FUNC_EXIT_VERBOSE,

);  # %comboLevel

##
# Legacy logging levels.
# 
# These values provide backward compatibility with earlier versions 
# of TestPoint. They are not displayed by the 'help' keyword.
# 
my %legacyLevel =
(

    'all_api'           =>  $FM_LOG_LEVEL_API,

    'all_func'          =>  $FM_LOG_LEVEL_FUNC_ENTRY_API    |
                            $FM_LOG_LEVEL_FUNC_EXIT_API,

    'func_entry_api'    =>  $FM_LOG_LEVEL_FUNC_ENTRY_API,
    'func_exit_api'     =>  $FM_LOG_LEVEL_FUNC_EXIT_API,

);  # %legacyLevel

##
# Special logging levels.
# 
# These are considered to be whole values, rather than simple flags
# or combinations.
# 
my %specialLevel =
(
    'all'               =>  $FM_LOG_LEVEL_ALL,
    'default'           =>  $FM_LOG_LEVEL_DEFAULT,
    'none'              =>  $FM_LOG_LEVEL_NONE,

);  # %specialLevel

##
# Basic logging categories.
# 
my %simpleCategory = 
(
   'acl'                =>  $FM_LOG_CAT_ACL,
   'addr'               =>  $FM_LOG_CAT_ADDR,
   'addr_offload'       =>  $FM_LOG_CAT_ADDR_OFFLOAD,
   'alos'               =>  $FM_LOG_CAT_ALOS,
   'alos_lock'          =>  $FM_LOG_CAT_ALOS_LOCK,
   'alos_rwlock'        =>  $FM_LOG_CAT_ALOS_RWLOCK,
   'alos_sem'           =>  $FM_LOG_CAT_ALOS_SEM,
   'alos_thread'        =>  $FM_LOG_CAT_ALOS_THREAD,
   'api'                =>  $FM_LOG_CAT_API,
   'attr'               =>  $FM_LOG_CAT_ATTR,
   'buffer'             =>  $FM_LOG_CAT_BUFFER,
   'bst'                =>  $FM_LOG_CAT_BST,
   'debug'              =>  $FM_LOG_CAT_DEBUG,
   'event'              =>  $FM_LOG_CAT_EVENT,
   'event_fast_maint'   =>  $FM_LOG_CAT_EVENT_FAST_MAINT,
   'event_intr'         =>  $FM_LOG_CAT_EVENT_INTR,
   'event_mac_maint'    =>  $FM_LOG_CAT_EVENT_MAC_MAINT,
   'event_pkt_rx'       =>  $FM_LOG_CAT_EVENT_PKT_RX,
   'event_pkt_tx'       =>  $FM_LOG_CAT_EVENT_PKT_TX,
   'event_port'         =>  $FM_LOG_CAT_EVENT_PORT,
   'event_port_fm2000'  =>  $FM_LOG_CAT_EVENT_PORT_FM2000,
   'event_port_fm4000'  =>  $FM_LOG_CAT_EVENT_PORT_FM4000,
   'event_routing'      =>  $FM_LOG_CAT_EVENT_ROUTING,
   'ffu'                =>  $FM_LOG_CAT_FFU,
   'fibm'               =>  $FM_LOG_CAT_FIBM,
   'general'            =>  $FM_LOG_CAT_GENERAL,
   'lag'                =>  $FM_LOG_CAT_LAG,
   'lag_fm2000'         =>  $FM_LOG_CAT_LAG_FM2000,
   'lag_fm4000'         =>  $FM_LOG_CAT_LAG_FM4000,
   'lbg'                =>  $FM_LOG_CAT_LBG,
   'link_state'         =>  $FM_LOG_CAT_LINK_STATE,
   'logging'            =>  $FM_LOG_CAT_LOGGING,
   'mirror'             =>  $FM_LOG_CAT_MIRROR,
   'multicast'          =>  $FM_LOG_CAT_MULTICAST,
   'phy'                =>  $FM_LOG_CAT_PHY,
   'platform'           =>  $FM_LOG_CAT_PLATFORM,
   'port'               =>  $FM_LOG_CAT_PORT,
   'port_autoneg'       =>  $FM_LOG_CAT_PORT_AUTONEG,
   'port_fm2000'        =>  $FM_LOG_CAT_PORT_FM2000,
   'port_fm4000'        =>  $FM_LOG_CAT_PORT_FM4000,
   'qos'                =>  $FM_LOG_CAT_QOS,
   'qos_fm2000'         =>  $FM_LOG_CAT_QOS_FM2000,
   'qos_fm4000'         =>  $FM_LOG_CAT_QOS_FM4000,
   'routing'            =>  $FM_LOG_CAT_ROUTING,
   'sflow'              =>  $FM_LOG_CAT_SFLOW,
   'stacking'           =>  $FM_LOG_CAT_STACKING,
   'storm'              =>  $FM_LOG_CAT_STORM,
   'stp'                =>  $FM_LOG_CAT_STP,
   'swag'               =>  $FM_LOG_CAT_SWAG,
   'switch'             =>  $FM_LOG_CAT_SWITCH,
   'switch_fm2000'      =>  $FM_LOG_CAT_SWITCH_FM2000,
   'switch_fm4000'      =>  $FM_LOG_CAT_SWITCH_FM4000,
   'trigger'            =>  $FM_LOG_CAT_TRIGGER,
   'vlan'               =>  $FM_LOG_CAT_VLAN,
   'vlan_fm2000'        =>  $FM_LOG_CAT_VLAN_FM2000,
   'vlan_fm4000'        =>  $FM_LOG_CAT_VLAN_FM4000,
   'vlan_stp'           =>  $FM_LOG_CAT_VLAN_STP,
   'vlan_stp_fm2000'    =>  $FM_LOG_CAT_VLAN_STP_FM2000,
   'vlan_stp_fm4000'    =>  $FM_LOG_CAT_VLAN_STP_FM4000,
   'vn'                 => $FM_LOG_CAT_VN,

);  # %simpleCategory

##
# Combination logging categories.
# 
my %comboCategory =
(
    'event_intr_all'    =>  $FM_LOG_CAT_EVENT_INTR_ALL,
    'event_port_all'    =>  $FM_LOG_CAT_EVENT_PORT_ALL,
    'lag_all'           =>  $FM_LOG_CAT_LAG_ALL,
    'port_all'          =>  $FM_LOG_CAT_PORT_ALL,
    'qos_all'           =>  $FM_LOG_CAT_QOS_ALL,
    'switch_all'        =>  $FM_LOG_CAT_SWITCH_ALL,
    'vlan_all'          =>  $FM_LOG_CAT_VLAN_ALL,
    'vlan_stp_all'      =>  $FM_LOG_CAT_VLAN_STP_ALL,
   
);  # %comboCategory

##
# Legacy logging categories.
# 
# These values provide backward compatibility with earlier versions 
# of TestPoint. They are not displayed by the 'help' keyword.
# 
my %legacyCategory =
(
    'all_event_intr'    =>  $FM_LOG_CAT_EVENT_INTR_ALL,
    'all_event_port'    =>  $FM_LOG_CAT_EVENT_PORT_ALL,
    'all_lag'           =>  $FM_LOG_CAT_LAG_ALL,
    'all_port'          =>  $FM_LOG_CAT_PORT_ALL,
    'all_qos'           =>  $FM_LOG_CAT_QOS_ALL,
    'all_switch'        =>  $FM_LOG_CAT_SWITCH_ALL,
    'all_vlan'          =>  $FM_LOG_CAT_VLAN_ALL,
    'all_vlan_stp'      =>  $FM_LOG_CAT_VLAN_STP_ALL,
   
);  # %legacyCategory

##
# Special logging categories.
# 
# These are considered to be whole values, rather than simple flags
# or combinations.
# 
my %specialCategory =
(
    'all'               =>  $FM_LOG_CAT_ALL,
    'default'           =>  $FM_LOG_CAT_DEFAULT,
    'none'              =>  $FM_LOG_CAT_NONE,

);  # %specialCategory

##
# Logging verbosity flags.
# 
my %simpleVerbosity =
(
    'date_time' => $FM_LOG_VERBOSITY_DATE_TIME,
    'level'     => $FM_LOG_VERBOSITY_LOG_LEVEL,
    'thread'    => $FM_LOG_VERBOSITY_THREAD,
    'file'      => $FM_LOG_VERBOSITY_FILE,
    'func'      => $FM_LOG_VERBOSITY_FUNC,
    'line'      => $FM_LOG_VERBOSITY_LINE,

);  # %simpleVerbosity

##
# Special logging verbosity values.
# 
my %specialVerbosity =
(
    'all'       =>  $FM_LOG_VERBOSITY_ALL,
    'default'   =>  $FM_LOG_VERBOSITY_DEFAULT,
    'none'      =>  $FM_LOG_VERBOSITY_NONE,

);  # %specialVerbosity


##
# Displays the logging categories.
# 
sub DisplayCategories
{

    print "\nSimple categories are:\n";
    DisplayList(sort keys(%simpleCategory));

    print "\nCombination categories are:\n";
    DisplayComboList(\%simpleCategory,
                     \%comboCategory,
                     \%specialCategory);

    print "\nSpecial categories are:\n";
    DisplayList(sort keys(%specialCategory));

    print "\n";

}   # end DisplayCategories


##
# Displays the logging levels
# 
sub DisplayLevels
{

    print "\nSimple levels are:\n";
    DisplayList(sort keys(%simpleLevel));

    print "\nCombination levels are:\n";
    DisplayComboList(\%simpleLevel,
                     \%comboLevel,
                     \%specialLevel);

    print "\nSpecial levels are:\n";
    DisplayList(sort keys(%specialLevel));

    print "\n";

}   # end DisplayLevels


##
# Displays a list of combination categories or levels.
# 
# @param[in]    $simpleFlags is the hash of simple values.
# 
# @param[in]    $comboFlags is the hash of combination values.
# 
# @param[in]    $specialFlags is the hash of special values.
# 
sub DisplayComboList
{
    my ($simpleFlags, $comboFlags, $specialFlags) = @_;

    foreach my $key (sort keys(%{$comboFlags}))
    {
        printf("  %-14s : %s\n",
               $key,
               MaskToString($specialFlags, $simpleFlags, $comboFlags->{$key}));
    }

}   # end DisplayComboList


##
# Displays a list of logging categories or levels.
# 
# @param[in]    A sorted list of keywords to be displayed.
# 
sub DisplayList
{
    my $linePos = 0;

    foreach my $key (@_)
    {
        if ($linePos != 0)
        {
            print ", ";
            $linePos += 2;
        }

        if ($linePos > 72)
        {
            print "\n";
            $linePos = 0;
        }

        if ($linePos == 0)
        {
            print "  ";
            $linePos += 2;
        }

        print $key;
        $linePos += length($key);
    }

    print "\n" if $linePos != 0;

}   # end DisplayList


##
# Returns a (flags, mask) flag tuple.
# 
# A couple of the basic logging levels are actually multi-bit values that
# overlap other simple values. This function is a hack that lets us provide
# a separate mask only for those flags that need them.
# 
# @param[in]    $entry is an entry from the level (or category) table.
#               May be either a singleton entry or a two-element array.
# 
# @return       ($flags, $mask) tuple
# @return       $flags is the bitmask of flags to be set
# @return       $mask is the bitmask of flags to be cleared
# 
sub GetFlagTuple
{
    my ($entry) = @_;

    if (isArray($entry))
    {
        return ($entry->[0], $entry->[1]);
    }
    else
    {
        return ($entry, $entry);
    }

}   # end GetFlagTuple


##
# Parses a logging category specification.
# 
# @param[in]    $catSpec is a comma-separated list of category names.
# 
# @return       ($status, $catValue)
# 
sub ParseCategorySpec
{
    my ($catSpec) = @_;

    my $catValue = Math::BigInt->new(0);

    $catSpec =~ s/\s//g;
    $catSpec =~ tr/A-Z/a-z/;

    foreach my $item (split(/,/, $catSpec))
    {
        if (defined($specialCategory{$item}))
        {
            $catValue = $specialCategory{$item};
        }
        elsif (defined($simpleCategory{$item}))
        {
            $catValue |= $simpleCategory{$item};
        }
        elsif (defined($comboCategory{$item}))
        {
            $catValue |= $comboCategory{$item};
        }
        elsif (defined($legacyCategory{$item}))
        {
            $catValue |= $legacyCategory{$item};
        }
        else
        {
            printf("'%s' is not a valid category!\n", $item);
            return ($FM_ERR_INVALID_ARGUMENT, undef);
        }
    }

    return ($FM_OK, $catValue);

}   # end ParseCategorySpec


##
# Parses a logging level specification.
# 
# @param[in]    $levelSpec is "all" or "none" or a comma-separated list of
#               level names.
# 
# @return       ($status, $levelValue)
# 
sub ParseLevelSpec
{
    my ($levelSpec) = @_;

    my $levelValue = Math::BigInt->new(0);

    $levelSpec =~ s/\s//g;
    $levelSpec =~ tr/A-Z/a-z/;

    foreach my $item (split(/,/, $levelSpec))
    {
        if (defined($specialLevel{$item}))
        {
            $levelValue = $specialLevel{$item};
        }
        elsif (defined($simpleLevel{$item}))
        {
            my ($flags, $mask) = GetFlagTuple($simpleLevel{$item});
            $levelValue |= $flags;
        }
        elsif (defined($comboLevel{$item}))
        {
            $levelValue |= $comboLevel{$item};
        }
        elsif (defined($legacyLevel{$item}))
        {
            $levelValue |= $legacyLevel{$item};
        }
        else
        {
            printf("'%s' is not a valid level!\n", $item);
            return ($FM_ERR_INVALID_ARGUMENT, undef);
        }
    }

    return ($FM_OK, $levelValue);

}   # end ParseLevelSpec


##
# MaskToString
# 
# @desc         Converts a bitmask to a comma-separated string of 
#               keywords.
# 
# @param[in]    $valueTable is a hash of whole values.
# 
# @param[in]    $flagTable is a hash of flag bits.
# 
# @param[in]    $value is the bitmask to be converted.
# 
# @return       Comma-separated string.
# 
sub MaskToString
{
    my ($valueTable, $flagTable, $value) = @_;

    my $name;

    # We omit 'default' because we want to return its underlying value.
    foreach $name ('all', 'none')
    {
        my $mask = $valueTable->{$name};
        return $name if (defined($mask) && $value == $mask);
    }

    my @results = ();

    foreach my $name (sort keys(%{$flagTable}))
    {
        my ($flags, $mask) = GetFlagTuple($flagTable->{$name});

        if (($value & $mask) == $flags)
        {
            push @results, $name;
            $value &= ~$flags;
            last if $value == 0;
        }
    }

#   push @results, sprintf("0x%02x", $value) if $value != 0;

    return join(', ', @results);

}   # end MaskToString


##
# Handles the 'reset logging' command.
# 
sub tpHandleResetLogging
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    return $chip->fmResetLogging();

}   # end tpHandleResetLogging


##
# Handles the 'set logging category' command.
# 
# Syntax:
#   set logging category enable <categories>
#   set logging category disable <categories>
#   set logging category help
# 
sub tpHandleSetLoggingCategory
{
    my ($self, $operation, $catSpec) = @_;

    my $chip = $self->{CHIP};

    if (!defined($operation))
    {
        print("Must specify a valid operation!\n");
        return;
    }

    if ($operation eq "help")
    {
        DisplayCategories();
        return;
    }

    if (!defined($catSpec))
    {
        print "Must specify <categories>!\n";
        return;
    }

    my ($status, $catValue) = ParseCategorySpec($catSpec);
    return if $status != $FM_OK;

    if ($operation eq "enable")
    {
        $status = $chip->fmEnableLoggingCategory($catValue);
    }
    elsif ($operation eq "disable")
    {
        $status = $chip->fmDisableLoggingCategory($catValue);
    }
    else
    {
        print "Operation must be 'enable' or 'disable'\n";
        return;
    }

    if ($status != $FM_OK)
    {
        printf("Could not set logging category: %s\n", fmErrorMsg($status));
    }

}   # end tpHandleSetLoggingCategory


##
# Handles the 'set logging destination' command.
# 
# Syntax:
#   set logging destination console
#   set logging destination buffer [append | reset]
#   set logging destination file <filename>
# 
sub tpHandleSetLoggingDestination
{
    my $self = shift @_;
    my $chip = $self->{CHIP};

    my $operation = shift @_;

    my ($logType, $clear, $filename);
    my %void = (type => 'fm_uint', value => 0);

    if ($operation eq "console")
    {
        # set logging destination console
        $logType = $FM_LOG_TYPE_CONSOLE;
        $clear = "append";
    }
    elsif ($operation eq "buffer")
    {
        # set logging destination buffer
        $logType = $FM_LOG_TYPE_MEMBUF;
        $clear = shift @_;
    }
    elsif ($operation eq "file")
    {
        # set logging destination file <filename>
        $logType = $FM_LOG_TYPE_FILE;
        $filename = shift @_;
        if (!defined($filename))
        {
            print "must specify filename\n!";
            return $FM_FAIL;
        }
        %void = (type => 'fm_char', value => $filename);
        $clear = "reset";
    }
    else
    {
        print "'$operation' is not a valid destination!\n";
        return $FM_FAIL;
    }

    if (!defined($clear) || $clear eq "append")
    {
        $clear = $FALSE;
    }
    elsif ($clear eq "reset" || $clear eq "clear")
    {
        $clear = $TRUE;
    }
    else
    {
        print "'$clear' must be \"append\" or \"reset\"\n";
        return $FM_FAIL;
    }

    return $chip->fmSetLoggingType($logType, $clear, \%void);

}   # end tpHandleSetLoggingDestination


##
# Handles the 'set logging disable' command.
# 
sub tpHandleSetLoggingDisable
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    return $chip->fmLoggingDisable();

}   # end tpHandleSetLoggingDisable


##
# Handles the 'set logging enable' command.
# 
sub tpHandleSetLoggingEnable
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    return $chip->fmLoggingEnable();

}   # end tpHandleSetLoggingEnable


##
# Handles the 'set logging filter' command.
# 
# Syntax:
#   set logging filter <categories> <levels>
# 
sub tpHandleSetLoggingFilter
{
    my ($self, $catSpec, $levelSpec) = @_;

    my $chip = $self->{CHIP};

    my $catValue;
    my $levelValue;
    my $status;

    if (!defined($catSpec))
    {
        print "Must specify <categories>!\n";
        return;
    }
    elsif ($catSpec eq "help")
    {
        DisplayCategories();
        return;
    }

    ($status, $catValue) = ParseCategorySpec($catSpec);
    return if $status != $FM_OK;

    if (!defined($levelSpec))
    {
        print "Must specify <levels>!\n";
        return;
    }
    elsif ($levelSpec eq "help")
    {
        DisplayLevels();
        return;
    }

    ($status, $levelValue) = ParseLevelSpec($levelSpec);
    return if $status != $FM_OK;

    $status = $chip->fmSetLoggingFilter($catValue, $levelValue, "", "");
    if ($status != $FM_OK)
    {
        printf("Could not set logging filter: %s\n", fmErrorMsg($status));
    }

}   # end tpHandleSetLoggingFilter


##
# Handles the 'set logging level' command.
# 
# Syntax:
#   set logging level enable <levels>
#   set logging level disable <levels>
#   set logging level help
# 
sub tpHandleSetLoggingLevel
{
    my ($self, $operation, $levelSpec) = @_;

    my $chip = $self->{CHIP};

    if (!defined($operation))
    {
        print("Must specify a valid operation!\n");
        return;
    }

    if ($operation eq "help")
    {
        DisplayLevels();
        return;
    }

    if (!defined($levelSpec))
    {
        print "Must specify <levels>!\n";
        return;
    }

    my ($status, $levelValue) = ParseLevelSpec($levelSpec);
    return if $status != $FM_OK;

    if ($operation eq "enable")
    {
        $status = $chip->fmEnableLoggingLevel($levelValue);
    }
    elsif ($operation eq "disable")
    {
        $status = $chip->fmDisableLoggingLevel($levelValue);
    }
    else
    {
        print "Operation must be 'enable' or 'disable'\n";
        return;
    }

    if ($status != $FM_OK)
    {
        printf("Could not set logging level: %s\n", fmErrorMsg($status));
    }

}   # end tpHandleSetLoggingLevel


##
# Handles the 'show logging' command.
# 
sub tpHandleShowLogging
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    my $status;

    # Subsystem status.
    my $getLoggingStatus = sub {
        my %void = (type => 'fm_bool', value => 0);
        my $status = $chip->fmGetLoggingAttribute(
                $FM_LOG_ATTR_ENABLED,
                0,
                \%void);
        my $result;
        if ($status != $FM_OK)
        {
            $result = "<invalid>";
        }
        elsif ($void{value})
        {
            $result = "enabled";
        }
        else
        {
            $result = "disabled";
        }
        return $result;
    };

    # Destination type.
    my $getLoggingType = sub {
        my %void = (type => 'fm_int', value => 0);
        my $status = $chip->fmGetLoggingAttribute(
                $FM_LOG_ATTR_LOG_TYPE,
                0,
                \%void);
        my $result;
        if ($status != $FM_OK)
        {
            $result = "<invalid>";
        }
        elsif ($void{value} == $FM_LOG_TYPE_CONSOLE)
        {
            $result = "console";
        }
        elsif ($void{value} == $FM_LOG_TYPE_MEMBUF)
        {
            $result = "buffer";
        }
        elsif ($void{value} == $FM_LOG_TYPE_FILE)
        {
            $result = "file";
        }
        elsif ($void{value} == $FM_LOG_TYPE_CALLBACK)
        {
            $result = "callback";
        }
        else
        {
            $result = sprintf("unknown (%d)", $void{value});
        }
        return $result;
    };

    # Category mask.
    my $getCategoryMask = sub {
        my %void = (type => 'fm_uint64', value => Math::BigInt->new(0));
        my $status = $chip->fmGetLoggingAttribute(
                $FM_LOG_ATTR_CATEGORY_MASK,
                0,
                \%void);
        my $result;
        if ($status != $FM_OK)
        {
            $result = "<invalid>";
        }
        else
        {
            $result = MaskToString(\%specialCategory,
                                   \%simpleCategory,
                                   $void{value});
        }
        return $result;
    };

    # Level mask.
    my $getLevelMask = sub {
        my %void = (type => 'fm_uint64', value => Math::BigInt->new(0));
        my $status = $chip->fmGetLoggingAttribute(
                $FM_LOG_ATTR_LEVEL_MASK,
                0,
                \%void);
        my $result;
        if ($status != $FM_OK)
        {
            $result = "<invalid>";
        }
        else
        {
            $result = MaskToString(\%specialLevel,
                                   \%simpleLevel,
                                   $void{value});
        }
        return $result;
    };

    # Verbosity mask.
    my $getVerbosityMask = sub {
        my %void = (type => 'fm_uint32', value => 0);
        my $status = $chip->fmGetLoggingAttribute(
                $FM_LOG_ATTR_VERBOSITY_MASK,
                0,
                \%void);
        my $result;
        if ($status != $FM_OK)
        {
            $result = "<invalid>";
        }
        else
        {
            $result = MaskToString(\%specialVerbosity,
                                   \%simpleVerbosity,
                                   $void{value});
        }
        return $result;
    };

    # Log file name.
    my $getLogFileName = sub {
        my %void = (type => 'fm_char', value => "");
        my $status = $chip->fmGetLoggingAttribute(
                $FM_LOG_ATTR_LOG_FILENAME,
                $FM_MAX_FILENAME_LENGTH,
                \%void);
        my $result;
        if ($status != $FM_OK)
        {
            $result = "<invalid>";
        }
        else
        {
            $result = $void{value};
        }
        return $result;
    };

    # Function filter.
    my $getFunctionFilter = sub {
        my %void = (type => 'fm_char', value => "");
        my $status = $chip->fmGetLoggingAttribute(
                $FM_LOG_ATTR_FUNCTION_FILTER,
                $FM_LOG_MAX_FILTER_LEN,
                \%void);
        my $result;
        if ($status != $FM_OK)
        {
            $result = "<invalid>";
        }
        else
        {
            $result = $void{value};
        }
        return $result;
    };

    # File filter.
    my $getFileFilter = sub {
        my %void = (type => 'fm_char', value => "");
        my $status = $chip->fmGetLoggingAttribute(
                $FM_LOG_ATTR_FILE_FILTER,
                $FM_LOG_MAX_FILTER_LEN,
                \%void);
        my $result;
        if ($status != $FM_OK)
        {
            $result = "<invalid>";
        }
        else
        {
            $result = $void{value};
        }
        return $result;
    };

    printf("status          : %s\n", $getLoggingStatus->());

    my $loggingType = $getLoggingType->();
    printf("destination     : %s\n", $loggingType);

    if ($loggingType eq "file")
    {
        printf("log file        : %s\n", $getLogFileName->());
    }

    printf("category mask   : %s\n", $getCategoryMask->());
    printf("level mask      : %s\n", $getLevelMask->());
    printf("verbosity mask  : %s\n", $getVerbosityMask->());
    printf("function filter : %s\n", $getFunctionFilter->());
    printf("file filter     : %s\n", $getFileFilter->());

}   # end tpHandleShowLogging

1;

