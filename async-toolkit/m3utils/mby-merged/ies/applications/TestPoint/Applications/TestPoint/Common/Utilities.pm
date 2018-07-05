# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/Utilities.pm
# Creation Date:    09/05/07
# Description:      Common utility routines.
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

package Applications::TestPoint::Common::Utilities;
use strict;
use warnings;

use POSIX qw(strtoul);
use Scalar::Util qw(blessed);
use SDKScalars;
use Types::tp_vpPair;

##@cmethod public void printCounter(Math::BigInt &value, char *message)
#
# @brief        Prints the value of a statistics counter
#
# @param[in]    value The Math::BigInt Perl object whose value is to be printed
#
# @param[in]    message The message describing the value to be printed
#
# @param[in]    quiet If defined will suppress output if value equal to zero
sub printCounter
{
    my ($self, $value, $message, $quiet) = @_;

    if (defined($value))
    {
        if ((!defined($quiet)) || (!$value->is_zero()))
        {
            my @y = split(//, $value->as_hex());
            splice(@y, 2, 0, (0) x (18 - scalar(@y)));
            printf("%-42s %s [%s]\n", $message, join('', @y), $value->bstr());
            return $value;
        }
    }
    return 0;
}

##@cmethod public void printRate(Math::BigFloat &value, char *message)

#
# @brief        Prints the value of a port rate
#
# @param[in]    value The Math::BigFloat Perl object whose value is to be printed
#
# @param[in]    message The message describing the value to be printed
#
# @param[in]    quiet If defined will suppress output if value equal to zero
sub printRate
{
    my ($self, $value, $message, $quiet) = @_;

    if (defined($value))
    {
        if ((!defined($quiet)) || (!$value->is_zero()))
        {
            printf("%-42s %20s\n", $message, $value->bceil()->bstr());
            return $value;
        }
    }
    return 0;
}

##@cmethod public void printUtilization(Math::BigFloat &value, char *message)

#
# @brief        Prints the value of a port rate
#
# @param[in]    value The Math::BigFloat Perl object whose value is to be printed
#
# @param[in]    message The message describing the value to be printed
#
# @param[in]    quiet If defined will suppress output if value equal to zero
sub printUtilization
{
    my ($self, $value, $message, $quiet) = @_;

    if (defined($value))
    {
        if ((!defined($quiet)) || (!$value->is_zero()))
        {
            $value->accuracy(4);
            printf("%-42s           %10s%% +- 1%%\n", $message, $value->bstr());
            return $value;
        }
    }
    return 0;
}

##@cmethod private char* StringifyList(int[] portList)
#
# @brief        Converts a list of ports to its string representation
#
# @param[in]    portList The list of ports to be converted
#
# @return       The string representation of the list of ports
sub StringifyList
{
    my ($self, @portList) = @_;

    my $value = "";
    if (scalar(@portList) > 0)
    {
        @portList = sort {$a <=> $b} @portList;
        my $currentPort = shift(@portList);
        $value .= $currentPort;
        while (scalar(@portList) > 0)
        {
            my $previousPort = $currentPort;
            my $nextPort = shift(@portList);
            while (defined($nextPort) && $nextPort == ($previousPort + 1))
            {
                $previousPort = $nextPort;
                $nextPort = shift(@portList);
            }
            if ($previousPort != $currentPort)
            {
                $value .= ($previousPort - $currentPort) > 1 ? '..' : ',';
                $value .= $previousPort;
            }
            if (defined($nextPort))
            {
                $value .= ',' . $nextPort;
            }
            $currentPort = $nextPort;
        }
    }
    return $value;
}

##@cmethod public char* stringifyMask(int switchNum, int portMask)
#
# @brief        Converts a bitmask (32-bit at most) to its string
#               representation
#
# @param[in]    bitask The bitmask to be converted
#
# @return       The string representation of the bitmask
sub stringifyMask
{
    my ($self, $bitmask) = @_;

    # Convert the logical port mask to a faceplate port mask.
    my @bits = ();
    for (my $bit = 0; $bit < 32; $bit++)
    {
        if ($bitmask & (1 << $bit))
        {
            push(@bits, $bit);
        }
    }
    return $self->StringifyList(@bits);
}

##@cmethod public char* stringifyPortMask(int switchNum, int portMask)
#
# @brief        Converts a port mask (32-bit at most) to its string
#               representation
#
# @param[in]    switchNum The switch number to which the port mask that is to
#               be converted belongs to
#
# @param[in]    portMask The port mask to be converted
#
# @return       The string representation of the port mask
sub stringifyPortMask
{
    my ($self, $switchNum, $portMask) = @_;

    my $chip = $self->{CHIP};

    my $infoW = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($switchNum, $infoW);

    # Convert the logical port mask to a faceplate port mask.
    my @portMask = ();
    my ($logicalPort);
    for ($logicalPort = 0; $logicalPort < $infoW->{'numPorts'}; $logicalPort++)
    {
        if ($portMask & (1 << $logicalPort))
        {
            my $faceplatePort = 
                      $self->tpPlatformMapLogicalToFaceplatePort($switchNum,
                                                                 $logicalPort);
            if ($faceplatePort != -1)
            {
                push(@portMask, $faceplatePort);
            }
        }
    }
    return $self->StringifyList(@portMask);
}

##@cmethod public char* stringifyPortMaskWide(int               switchNum,
#                                             SDK::fm_bitArray& portMask)
#
# @brief        Converts a wide port mask to its string representation.
#
# @param[in]    switchNum is the switch number associated with the wide port
#               mask.
#
# @param[in]    portMask is the wide port mask to be converted.
#
# @return       The string representation of the wide port mask.
sub stringifyPortMaskWide
{
    my ($self, $switchNum, $portMask) = @_;

    my $chip = $self->{CHIP};

    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($switchNum, $info);

    # Convert the logical port mask to a faceplate port mask.
    my $firstPort = 0;
    my @ports = ();
    my ($logPort);
    while ($TRUE)
    {
        my $status = 
            $chip->fmFindBitInBitArray($portMask, $firstPort, 1, \$logPort);
        last if ( ( $status != $FM_OK ) || ( $logPort == -1 ) );
        my $faceplatePort = 
            $self->tpPlatformMapLogicalToFaceplatePort($switchNum, $logPort);
        if ($faceplatePort != -1)
        {
            push(@ports, $faceplatePort);
        }
        $firstPort = $logPort + 1;
    }
    return $self->StringifyList(@ports);
}

##@cmethod public int str2decnum(char *string)
#
# @brief        Converts the decimal string representation of an integer
#               number back to its original representation
#
# @param[in]    string The string to be converted
#
# @return       The integer number if successful
# @return       @c undef otherwise
sub str2decnum
{
    my ($self, $string) = @_;

    return $self->_str2num($string, 10);
}

##@cmethod public int str2hexnum(char *string)
#
# @brief        Converts the hexadecimal string representation of an integer
#               number back to its original representation
#
# @param[in]    string The string to be converted
#
# @return       The integer number if successful
# @return       @c undef otherwise
sub str2hexnum
{
    my ($self, $string) = @_;

    return $self->_str2num($string, 16);
}

##@cmethod public int str2intnum(char *string)
#
# @brief        Converts the string representation of an integer number back to
#               its original representation
#
# @param[in]    string The string to be converted
#
# @return       The integer number if successful
# @return       @c undef otherwise
sub str2intnum
{
    my ($self, $string) = @_;

    return $self->_str2num($string, 0);
}

##@cmethod private int str2num(char *string, int base)
#
# @brief        Converts the string representation of an integer number 
# *             to its internal representation
#
# @param[in]    string The string to be converted
#
# @param[in]    base The base to use
#
# @return       The integer number if successful
# @return       @c undef otherwise
sub _str2num
{
    my ($self, $string, $base) = @_;

    if (!defined($string) || $string eq '')
    {
        return undef;
    }

    $string =~ s/\s+//;
    # Reset ERRNO to allow fully compliant POSIX platforms to indicate that an
    # overflow has occurred.
    $! = 0;
    my ($number, $nUnparsed) = strtoul($string, $base);
    if ($nUnparsed != 0 || $!)
    {
        return undef;
    }
    return $number;
}

##@cmethod public int *validateList(char *list, int min, int max)
#
# @brief        Returns the full list from the given argument, supporting
#               dotted ranges, comma separated lists or single values (and
#               combinations)
#
# @param[in]    list The range/list string to parse
#
# @param[in]    min The minimum value for "all"
#
# @param[in]    max The maximum value for "all"
#
# @param[in]    verbose should be TRUE (or omitted) to generate "invalid input"
#               message
#
# @return       The validated list as an array, or undef if there was an error
sub validateList
{
    my ($self, $list, $min, $max, $verbose) = @_;

    if (!defined($list) or ($list eq ""))
    {
        return ();
    }

    if (!defined($verbose))
    {
        $verbose = $TRUE
    }

    if ($list eq "all")
    {
        return $min..$max;
    }
    # ranges with both , and .. are valid too
    elsif ($list =~ m/,/)
    {
        my @refParts = split(/,/, $list);
        my @list = ();

        while (@refParts)
        {
            my $p = shift(@refParts);

            if ($p =~ m/(\d+)\.\.(\d+)/)
            {
                if ($1 >= $min && $2 <= $max)
                {
                    push(@list, $1..$2);
                }
                elsif ($verbose > 1)
                {
                    print "'$p' is not a valid range!\n";
                    return ();
                }
                else
                {
                    print("invalid input!\n") if $verbose;
                    return ();
                }
            }
            elsif ($p =~ m/\d+/)
            {
                if ($p >= $min && $p <= $max)
                {
                    push(@list, $p);
                }
                elsif ($verbose > 1)
                {
                    print "'$p' is out of range!\n";
                    return ();
                }
                else
                {
                    print("invalid input!\n") if $verbose;
                    return ();
                }
            }
            elsif ($verbose > 1)
            {
                print "'$p' is not a legal number!\n";
                return ();
            }
            else
            {
                print("invalid input!\n") if $verbose;
                return ();
            }
        }

        return @list;
    }
    # dotted list ranges are valid
    elsif ($list =~ m/(\d+)\.\.(\d+)/)
    {
        if ($1 >= $min && $2 <= $max )
        {
            return $1..$2;
        }
        elsif ($verbose > 1)
        {
            print "'$list' is not a valid range!\n";
            return ();
        }
        else
        {
            print("invalid input!\n") if $verbose;
            return ();
        }
    }
    else
    {
        if ($list =~ m/\d+/)
        {
            if ($list >= $min && $list <= $max)
            {
                return ($list);
            }
            elsif ($verbose > 1)
            {
                print "'$list' is out of range!\n";
                return ();
            }
            else
            {
                print("invalid input!\n") if $verbose;
                return ();
            }
        }
        elsif ($verbose > 1)
        {
            print "'$list' is not a legal number!\n";
            return ();
        }
        else
        {
            print("invalid input!\n") if $verbose;
            return ();
        }
    }
}   # end validateList

##@cmethod public int *validateExplicitList(int filter, char *list, char *validList)
#
# @brief        Returns the full list from the given argument, supporting
#               dotted ranges, comma separated lists or single values (and
#               combinations) from a template list of valid values.
#
# @param[in]    filter indicates whether this function should silently filter 
#               out port numbers not in validList while returning ports that
#               are in vaildList instead of returning an empty list.
#
# @param[in]    list The range/list string to parse
#
# @param[in]    validList is an array of possible valid values.
#
# @return       The validated list as an array, or undef if there was an error
sub validateExplicitList
{
    my ($self, $filter, $list, @validList) = @_;

    if (!defined($list) or ($list eq ""))
    {
        return ();
    }
    
    # sort the valid list numerically
    @validList = sort { $a <=> $b } @validList;

    my @list = ();
    
    if ($list eq "all")
    {
        return @validList;
    }
    # ranges with both , and .. are valid too
    elsif ($list =~ m/,/)
    {
        my @refParts = split(/,/, $list);

        while (@refParts)
        {
            my $p = shift(@refParts);

            if ($p =~ m/(\d+)\.\.(\d+)/)
            {
                for (my $i = $1; $i <= $2; $i++)
                {
                    if ( grep { $_ eq $i } @validList )
                    {
                        push(@list, $i);
                    }
                    else
                    {
                        if (!$filter)
                        {
                            return ();
                        }
                    }
                }
            }
            elsif ($p =~ m/\d+/)
            {
                if ( grep { $_ eq $p } @validList )
                {
                    push(@list, $p);
                }
                else
                {
                    if (!$filter)
                    {
                        return ();
                    }
                }
            }
            else
            {
                # Syntax error.
                return ();
            }
        }

        return @list;
    }
    # dotted list ranges are valid
    elsif ($list =~ m/(\d+)\.\.(\d+)/)
    {
        for (my $i = $1; $i <= $2; $i++)
        {
            if ( grep { $_ eq $i } @validList )
            {
                push(@list, $i);
            }
            else
            {
                if (!$filter)
                {
                    return ();
                }
            }
        }
        
        return @list;
    }
    else
    {
        if ($list =~ m/\d+/)
        {
            if ( grep { $_ eq $list } @validList )
            {
                return ($list);
            }
            else
            {
                return ();
            }
        }
        else
        {
            return ();
        }
    }
}

##@cmethod public void ReportMissedPorts(char **affectedPorts, char **availablePorts)
#
# @brief        Print out the ports that appear in an available port list
#               that do not appear in an affected port list for the purpose
#               of informing the user of which ports in their request did
#               not get serviced.
#
# @param[in]    affectedPorts is a reference to a list of ports that were
#               operated on. There may be duplicates in this list, which
#               will be filtered out before being compared to availablePorts.
#
# @param[in]    availablePorts is a reference to a list of ports that could have
#               been operated on and must be a superset of affectedPorts
#               (after duplicates have been filtered out).
sub ReportMissedPorts
{
    my ($self, $affectedPorts, $availablePorts) = @_;
    
    my @missingPortList = ();
    
    if (scalar(@$affectedPorts) == 0)
    {
        @missingPortList = @$availablePorts;
    }
    else
    {
        my @filteredAffectedPorts = sort { $a <=> $b } @$affectedPorts; 
        @$availablePorts = sort { $a <=> $b } @$availablePorts;
        
        # Eliminate duplicates in filteredAffectedPorts
        my $lastIndex = $#filteredAffectedPorts;
        
        for (my $i = 0 ; $i < $lastIndex ; $i++)
        {
            if ($filteredAffectedPorts[$i] == $filteredAffectedPorts[$i + 1])
            {
                # Get rid of duplicate
                my @tail = splice(@filteredAffectedPorts, $i + 1, $lastIndex - $i);
                splice(@filteredAffectedPorts, $i, 1, @tail);
                
                # Array is now one entry shorter
                --$lastIndex;
                
                # Revisit the same index again since we just deleted the
                # entry at that index and pulled up the tail.
                --$i;
            }
        }
        
        while (scalar(@filteredAffectedPorts) > 0)
        {
            my $port1 = shift(@filteredAffectedPorts);
            my $port2 = shift(@$availablePorts);
            
            while (defined($port2) && defined($port1) && ($port1 != $port2))
            {
                push(@missingPortList, $port2);
                $port2 = shift(@$availablePorts);
            }
        }
        
        push(@missingPortList, @$availablePorts);
    }
    
    if (scalar(@missingPortList) > 0)
    {
        my $missingPorts = $self->StringifyList(@missingPortList);
        my $moreThanOnePort = $missingPorts =~ /\D/;
        my $switchList = $self->StringifyList($self->tpGetSwitches);
        my $moreThanOneSwitch = $switchList =~ /\D/;
        
        printf("Port%s $missingPorts do%s not appear on switch%s $switchList.\n",
                $moreThanOnePort ? "s" : "",
                $moreThanOnePort ? "" : "es",
                $moreThanOneSwitch ? "es" : "");
    }
    
}
    
##@method private int validateL2Address(char *macAddress)
#   Validates a Layer 2 address
#
#   @return FM_OK on success, FM_FAIL otherwise
sub validateL2Address
{
    my ($self, $macAddress) = @_;

    if ($macAddress =~ m/^[0-9A-Fa-f]{2}(:[0-9A-Fa-f]{2}){5}$/
        || $macAddress =~ m/^[0-9A-Fa-f]{12}$/)
    {
        return $FM_OK;
    }
    return $FM_FAIL;
}

##@cmethod private int VPPairNToP(Types::tp_vpPair &vpPair, char **value)
sub VPPairNToP
{
    my $self                    = shift(@_);
    my Types::tp_vpPair $vpPair = shift(@_);
    my $value                   = shift(@_);

    if (!blessed($vpPair) || !$vpPair->isa('Types::tp_vpPair'))
    {
        return $FM_FAIL;
    }

    ${$value} = sprintf("%d/%d", $vpPair->{'value'}, $vpPair->{'prefix'});

    return $FM_OK;
}

##@cmethod private int VPPairPToN(char *value, Types::tp_vpPair &vpPair)
#
# @brief
#
# @param[in]    value
#
# @param[out]   vpPair
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub VPPairPToN
{
    my $self                    = shift(@_);
    my $value                   = shift(@_);
    my Types::tp_vpPair $vpPair = shift(@_); 

    if (!blessed($vpPair) || !$vpPair->isa('Types::tp_vpPair'))
    {
        return $FM_FAIL;
    }

    if (!defined($vpPair->{'value'} = $self->str2intnum($value)))
    {
        return $FM_FAIL;
    }
    $vpPair->{'prefix'} = $vpPair->{'width'};
    if ($vpPair->{'value'} =~ m/^([^\/]*)\/(.*)$/)
    {
        $vpPair->{'value'} = $1;
        if (!defined($vpPair->{'prefix'} = $self->str2decnum($2))
            || $vpPair->{'prefix'} < 0
            || $vpPair->{'prefix'} > $vpPair->{'width'})
        {
            return $FM_FAIL;
        }
    }
    return $FM_OK;
}

1;
