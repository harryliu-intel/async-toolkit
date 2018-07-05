# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Base/Chip.pm
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
###############################################################################

##@namespace Base::Chip
package Base::Chip;
use strict;
use warnings;

use base qw(Base::Device);

use Base::Const;
our $AUTOLOAD;

##@cmethod public Base::Chip& new(int               id,
#                                 char              *type,
#                                 hash              &registers,
#                                 Base::Environment &environment) 
#
# @desc         Initializes a Base::Chip Perl object
#
# @param[in]    id The chip identifier
#
# @param[in]    type The chip type
#
# @param[in]    registers The chip's register symbol table
#
# @param[in]    environment The chip's environment
#
# @return       A Perl reference to an instantiated Base::Chip Perl object
sub new($$$$$)
{
    my ($class, $id, $type, $registers, $environment) = @_;

    $class = ref($class) || $class;
    my $self = new Base::Device($id, $type, undef, $environment);
    bless($self, $class);
    $self->SetRegisters($registers);
    return $self;
}

##@cmethod private void SetRegisters(hash &registers)
#
# @desc         Populates an internal data structure with register information
sub SetRegisters
{
    my ($self, $registers) = @_;

    $self->{REGISTERS} = $registers;
}

##@cmethod void disableErrors(void)
#
# @desc         Disables errors from the API being visibly displayed
sub disableErrors
{
    my ($self) = @_;

    $self->{ENV}->disableErrors();
}

##@cmethod void enableErrors(void)
#
# @desc         Enables errors from the API to be visibly displayed
sub enableErrors
{
    my ($self) = @_;

    $self->{ENV}->enableErrors();
}


##@cmethod public bool isRegister(char *register)
#
# @desc         Determines whether the specified register is truely a register
#
# @param[in]    register The register under investigation
#
# @return       FM_TRUE if the specified register pointed is truely a register
# @return       FM_FALSE otherwise
sub isRegister
{
    my ($self, $register) = @_;

    if (exists($self->{REGISTERS}->{$register}))
    {
        return FM_TRUE;
    }
    return FM_FALSE;
}

##@cmethod public int registerGetWidth(char *register)
#
# @desc         Retrieves the specified register's width
#
# @param[in]    register The register for which the width is to be retrieved
#
# @return       The width of the register in units of words
sub registerGetWidth
{
    my ($self, $register) = @_;

    return ${$self->{REGISTERS}->{$register . '_WIDTH'}};
}

##@cmethod public int** registerGetBounds(char *register)
#
# @desc         Retrieves the specified register's lower and upper bound for
#               all N dimensions
#
# @param[in]    register The register under investigation
#
# @return       An array consisting of N entries with entry i pointing to an
#               array containing the lower and upper bounds of dimension i + 1
sub registerGetBounds
{
    my ($self, $register) = @_;

    my ($null, $zero) = (0, sub { return 0; });
    if (exists($self->{REGISTERS}->{$register . '_ENTRIES'}))
    {
        my $offset = ${$self->{REGISTERS}->{$register . '_OFFSET'} || \$null};
        return
        (
            [
                $offset,
                $offset + &{$self->{REGISTERS}->{$register . '_ENTRIES'}} - 1
            ]
        );
    }
    elsif (exists($self->{REGISTERS}->{$register . '_ENTRIES_0'})
           && exists($self->{REGISTERS}->{$register . '_ENTRIES_1'}))
    {
        my $offsetX =
                    ${$self->{REGISTERS}->{$register . '_OFFSET_0'} || \$null};
        my $offsetY =
                    ${$self->{REGISTERS}->{$register . '_OFFSET_1'} || \$null};
        return
        (
            [
                $offsetY,
                $offsetY + &{$self->{REGISTERS}->{$register . '_ENTRIES_1'}} - 1
            ],
            [
                $offsetX,
                $offsetX + &{$self->{REGISTERS}->{$register . '_ENTRIES_0'}} - 1
            ]
        );
    }
    return
    (
        [
            0, 0
        ]
    );
}

##@cmethod public bool registerIs1D(char *register)
#
# @desc         Determines whether the specified register is a single-indexed
#               register
#
# @param[in]    register The register under investigation
#
# @return       FM_TRUE if the specified register is a single-indexed register
# @return       FM_FALSE otherwise
sub registerIs1D
{
    my ($self, $register) = @_;

    if (exists($self->{REGISTERS}->{$register . '_ENTRIES'}))
    {
        return FM_TRUE;
    }
    return FM_FALSE;
}

##@cmethod public bool registerIs2D(char *register)
#
# @desc         Determines whether the specified register is a double-indexed
#               register
#
# @param[in]    register The register under investigation
#
# @return       FM_TRUE if the specified register is a double-indexed register
# @return       FM_FALSE otherwise
sub registerIs2D
{
    my ($self, $register) = @_;

    if (exists($self->{REGISTERS}->{$register . '_ENTRIES_1'})
        && exists($self->{REGISTERS}->{$register . '_ENTRIES_0'}))
    {
        return FM_TRUE;
    }
    return FM_FALSE;
}

##@cmethod public bool registerIsMultiWord(char *register)
#
# @desc         Determines whether the specified register is a multi-word
#               register
#
# @param[in]    register The register under investigation
#
# @return       FM_TRUE if the specified register is a multi-word register
# @return       FM_FALSE otherwise
sub registerIsMultiWord
{
    my ($self, $register) = @_;

    if (${$self->{REGISTERS}->{$register . '_WIDTH'}} > 1)
    {
        return FM_TRUE;
    }
    return FM_FALSE;
}

##@cmethod public bool registerIsND(char *register)
#
# @desc         Determines whether the specified register is a N-dimensional
#               register
#
# @param[in]    register The register under investigation
#
# @return       FM_TRUE if the specified register is a N-dimensional register
# @return       FM_FALSE otherwise
sub registerIsND
{
    my ($self, $register) = @_;

    if (exists($self->{REGISTERS}->{$register . '_ENTRIES'})
        || exists($self->{REGISTERS}->{$register . '_ENTRIES_0'})
        || exists($self->{REGISTERS}->{$register . '_ENTRIES_1'}))
    {
        return FM_TRUE;
    }
    return FM_FALSE;
}

##@cmethod public bool registerIsPortIndexed(char *register)
#
# @desc         Determines whether the specified register is a port indexed
#               register
#
# @param[in]    register The register under investigation
#
# @return       FM_TRUE if the specified is a port indexed register
# @return       FM_FALSE otherwise
sub registerIsPortIndexed
{
    my ($self, $register) = @_;

    my ($null, $zero) = (0, sub { return 0; });
    my $range1D = ${$self->{REGISTERS}->{$register . '_OFFSET'} || \$null};
    $range1D += &{$self->{REGISTERS}->{$register . '_ENTRIES'}  || $zero};
    my $range2D = ${$self->{REGISTERS}->{$register . '_OFFSET_1'}   || \$null};
    $range2D += &{$self->{REGISTERS}->{$register . '_ENTRIES_1'}    || $zero};
    if ($range1D == 25 || $range2D == 25)
    {
        return FM_TRUE;
    }
    return FM_FALSE;
}

##@cmethod private int AUTOLOAD(...)
#
# @desc         Passes Fulcrum Microsystems API function calls down to the SDK
#               package
#
# @return       The API function's return value
sub AUTOLOAD
{
    my ($self, @arguments) = @_;

    return if $AUTOLOAD =~ /::DESTROY$/;

    $AUTOLOAD =~ s/^.*:://;
    if (($AUTOLOAD =~ m/^fm/) || ($AUTOLOAD =~ m/^tp/))
    {
        return $self->{ENV}->$AUTOLOAD(@arguments);
    }
    die sprintf("%s: Unknown AUTOLOAD method!", $AUTOLOAD);
}

1;
