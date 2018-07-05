# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

##############################################################################
# File:             Base/Environments/SDK.pm
# Creation Date:    08/30/07
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
##############################################################################

##@class Base::Environments::SDK
# The Base::Environments::SDK package is derived from the Exporter package. It
# imports all Fulcrum Microsystems Software Development Kit (SDK) subroutine
# symbols to allow access to all API functions. Unfortunately, it is not
# possible to also derive the Base::Environments::SDK package from the SDK
# package: the SDK package does not provide proper object methods, i.e. it does
# not expect the @code $self @endcode argument that is passed as the first
# argument in case of object methods. The Perl AUTOLOAD mechanism is used to
# selectively ignore this first argument.
package Base::Environments::SDK;
use strict;
no strict 'refs';
use warnings;

use base qw(Exporter);

# Import all SDK constants, such as $FM_OK. All subroutines are ignored to
# force all API function calls to use the AUTOLOAD mechanism for the above
# mentioned reasons.
use SDK qw(/^\$/);

BEGIN
{
    our @EXPORT = qw(@SYMBOLS);
    our @SYMBOLS = qw();

    # Define a set of SDK symbols that do not match any of the regular
    # expressions, but should nonetheless be exported.
    my %symbols = (
        '$TRUE'     => 1,
        '$FALSE'    => 1,
    );

    # Export all public SDK constants.
    foreach my $symbol (@SDK::EXPORT)
    {
        if ($symbol =~ m/^\$FM_/
            || (exists($symbols{$symbol}) && $symbols{$symbol}))
        {
            push(@EXPORT, $symbol);
        }
    }
    # Export all public SDK symbols to allow TestPoint symbol completion.
    foreach my $entry (@SDK::EXPORT)
    {
        my $symbol = $entry;
        if ($symbol =~ m/^fm[A-Z]/ || $symbol =~ m/^\$FM_/)
        {
            $symbol =~ s/^(\$|@|%)//;
            push(@SYMBOLS, $symbol);
        }
    }
}

our $AUTOLOAD;

##@cmethod public Base::Environments::SDK&
#                 new(int                       id,
#                     Base::Configurations::SDK &configuration)
#
# @desc         Instantiates a Base::Environments::SDK Perl object. This object
#               provides access to the Fulcrum Microsystems Software
#               Development Kit (SDK).
#
# @param[in]    id The global identifier for the Base::Environments::SDK object
#               to be created
#
# @param[in]    configuration An instantiated Base::Configurations::SDK Perl
#               object containing configuration information with which the SDK
#               is to be initialized
#
# @return       An instantiated Base::Environments::SDK Perl object
sub new
{
    my ($class, $id, $configuration) = @_;

    $class = ref($class) || $class;
    my $self  = {
        ID          => $id,
        API_ERROR   => 1,
    };
    bless($self, $class);

    $self->fmOSInitialize();

    # Disable reset in debug mode.
    if ($configuration->debug)
    {
        my %void = (type => "fm_bool", value => $FALSE);
        $self->fmSetApiAttribute("debug.boot.reset", $FM_API_ATTR_BOOL, \%void);
    }

    if ($configuration->debugPlatform)
    {
        my %void = (type => "fm_bool", value => $FALSE);
        $self->fmSetApiAttribute("debug.boot.identifySwitch",
                                 $FM_API_ATTR_BOOL, \%void);
    }

    # Initialize the API.
    $self->fmInitializeW($configuration->switchCount);
    return $self;
}

##@cmethod void disableErrors()
#
# @desc         Disables errors from the API being visibly displayed
sub disableErrors
{
    my ($self) = @_;

    $self->{API_ERROR} = 0;
}

##@cmethod void enableErrors()
#
# @desc         Enables errors from the API to be visibly displayed
sub enableErrors
{
    my ($self) = @_;

    $self->{API_ERROR} = 1;
}

##@cmethod private int AUTOLOAD(...)
#
# @desc         Special method to handle calling into the API directly from the
#               environment object
#
# @return       The API function's return value
sub AUTOLOAD
{
    my ($self, @arguments) = @_;

    return if $AUTOLOAD =~ /::DESTROY$/;

    my $prefix = "SDK";

    $AUTOLOAD =~ s/^.*:://;
    if ($AUTOLOAD =~ m/^fmErrorMsg/)
    {
        return &{($prefix . "::" . $AUTOLOAD)}(@arguments);
    }
    elsif (($AUTOLOAD =~ m/^fm/) || ($AUTOLOAD =~ m/^tp/))
    {
        my $status = &{($prefix . "::" . $AUTOLOAD)}(@arguments);
        if ($self->{API_ERROR} && defined($status) && $status != $FM_OK)
        {
            printf("ERROR: %s: %d: %s\n", $prefix . "::" . $AUTOLOAD, $status,
                   $self->fmErrorMsg($status));
        }
        return $status;
    }
    die sprintf("%s: Unknown AUTOLOAD method!", $AUTOLOAD);
}

1;
