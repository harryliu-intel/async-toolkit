# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FunctionTreeCore.pm
# Creation Date:    02/16/07
# Description:      Manages the menu tree for the TestPoint application.  This
#                   is a generic base class, it should be inherited from.
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

package Applications::TestPoint::Common::FunctionTreeCore;
use strict;
use warnings;

use SDKScalars;
use Base::Const;
use Applications::TestPoint::Common::CommandDescription;
use Applications::TestPoint::Common::Types::Filter;
use Applications::TestPoint::Common::UpdateCore;

##@method FunctionTreeCore *new()
#   Constructs a generic function tree instance 
#
#   @return Reference to the created object
sub new
{
    # the following allows new to be used as a class method or
    # as the constructor
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $platform = shift;
    my $testpoint = shift;
    my $self = { 
        CT          => undef,
        XML         => undef,
        PLATFORM    => $platform,
        TESTPOINT   => $testpoint,
        CHIP        => undef,
        SWITCHES    => undef,
        SCHEDULER   => undef,
    };
    bless($self, $class);

    $self->{XML} = [ "./Config/xml/DefaultCommands.xml",
                     "./Config/$platform/xml/PlatformCommands.xml" ];

    return $self;
}

##@cmethod public SDK::fm_switchInfo[] tpGetSwitchInfo(void)
#
# @brief        Retrieves the switch information for all physical switches
#
# @return       The switch information for all physical switches
sub tpGetSwitchInfo
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    return $chip->GetSwitchInfo;
}

##@cmethod public int[] tpGetSwitches(void)
#
# @brief        Retrieves the switch numbers for all switches selected for
#               configuration.
#
# @return       The switch numbers for all selected switches
sub tpGetSwitches
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    return $chip->GetSelectedSwitches;
}

##@cmethod public int[] tpGetPhysicalSwitches(void)
#
# @brief        Retrieves the switch numbers for all physical switches
#
# @return       The switch numbers for all physical switches
sub tpGetPhysicalSwitches
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    return $chip->GetSwitches;
}

##@cmethod public void tpSetSelectedSwitches(char *switchList)
#
# @brief        Sets the switch numbers for all switches selected for
#               subsequent configuration.
#
# @param[in]    switchList is the list of switch numbers to select for
#               subsequent configuration.
sub tpSetSelectedSwitches
{
    my ($self, @switchList) = @_;

    my $chip = $self->{CHIP};
    
    return $chip->SetSelectedSwitches(@switchList);
}

##@cmethod public void tpSetSwitchInfo(void)
#
# @brief        Populates an internal data structure with information about all
#               physical switches
sub tpSetSwitchInfo
{
    my ($self) = @_;

    die if (!exists($self->{CHIP}) || !defined($self->{CHIP}));

    my $chip = $self->{CHIP};

    $chip->SetSwitchInfo;
}

##@cmethod public void tpSetSwitches(void)
#
# @brief        Populates an internal data structure with all physical switches
sub tpSetSwitches
{
    my ($self) = @_;

    die if (!exists($self->{CHIP}) || !defined($self->{CHIP}));

    my $chip = $self->{CHIP};

    $chip->SetSwitches;
}

##@method char **getPackageListForSymbolLoading()
#   Returns a list of package names to load symbols from
#
#   @return Array of package names
sub getPackageListForSymbolLoading
{
}

##@method void initializeFunctionTree
#   Loads the internal hash with the tree of menu items.  A menu itme contains
#   the following fields:
#       NAME    The menu item's name (configure, port, etc.)
#       FUNC    The function to handle this command state 
#       NODES   A list of child nodes (array reference)
#       PARENT  The parent of this node (undef for toplevel)
#   
#   @return A list reference to the top level nodes
sub initializeFunctionTree
{
    my ($self) = @_;
    my $xml = $self->{XML};

    my $cmdParser = new Applications::TestPoint::CommandDescription($xml);

    # initialize hierarchy to point to top node
    $self->{CT} = $cmdParser->getRoot();
}

##@method isLeaf(void *node)
#   Tests if a node is a leaf by checking the length of the child node list
#
#   @param[in] node The node reference
#
#   @return 1 if the node is a leaf, 0 otherwise
sub isLeaf
{
    my ($self, $node) = @_;

    return (scalar(@{$node->{NODES}}) > 0) ? 0 : 1;
}

##@method getNodeForCommand(char *cmd)
#   Runs a command through the command tree and returns the matching node
#
#   @param[in] cmd The command string
#
#   @return A tuple containing the node reference, and the remaining argument
#           list, with undefs in the case of no match
sub getNodeForCommand
{
    my ($self, $cmd) = @_;
    my $cn = $self->{CT};
    my $found = 0;

    # commands are space separated
    my @parts = split(/\s+/, $cmd);

    while(@parts)
    {
        $found = 0;

        my $part = shift(@parts);
        # printf("part = $part\n");

        if ($part eq "")
        {
            next;
        }

        foreach my $node (@{$cn->{NODES}})
        {
            if ($node->{NAME} eq $part)
            {
                $found = 1;

                # once we hit an argument node we stop
                if ($node->{TYPE} eq "ARG")
                {
                    unshift(@parts, $part);
                    return ($cn, @parts);
                }
                else
                {
                    $cn = $node;
                }
            }
        }

        # no match? break out
        if ($found == 0)
        {
            unshift(@parts, $part);
            # printf("no matching node, returning %s(%s)\n", $cn->{NAME}, @parts);
            return ($cn, @parts);
        }
    }
    
    if ( $found ) {
        return ($cn, @parts);
    }

    return (undef, undef);
}

##@cmethod private {XML::Twig::Ent&, char*} getNodeForHelp(char *cmd,
#                                                          bool ignoreBangs)
#
# @brief        Runs a command through the command tree and returns the
#               matching node
#
# @param[in]    cmd The command string
#
# @param[in]    ignoreBangs 
#
# @return       A tuple containing the node reference, and the fragment of the
#               last non-matching argument
sub getNodeForHelp
{
    my ($self, $cmd, $ignoreBangs) = @_;
    my $cn = $self->{CT};
    my $dn = undef;
    my $found = 0;

    $ignoreBangs = defined($ignoreBangs) ? $ignoreBangs : 0;

    # commands are space separated
    my @parts = split(/\s+/, $cmd);

    PART: while(defined($parts[0]))
    {
        $found = 0;
        $dn = undef;

        my $part = shift(@parts);

        # Try to find an exact match.
        NODE: foreach my $node (@{$cn->{NODES}})
        {
            if ($node->{NAME} eq $part)
            {
                $found = FM_TRUE;
                $cn = $node;
                last NODE;
            }
        }
        if (!$found)
        {
            # Try to find a matching parameter node using the subsequent
            # command line fragments.
            PARAMETER: foreach my $node (@{$cn->{NODES}})
            {
                if ($node->{NAME} =~ m/^\!/)
                {
                    $dn = $node;
                    if (scalar(@parts) > 0)
                    {
                        my $nextPart = $parts[0];
                        LOOK_AHEAD: foreach my $node (@{$dn->{NODES}})
                        {
                            if ($node->{NAME} =~ m/\Q$nextPart\E/)
                            {
                                $found = FM_TRUE;
                                $cn = $dn;
                                $dn = undef;
                                last PARAMETER;
                            }
                        }
                    }
                }
            }
        }

        # no match? break out
        if (!$found)
        {
            if (defined($dn) && !$ignoreBangs)
            {
                # in this case there was a default argument node, so we'll just
                # assume that they typed the right thing in and keep going
                $cn = $dn;
                $dn = undef;
            }
            elsif (@parts)
            {
                # anything left? then the error happened mid way
                # otherwise the last bit of the command is a fragment
                return undef;
            }
            else
            {
                return ($cn, $part);
            }
        }
    }

    return $cn;
}

##@method void processThroughFunctionTree(char *command)
#   Checks the command against the command hierarchy and implements
#   the command if it maches
#
#   @param[in] command The command string so far
sub processThroughFunctionTree
{
    my ($self, $command) = @_;

    my ($node, @args) = $self->getNodeForCommand($command);

    # validate good result
    if (!defined($node) || !$self->IsValidNode($node))
    {
        printf("\nInvalid command!\n\n");
        return 0;
    }
    elsif (defined($node->{FUNC}))
    {
        # process the command otherwise
        $self->executeHandler($node->{FUNC}, @args);
    }
    else
    {
        printf("\nInvalid command! (No associated handler)\n\n");
        return 0;
    }

    return 1;
}

##@cmethod private char** GenerateCompletions(char *text)
#
# @brief        Generates a completion list for the given TestPoint fragment
#
# @param[in]    text The command line fragment to be completed
#
# @return       A reference to an ARRAY of completion matches
sub GenerateCompletions
{
    my ($self, $text) = @_;

    my $matches = [];

    my ($node, $fragment) = $self->getNodeForHelp($text, 1);

    if ($node)
    {
        foreach my $child (@{$node->{NODES}})
        {
            # Exclude all parameters, such as <port> or <cr>, from <Tab>
            # completion. Check that the name of the current child matches the
            # TestPoint fragment, if it exists,  and ensure that the current
            # child is a valid node for <Tab> completion.
            if ($child->{NAME} !~ m/^\!/
                && ((defined($fragment) && $child->{NAME} =~ m/^\Q$fragment\E/)
                    || !defined($fragment))
                && $self->IsValidNode($child))
            {
                push(@{$matches}, $child->{NAME});
            }
        }
    }
    return $matches;
}

##@cmethod private bool IsValidNode(void *node)
#
# @brief        Determines if the specified node is a valid node
#
# @param[in]    node The node whose validity is to be checked
#
# @return       FM_TRUE if the node is a valid node
# @return       FM_FALSE otherwise
sub IsValidNode
{
    my ($self, $node) = @_;

    # Check for XML parse errors.
    if (!exists($node->{FILTER}) || !defined($node->{FILTER}))
    {
        return FM_FALSE;
    }

    # By default all nodes are considered to be valid nodes, unless specified
    # otherwise.
    my $currentValid = FM_TRUE;
    # Exclusion takes precedence over inclusion of a filter target.
    ACTION: foreach my $action (FILTER_ACTION_ALLOW, FILTER_ACTION_DENY)
    {
        FILTER: foreach my $filter (@{$node->{FILTER}})
        {
            if ($filter->action != $action)
            {
                next FILTER;
            }

            my $nextValid = FM_FALSE;

            SWITCH: for ($filter->type)
            {
                $_ == FILTER_TYPE_PLATFORM && do
                {
                    my @platformList = split(',', $filter->value);
                    foreach my $filterPlat (@platformList)
                    {
                        if ($filterPlat eq ($self->{PLATFORM}))
                        {
                            $nextValid |= FM_TRUE;
                        }
                    }
                    last SWITCH;
                };

                $_ == FILTER_TYPE_FAMILY && do
                {
                    foreach my $infoW ($self->tpGetSwitchInfo)
                    {
                        FAMILY: for ($infoW->{'switchFamily'})
                        {
                            $_ == $FM_SWITCH_FAMILY_FM2000
                            && $filter->value eq 'FM2000'
                            && do
                            {
                                $nextValid |= FM_TRUE;
                                last FAMILY;
                            };

                            $_ == $FM_SWITCH_FAMILY_FM4000
                            && $filter->value eq 'FM4000'
                            && do
                            {
                                $nextValid |= FM_TRUE;
                                last FAMILY;
                            };

                            $_ == $FM_SWITCH_FAMILY_FM6000
                            && $filter->value eq 'FM6000'
                            && do
                            {
                                $nextValid |= FM_TRUE;
                                last FAMILY;
                            };
                        };
                    }
                    last SWITCH;
                };

                $_ == FILTER_TYPE_SWITCH_COUNT && do
                {
                    my $switchCount = scalar(@{[$self->tpGetPhysicalSwitches]});
                    if ($filter->value == $switchCount)
                    {
                        $nextValid |= FM_TRUE;
                        last SWITCH;
                    }
                };

                $_ == FILTER_TYPE_UPDATE_SUPPORT && do
                {
                    if ($filter->value eq 'STARTUP2.0' &&
                        &tpUpdateCommandSupport() == $FM_OK)
                    {
                        $nextValid |= FM_TRUE;
                        last SWITCH;
                    }
                };
                
            }   # end SWITCH: for ($filter->type)

            $currentValid = $action == FILTER_ACTION_ALLOW
                            ? $nextValid
                            : !$nextValid;
                            
        }   # end FILTER: foreach my $filter (@{$node->{FILTER}})
        
    }   # end ACTION: foreach my $action (FILTER_ACTION_ALLOW, FILTER_ACTION_DENY)
    
    return $currentValid;
}

##@method void processHelpForCommand(char *cmd)
#   Prints out the help for the given command sequence if any
#   @param[in] currInput The current readline input
sub processHelpForCommand
{
    my ($self, $currInput) = @_;

    # Strip any switch select override
    $currInput =~ s/^\s*!\S*\s+//;
    
    my ($node, $fragment) = $self->getNodeForHelp($currInput);

    my %helpText = ();

    foreach my $next (@{$node->{NODES}})
    {
        my $cmdName = $next->{NAME};
        # printf("cmd: %s valid: %d\n",$cmdName, $self->IsValidNode($next));

        if ($cmdName =~ m/^\!/)
        {
            $cmdName =~ s/^\!//;
        }

        if (!$next->{HIDDEN} && $self->IsValidNode($next))
        {
            my $help = $next->{HELP};
            $helpText{$cmdName} = defined($help) ? $help : "";
        }
    }

    printf("\n");
    foreach my $cmdName (sort keys(%helpText))
    {
        printf("    %-24s %s\n", $cmdName, $helpText{$cmdName});
    }
    printf("\n");
}

##@cmethod void executeHandler(char *function, ...)
#   Required so that the base object can call methods defined in this
#   class (for command handlers)
sub executeHandler
{
    my ($self, $function, @arguments) = @_;

    return $self->$function(@arguments);
}

sub tpDisableErrors
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    $chip->disableErrors();
}

sub tpEnableErrors
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    $chip->disableErrors();
}

1;
