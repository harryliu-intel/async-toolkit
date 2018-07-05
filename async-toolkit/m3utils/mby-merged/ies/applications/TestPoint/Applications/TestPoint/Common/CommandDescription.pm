# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/CommandDescription.pm
# Creation Date:    03/15/07
# Description:      Object to hold the command parse structure for the
# TestPoint
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

package Applications::TestPoint::CommandDescription;
use strict;
use warnings;

use List::Util qw(sum);
use XML::Twig;
use Base::Const;
use Applications::TestPoint::Common::Types::Filter;
use FileHandle;

##@method public CommandDescription& new(char *file)
# Instantiate a new Perl CommandDescription object
# @param[in] file The command description XML file
# @return A Perl reference to an instantiated Perl CommandDescription object
sub new($$)
{
    my ($proto, $fileList) = @_;
    # the following allows new to be used as a class method or
    # as the constructor
    my $class = ref($proto) || $proto;

    my $self = {};

    # root is initialized to an empty top node
    $self->{ROOT} = 
    {
        NAME => "",
        HELP => "",
        FUNC => "",
        DESC => "",
        NODES => []
    };

    bless($self, $class);

    foreach my $file (@$fileList)
    {
        $self->load($file);
    }

    return $self;
}

##@method public PrintNode(node, filehandle)
# Outputs a single node to the specified file output handle
sub printNode
{
    my ($node,$fh) = @_;

    my $name = defined($node->{NAME})?$node->{NAME}:"";
    $name =~ s/</&lt;/;
    $name =~ s/>/&gt;/;
    my $help = $node->{HELP};
    my $desc = $node->{DESC};
    my $func = $node->{FUNC};
    my $type = defined($node->{TYPE})?$node->{TYPE}:"CMD";
    $type =~ s/CMD/command/;
    $type =~ s/ARG/argument/;
    
    print $fh "<$type>";
    if (defined($name))
    {
        print $fh "<name>" .  $name . "</name>";
    }

    if (defined($help))
    {
        print $fh "<help>" .  $help . "</help>";
    }
    if (defined($desc))
    {
        print $fh "<description>" .  $desc . "</description>";
    }
    if (defined($func))
    {
        "<handler>" .  $func . "</handler>";
    }
    print $fh "\n";
    foreach my $inode (@{$node->{NODES}})
    {
        printNode($inode, $fh);
    }
    print $fh "</$type> <!-- " . $name . " -->\n";
}

##@method public PrintCommands(filename)
# Outputs the command tree to a file
sub PrintCommands($)
{
    my ($self, $filename) = @_;

    my $fh = new FileHandle;
    open($fh, '>'  . $filename);
    print $fh "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n";
    print $fh "<?xml-stylesheet href=\"TestPoint.xsl\" type=\"text/xsl\" ?>\n";
    print $fh "<config>\n";
    my $cn = $self->getRoot();

    foreach my $inode (@{$cn->{NODES}})
    {
        printNode($inode, $fh);
    }

    print $fh "</config>\n";
    close $fh;
}


##@method public bool isFoundInNodeList(list, node)
# Checks whether the given list contains the node
# and if so, returns the list's version
sub isFoundInNodeList($$)
{
    my ($self, $list, $node) = @_;

    foreach my $listnode (@$list)
    {
        if ($listnode->{NAME} eq $node->{NAME})
        {
            return $listnode;
        }
    }

    return undef;
}

##@method public void rootTreeOverlay(root, tree)
# Adds the given node tree on top of the root
sub rootTreeOverlay
{
    my ($self, $root, $tree) = @_;
    my $existing;

    if (!defined($tree))
    {
        return;
    }

    # case 1 : overlay of a node within the root
    if (defined($existing = $self->isFoundInNodeList($root->{NODES}, $tree)))
    {
        # copy over only if the new node modifies something other than name
        if (defined($tree->{FUNC}) && ($tree->{FUNC} ne ""))
        {
            $existing->{FUNC} = $tree->{FUNC};
        }
        if (defined($tree->{HELP}) && ($tree->{HELP} ne ""))
        {
            $existing->{HELP} = $tree->{HELP};
        }

        foreach my $treeChild (@{$tree->{NODES}})
        {
            $self->rootTreeOverlay($existing, $treeChild);
        }
    }
    # case 2 : adding a new root child
    else
    {
        push(@{$root->{NODES}}, $tree);
    }
}

##@method public void load(void)
# Parses the XML description file, and reads information about nodes and links,
# placing them in the appropriate maps
sub load($$)
{
    my ($self, $file) = @_;

    # skip empty files
    if (!(-e $file))
    {
        print " [Command XML Load] Error: file not found: $file\n";
        return;
    }

    my $parser = new XML::Twig();

    # hackery! we need to store our self instance so that
    # the handleInclude call knows who called it
    $parser->{INSTANCE} = $self;

    $parser->parsefile($file) or die "Error parsing XML file $file: $!";

    my $config = $parser->root;
    my @toplevelNodes = $config->children();

    foreach my $toplevelNode (@toplevelNodes)
    {
        if ($toplevelNode->tag() eq "command")
        {
            my $tree = $self->ParseCommand($toplevelNode, undef);

            $self->rootTreeOverlay($self->{ROOT}, $tree); 
        }
        elsif ($toplevelNode->tag() eq "include")
        {
            $self->load($toplevelNode->att("file"));
        }
    }
}

##@cmethod private char* getFirstChild(XML::Twig::Elt &node, char *tag)
#
# @brief        Utility method to grab the contents of a <foo>content</foo> tag
#
# @param[in]    node The parent twig of the tag whose contents is to be
#               retrieved
#
# @param[in]    tag The tag whose contents is to be retrieved
#
# @return       The contents of the tag if successful
# @return       @c undef otherwise
sub getFirstChild
{
    my ($self, $node, $tag) = @_;

    my $child = $node->first_child($tag);
    if (!defined($child))
    {
        return undef;
    }
    my $contents = $child->first_child()->pcdata();
    if (defined($contents))
    {
        # Remove all embedded newline characters.
        $contents =~ s/\s*\n\s*/ /g;
        # Remove leading and trailing whitespace.
        $contents =~ s/^\s+(.*)\s+$/$1/;
    }
    return $contents;
}

##@method public getRoot(void)
# Returns the node tree
sub getRoot
{
    my ($self) = @_;

    return $self->{ROOT};
}

##@cmethod private hash ParseCommand(XML::Twig::Elt &cmd,
#                                    XML::Twig::Elt &parent,
#                                    char           *type)
#
# @brief        Parses a command XML tree
#
# @param[in]    cmd A reference to the command XML tree to parse
#
# @param[in]    parent @optional
#
# @param[in]    type @optional
#
# @return       A simplified node tree representing the command tree
sub ParseCommand
{
    my ($self, $cmd, $parent, $type) = @_; 

    my $node = {};
    my $subcmd = undef;

    $node->{NAME} = $self->getFirstChild($cmd, "name");
    $node->{HELP} = $self->getFirstChild($cmd, "help");
    $node->{DESC} = $self->getFirstChild($cmd, "description");
    $node->{FUNC} = $self->getFirstChild($cmd, "handler");

    $node->{HIDDEN} = $self->getFirstChild($cmd, "hidden");
    if(!defined($node->{HIDDEN}))
    {
        $node->{HIDDEN} = 0;
    }
    else
    {
        if ($node->{HIDDEN} eq "true")
        {
            $node->{HIDDEN} = 1;
        }
        else
        {
            $node->{HIDDEN} = 0;
        }
    }

    $node->{FILTER} = $self->ParseFilter($cmd->first_child('filter'));

    $node->{TYPE} = defined($type) ? $type : "CMD";
    $node->{PARENT} = $parent;
    $node->{NODES} = [];

    foreach $subcmd ($cmd->children("command"))
    {
        push(@{$node->{NODES}}, $self->ParseCommand($subcmd));
    }

    foreach $subcmd ($cmd->children("argument"))
    {
        push(@{$node->{NODES}}, $self->ParseCommand($subcmd, $node, "ARG"));
    }

    return $node;
}

##@cmethod private Applications::TestPoint::Common::Types::Filter[]&
#          ParseFilter(XML::Twig::Ent &node)
#
# @brief        Retrieves all filters by parsing the XML tree starting at the
#               specified branch
#
# @param[in]    node The starting point of the parser
#
# @return       A reference to an ARRAY containing a set of
#               Applications::TestPoint::Common::Types::Filter Perl objects
sub ParseFilter
{
    my ($self, $node) = @_;

    my ($currentFilter, $nextFilter);

    $currentFilter = $node;
    my $filters = [];
    while (defined($currentFilter))
    {
        my ($currentAction, $nextAction);

        my $filter = Applications::TestPoint::Common::Types::Filter->new();
        my $parseError = FM_FALSE;

        my $type = $currentFilter->att('type');
        if (!defined($type))
        {
            $parseError = FM_TRUE;
        }
        else
        {
            $filter->type($type);
            SWITCH: for ($filter->type)
            {
                $_ eq 'platform' && do
                {
                    $filter->type(FILTER_TYPE_PLATFORM);
                    last SWITCH;
                };

                $_ eq 'family' && do
                {
                    $filter->type(FILTER_TYPE_FAMILY);
                    last SWITCH;
                };

                $_ eq 'switch-count' && do
                {
                    $filter->type(FILTER_TYPE_SWITCH_COUNT);
                    last SWITCH;
                };

                $_ eq 'update-support' && do
                {
                    $filter->type(FILTER_TYPE_UPDATE_SUPPORT);
                    last SWITCH;
                };

                do
                {
                    warn(sprintf("%s: Unrecognized filter type attribute",
                                 $filter->type));
                    $parseError = FM_TRUE;
                };
            }
        }

        if (!$parseError)
        {
            my $clone = $filter->clone();

            $currentAction = $currentFilter->first_child();
            while (defined($currentAction))
            {
                SWITCH: for ($currentAction->tag())
                {
                    $_ eq 'allow' && do
                    {
                        $clone->action(FILTER_ACTION_ALLOW);
                        $clone->value ($currentAction->first_child()->pcdata());
                        last SWITCH;
                    };

                    $_ eq 'deny' && do
                    {
                        $clone->action(FILTER_ACTION_DENY);
                        $clone->value ($currentAction->first_child()->pcdata());
                        last SWITCH;
                    };

                    do
                    {
                        warn(sprintf("%s: Unrecognized filter action tag",
                                     $currentAction->tag()));
                        $parseError = FM_TRUE;
                    };
                }

                if (!$parseError)
                {
                    push(@{$filters}, $clone);
                }

                $nextAction = $currentAction->next_sibling();
                $currentAction = $nextAction;
            }
        }
        $nextFilter = $currentFilter->next_sibling('filter');
        $currentFilter = $nextFilter;
    }
    return $filters;
}

1;
