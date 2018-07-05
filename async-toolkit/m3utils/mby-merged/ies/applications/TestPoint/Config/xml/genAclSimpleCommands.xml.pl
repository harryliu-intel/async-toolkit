#!/usr/local/bin/perl

###############################################################################
# File:             genAclSimpleCommands.xml.pl
# Creation Date:    11/2/07
# Description:      generates AclSimpleCommands.xml
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
use strict;
use warnings;

# By default, don't generate acl-simple commands.
my $suppressCommands = 1;

my $actions = printActions();

###############################################################################
# Edit this section to add a new ACL type
###############################################################################

my %types = (
    "sip" => "Source IP ACL",
    "dip" => "Dest IP ACL",
    "l34" => "L34 ACL (SIP, DIP, L4SRC, L4DST, PROT)",
    "smac-ethertype" => "Source MAC and EtherType ACL",
    "dmac-ethertype" => "Dest MAC and EtherType ACL",
);

my %rules = ();

$rules{"sip"} = <<END;
                            <argument>
                                <name>!&lt;sip&gt;</name>
                                <help>source IP value and mask(i.e. 10.0.0.0/16)</help>
$actions                            </argument>
END


$rules{"dip"} = <<END;
                            <argument>
                                <name>!&lt;dip&gt;</name>
                                <help>dest IP value and mask(i.e. 10.0.0.0/16)</help>
$actions                            </argument>
END


$rules{"l34"} = <<END;
                            <argument>
                                <name>!&lt;sip&gt;</name>
                                <help>source IP value and mask(i.e. 10.0.0.0/16)</help>
                                <argument>
                                    <name>!&lt;dip&gt;</name>
                                    <help>dest IP value and mask(i.e. 10.0.0.0/16)</help>
                                    <argument>
                                        <name>!&lt;sport&gt;</name>
                                        <help>source port value and mask(i.e. 123/0xf)</help>
                                        <argument>
                                            <name>!&lt;dport&gt;</name>
                                            <help>source port value and mask(i.e. 123/0xf)</help>
                                            <argument>
                                                <name>!&lt;prot&gt;</name>
                                                <help>Protocol value and mask(i.e. 123/0xf)</help>
$actions                                        </argument>
                                        </argument>
                                    </argument>
                                </argument>
                            </argument>
END

$rules{"smac-ethertype"} = <<END;
                            <argument>
                                <name>!&lt;smac&gt;</name>
                                <help>source MA value and mask(i.e. 68:46:A3:07:CA:D3/FF:FF:FF:00:00:00)</help>
                                <argument>
                                    <name>!&lt;ethertype&gt;</name>
                                    <help>EtherType value and mask(i.e. 0x8100/0xefff)</help>
$actions                            </argument>
                            </argument>
END

$rules{"dmac-ethertype"} = <<END;
                            <argument>
                                <name>!&lt;dmac&gt;</name>
                                <help>dest MA value and mask(i.e. 68:46:A3:07:CA:D3/FF:FF:FF:00:00:00)</help>
                                <argument>
                                    <name>!&lt;ethertype&gt;</name>
                                    <help>EtherType value and mask(i.e. 0x8100/0xefff)</help>
$actions                            </argument>
                            </argument>
END

###############################################################################
# Helper functions
###############################################################################

sub printActions
{
    return <<END;
                                            <argument>
                                                <name>permit</name>
                                                <help>Traffic matching this rule will be permitted</help>
                                                <argument>
                                                    <name>!&lt;count&gt;</name>
                                                    <help>Count frames matching this rule</help>
                                                    <argument>
                                                        <name>!&lt;cr&gt;</name>
                                                        <help>Add ACL rule</help>
                                                    </argument>
                                                </argument>
                                                <argument>
                                                    <name>!&lt;cr&gt;</name>
                                                    <help>Add ACL rule without counting</help>
                                                </argument>
                                            </argument>
                                            <argument>
                                                <name>deny</name>
                                                <help>Traffic matching this rule will be dropped</help>
                                                <argument>
                                                    <name>!&lt;count&gt;</name>
                                                    <help>Count frames matching this rule</help>
                                                    <argument>
                                                        <name>!&lt;cr&gt;</name>
                                                        <help>Add ACL rule</help>
                                                    </argument>
                                                </argument>
                                                <argument>
                                                    <name>!&lt;cr&gt;</name>
                                                    <help>Add ACL rule without counting</help>
                                                </argument>
                                            </argument>
                                            <argument>
                                                <name>switch</name>
                                                <help>Traffic matching this rule will be switched</help>
                                                <argument>
                                                    <name>!&lt;destination&gt;</name>
                                                    <help>Global Destination Address</help>
                                                  <argument>
                                                    <name>!&lt;cr&gt;</name>
                                                    <help>Add ACL switch rule</help>
                                                  </argument>
                                                </argument>
                                            </argument>
END
}

sub printCr
{
    my ($verb) = @_;

    return <<END;
                            <argument>
                                <name>!&lt;cr&gt;</name>
                                <help>\u$verb given ACL</help>
                            </argument>
END

}


sub printTypeNumber
{
    my ($verb, $inner) = @_;

    foreach my $type (reverse keys %types)
    {
        print <<END;
                    <argument>
                        <name>$type</name>
                        <help>\u$verb a $types{$type}</help>
                        <argument>
                            <name>!&lt;number&gt;</name>
                            <help>The ACL to $verb</help>
$inner                        </argument>
                    </argument>
END
    }
}

sub printPortlist
{
    my ($verb, $inner) = @_;
    
    return <<END;
                        <argument>
                            <name>!&lt;port list&gt;</name>
                            <help>List of ports to $verb</help>
$inner                        </argument>
END
}

sub printSeqnum
{
    my ($verb, $inner) = @_;
    
    return <<END;
                        <argument>
                            <name>!&lt;seqNum&gt;</name>
                            <help>Sequence number</help>
$inner                        </argument>
END
}

sub printRules
{
    foreach my $type (reverse keys %types)
    {
        my $functype = $type;
        # convert smac-ethertype to smacEthertype to follow code conventions
        $functype =~ s/-(.)/\u$1/g;

        print <<END;
                <command>
                    <name>$type</name>
                    <help>Create a $types{$type}</help>
                    <handler>tp4000HandleAddAclSimpleRule\u$functype</handler>
                    <argument>
                        <name>!&lt;number&gt;</name>
                        <help>The ACL to create</help>
                        <argument>
                            <name>!&lt;seqNum&gt;</name>
                            <help>Sequence number</help>
$rules{$type}                        </argument>
                    </argument>
                </command> <!-- $type -->

END
    }
}

###############################################################################
# File output
###############################################################################


my $verb;

if ($suppressCommands == 1)
{
print <<'END';
<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE config PUBLIC "-//Fulcrum Microsystems//DTD DefaultCommands/EN" "DefaultCommands.dtd">
<!--
    vim:autoindent:et:sw=4:ts=4:tw=79:
-->
<!-- DO NOT EDIT THIS FILE! -->
<!-- It is generated by genAclSimpleCommands.xml.pl -->

<config>
</config>
END
}
else
{
    print <<'END';
<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE config PUBLIC "-//Fulcrum Microsystems//DTD DefaultCommands/EN" "DefaultCommands.dtd">
<!--
    vim:autoindent:et:sw=4:ts=4:tw=79:
-->
<!-- DO NOT EDIT THIS FILE! -->
<!-- It is generated by genAclSimpleCommands.xml.pl -->

<config>
    <command>
        <name>reset</name>
        <command>
            <name>acl-simple</name>
            <help>Add a rule to a simple ACL</help>
            <command>
                <name>association</name>
                <help>reset a simple ACL association</help>
                <handler>tp4000HandleResetAclSimple</handler>
                <argument>
                    <name>!&lt;cr&gt;</name>
                    <help>Reset all ACLs</help>
                </argument>
END

$verb = "reset";
printTypeNumber($verb, printCr($verb));

print <<END;
            </command>
        </command>                
    </command> <!-- reset -->

    <command>
        <name>set</name>
        <command>
            <name>acl-simple</name>
            <help>Add a rule to a simple ACL</help>
            <command>
                <name>association</name>
                <help>associate a simple ACL with a list of ports</help>
                <handler>tp4000HandleApplyAclSimple</handler>
END

$verb = "associate";
printTypeNumber($verb, printPortlist($verb, printCr($verb)));

print <<END;
            </command>
        </command>
    </command> <!-- set -->

    <command>
        <name>show</name>
        <command>
            <name>acl-simple</name>
            <help>Show a simple ACL</help>
            <handler>tp4000HandleShowAclSimple</handler>
            <argument>
                <name>!&lt;cr&gt;</name>
                <help>Show all ACLs</help>
            </argument>
END

$verb = "show";
printTypeNumber($verb, printCr($verb));

print <<END;
        </command>
    
        <command>
            <name>port</name>
            <command>
                <name>acl-simple</name>
                <help>Show ACLs associated with a port</help>
                <handler>tp4000HandleShowAclSimplePort</handler>
                <argument>
                    <name>!&lt;port&gt;</name>
                    <help>port list</help>
                    <argument>
                        <name>!&lt;cr&gt;</name>
                        <help>Show ACL</help>
                    </argument>
                </argument>
            </command>
        </command>
    </command> <!-- show -->


    <command>
        <name>del</name>
        <command>
            <name>acl-simple</name>
            <help>Delete a simple ACL</help>
            <handler>tp4000HandleDeleteAclSimple</handler>
            <argument>
                <name>!&lt;cr&gt;</name>
                <help>Delete all ACLs</help>
            </argument>
END

$verb = "delete";
printTypeNumber($verb, printCr($verb));

print <<END;
            <command>
                <name>rule</name>
                <help>Remove a rule from a simple ACL</help>
                <handler>tp4000HandleRemoveAclSimpleRule</handler>
END

$verb = "delete a rule from";
printTypeNumber($verb, printSeqnum($verb, printCr($verb)));

print <<END;
            </command>
        </command>
    </command> <!-- del -->


    <command>
        <name>create</name>
        <command>
            <name>acl-simple</name>
            <help>Create a simple ACL</help>
            <handler>tp4000HandleCreateAclSimple</handler>
END

$verb = "create";
printTypeNumber($verb, printCr($verb));

print <<END;
        </command>
    </command> <!-- create -->


    <command>
        <name>add</name>
        <command>
            <name>acl-simple</name>
            <help>Add a rule to a simple ACL</help>
            <command>
                <name>rule</name>
                <help>Add a rule to a simple ACL</help>
END

printRules();

print <<END;
            </command> <!-- rule -->
        </command> <!-- acl-simple -->
    </command> <!-- add -->
</config>
END
    
}

