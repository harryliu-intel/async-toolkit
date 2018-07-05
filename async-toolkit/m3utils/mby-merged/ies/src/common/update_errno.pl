#!/usr/bin/perl -w
# File:           update_errno.pl
# Creation Date:  March 19, 2007
# Description:    Update fm_errno.c to make sure it has a string for
#                 every constant in fm_errno.h
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

use strict;
use vars qw($ErrnoC $ErrnoH %First %Last $Comment $OpenComment %Description);
use vars qw($Category @Categories $InCategory @Lines %BeforeLines %ErrList);
use vars qw($Modified %StatusCode $InStatusCodes %DefinedCodes $Code $Line);

use FindBin;

$ErrnoC = $FindBin::Bin . "/fm_errno.c";
$ErrnoH = $FindBin::Bin . "/../../include/common/fm_errno.h";

$OpenComment = 0;
$InStatusCodes = 0;

open H, $ErrnoH or die;
while (<H>) {
    if (/^\#define\s+FM_FIRST_(\w+)_ERROR\s+(\d+)/) {
        $First{lc($1)} = $2;
    } elsif (/^\#define\s+FM_LAST_(\w+)_ERROR\s+(\d+)/) {
        $Last{lc($1)} = $2;
    } elsif (m%^/\*\*\s*(.*?)\s*\*/%) {
        $Comment = $1;
        $OpenComment = 0;
    } elsif (m%^/\*\*\s*(.*?)\s*$%) {
        $Comment = $1;
        $OpenComment = 1;
    } elsif ($OpenComment and m%^\s*\**\s*(.*?)\s*\*/%) {
        $Comment .= " ";
        $Comment .= $1;
        $OpenComment = 0;
    } elsif ($OpenComment and m%^\s*\**\s*(.*?)\s*$%) {
        $Comment .= " ";
        $Comment .= $1;
        $OpenComment = 1;
    } elsif (/^\#define\s+(\w+)\s+(\d+)/) {
        $Description{$2} = $Comment;
        $StatusCode{$2} = $1;
    }
}
close H;

open C, $ErrnoC or die;
while (<C>) {
    push @Lines, $_;

    if (/^static\s+.*?fm(\w+)ErrorStrings\[\]\s*\=\s*$/) {
        $Category = lc($1);
        $Category = "alos" if ($Category eq "os"); # fix inconsistent naming
    } elsif (/^\{\s*$/ and defined $Category) {
        $BeforeLines{$Category} = [ @Lines ];
        push @Categories, $Category;
        $InCategory = 1;
    } elsif ($InCategory and /^\}\;\s*$/) {
        $InCategory = 0;
        $Category = undef;
        @Lines = ("};\n");
    } elsif ($InCategory and /^\s*\"(.*?)\"\s*,?\s*$/) {
        push @{$ErrList{$Category}}, $1;
    } elsif (/^static const statusCode\s+fmStatusCodeNames/) {
        $InStatusCodes = 1;
    } elsif (m%^\s+/\* This entry must be last \*/%) {
        $InStatusCodes = 0;
    } elsif ($InStatusCodes and /^\s+\{\s*\"(\w+)\",/) {
        $DefinedCodes{$1} = 1;
    }
}
close C;

$Modified = 0;

# Update error strings
foreach $Category (@Categories) {
    my $errs = $ErrList{$Category};
    my $first = $First{$Category};
    my $last = $Last{$Category};
    my $errno;
    my $i;

    # Use the Doxygen comment for any error which does not already
    # have a string.
    foreach $errno (sort keys %Description) {
        if ($errno >= $first and $errno <= $last) {
            my $idx = $errno - $first;
            if (not defined $errs->[$idx] or
                $errs->[$idx] =~ /^Unknown Error \d+$/) {
                my $desc = $Description{$errno};
                $desc =~ s/\.$//; # delete trailing period
                $desc =~ s/\\/\\\\/g; # escape backslashes
                $desc =~ s/\"/\\\"/g; # escape double quotes
                $Modified = 1;
                $errs->[$idx] = $desc;
            }
        }
    }

    # Fill in any gaps this created
    for ($i = 0; $i <= $#$errs; $i++) {
        if (not defined $errs->[$i]) {
            $errs->[$i] = sprintf("Unknown Error %d", $i + $first);
        }
    }
}

# Check if all status codes have been defined
foreach $Code (sort values %StatusCode) {
    if (not exists $DefinedCodes{$Code}) {
        $Modified = 1;          # missing code, means we need to update file
    }
}

if (not $Modified) {
    print "$ErrnoC is up-to-date.\n";
} elsif (not -w $ErrnoC) {
    die "Can't write $ErrnoC\n", "Maybe you need to 'p4 edit' it?\n";
} else {
    open C, ">$ErrnoC" or die;
    foreach $Category (@Categories) {
        print C join("", @{$BeforeLines{$Category}});
        my $errs = $ErrList{$Category};
        my $first = $First{$Category};
        my $i;
        for ($i = 0; $i <= $#$errs; $i++) {
            printf C "    /* %d */\n", $first + $i if ($i % 5 == 0);
            print C '    "', $errs->[$i], "\",\n";
        }
    }

    $InStatusCodes = 0;
    foreach $Line (@Lines) {
        print C $Line unless ($InStatusCodes);

        if ($Line =~ /^static const statusCode\s+fmStatusCodeNames/) {
            $InStatusCodes = 1;
            print C "{\n";
            foreach $Code (sort {$a <=> $b} keys %StatusCode) {
                my $name = $StatusCode{$Code};
                my $quoted = '"' . $name . '",';
                printf C "    { %-38s %-35s },\n", $quoted, $name;
            }
        } elsif ($Line =~ m%^\s+/\* This entry must be last \*/%) {
            $InStatusCodes = 0;
            print C "\n";
            print C $Line;
        }
    }

    close C;
    print "Updated $ErrnoC\n";
}
