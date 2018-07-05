#!/usr/local/bin/perl
#---------------------------------------------------------------------------
# This program generates TestPoint command XML. It takes a single command
# line argument, being the name of a command specification file. Output is
# to STDOUT and may be redirected from the command line to an XML file.
#
# The command specification file is a a database, one record per line with
# commas between the fields. Hash characters (#) act as end-of-line comment
# delimiters. Each entry in the table has the following fields:
#
#   lvl:      Ordinal level number
#   arg:      The string that appears as the field place holder
#   type:     "c" for a command, "t" for a token type argument, "v" for 
#             a value type argument (values get surrounded by angle brackets).
#   mand:     "m" for mandatory, "o" for optional [optional feature not tested].
#   default:  Default value for optional arguments, name of command handler for
#             commands.
#   help:     Help text
#
# The table must be constructed according to the following rules and 
# assumptions:
#
#   1. A small lvl value indicates a top level argument.
#   2. A larger lvl following a smaller lvl will appear as a sub-argument of
#      the previous entry.
#   3. A smaller lvl following a larger lvl termintes sub-argument nesting.
#   4. Entries at the same level must appear together in the table in the
#      order they are to appear in the XML.
#   4. A default field need be supplied only for optional arguments. This
#      field will be used to build the help text for <cr> at higher levels.
#      The content of this field may be any string, but should be as brief
#      as practical. Only one argument should have a default field at any
#      level, the rest getting a null string. [NOT TESTED]
#   5. Value type arguments may not be optional (a TestPoint restriction).
#   6. No <cr> entries are required.
#
# For example, the following specification:
#
##       lvl arg                 type    mand    default                     help
##       --- --------------      ----    ----    ------------------          ----------------------------------
#        1,  "set",              "c",    "m",    "",                         ""
#        2,  "eeprom",           "c",    "m",    "",                         "Set an EEPROM image related property"
#        3,  "config",           "c",    "m",    "handleSetEepromConfig",    "Set an EEPROM image configuration property"
#
#        4,  "size",             "t",    "m",    "",                         "Size of the EEPROM"
#        5,  "kbytes",           "v",    "m",    "",                         "EEPROM size in Kbytes"
#                                                                    
#        1,  "show",             "c",    "m",    "",                         ""
#        2,  "eeprom",           "c",    "m",    "",                         "Show an EEPROM image related property"
#        3,  "config",           "c",    "m",    "handleShowEepromConfig",   "Show the EEPROM image configuration"
#
# will produce the following XML:
#
#    <command>    <!-- set -->
#        <name>set</name>
#        <help> </help>
#        <command>    <!-- eeprom -->
#            <name>eeprom</name>
#            <help>Set an EEPROM image related property</help>
#            <command>    <!-- config -->
#                <name>config</name>
#                <help>Set an EEPROM image configuration property</help>
#                <handler>handleSetEepromConfig</handler>
#                <argument>    <!-- size -->
#                    <name>size</name>
#                    <help>Size of the EEPROM</help>
#                    <argument>    <!-- kbytes -->
#                        <name>!&lt;kbytes&gt;</name>
#                        <help>EEPROM size in Kbytes</help>
#                        <argument>    <!-- cr -->
#                            <name>!&lt;cr&gt;</name>
#                            <help> </help>
#                        </argument>    <!-- cr -->
#                    </argument>    <!-- kbytes -->
#                </argument>    <!-- size -->
#            </command>    <!-- config -->
#        </command>    <!-- eeprom -->
#    </command>    <!-- set -->
#    <command>    <!-- show -->
#        <name>show</name>
#        <help> </help>
#        <command>    <!-- eeprom -->
#            <name>eeprom</name>
#            <help>Show an EEPROM image related property</help>
#            <command>    <!-- config -->
#                <name>config</name>
#                <help>Show the EEPROM image configuration</help>
#                <handler>handleShowEepromConfig</handler>
#                <argument>    <!-- cr -->
#                    <name>!&lt;cr&gt;</name>
#                    <help> </help>
#                </argument>    <!-- cr -->
#            </command>    <!-- config -->
#        </command>    <!-- eeprom -->
#    </command>    <!-- show -->
#
#---------------------------------------------------------------------------
my @cmdFields;

#---------------------------------------------------------------------------
# Mode indicates whether the command tree should be globally fractalized
# (all level N entries contain all level N+1 entries) or grouped (a level
# N entry contains only the level N+1 entries that directly follow it in
# the table):
#
#   0 - grouped (normal mode)
#   1 - fractalized (not tested)
#
#---------------------------------------------------------------------------
my $mode = 0;

#---------------------------------------------
# Array indexes to various table entry fields.
#---------------------------------------------

my $LVL = 0;
my $NAME = 1;
my $TYPE = 2;
my $MAND = 3;
my $DEFAULT = 4;
my $HELP = 5;

#----------------------------------------------------------
# Read the specification file 
#----------------------------------------------------------
sub ReadSpec
{
    my($specFileName) = @_;
    my $line = 0;

    open SPEC,  '<', "$specFileName" or die "Cannot open $specFileName: $!";
    
    while ( <SPEC> ) 
    {
        # Delete newlines
        chomp;
        
        # Delete comments
        s/#.*//;
        
        # Delete leading white space
        s/^\s+//;
        
        # Delete trailing white space
        s/\s+$//;
        
        # If there is nothing left, go to the next line in the file.
        next unless length;
        
        # Strip double quotes
        #s/"(.+?)"/$1/g;
        s/"//g;
        
        # Otherwise, gather the fields.
        @cmdFields[$line++] = [ split(/\s*,\s*/, $_, $HELP + 1) ];
    }
    
    close(SPEC) || die "Cannot close $specFileName: $!";
    
}   # end ReadSpec


#---------------------------------------------------------------------------
# Indent by outputing a number of spaces proportional to the specified
# argument level.
#---------------------------------------------------------------------------
sub Indent
{
    my($level) = @_;
    
    for my $i ( 1 .. $level ) 
    {
        printf ("    ");
    }
}


#---------------------------------------------------------------------------
# build an array of default values at the specified level and beyond.
#---------------------------------------------------------------------------
sub GatherDefaults 
{
    my($level) = @_;
    my @defaultValues;

    for my $entry ( 0 .. $#cmdFields ) 
    {
        if ($cmdFields[$entry][$LVL] >= $level)
        {
            if ($cmdFields[$entry][$DEFAULT] ne "")
            {
                push(@defaultValues, $cmdFields[$entry][$DEFAULT]);
            }
        }
    }
    
    return @defaultValues;
    
}   # end GatherDefaults


#---------------------------------------------------------------------------
# Convert an array of strings into a single string with commas between the
# former array members.
#---------------------------------------------------------------------------
sub commas 
{
    (@_ == 0) ? '' :
    (@_ == 1) ? $_[0] :
    (@_ == 2) ? join(", ", @_) :
                join(", ", @_[0 .. ($#_)]);
                
}   # end commas


#---------------------------------------------------------------------------
# Add a <cr> argument at the specified level.
#
# Arguments:    $level is the indentation level
#               $helpText is the help text string.
#---------------------------------------------------------------------------
sub AddCr
{
    my($level, $helpText) = @_;
    
    Indent($level);
    print ("<argument>    <!-- cr -->\n");
    
    Indent($level + 1);
    print ("<name>!&lt;cr&gt;</name>\n");
    
    Indent($level + 1);
    print ("<help>");
    
    if (length($helpText) > 0)
    {
        print ("$helpText");
    }
    else
    {
        print (" ");
    }
    
    print ("</help>\n");
    
    Indent($level);
    print ("</argument>    <!-- cr -->\n");
    
}   # end AddCr


#---------------------------------------------------------------------------
# Add an argument, name and help tag.
#---------------------------------------------------------------------------
sub TagIn
{
    my($entry) = @_;
    
    # indent per level of this entry.
    Indent($cmdFields[$entry][$LVL]);
    
    if ($cmdFields[$entry][$TYPE] eq "c")
    {
        print ("<command>");
    }
    else
    {
        print ("<argument>");
    }
    
    print ("    <!-- $cmdFields[$entry][$NAME] -->\n");
 
    # Name of argument
    Indent($cmdFields[$entry][$LVL] + 1);
    print ("<name>");
    
    # Format name differently if variable or token
    if ($cmdFields[$entry][$TYPE] eq "v")
    {
        # Variables get angle bracket codes around it.
        print ("!&lt;$cmdFields[$entry][$NAME]&gt;");
    }
    else
    {
        # Tokens do not get angle bracket codes.
        print ("$cmdFields[$entry][$NAME]");
    }
    
    # Close off the <name> tag
    print ("</name>\n");
    
    # Help text for argument.
    Indent($cmdFields[$entry][$LVL] + 1);
    print ("<help>");
    
    if (length($cmdFields[$entry][$HELP]) > 0)
    {
        print ("$cmdFields[$entry][$HELP]");
    }
    else
    {
        print (" ");
    }
    
    print ("</help>\n");
    
    # Command handler.
    if ($cmdFields[$entry][$TYPE] eq "c")
    {
        if (length($cmdFields[$entry][$DEFAULT]) > 0)
        {
            Indent($cmdFields[$entry][$LVL] + 1);
            print ("<handler>$cmdFields[$entry][$DEFAULT]</handler>\n");
        }
    }
    
}   # end TagIn


#---------------------------------------------------------------------------
# Add an end argument tag.
#---------------------------------------------------------------------------
sub TagOut
{
    my($entry) = @_;
    
    # Close off the <argument> tag.
    Indent($cmdFields[$entry][$LVL]);
    
    if ($cmdFields[$entry][$TYPE] eq "c")
    {
        print ("</command>");
    }
    else
    {
        print ("</argument>");
    }
    
    print ("    <!-- $cmdFields[$entry][$NAME] -->\n");
    
}   # end TagOut



#---------------------------------------------------------------------------
# Process argument from table entry. This subroutine calls itself
# recursively in order to nest table entries.
#---------------------------------------------------------------------------
sub procFractal
{
    # Get the table entry number
    my($entry) = @_;
    
    TagIn($entry);
    
    # If there are any subsequent table entries at the next level,
    # process them recursively.
    for my $i ( $entry .. $#cmdFields ) 
    {
        if ($cmdFields[$i][$LVL] == $cmdFields[$entry][$LVL] + 1)
        {
            # An entry at the next level was found. Nest it.
            procFractal($i);
        }
    }
        
    TagOut($entry);
    
    # If this was an optional argument, and there are no more at this
    # level, add a cr option.
    if ($cmdFields[$entry][$MAND] eq "o")
    {
        if ($cmdFields[$entry][$LVL] != $cmdFields[$entry + 1][$LVL])
        {
            #-------------------------------------------------
            # We want the help text of <cr> to indicate what
            # defaults will be applied, so build a list of
            # default values for the remaining optional
            # arguments.
            #-------------------------------------------------

            my @defaults = GatherDefaults($cmdFields[$entry][$LVL]);

            #-------------------------------------------------
            # Now write the <cr> argument.
            #-------------------------------------------------
            
            my $helpDefaults = commas(@defaults);
            AddCr($cmdFields[$entry][$LVL], "Execute with $helpDefaults");
            
        }   # end if ($cmdFields[$entry][$LVL] != $cmdFields[$entry + 1][$LVL])
        
    }   # end if ($cmdFields[$entry][$MAND] eq "o")
    
}   # end procFractal


#---------------------------------------------------------------------------
# Process argument from table entry. This subroutine calls itself
# recursively in order to nest table entries.
#
# Returns the table entry number we bailed out at.
#---------------------------------------------------------------------------
sub procGroup
{
    # Get the table entry number
    my($entry) = @_;
    my $didSubArgument = 0;
    
    TagIn($entry);
    
    # If there are any subsequent table entries at the next level,
    # process them recursively.
    if ($entry < $#cmdFields)
    {
        for my $i ( $entry + 1 .. $#cmdFields ) 
        {
            if ($cmdFields[$i][$LVL] == -1)
            {
                next;
            }
            elsif ($cmdFields[$i][$LVL] > $cmdFields[$entry][$LVL] + 1)
            {
                # Skip entries below the next level.
                next;
            }
            elsif ($cmdFields[$i][$LVL] == $cmdFields[$entry][$LVL] + 1)
            {
                # An entry at the next level was found. Nest it.
                procGroup($i);
                $didSubArgument = 1;
            }
            else
            {
                # Next entry is at the same or higher level.
                last;
            }
        }
    }
        
    # If there were no sub-arguments...
    if ($didSubArgument == 0)
    {
        # ...add a final <cr> argument.
        AddCr($cmdFields[$entry][$LVL] + 1, "");
    }
    
    # Close off the <argument> tag.
    TagOut($entry);
    
    # Mark this entry as done.
    $cmdFields[$entry][$LVL] = -1;
    
}   # end procGroup


#---------------------------------------------------------------------------
# Main entry point. Process all lvl 1 table entries.
#---------------------------------------------------------------------------

my($specFileName) = $ARGV[0];

ReadSpec($specFileName);
    
# If in fractal mode
if ($mode == 1)
{
    for my $i ( 0 .. $#cmdFields ) 
    {
        # Process all level 1 entries in the table.
        if ($cmdFields[$i][$LVL] == 1)
        {
            procFractal($i);
        }
    }
}
else
{
    print <<LEADIN;
<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE config PUBLIC "-//Fulcrum Microsystems//DTD DefaultCommands/EN" "DefaultCommands.dtd">
<!--
    vim:autoindent:et:sw=4:ts=4:tw=79:
-->

LEADIN
    
    printf ("\n<!--\n");
    printf ("     ********************************************************\n");
    printf ("       This file was auto-generated from the TestPoint command\n");
    printf ("       specification file, $specFileName, using %s.\n", __FILE__);
    printf ("     ********************************************************\n");
    printf ("-->\n\n");
    
    print ("<config>\n");
    
    for my $i ( 0 .. $#cmdFields ) 
    {
        if ($cmdFields[$i][$LVL] != -1)
        {
            # if ($cmdFields[$i][$LVL] == 1)
            # {
                # print ("\n<!-- ------------------------------ -->\n\n");
            # }
            
            procGroup($i);
        }
    }
    
    print ("\n</config>\n");
}

